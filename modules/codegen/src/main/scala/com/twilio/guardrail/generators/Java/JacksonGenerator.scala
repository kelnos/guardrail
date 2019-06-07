package com.twilio.guardrail
package generators
package Java

import _root_.io.swagger.v3.oas.models.media._
import cats.data.NonEmptyList
import cats.implicits._
import cats.~>
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, UnknownType }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.{ Node, NodeList }
import com.twilio.guardrail.Discriminator
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.protocol._
import java.util
import scala.collection.JavaConverters._
import scala.compat.java8.OptionConverters._
import scala.language.existentials

object JacksonGenerator {
  private val BUILDER_TYPE = JavaParser.parseClassOrInterfaceType("Builder")
  private val PROPERTY_VALUE_TYPE = JavaParser.parseClassOrInterfaceType("JacksonSupport.PropertyValue")
  private val PROPERTY_VALUE_TYPE_DIAMONDED = PROPERTY_VALUE_TYPE.clone().setTypeArguments(new NodeList[Type])

  private def propertyValueType(of: Type): ClassOrInterfaceType = PROPERTY_VALUE_TYPE.clone().setTypeArguments(of)

  private case class ParameterTerm(propertyName: String,
                                   parameterName: String,
                                   fieldType: Type,
                                   parameterType: Type,
                                   defaultValue: Option[Expression],
                                   presence: ParameterPresence,
                                   dataRedacted: RedactionBehaviour)

  // returns a tuple of (requiredTerms, optionalTerms)
  // note that required terms _that have a default value_ are conceptually optional.
  private def sortParams(params: List[ProtocolParameter[JavaLanguage]]): (List[ParameterTerm], List[ParameterTerm]) = {
    def defaultValueToExpression(defaultValue: Option[Node], presence: ParameterPresence): Option[Expression] = defaultValue match {
      case Some(mce: MethodCallExpr) if presence == ParameterPresence.OptionalNullable && mce.getScope.asScala.exists(_.toString == "java.util.Optional") =>
        if (mce.getNameAsString == "empty") {
          Some(new ObjectCreationExpr(null, PROPERTY_VALUE_TYPE_DIAMONDED, new NodeList))
        } else {
          Some(new ObjectCreationExpr(null, PROPERTY_VALUE_TYPE_DIAMONDED, new NodeList(mce)))
        }
      case Some(expr: Expression) => Some(expr)
      case _                      => None
    }

    params
      .map({
        case ProtocolParameter(term, name, _, presence, _, _, dataRedaction, selfDefaultValue) =>
          val fieldType = presence match {
            case ParameterPresence.OptionalNullable =>
              propertyValueType(if (term.getType.isOptional) term.getType.containedType else term.getType)
            case _ =>
              term.getType.unbox
          }
          val parameterType = presence match {
            case ParameterPresence.Required => term.getType.unbox
            case _ => term.getType.containedType.unbox
          }
          val defaultValue = defaultValueToExpression(selfDefaultValue, presence)

          ParameterTerm(name, term.getNameAsString, fieldType, parameterType, defaultValue, presence, dataRedaction)
      })
      .partition(
        pt => !pt.fieldType.isOptional && pt.defaultValue.isEmpty
      )
  }

  private def addParents(cls: ClassOrInterfaceDeclaration, parentOpt: Option[SuperClass[JavaLanguage]]): Unit =
    parentOpt.foreach({ parent =>
      val directParent = JavaParser.parseClassOrInterfaceType(parent.clsName)
      val otherParents = parent.interfaces.map(JavaParser.parseClassOrInterfaceType)
      cls.setExtendedTypes(new NodeList(directParent))
      cls.setImplementedTypes(otherParents.toNodeList)
    })

  private def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta[JavaLanguage]])(f: Type => Target[Type]): Option[Target[Type]] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)
      .map(f)

  // TODO: handle emptyToNull in the return for the getters
  private def addParameterGetter(cls: ClassOrInterfaceDeclaration, param: ParameterTerm): Unit =
    cls
      .addMethod(s"get${param.parameterName.unescapeIdentifier.capitalize}", PUBLIC)
      .setType(param.fieldType)
      .setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(new FieldAccessExpr(new ThisExpr, param.parameterName))
          )
        )
      )

  private def dtoConstructorBody(superCall: Expression, terms: List[ParameterTerm]): BlockStmt =
    new BlockStmt(
      (
        List[Statement](new ExpressionStmt(superCall)) ++
          terms
            .map(
              term =>
                new ExpressionStmt(
                  new AssignExpr(
                    new FieldAccessExpr(new ThisExpr, term.parameterName),
                    term.fieldType match {
                      case _: PrimitiveType => new NameExpr(term.parameterName)
                      case ft if ft.isOptional =>
                        new ConditionalExpr(
                          new BinaryExpr(new NameExpr(term.parameterName), new NullLiteralExpr, BinaryExpr.Operator.EQUALS),
                          new MethodCallExpr(new NameExpr("Optional"), "empty"),
                          new NameExpr(term.parameterName)
                        )
                      case _ => requireNonNullExpr(term.parameterName)
                    },
                    AssignExpr.Operator.ASSIGN
                  )
              )
            )
      ).toNodeList
    )

  object EnumProtocolTermInterp extends (EnumProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: EnumProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractEnum(swagger) =>
        val enumEntries: Option[List[String]] = swagger match {
          case x: StringSchema =>
            Option[java.util.List[String]](x.getEnum()).map(_.asScala.toList)
          case x =>
            Option[java.util.List[_]](x.getEnum()).map(_.asScala.toList.map(_.toString()))
        }
        Target.pure(Either.fromOption(enumEntries, "Model has no enumerations"))

      case RenderMembers(clsName, elems) =>
        Target.pure(None)

      case EncodeEnum(clsName) =>
        Target.pure(None)

      case DecodeEnum(clsName) =>
        Target.pure(None)

      case RenderClass(clsName, tpe, elems) =>
        val enumType = JavaParser.parseType(clsName)

        val enumDefns = elems.map {
          case (value, termName, _) =>
            new EnumConstantDeclaration(
              new NodeList(),
              new SimpleName(termName.getIdentifier),
              new NodeList(new StringLiteralExpr(value)),
              new NodeList()
            )
        }

        val nameField = new FieldDeclaration(
          util.EnumSet.of(PRIVATE, FINAL),
          new VariableDeclarator(STRING_TYPE, "name")
        )

        val constructor = new ConstructorDeclaration(util.EnumSet.of(PRIVATE), clsName)
        constructor.addParameter(new Parameter(util.EnumSet.of(FINAL), STRING_TYPE, new SimpleName("name")))
        constructor.setBody(
          new BlockStmt(
            new NodeList(
              new ExpressionStmt(
                new AssignExpr(
                  new FieldAccessExpr(new ThisExpr, "name"),
                  new NameExpr("name"),
                  AssignExpr.Operator.ASSIGN
                )
              )
            )
          )
        )

        val getNameMethod = new MethodDeclaration(
          util.EnumSet.of(PUBLIC),
          STRING_TYPE,
          "getName"
        )
        getNameMethod.addMarkerAnnotation("JsonValue")
        getNameMethod.setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(new FieldAccessExpr(new ThisExpr, "name"))
            )
          )
        )

        val parseMethod = new MethodDeclaration(
          util.EnumSet.of(PUBLIC, STATIC),
          enumType,
          "parse"
        )
        parseMethod.addMarkerAnnotation("JsonCreator")
        parseMethod.addParameter(new Parameter(util.EnumSet.of(FINAL), STRING_TYPE, new SimpleName("name")))
        parseMethod.setBody(
          new BlockStmt(
            new NodeList(
              new ForEachStmt(
                new VariableDeclarationExpr(new VariableDeclarator(enumType, "value"), FINAL),
                new MethodCallExpr("values"),
                new BlockStmt(
                  new NodeList(
                    new IfStmt(
                      new MethodCallExpr("value.name.equals", new NameExpr("name")),
                      new ReturnStmt(new NameExpr("value")),
                      null
                    )
                  )
                )
              ),
              new ThrowStmt(
                new ObjectCreationExpr(
                  null,
                  JavaParser.parseClassOrInterfaceType("IllegalArgumentException"),
                  new NodeList(
                    new BinaryExpr(
                      new BinaryExpr(new StringLiteralExpr("Name '"), new NameExpr("name"), BinaryExpr.Operator.PLUS),
                      new StringLiteralExpr(s"' is not valid for enum '${clsName}'"),
                      BinaryExpr.Operator.PLUS
                    )
                  )
                )
              )
            )
          )
        )

        val staticInitializer = new InitializerDeclaration(
          true,
          new BlockStmt(
            new NodeList(
              new ExpressionStmt(
                new MethodCallExpr(
                  new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
                  "register",
                  new NodeList[Expression](
                    new ClassExpr(JavaParser.parseClassOrInterfaceType(clsName)),
                    new MethodReferenceExpr(new NameExpr(clsName), null, "getName")
                  )
                )
              )
            )
          )
        )

        val enumClass = new EnumDeclaration(
          util.EnumSet.of(PUBLIC),
          new NodeList(),
          new SimpleName(clsName),
          new NodeList(),
          new NodeList(enumDefns: _*),
          new NodeList(
            staticInitializer,
            nameField,
            constructor,
            getNameMethod,
            parseMethod
          )
        )

        Target.pure(enumClass)

      case RenderStaticDefns(clsName, members, accessors, encoder, decoder) =>
        for {
          extraImports <- List(
            "com.fasterxml.jackson.annotation.JsonCreator",
            "com.fasterxml.jackson.annotation.JsonValue"
          ).traverse(safeParseRawImport)
        } yield
          StaticDefns[JavaLanguage](
            className = clsName,
            extraImports = extraImports,
            definitions = List.empty
          )

      case BuildAccessor(clsName, termName) =>
        Target.pure(new Name(s"${clsName}.${termName}"))
    }
  }

  private def renderDTOClass(clsName: String,
                             selfParams: List[ProtocolParameter[JavaLanguage]],
                             parents: List[SuperClass[JavaLanguage]]): Target[TypeDeclaration[_ <: TypeDeclaration[_]]] = {
    val parentsWithDiscriminators = parents.collect({ case p if p.discriminators.nonEmpty => p })
    for {
      dtoClassType <- safeParseClassOrInterfaceType(clsName)
      parentOpt <- (parentsWithDiscriminators, parents) match {
        case _ if parentsWithDiscriminators.length > 1 =>
          Target.raiseError[Option[SuperClass[JavaLanguage]]](
            s"${clsName} requires unsupported multiple inheritance due to multiple parents with discriminators (${parentsWithDiscriminators.map(_.clsName).mkString(", ")})"
          )
        case _ if parentsWithDiscriminators.length == 1 => Target.pure(parentsWithDiscriminators.headOption)
        case _ if parents.length == 1                   => Target.pure(parents.headOption)
        case _                                          => Target.pure(None)
      }
    } yield {
      val discriminators                             = parents.flatMap(_.discriminators)
      val discriminatorNames                         = discriminators.map(_.propertyName).toSet
      val parentParams                               = parentOpt.toList.flatMap(_.params)
      val parentParamNames                           = parentParams.map(_.name)
      val (parentRequiredTerms, parentOptionalTerms) = sortParams(parentParams)
      val parentTerms                                = parentRequiredTerms ++ parentOptionalTerms
      val params = parents.filterNot(parent => parentOpt.contains(parent)).flatMap(_.params) ++ selfParams.filterNot(
        param => discriminatorNames.contains(param.term.getName.getIdentifier) || parentParamNames.contains(param.term.getName.getIdentifier)
      )
      val (requiredTerms, optionalTerms) = sortParams(params)
      val terms                          = requiredTerms ++ optionalTerms

      val dtoClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), false, clsName)
      dtoClass.addAnnotation(
        new NormalAnnotationExpr(
          new Name("JsonIgnoreProperties"),
          new NodeList(
            new MemberValuePair(
              "ignoreUnknown",
              new BooleanLiteralExpr(true)
            )
          )
        )
      )

      addParents(dtoClass, parentOpt)

      def withoutDiscriminators(terms: List[ParameterTerm]): List[ParameterTerm] =
        terms.filterNot(term => discriminatorNames.contains(term.propertyName))

      terms.foreach({
        case ParameterTerm(propertyName, parameterName, fieldType, _, _, presence, _) =>
          dtoClass.addField(fieldType, parameterName, PRIVATE, FINAL)
            .addSingleMemberAnnotation("JsonProperty", new StringLiteralExpr(propertyName))
            .addSingleMemberAnnotation("JsonInclude", new FieldAccessExpr(
              new FieldAccessExpr(new NameExpr("JsonInclude"), "Include"),
              presence match {
                case ParameterPresence.Required => "ALWAYS"
                case ParameterPresence.RequiredNullable => "ALWAYS"
                case ParameterPresence.Optional => "NON_ABSENT"
                case ParameterPresence.OptionalNullable => "NON_ABSENT"
              }
            ))
      })

      val primaryConstructor = dtoClass.addConstructor(PROTECTED)
      primaryConstructor.addMarkerAnnotation("JsonCreator")
      primaryConstructor.setParameters(
        new NodeList(
          withoutDiscriminators(parentTerms ++ terms).map({
            case ParameterTerm(propertyName, parameterName, fieldType, _, _, _, _) =>
              new Parameter(util.EnumSet.of(FINAL), fieldType, new SimpleName(parameterName))
                .addAnnotation(new SingleMemberAnnotationExpr(new Name("JsonProperty"), new StringLiteralExpr(propertyName)))
          }): _*
        )
      )
      val superCall = new MethodCallExpr(
        "super",
        parentTerms.map({ term =>
          discriminators
            .find(_.propertyName == term.propertyName)
            .fold[Expression](new NameExpr(term.parameterName))(
              discriminator =>
                new StringLiteralExpr(
                  discriminator.mapping
                    .collectFirst({ case (value, elem) if elem.name == clsName => value })
                    .getOrElse(clsName)
              )
            )
        }): _*
      )
      primaryConstructor.setBody(dtoConstructorBody(superCall, terms))

      terms.foreach(addParameterGetter(dtoClass, _))

      def parameterGetterCall(term: ParameterTerm, scope: Option[String] = None): MethodCallExpr = {
        val methodName = s"get${term.parameterName.unescapeIdentifier.capitalize}"
        scope.fold(new MethodCallExpr(methodName))(s => new MethodCallExpr(new NameExpr(s), methodName))
      }

      def parameterToStringExpr(term: ParameterTerm, scope: Option[String] = None): Expression = term.dataRedacted match {
        case DataVisible  => parameterGetterCall(term, scope)
        case DataRedacted => new StringLiteralExpr("[redacted]")
      }

      val toStringFieldExprs = NonEmptyList
        .fromList(parentTerms ++ terms)
        .toList
        .flatMap(
          l =>
            (new StringLiteralExpr(s"${l.head.parameterName}="), parameterToStringExpr(l.head)) +:
              l.tail.map(
              term =>
                (
                  new StringLiteralExpr(s", ${term.parameterName}="),
                  parameterToStringExpr(term)
              )
          )
        )

      val toStringMethod = dtoClass
        .addMethod("toString", PUBLIC)
        .setType(STRING_TYPE)
        .addMarkerAnnotation("Override")
      toStringMethod.setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(
              new BinaryExpr(
                toStringFieldExprs.foldLeft[Expression](new StringLiteralExpr(s"${clsName}{"))({
                  case (prevExpr, (strExpr, fieldExpr)) =>
                    new BinaryExpr(
                      new BinaryExpr(prevExpr, strExpr, BinaryExpr.Operator.PLUS),
                      fieldExpr,
                      BinaryExpr.Operator.PLUS
                    )
                }),
                new StringLiteralExpr("}"),
                BinaryExpr.Operator.PLUS
              )
            )
          )
        )
      )

      val equalsConditions: List[Expression] = terms.map(
        term =>
          term.fieldType match {
            case _: PrimitiveType =>
              new BinaryExpr(
                parameterGetterCall(term),
                parameterGetterCall(term, Some("other")),
                BinaryExpr.Operator.EQUALS
              )
            case _ =>
              new MethodCallExpr(
                parameterGetterCall(term),
                "equals",
                new NodeList[Expression](parameterGetterCall(term, Some("other")))
              )
        }
      )
      val returnExpr = NonEmptyList
        .fromList(equalsConditions)
        .map(
          _.reduceLeft(
            (prevExpr, condExpr) => new BinaryExpr(prevExpr, condExpr, BinaryExpr.Operator.AND)
          )
        )
        .getOrElse(new BooleanLiteralExpr(true))

      val equalsMethod = dtoClass
        .addMethod("equals", PUBLIC)
        .setType(PrimitiveType.booleanType)
        .addMarkerAnnotation("Override")
        .addParameter(new Parameter(util.EnumSet.of(FINAL), OBJECT_TYPE, new SimpleName("o")))
      equalsMethod.setBody(
        new BlockStmt(
          new NodeList(
            new IfStmt(
              new BinaryExpr(new ThisExpr, new NameExpr("o"), BinaryExpr.Operator.EQUALS),
              new BlockStmt(new NodeList(new ReturnStmt(new BooleanLiteralExpr(true)))),
              null
            ),
            new IfStmt(
              new BinaryExpr(
                new BinaryExpr(new NameExpr("o"), new NullLiteralExpr, BinaryExpr.Operator.EQUALS),
                new BinaryExpr(new MethodCallExpr("getClass"), new MethodCallExpr(new NameExpr("o"), "getClass"), BinaryExpr.Operator.NOT_EQUALS),
                BinaryExpr.Operator.OR
              ),
              new BlockStmt(new NodeList(new ReturnStmt(new BooleanLiteralExpr(false)))),
              null
            ),
            new ExpressionStmt(
              new VariableDeclarationExpr(new VariableDeclarator(
                                            dtoClassType,
                                            "other",
                                            new CastExpr(dtoClassType, new NameExpr("o"))
                                          ),
                                          FINAL)
            ),
            new ReturnStmt(returnExpr)
          )
        )
      )

      val hashCodeMethod = dtoClass
        .addMethod("hashCode", PUBLIC)
        .setType(PrimitiveType.intType)
        .addMarkerAnnotation("Override")
      hashCodeMethod.setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(
              new MethodCallExpr(
                new NameExpr("java.util.Objects"),
                "hash",
                new NodeList[Expression]((parentTerms ++ terms).map(parameterGetterCall(_, None)): _*)
              )
            )
          )
        )
      )

      val builderClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, STATIC), false, "Builder")

      withoutDiscriminators(parentRequiredTerms ++ requiredTerms).foreach({
        case ParameterTerm(_, parameterName, fieldType, _, _, _, _) =>
          builderClass.addField(fieldType, parameterName, PRIVATE)
      withoutDiscriminators(parentOptionalTerms ++ optionalTerms).foreach({
        case ParameterTerm(_, parameterName, fieldType, _, defaultValue, presence, _) =>
          val initializer = presence match {
            case ParameterPresence.OptionalNullable =>
              defaultValue.getOrElse[Expression](new ObjectCreationExpr(null, PROPERTY_VALUE_TYPE_DIAMONDED, new NodeList))
            case _ =>
              defaultValue.getOrElse[Expression](new MethodCallExpr(new NameExpr("java.util.Optional"), "empty"))
          }
          builderClass.addFieldWithInitializer(fieldType, parameterName, initializer, PRIVATE)
      })

      val builderConstructor = builderClass.addConstructor(PUBLIC)
      builderConstructor.setParameters(
        new NodeList(
          withoutDiscriminators(parentRequiredTerms ++ requiredTerms).map({
            case ParameterTerm(_, parameterName, _, parameterType, _, _, _) =>
              new Parameter(util.EnumSet.of(FINAL), parameterType, new SimpleName(parameterName))
          }): _*
        )
      )
      builderConstructor.setBody(
        new BlockStmt(
          new NodeList(
            withoutDiscriminators(parentRequiredTerms ++ requiredTerms).map({
              case ParameterTerm(_, parameterName, fieldType, _, _, _, _) =>
                new ExpressionStmt(
                  new AssignExpr(
                    new FieldAccessExpr(new ThisExpr, parameterName),
                    fieldType match {
                      case _: PrimitiveType => new NameExpr(parameterName)
                      case _                => requireNonNullExpr(parameterName)
                    },
                    AssignExpr.Operator.ASSIGN
                  )
                )
            }): _*
          )
        )
      )

      builderClass
        .addConstructor(PUBLIC)
        .setParameters(new NodeList(new Parameter(util.EnumSet.of(FINAL), dtoClassType, new SimpleName("template"))))
        .setBody(
          new BlockStmt(
            withoutDiscriminators(parentTerms ++ terms)
              .map({
                case term @ ParameterTerm(_, parameterName, _, _, _, _, _) =>
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, parameterName),
                      parameterGetterCall(term, Some("template")),
                      AssignExpr.Operator.ASSIGN
                    )
                  ): Statement
              })
              .toNodeList
          )
        )

      // TODO: leave out with${name}() if readOnlyKey?
      withoutDiscriminators(parentTerms ++ terms).foreach({
        case ParameterTerm(_, parameterName, fieldType, parameterType, _, _, _) =>
          val methodName = s"with${parameterName.unescapeIdentifier.capitalize}"

          builderClass
            .addMethod(methodName, PUBLIC)
            .setType(BUILDER_TYPE)
            .addParameter(new Parameter(util.EnumSet.of(FINAL), parameterType, new SimpleName(parameterName)))
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, parameterName),
                      (fieldType, parameterType) match {
                        case (_: PrimitiveType, _) => new NameExpr(parameterName)
                        case (ft, pt) if ft.isOptional && pt.isPrimitiveType =>
                          new MethodCallExpr(new NameExpr("Optional"), "of", new NodeList[Expression](new NameExpr(parameterName)))
                        case (ft, _) if ft.isOptional => optionalOfExpr(new NameExpr(parameterName))
                        case _                        => requireNonNullExpr(parameterName)
                      },
                      AssignExpr.Operator.ASSIGN
                    )
                  ),
                  new ReturnStmt(new ThisExpr)
                )
              )
            )

          if (!parameterType.isOptional) {
            val newParameterName = s"optional${parameterName.unescapeIdentifier.capitalize}"
            val newParameterType = fieldType match {
              case pt: PrimitiveType   => optionalType(pt.toBoxedType)
              case ft if ft.isOptional => ft
              case ft                  => optionalType(ft)
            }
            builderClass
              .addMethod(methodName, PUBLIC)
              .setType(BUILDER_TYPE)
              .addParameter(new Parameter(util.EnumSet.of(FINAL), newParameterType, new SimpleName(newParameterName)))
              .setBody(
                new BlockStmt(
                  new NodeList(
                    new ExpressionStmt(
                      if (fieldType.isOptional) {
                        new AssignExpr(
                          new FieldAccessExpr(new ThisExpr, parameterName),
                          requireNonNullExpr(newParameterName),
                          AssignExpr.Operator.ASSIGN
                        )
                      } else {
                        new MethodCallExpr(
                          requireNonNullExpr(newParameterName),
                          "ifPresent",
                          new NodeList[Expression](
                            new LambdaExpr(
                              new NodeList(new Parameter(new UnknownType, parameterName)),
                              new ExpressionStmt(
                                new AssignExpr(
                                  new FieldAccessExpr(new ThisExpr, parameterName),
                                  new NameExpr(parameterName),
                                  AssignExpr.Operator.ASSIGN
                                )
                              ),
                              false
                            )
                          )
                        )
                      }
                    ),
                    new ReturnStmt(new ThisExpr)
                  )
                )
              )
          }
      })

      val builderBuildTerms = withoutDiscriminators(parentTerms ++ terms)
      builderClass
        .addMethod("build", PUBLIC)
        .setType(clsName)
        .setBody(
          new BlockStmt(
            (
              builderBuildTerms
                .filterNot(_.fieldType.isPrimitiveType)
                .map(term => new ExpressionStmt(requireNonNullExpr(new FieldAccessExpr(new ThisExpr, term.parameterName)))) :+ new ReturnStmt(
                new ObjectCreationExpr(
                  null,
                  JavaParser.parseClassOrInterfaceType(clsName),
                  new NodeList(
                    builderBuildTerms.map(param => new FieldAccessExpr(new ThisExpr, param.parameterName)): _*
                  )
                )
              )
            ).toNodeList
          )
        )

      dtoClass.addMember(builderClass)

      dtoClass
    }
  }

  object ModelProtocolTermInterp extends (ModelProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ModelProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractProperties(swagger) =>
        (swagger match {
          case m: ObjectSchema => Target.pure(Option(m.getProperties))
          case comp: ComposedSchema =>
            Target.pure(Option(comp.getAllOf).flatMap(_.asScala.toList.lastOption).flatMap(prop => Option(prop.getProperties)))
          case comp: Schema[_] if Option(comp.get$ref).isDefined =>
            Target.raiseError(s"Attempted to extractProperties for a ${comp.getClass()}, unsure what to do here")
          case _ => Target.pure(None)
        }).map(_.map(_.asScala.toList).toList.flatten)

      case RenderDTOClass(clsName, selfParams, parents) =>
        renderDTOClass(clsName, selfParams, parents)

      case EncodeModel(clsName, selfParams, parents) =>
        Target.pure(None)

      case DecodeModel(clsName, selfParams, parents) =>
        Target.pure(None)

      case RenderDTOStaticDefns(clsName, deps, encoder, decoder) =>
        Target.pure(StaticDefns(clsName, List.empty, List.empty))
    }
  }

  object ArrayProtocolTermInterp extends (ArrayProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ArrayProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractArrayType(arr, concreteTypes) =>
        for {
          result <- arr match {
            case SwaggerUtil.Resolved(tpe, dep, default) => Target.pure(tpe)
            case SwaggerUtil.Deferred(tpeName) =>
              Target.fromOption(lookupTypeName(tpeName, concreteTypes)(Target.pure(_)), s"Unresolved reference ${tpeName}").flatten
            case SwaggerUtil.DeferredArray(tpeName) =>
              Target
                .fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => safeParseType(s"java.util.List<${tpe}>")), s"Unresolved reference ${tpeName}")
                .flatten
            case SwaggerUtil.DeferredMap(tpeName) =>
              Target
                .fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => safeParseType(s"java.util.List<java.util.Map<String, ${tpe}>>")),
                            s"Unresolved reference ${tpeName}")
                .flatten
          }
        } yield result
    }
  }

  object ProtocolSupportTermInterp extends (ProtocolSupportTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ProtocolSupportTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractConcreteTypes(definitions) =>
        definitions.fold[Target[List[PropMeta[JavaLanguage]]]](Target.raiseError, Target.pure)

      case ProtocolImports() =>
        (List(
          "com.fasterxml.jackson.annotation.JsonCreator",
          "com.fasterxml.jackson.annotation.JsonIgnoreProperties",
          "com.fasterxml.jackson.annotation.JsonInclude",
          "com.fasterxml.jackson.annotation.JsonProperty",
          "java.util.Optional"
        ).map(safeParseRawImport) ++ List(
          "java.util.Objects.requireNonNull"
        ).map(safeParseRawStaticImport)).sequence

      case PackageObjectImports() =>
        Target.pure(List.empty)

      case PackageObjectContents() =>
        Target.pure(List.empty)
    }
  }

  private def renderSealedTrait(className: String,
                                selfParams: List[ProtocolParameter[JavaLanguage]],
                                discriminator: Discriminator[JavaLanguage],
                                parents: List[SuperClass[JavaLanguage]],
                                children: List[String]): Target[ClassOrInterfaceDeclaration] = {
    val parentsWithDiscriminators = parents.collect({ case p if p.discriminators.nonEmpty => p })
    for {
      parentOpt <- (parentsWithDiscriminators, parents) match {
        case _ if parentsWithDiscriminators.length > 1 =>
          Target.raiseError[Option[SuperClass[JavaLanguage]]](
            s"${className} requires unsupported multiple inheritance due to multiple parents with discriminators (${parentsWithDiscriminators.map(_.clsName).mkString(", ")})"
          )
        case _ if parentsWithDiscriminators.length == 1 => Target.pure(parentsWithDiscriminators.headOption)
        case _ if parents.length == 1                   => Target.pure(parents.headOption)
        case _                                          => Target.pure(None)
      }
    } yield {
      val parentParams                               = parentOpt.toList.flatMap(_.params)
      val parentParamNames                           = parentParams.map(_.name)
      val (parentRequiredTerms, parentOptionalTerms) = sortParams(parentParams)
      val parentTerms                                = parentRequiredTerms ++ parentOptionalTerms
      val params = parents.filterNot(parent => parentOpt.contains(parent)).flatMap(_.params) ++ selfParams.filterNot(
        param => parentParamNames.contains(param.term.getName.getIdentifier)
      )
      val (requiredTerms, optionalTerms) = sortParams(params)
      val terms                          = requiredTerms ++ optionalTerms

      val abstractClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, ABSTRACT), false, className)
      abstractClass.addAnnotation(
        new NormalAnnotationExpr(
          new Name("JsonIgnoreProperties"),
          new NodeList(
            new MemberValuePair(
              "ignoreUnknown",
              new BooleanLiteralExpr(true)
            )
          )
        )
      )
      abstractClass.addAnnotation(
        new NormalAnnotationExpr(
          new Name("JsonTypeInfo"),
          new NodeList(
            new MemberValuePair(
              "use",
              new FieldAccessExpr(new NameExpr("JsonTypeInfo.Id"), "NAME")
            ),
            new MemberValuePair(
              "include",
              new FieldAccessExpr(new NameExpr("JsonTypeInfo.As"), "PROPERTY")
            ),
            new MemberValuePair(
              "property",
              new StringLiteralExpr(discriminator.propertyName)
            )
          )
        )
      )
      abstractClass.addSingleMemberAnnotation(
        "JsonSubTypes",
        new ArrayInitializerExpr(
          new NodeList(
            children.map(
              child =>
                new NormalAnnotationExpr(
                  new Name("JsonSubTypes.Type"),
                  new NodeList(
                    new MemberValuePair("name",
                                        new StringLiteralExpr(
                                          discriminator.mapping
                                            .collectFirst({ case (value, elem) if elem.name == child => value })
                                            .getOrElse(child)
                                        )),
                    new MemberValuePair("value", new ClassExpr(JavaParser.parseType(child)))
                  )
              )
            ): _*
          )
        )
      )

      addParents(abstractClass, parentOpt)

      terms.foreach({ term =>
        val field = abstractClass.addField(term.fieldType, term.parameterName, PRIVATE, FINAL)
        field.addAnnotation(new SingleMemberAnnotationExpr(new Name("JsonProperty"), new StringLiteralExpr(term.propertyName)))
      })

      val superCall = new MethodCallExpr("super", parentTerms.map(term => new NameExpr(term.parameterName)): _*)
      abstractClass
        .addConstructor(PROTECTED)
        .setParameters(
          (parentTerms ++ terms)
            .map(term => new Parameter(util.EnumSet.of(FINAL), term.fieldType, new SimpleName(term.parameterName)))
            .toNodeList
        )
        .setBody(dtoConstructorBody(superCall, requiredTerms ++ optionalTerms))

      terms.foreach(addParameterGetter(abstractClass, _))

      abstractClass
    }
  }

  object PolyProtocolTermInterp extends (PolyProtocolTerm[JavaLanguage, ?] ~> Target) {
    override def apply[A](fa: PolyProtocolTerm[JavaLanguage, A]): Target[A] = fa match {
      case ExtractSuperClass(swagger, definitions) =>
        def allParents(model: Schema[_]): List[(String, Schema[_], List[Schema[_]])] =
          model match {
            case schema: ComposedSchema =>
              Option(schema.getAllOf)
                .map(_.asScala.toList)
                .getOrElse(List.empty)
                .flatMap({ elem =>
                  definitions
                    .collectFirst({
                      case (clsName, e) if Option(elem.get$ref).exists(_.endsWith(s"/$clsName")) =>
                        (clsName, e, List.empty) :: allParents(e)
                    })
                    .getOrElse(List.empty)
                })
            case _ => List.empty
          }

        Target.pure(allParents(swagger))

      case RenderADTStaticDefns(clsName, discriminator, encoder, decoder) =>
        for {
          extraImports <- List(
            "com.fasterxml.jackson.annotation.JsonIgnoreProperties",
            "com.fasterxml.jackson.annotation.JsonSubTypes",
            "com.fasterxml.jackson.annotation.JsonTypeInfo"
          ).traverse(safeParseRawImport)
        } yield
          StaticDefns[JavaLanguage](
            clsName,
            extraImports,
            List.empty
          )

      case DecodeADT(clsName, discriminator, children) =>
        Target.pure(None)

      case EncodeADT(clsName, discriminator, children) =>
        Target.pure(None)

      case RenderSealedTrait(className, selfParams, discriminator, parents, children) =>
        renderSealedTrait(className, selfParams, discriminator, parents, children)
    }
  }
}
