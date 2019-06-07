package com.twilio.guardrail

import _root_.io.swagger.v3.oas.models._
import _root_.io.swagger.v3.oas.models.media._
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.extract.VendorExtension.VendorExtensible._
import com.twilio.guardrail.extract.{ DataRedaction, Default, EmptyValueIsNull }
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
import scala.collection.JavaConverters._
import scala.language.higherKinds

case class ProtocolDefinitions[L <: LA](elems: List[StrictProtocolElems[L]],
                                        protocolImports: List[L#Import],
                                        packageObjectImports: List[L#Import],
                                        packageObjectContents: List[L#ValueDefinition])
sealed trait EmptyToNullBehaviour
case object EmptyIsNull  extends EmptyToNullBehaviour
case object EmptyIsEmpty extends EmptyToNullBehaviour

sealed trait RedactionBehaviour
case object DataVisible  extends RedactionBehaviour
case object DataRedacted extends RedactionBehaviour

sealed trait ParameterPresence
object ParameterPresence {
  case object Optional         extends ParameterPresence
  case object OptionalNullable extends ParameterPresence
  case object Required         extends ParameterPresence
  case object RequiredNullable extends ParameterPresence

  sealed trait CompatMode
  case object SwaggerV2Compat extends CompatMode
  case object NoCompat        extends CompatMode

  def apply(prop: Schema[_], isRequired: Boolean, compatMode: CompatMode): ParameterPresence = {
    val (finalIsRequired, isNullable) = Option(prop.getNullable).fold({
      // Swagger v2 does not have `nullable`, but `x-nullable` is recognized as the de-facto
      // extension for v2 that has the same meaning in v3.  The OpenAPI v2-to-v3 converter
      // will set `nullable` based on `x-nullable` in a v2 spec.  If it's not present at all,
      // we're going to maintain the old Guardrail behavior for v2 specs:
      //    required - > (required: true, nullable: false)
      //    !required -> (required: true, nullable: true)
      // This will maintain backwards-compat for people relying on our existing v2 behavior,
      // but will a) 'upgrade' them to the correct behavior if they use `x-nullable`, and b)
      // makes Guardrail behave according to spec if it's a v3 spec.
      compatMode match {
        case SwaggerV2Compat if !isRequired => (true, true)
        case NoCompat                       => (isRequired, false)
      }
    })((isRequired, _))

    (finalIsRequired, isNullable) match {
      case (false, false) => Optional
      case (false, true)  => OptionalNullable
      case (true, false)  => Required
      case (true, true)   => RequiredNullable
    }
  }
}

case class ProtocolParameter[L <: LA](term: L#MethodParameter,
                                      name: String,
                                      dep: Option[L#TermName],
                                      presence: ParameterPresence,
                                      readOnlyKey: Option[String],
                                      emptyToNull: EmptyToNullBehaviour,
                                      dataRedaction: RedactionBehaviour,
                                      defaultValue: Option[L#Term])

case class Discriminator[L <: LA](propertyName: String, mapping: Map[String, ProtocolElems[L]])

object Discriminator {
  def fromSchema[L <: LA, F[_]](schema: Schema[_])(implicit Sc: ScalaTerms[L, F]): Free[F, Option[Discriminator[L]]] = {
    import Sc._
    Option(schema.getDiscriminator)
      .flatMap(x => Option(x.getPropertyName).map((x, _)))
      .traverse {
        case (x, propertyName) =>
          val possibleMappings = Option(x.getMapping)
            .map(_.asScala)
            .getOrElse(Map.empty[String, String])
            .flatMap({
              case (k, s) if s.startsWith("#/") => s.split("/").lastOption.filter(_.nonEmpty).map((k, _))
              case (k, s)                       => Option((k, s))
            })
            .toList
          for {
            mapping <- possibleMappings.flatTraverse({
              case (key, name) =>
                parseType(name).map(_.map(tpe => (key, RandomType(name, tpe))).toList)
            })
          } yield Discriminator[L](propertyName, mapping.toMap)
      }
  }
}

case class SuperClass[L <: LA](
    clsName: String,
    tpl: L#TypeName,
    interfaces: List[String],
    params: List[ProtocolParameter[L]],
    discriminators: List[Discriminator[L]]
)

object ProtocolGenerator {
  private[this] def getRequiredFieldsRec(root: Schema[_]): List[String] = {
    @scala.annotation.tailrec
    def work(values: List[Schema[_]], acc: List[String]): List[String] = {
      val required = values.flatMap(value => Option(value.getRequired()).fold(List.empty[String])(_.asScala.toList))
      val next: List[Schema[_]] = values.flatMap({
        case x: ComposedSchema =>
          Option(x.getAllOf())
            .fold(List.empty[Schema[_]])(_.asScala.toList)
        case _ => Nil
      })

      val newRequired = acc ++ required

      next match {
        case next @ (_ :: _) => work(next, newRequired)
        case Nil             => newRequired
      }
    }
    work(List(root), Nil)
  }

  private[this] def transformProperty[L <: LA, F[_]](
      clsName: String,
      name: String,
      property: Schema[_],
      presence: ParameterPresence,
      meta: SwaggerUtil.ResolvedType[L],
      concreteTypes: List[PropMeta[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sw: SwaggerTerms[L, F], Sc: ScalaTerms[L, F]): Free[F, (String, ProtocolParameter[L])] = {
    import Fw._
    import Sc._

    def wrapDefault(op: Option[Free[F, L#Term]]): Free[F, Option[L#Term]] =
      op.fold(Free.pure[F, Option[L#Term]](Option.empty))(_.map(Option.apply))

    def orObjectTypeFallback(tpe: Option[L#Type], tpeName: String): Free[F, L#Type] =
      tpe.fold({
        Sw.log.warning(s"Unable to parse type $tpeName, falling back to the object type") >>
          objectType(None)
      })(Free.pure[F, L#Type])

    Sw.log.function("constructProtocolParameter") {
      for {
        argName     <- formatVariableName(name)
        argTermName <- pureTermName(argName)

        readOnlyKey = Option(name).filter(_ => Option(property.getReadOnly).contains(true))
        emptyToNull = (property match {
          case d: DateSchema      => EmptyValueIsNull(d)
          case dt: DateTimeSchema => EmptyValueIsNull(dt)
          case s: StringSchema    => EmptyValueIsNull(s)
          case _                  => None
        }).getOrElse(EmptyIsEmpty)
        dataRedaction = DataRedaction(property).getOrElse(DataVisible)

        tpeClassDep <- meta match {
          case SwaggerUtil.Resolved(declType, classDep, _) =>
            Free.pure[F, (L#Type, Option[L#TermName])]((declType, classDep))

          case SwaggerUtil.Deferred(tpeName) =>
            val tpe = concreteTypes
              .find(_.clsName == tpeName)
              .fold({
                Sw.log.warning(s"Unable to find definition for ${tpeName}, just inlining") >>
                  (for {
                    parsed  <- parseType(tpeName)
                    checked <- orObjectTypeFallback(parsed, tpeName)
                  } yield checked)
              })(x => Free.pure[F, L#Type](x.tpe))
            tpe.map((_, Option.empty[L#TermName]))

          case SwaggerUtil.DeferredArray(tpeName) =>
            for {
              innerType <- parseType(tpeName).flatMap(orObjectTypeFallback(_, tpeName))
              tpe       <- liftVectorType(innerType)
            } yield (tpe, Option.empty)

          case SwaggerUtil.DeferredMap(tpeName) =>
            for {
              innerType <- parseType(tpeName).flatMap(orObjectTypeFallback(_, tpeName))
              tpe       <- liftMapType(innerType)
            } yield (tpe, Option.empty)
        }
        (tpe, classDep) = tpeClassDep

        defaultValue <- property match {
          case _: MapSchema =>
            emptyMapTerm().map(Option.apply)
          case _: ArraySchema =>
            emptyVectorTerm().map(Option.apply)
          case p: BooleanSchema =>
            wrapDefault(Default(p).extract[Boolean].map(litBoolean))
          case p: NumberSchema if p.getFormat == "double" =>
            wrapDefault(Default(p).extract[Double].map(litDouble))
          case p: NumberSchema if p.getFormat == "float" =>
            wrapDefault(Default(p).extract[Float].map(litFloat))
          case p: IntegerSchema if p.getFormat == "int32" =>
            wrapDefault(Default(p).extract[Int].map(litInt))
          case p: IntegerSchema if p.getFormat == "int64" =>
            wrapDefault(Default(p).extract[Long].map(litLong))
          case p: StringSchema =>
            wrapDefault(Default(p).extract[String].map(litString))
          case _ =>
            Free.pure[F, Option[L#Term]](Option.empty)
        }

        declDefaultPair <- presence match {
          case ParameterPresence.OptionalNullable =>
            for {
              optTpe <- liftOptionalType(tpe)
              optOptTpe <- liftOptionalType(optTpe)
              defVal <- defaultValue.fold(emptyOptionalTerm())(liftOptionalTerm(_).flatMap(liftOptionalTerm))
            } yield (optOptTpe, Option(defVal))

          case ParameterPresence.Optional | ParameterPresence.RequiredNullable =>
            for {
              optTpe <- liftOptionalType(tpe)
              defVal <- defaultValue.fold(emptyOptionalTerm())(liftOptionalTerm)
            } yield (optTpe, Option(defVal))

          case ParameterPresence.Required =>
            Free.pure[F, (L#Type, Option[L#Term])]((tpe, defaultValue))
        }
        (finalDeclType, finalDefaultValue) = declDefaultPair

        term <- pureMethodParameter(argTermName, finalDeclType, finalDefaultValue)

        dep = classDep.filterNot(_.value == clsName) // Filter out our own class name
      } yield (argName, ProtocolParameter[L](term, name, dep, presence, readOnlyKey, emptyToNull, dataRedaction, finalDefaultValue))
    }
  }

  private[this] def dedupeParams[L <: LA, F[_]](
      params: List[(String, ProtocolParameter[L])]
  )(implicit Sc: ScalaTerms[L, F]): Free[F, List[ProtocolParameter[L]]] = {
    import Sc._

    val counts = params.groupBy(_._1).mapValues(_.length)
    params.traverse({
      case (argName, param) =>
        if (counts.getOrElse(argName, 0) > 1) {
          for {
            escapedName <- pureTermName(param.name)
            newTerm     <- alterMethodParameterName(param.term, escapedName)
          } yield
            ProtocolParameter[L](
              newTerm,
              param.name,
              param.dep,
              param.presence,
              param.readOnlyKey,
              param.emptyToNull,
              param.dataRedaction,
              param.defaultValue
            )
        } else {
          Free.pure[F, ProtocolParameter[L]](param)
        }
    })
  }

  private[this] def fromEnum[L <: LA, F[_]](
      clsName: String,
      swagger: Schema[_]
  )(implicit E: EnumProtocolTerms[L, F], F: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, Either[String, ProtocolElems[L]]] = {
    import E._
    import Sc._

    def validProg(enum: List[String], tpe: L#Type): Free[F, EnumDefinition[L]] =
      for {
        elems <- enum.traverse { elem =>
          for {
            termName  <- formatEnumName(elem)
            valueTerm <- pureTermName(termName)
            accessor  <- buildAccessor(clsName, termName)
          } yield (elem, valueTerm, accessor)
        }
        pascalValues = elems.map(_._2)
        members <- renderMembers(clsName, elems)
        encoder <- encodeEnum(clsName)
        decoder <- decodeEnum(clsName)

        defn        <- renderClass(clsName, tpe, elems)
        staticDefns <- renderStaticDefns(clsName, members, pascalValues, encoder, decoder)
        classType   <- pureTypeName(clsName)
      } yield EnumDefinition[L](clsName, classType, elems, defn, staticDefns)

    // Default to `string` for untyped enums.
    // Currently, only plain strings are correctly supported anyway, so no big loss.
    val tpeName = Option(swagger.getType).filterNot(_ == "object").getOrElse("string")

    for {
      enum          <- extractEnum(swagger)
      customTpeName <- SwaggerUtil.customTypeName(swagger)
      tpe           <- SwaggerUtil.typeName(tpeName, Option(swagger.getFormat()), customTpeName)
      res           <- enum.traverse(validProg(_, tpe))
    } yield res
  }

  /**
    * Handle polymorphic model
    */
  private[this] def fromPoly[L <: LA, F[_]](
      hierarchy: ClassParent[L],
      concreteTypes: List[PropMeta[L]],
      definitions: List[(String, Schema[_])],
      compatMode: ParameterPresence.CompatMode
  )(implicit F: FrameworkTerms[L, F],
    P: PolyProtocolTerms[L, F],
    M: ModelProtocolTerms[L, F],
    Sc: ScalaTerms[L, F],
    Sw: SwaggerTerms[L, F]): Free[F, ProtocolElems[L]] = {
    import M._
    import P._
    import Sc._

    def child(hierarchy: ClassHierarchy[L]): List[String] =
      hierarchy.children.map(_.name) ::: hierarchy.children.flatMap(child)
    def parent(hierarchy: ClassHierarchy[L]): List[String] =
      if (hierarchy.children.nonEmpty) hierarchy.name :: hierarchy.children.flatMap(parent)
      else Nil

    val children      = child(hierarchy).diff(parent(hierarchy)).distinct
    val discriminator = hierarchy.discriminator

    for {
      parents <- hierarchy.model match {
        case c: ComposedSchema => extractParents(c, definitions, concreteTypes, compatMode)
        case _                 => Free.pure[F, List[SuperClass[L]]](Nil)
      }
      props <- extractProperties(hierarchy.model)
      requiredFields = hierarchy.required ::: hierarchy.children.flatMap(_.required)
      normalizedParams <- props.traverse({
        case (name, prop) =>
          val isRequired = requiredFields.contains(name)
          val presence   = ParameterPresence(prop, isRequired, compatMode)
          SwaggerUtil
            .propMeta[L, F](prop)
            .flatMap(transformProperty(hierarchy.name, name, prop, presence, _, concreteTypes))
      })
      params      <- dedupeParams(normalizedParams)
      definition  <- renderSealedTrait(hierarchy.name, params, discriminator, parents, children)
      encoder     <- encodeADT(hierarchy.name, hierarchy.discriminator, children)
      decoder     <- decodeADT(hierarchy.name, hierarchy.discriminator, children)
      staticDefns <- renderADTStaticDefns(hierarchy.name, discriminator, encoder, decoder)
      tpe         <- pureTypeName(hierarchy.name)
    } yield {
      ADT[L](
        name = hierarchy.name,
        tpe = tpe,
        trt = definition,
        staticDefns = staticDefns
      )
    }
  }

  def extractParents[L <: LA, F[_]](
      elem: ComposedSchema,
      definitions: List[(String, Schema[_])],
      concreteTypes: List[PropMeta[L]],
      compatMode: ParameterPresence.CompatMode
  )(
      implicit M: ModelProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      P: PolyProtocolTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, List[SuperClass[L]]] = {
    import M._
    import P._
    import Sc._

    for {
      a <- extractSuperClass(elem, definitions)
      supper <- a.flatTraverse {
        case (clsName, _extends, interfaces) =>
          val concreteInterfaces = interfaces
            .flatMap(
              x =>
                definitions.collectFirst[Schema[_]] {
                  case (cls, y: ComposedSchema) if Option(x.get$ref).exists(_.endsWith(s"/${cls}")) => y
                  case (cls, y: Schema[_]) if Option(x.get$ref).exists(_.endsWith(s"/${cls}"))      => y
              }
            )
          for {
            _extendsProps <- extractProperties(_extends)
            requiredFields = getRequiredFieldsRec(_extends) ++ concreteInterfaces.flatMap(getRequiredFieldsRec)
            _withProps <- concreteInterfaces.traverse(extractProperties)
            props = _extendsProps ++ _withProps.flatten
            normalizedParams <- props.traverse({
              case (name, prop) =>
                val isRequired = requiredFields.contains(name)
                val presence   = ParameterPresence(prop, isRequired, compatMode)
                SwaggerUtil
                  .propMeta[L, F](prop)
                  .flatMap(transformProperty(clsName, name, prop, presence, _, concreteTypes))
            })
            params <- dedupeParams(normalizedParams)
            interfacesCls = interfaces.flatMap(i => Option(i.get$ref).map(_.split("/").last))
            tpe <- parseTypeName(clsName)

            discriminators <- (_extends :: concreteInterfaces).flatTraverse({
              case m: ObjectSchema => Discriminator.fromSchema(m).map(_.toList)
              case _               => Free.pure[F, List[Discriminator[L]]](List.empty)
            })
          } yield
            tpe
              .map(
                SuperClass[L](
                  clsName,
                  _,
                  interfacesCls,
                  params,
                  discriminators
                )
              )
              .toList
      }

    } yield supper
  }

  private[this] def fromModel[L <: LA, F[_]](clsName: String,
                                             model: Schema[_],
                                             parents: List[SuperClass[L]],
                                             concreteTypes: List[PropMeta[L]],
                                             compatMode: ParameterPresence.CompatMode)(
      implicit M: ModelProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, Either[String, ProtocolElems[L]]] = {
    import M._
    import Sc._

    for {
      props <- extractProperties(model)
      requiredFields = getRequiredFieldsRec(model)
      normalizedParams <- props.traverse({
        case (name, prop) =>
          val isRequired = requiredFields.contains(name)
          val presence   = ParameterPresence(prop, isRequired, compatMode)
          SwaggerUtil.propMeta[L, F](prop).flatMap(transformProperty(clsName, name, prop, presence, _, concreteTypes))
      })
      params <- dedupeParams(normalizedParams)
      defn   <- renderDTOClass(clsName, params, parents)
      deps = params.flatMap(_.dep)
      encoder     <- encodeModel(clsName, params, parents)
      decoder     <- decodeModel(clsName, params, parents)
      staticDefns <- renderDTOStaticDefns(clsName, List.empty, encoder, decoder)
      tpe         <- parseTypeName(clsName)
    } yield
      if (parents.isEmpty && props.isEmpty) Left("Entity isn't model")
      else tpe.toRight("Empty entity name").map(ClassDefinition[L](clsName, _, defn, staticDefns, parents))
  }

  def modelTypeAlias[L <: LA, F[_]](clsName: String, abstractModel: Schema[_])(
      implicit
      F: FrameworkTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ProtocolElems[L]] = {
    import F._
    val model = abstractModel match {
      case m: ObjectSchema => Some(m)
      case m: ComposedSchema =>
        m.getAllOf.asScala.toList.get(1).flatMap {
          case m: ObjectSchema => Some(m)
          case _               => None
        }
      case _ => None
    }
    for {
      tpe <- model
        .flatMap(model => Option(model.getType))
        .fold[Free[F, L#Type]](objectType(None))(
          raw =>
            model
              .flatTraverse(SwaggerUtil.customTypeName[L, F, ObjectSchema])
              .flatMap(customTypeName => SwaggerUtil.typeName[L, F](raw, model.flatMap(f => Option(f.getFormat)), customTypeName))
        )
      res <- typeAlias[L, F](clsName, tpe)
    } yield res
  }

  def plainTypeAlias[L <: LA, F[_]](
      clsName: String
  )(implicit F: FrameworkTerms[L, F], Sc: ScalaTerms[L, F]): Free[F, ProtocolElems[L]] = {
    import F._
    for {
      tpe <- objectType(None)
      res <- typeAlias[L, F](clsName, tpe)
    } yield res
  }

  def typeAlias[L <: LA, F[_]](clsName: String, tpe: L#Type): Free[F, ProtocolElems[L]] =
    Free.pure(RandomType[L](clsName, tpe))

  def fromArray[L <: LA, F[_]](clsName: String, arr: ArraySchema, concreteTypes: List[PropMeta[L]])(
      implicit R: ArrayProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      P: ProtocolSupportTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ProtocolElems[L]] = {
    import R._
    for {
      deferredTpe <- SwaggerUtil.modelMetaType(arr)
      tpe         <- extractArrayType(deferredTpe, concreteTypes)
      ret         <- typeAlias[L, F](clsName, tpe)
    } yield ret
  }

  sealed trait ClassHierarchy[L <: LA] {
    def name: String
    def model: Schema[_]
    def children: List[ClassChild[L]]
    def required: List[String]
  }
  case class ClassChild[L <: LA](name: String, model: Schema[_], children: List[ClassChild[L]], required: List[String]) extends ClassHierarchy[L]
  case class ClassParent[L <: LA](name: String, model: Schema[_], children: List[ClassChild[L]], discriminator: Discriminator[L], required: List[String])
      extends ClassHierarchy[L]

  /**
    * returns objects grouped into hierarchies
    */
  def groupHierarchies[L <: LA, F[_]](
      definitions: List[(String, Schema[_])]
  )(implicit Sc: ScalaTerms[L, F]): Free[F, (List[ClassParent[L]], List[(String, Schema[_])])] = {

    def firstInHierarchy(model: Schema[_]): Option[ObjectSchema] =
      (model match {
        case elem: ComposedSchema =>
          definitions.collectFirst {
            case (clsName, element) if Option(elem.getAllOf).toList.flatMap(_.asScala).exists(r => Option(r.get$ref).exists(_.endsWith(s"/$clsName"))) =>
              element
          }
        case _ => None
      }) match {
        case Some(x: ComposedSchema) => firstInHierarchy(x)
        case Some(x: ObjectSchema)   => Some(x)
        case _                       => None
      }

    def children(cls: String): List[ClassChild[L]] = definitions.collect {
      case (clsName, comp: ComposedSchema)
          if Option(comp.getAllOf)
            .map(_.asScala)
            .getOrElse(List.empty)
            .exists(x => Option(x.get$ref).exists(_.endsWith(s"/$cls"))) =>
        ClassChild(clsName, comp, children(clsName), getRequiredFieldsRec(comp))
    }

    def classHierarchy(cls: String, model: Schema[_]): Free[F, Option[ClassParent[L]]] =
      (model match {
        case c: ComposedSchema =>
          firstInHierarchy(c)
            .fold(Free.pure[F, Option[Discriminator[L]]](Option.empty))(Discriminator.fromSchema)
            .map(_.map((_, getRequiredFieldsRec(c))))
        case m: Schema[_]                            => Discriminator.fromSchema(m).map(_.map((_, getRequiredFieldsRec(m))))
        case _                                       => Free.pure[F, Option[(Discriminator[L], List[String])]](Option.empty)
      }).map(_.map({ case (discriminator, reqFields) => ClassParent(cls, model, children(cls), discriminator, reqFields) }))

    definitions
      .traverse({
        case (cls, model) =>
          for {
            hierarchy <- classHierarchy(cls, model)
          } yield hierarchy.filterNot(_.children.isEmpty).toLeft((cls, model))
      })
      .map(_.partitionEither[ClassParent[L], (String, Schema[_])](identity))
  }

  def fromSwagger[L <: LA, F[_]](swagger: OpenAPI)(
      implicit E: EnumProtocolTerms[L, F],
      M: ModelProtocolTerms[L, F],
      R: ArrayProtocolTerms[L, F],
      S: ProtocolSupportTerms[L, F],
      F: FrameworkTerms[L, F],
      P: PolyProtocolTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ProtocolDefinitions[L]] = {
    import S._
    import Sw._

    val compatMode = Option(swagger.getOpenapi)
      .fold[ParameterPresence.CompatMode](ParameterPresence.SwaggerV2Compat)({
        case x if x.trim.startsWith("2.") => ParameterPresence.SwaggerV2Compat
        case _                            => ParameterPresence.NoCompat
      })

    val definitions = Option(swagger.getComponents()).toList.flatMap(x => Option(x.getSchemas)).flatMap(_.asScala.toList)

    for {
      groupedHierarchies <- groupHierarchies(definitions)
      (hierarchies, definitionsWithoutPoly) = groupedHierarchies

      concreteTypes <- SwaggerUtil.extractConcreteTypes[L, F](definitions)
      polyADTs      <- hierarchies.traverse(fromPoly(_, concreteTypes, definitions, compatMode))
      elems <- definitionsWithoutPoly.traverse {
        case (clsName, model) =>
          model match {
            case m: StringSchema =>
              for {
                enum  <- fromEnum(clsName, m)
                model <- fromModel(clsName, m, List.empty, concreteTypes, compatMode)
                alias <- modelTypeAlias(clsName, m)
              } yield enum.orElse(model).getOrElse(alias)

            case comp: ComposedSchema =>
              for {
                parents <- extractParents(comp, definitions, concreteTypes, compatMode)
                model   <- fromModel(clsName, comp, parents, concreteTypes, compatMode)
                alias   <- modelTypeAlias(clsName, comp)
              } yield model.getOrElse(alias)

            case arr: ArraySchema =>
              fromArray(clsName, arr, concreteTypes)

            case m: ObjectSchema =>
              for {
                enum  <- fromEnum(clsName, m)
                model <- fromModel(clsName, m, List.empty, concreteTypes, compatMode)
                alias <- modelTypeAlias(clsName, m)
              } yield enum.orElse(model).getOrElse(alias)

            case x =>
              for {
                tpeName        <- getType(x)
                customTypeName <- SwaggerUtil.customTypeName(x)
                tpe            <- SwaggerUtil.typeName[L, F](tpeName, Option(x.getFormat()), customTypeName)
                res            <- typeAlias(clsName, tpe)
              } yield res
          }
      }
      protoImports      <- protocolImports()
      pkgImports        <- packageObjectImports()
      pkgObjectContents <- packageObjectContents()

      polyADTElems <- ProtocolElems.resolve[L, F](polyADTs)
      strictElems  <- ProtocolElems.resolve[L, F](elems)
    } yield ProtocolDefinitions[L](strictElems ++ polyADTElems, protoImports, pkgImports, pkgObjectContents)
  }
}
