package com.twilio.guardrail.generators

import cats.~>
import cats.instances.option._
import cats.syntax.traverse._
import com.github.javaparser.ast.{ CompilationUnit, ImportDeclaration, PackageDeclaration }
import com.github.javaparser.ast.`type`.{ PrimitiveType, Type }
import com.github.javaparser.ast.body.{ BodyDeclaration, ClassOrInterfaceDeclaration, Parameter }
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.Statement
import com.twilio.guardrail._
import com.twilio.guardrail.Common.resolveFile
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.terms._
import java.nio.charset.StandardCharsets

object JavaGenerator {
  object JavaInterp extends (ScalaTerm[JavaLanguage, ?] ~> Target) {
    def buildPkgDecl(parts: List[String]): Target[PackageDeclaration] = safeParseName(parts.mkString(".")).map(new PackageDeclaration(_))

    def apply[T](term: ScalaTerm[JavaLanguage, T]): Target[T] = term match {
      case LitString(value)        => Target.pure(new StringLiteralExpr(value))
      case LitFloat(value)         => Target.pure(new DoubleLiteralExpr(value))
      case LitDouble(value)        => Target.pure(new DoubleLiteralExpr(value))
      case LitInt(value)           => Target.pure(new IntegerLiteralExpr(value))
      case LitLong(value)          => Target.pure(new LongLiteralExpr(value))
      case LitBoolean(value)       => Target.pure(new BooleanLiteralExpr(value))
      case LiftOptionalType(value) => safeParseType(s"java.util.Optional<${value}>")
      case LiftOptionalTerm(value) => safeParseExpression[MethodCallExpr](s"java.util.Optional.ofNullable(${value}")
      case EmptyOptionalTerm()     => safeParseExpression[MethodCallExpr]("java.util.Optional.empty()")
      case LiftVectorType(value)   => safeParseType(s"java.util.List<${value}>")
      case LiftVectorTerm(value)   => safeParseExpression[MethodCallExpr](s"java.util.Collections.singletonList(${value})")
      case LiftMapType(value)      => safeParseType(s"java.util.Map<String, ${value}>")
      case LookupEnumDefaultValue(tpe, defaultValue, values) => {
        // FIXME: Is there a better way to do this? There's a gap of coverage here
        defaultValue match {
          case s: StringLiteralExpr =>
            values
              .find(_._1 == s.getValue)
              .fold(Target.raiseError[Expression](s"Enumeration ${tpe} is not defined for default value ${s.getValue}"))(value => Target.pure(value._3))
          case _ =>
            Target.raiseError[Expression](s"Enumeration ${tpe} somehow has a default value that isn't a string")
        }
      }
      case EmbedArray(tpe) =>
        tpe match {
          case SwaggerUtil.Deferred(tpe) =>
            Target.pure(SwaggerUtil.DeferredArray(tpe))
          case SwaggerUtil.DeferredArray(_) =>
            Target.raiseError("FIXME: Got an Array of Arrays, currently not supported")
          case SwaggerUtil.DeferredMap(_) =>
            Target.raiseError("FIXME: Got an Array of Maps, currently not supported")
        }
      case EmbedMap(tpe) =>
        tpe match {
          case SwaggerUtil.Deferred(inner) => Target.pure(SwaggerUtil.DeferredMap(inner))
          case SwaggerUtil.DeferredMap(_) =>
            Target.raiseError("FIXME: Got a map of maps, currently not supported")
          case SwaggerUtil.DeferredArray(_) =>
            Target.raiseError("FIXME: Got a map of arrays, currently not supported")
        }
      case ParseType(tpe) =>
        safeParseType(tpe)
          .fold({ err =>
            println(s"Warning: Unparsable x-java-type: ${tpe} ${err}")
            None
          }, Option.apply)
      case ParseTypeName(tpe) =>
        Option(tpe).map(_.trim).filterNot(_.isEmpty).map(safeParseType).sequence

      case PureTermName(tpe) =>
        Option(tpe).map(_.trim).filterNot(_.isEmpty).map(safeParseName).getOrElse(Target.raiseError("A structure's name is empty"))

      case PureTypeName(tpe) =>
        Option(tpe).map(_.trim).filterNot(_.isEmpty).map(safeParseName).getOrElse(Target.raiseError("A structure's name is empty"))

      case PureMethodParameter(name, tpe, default) =>
        // FIXME: java methods do not support default param values -- what should we do here?
        safeParseParameter(s"final ${tpe} ${name}")

      case TypeNamesEqual(a, b) =>
        Target.pure(a.asString == b.asString)

      case TypesEqual(a, b) =>
        Target.pure(a.equals(b))

      case ExtractTypeName(tpe) =>
        Target.pure(tpe match {
          case x: Name => Option(x)
          case _       => Option.empty
        })
      case ExtractTermName(term) =>
        Target.pure(term.getIdentifier)

      case AlterMethodParameterName(param, name) =>
        safeParseSimpleName(name.asString).map(new Parameter(param.getType, _))

      case DateType()                => safeParseType("java.time.LocalDate")
      case DateTimeType()            => safeParseType("java.time.OffsetDateTime")
      case StringType(format)        => format.fold(safeParseType("String"))(safeParseType)
      case FloatType()               => Target.pure(PrimitiveType.floatType)
      case DoubleType()              => Target.pure(PrimitiveType.doubleType)
      case NumberType(format)        => safeParseType("java.math.BigDecimal")
      case IntType()                 => Target.pure(PrimitiveType.intType)
      case LongType()                => Target.pure(PrimitiveType.longType)
      case IntegerType(format)       => safeParseType("java.math.BigInteger")
      case BooleanType(format)       => Target.pure(PrimitiveType.booleanType)
      case ArrayType(format)         => safeParseType("java.util.List<String>")
      case FallbackType(tpe, format) => safeParseType(tpe)

      case WidenTypeName(tpe)     => safeParseType(tpe.asString)
      case WidenTermSelect(value) => Target.pure(value) // FIXME: what is a TermSelect???

      case RenderImplicits(pkgPath, pkgName, frameworkImports, jsonImports, customImports) =>
        // FIXME
        Target.pure(WriteTree(pkgPath.resolve("Implicits.java"), new Array[Byte](0)))

      case RenderFrameworkImplicits(pkgPath, pkgName, frameworkImports, jsonImports, frameworkImplicits, frameworkImplicitName) =>
        // FIXME
        Target.pure(WriteTree(pkgPath.resolve(s"FrameworkImplicits.java"), new Array[Byte](0)))

      case WritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes) =>
        // FIXME
        Target.pure(WriteTree(dtoPackagePath.resolve("Package.java"), new Array[Byte](0)))

      case WriteProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, elem) =>
        elem match {
          case EnumDefinition(_, _, _, cls, staticDecls) =>
            val clsCopy = cls.clone()
            buildPkgDecl(pkgName).map { pkgDecl =>
              val cu = new CompilationUnit()
              cu.setPackageDeclaration(pkgDecl)
              imports.map(cu.addImport)
              val clsCopy = cls.clone()
              staticDecls.foreach(clsCopy.addMember)
              cu.addType(clsCopy)
              (
                List(
                  WriteTree(
                    resolveFile(outputPath)(dtoComponents).resolve(s"${cls.getName.getIdentifier}.java"),
                    cu.toString(printer).getBytes(StandardCharsets.UTF_8)
                  )
                ),
                List.empty[Statement]
              )
            }

          case ClassDefinition(_, _, cls, staticDecls, _) =>
            buildPkgDecl(pkgName).map { pkgDecl =>
              val cu = new CompilationUnit()
              cu.setPackageDeclaration(pkgDecl)
              imports.map(cu.addImport)
              val clsCopy = cls.clone()
              staticDecls.foreach(clsCopy.addMember)
              cu.addType(clsCopy)
              (
                List(
                  WriteTree(
                    resolveFile(outputPath)(dtoComponents).resolve(s"${cls.getName.getIdentifier}.java"),
                    cu.toString(printer).getBytes(StandardCharsets.UTF_8)
                  )
                ),
                List.empty[Statement]
              )
            }

          case ADT(name, tpe, trt, staticDecls) =>
            buildPkgDecl(pkgName).map { pkgDecl =>
              val cu = new CompilationUnit()
              cu.setPackageDeclaration(pkgDecl)
              imports.map(cu.addImport)
              val trtCopy = trt.clone()
              staticDecls.foreach(trtCopy.addMember)
              cu.addType(trtCopy)
              (
                List(
                  WriteTree(
                    resolveFile(outputPath)(dtoComponents).resolve(s"${name}.java"),
                    cu.toString(printer).getBytes(StandardCharsets.UTF_8)
                  )
                ),
                List.empty[Statement]
              )
            }

          case RandomType(_, _) =>
            (List.empty, List.empty)
        }
      case WriteClient(pkgPath,
                       pkgName,
                       customImports,
                       frameworkImplicitName,
                       dtoComponents,
                       Client(pkg, clientName, imports, staticDecls, client, responseDefinitions)) =>
        for {
          pkgDecl         <- buildPkgDecl(pkgName ++ pkg)
          implicitsImport <- safeParseName((pkgName ++ List("Implicits", "*")).mkString(".")).map(name => new ImportDeclaration(name, false, true))
          frameworkImplicitsImport <- safeParseName((pkgName ++ List(frameworkImplicitName.getIdentifier, "*")).mkString("."))
            .map(name => new ImportDeclaration(name, false, true))
          dtoComponentsImport <- safeParseName((dtoComponents :+ "*").mkString(".")).map(name => new ImportDeclaration(name, false, true))
        } yield {
          val cu = new CompilationUnit()
          cu.setPackageDeclaration(pkgDecl)
          imports.map(cu.addImport)
          customImports.map(cu.addImport)
          cu.addImport(implicitsImport)
          cu.addImport(frameworkImplicitsImport)
          cu.addImport(dtoComponentsImport)
          val clientCopy = cls.clone()
          staticDecls.foreach(clientCopy.addMember)
          responseDefinitions.foreach(clientCopy.addMember)
          cu.addType(clientCopy)
          (
            List(
              WriteTree(
                resolveFile(pkgPath)(pkg :+ s"${clientName}.java"),
                cu.toString(printer).getBytes(StandardCharsets.UTF_8)
              )
            ),
            List.empty[Statement]
          )
        }
      case WriteServer(pkgPath, pkgName, customImports, frameworkImplicitName, dtoComponents, Server(pkg, extraImports, src)) =>
        Target.raiseError("TODO: java server generation")
    }
  }

}
