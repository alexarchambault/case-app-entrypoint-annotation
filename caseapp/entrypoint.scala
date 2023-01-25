//> using lib "com.github.alexarchambault::case-app:2.1.0-M21+9-d8831905-SNAPSHOT"
//> using lib "com.lihaoyi::pprint:0.8.1"
//> using scala "3.3.0-RC1-bin-20230120-d6cc101-NIGHTLY"
//> using option "-deprecation"

//> using publish.organization "com.github.alexarchambault"
//> using publish.name "case-app-macros"
//> using publish.version "0.1.0-SNAPSHOT"

// https://contributors.scala-lang.org/t/scala-3-macro-annotations-and-code-generation/6035
// https://github.com/lampepfl/dotty/pull/16392
// https://github.com/lampepfl/dotty/pull/16454
// https://github.com/lampepfl/dotty/pull/16534/files#diff-542c48c1b615eb1f49a9702a5df4f750df3e63c2784f9e742080b14b75db6ec5

package caseapp

import caseapp.core.Scala3Helpers.*
import caseapp.core.app.CaseApp
import caseapp.core.argparser.ArgParser
import caseapp.core.help.Help
import caseapp.core.parser.Parser

import scala.annotation.*
import scala.quoted.*

@experimental
class entrypoint extends MacroAnnotation {
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = {
    import quotes.reflect.*
    tree match
      case DefDef(name, TermParamClause(params) :: Nil, tpt, Some(rhsTree)) =>
        val defaultMethodsMap = {
          val methodPrefix = s"$name$$default$$"
          val methods      = tree.symbol.owner.methodMembers.filter(_.name.startsWith(methodPrefix))
          methods.map(s => s.name.stripPrefix(methodPrefix) -> s).toMap
        }

        val lastIsVarArgs = params.lastOption
          .map(_.tpt)
          .collect {
            case Annotated(_, annotation) =>
              annotation
          }
          .exists {
            case Apply(Select(qual, _), Nil) =>
              qual.tpe =:= TypeRepr.of[scala.annotation.internal.Repeated]
            case _ => false
          }
        val actualParams =
          if (lastIsVarArgs) params.init
          else params

        val parserExpr = actualParams.zipWithIndex.foldRight('{ Parser.nil: Parser[Tuple] }) {
          (elem, acc) =>
            val (param, idx) = elem
            param.tpt.tpe.asType match {
              case '[t] =>
                val argParser = Expr.summon[ArgParser[t]].getOrElse {
                  sys.error(s"No ArgParser[${Type.show[t]}] found")
                }
                val default = defaultMethodsMap.get((idx + 1).toString) match {
                  case None =>
                    '{ () => Option.empty[t] }
                  case Some(m) =>
                    val e = Select(This(tree.symbol.owner), m).asExprOf[t]
                    '{ () => Some($e): Option[t] }
                }

                '{
                  entrypoint.add[t, Tuple]($acc, ${ Expr(param.name) }, $default)(using $argParser)
                }
            }
        }

        def userEntrypoint(args: Expr[Tuple], remaining: Expr[Seq[String]]): Expr[Unit] = {
          val f = Select.unique(This(tree.symbol.owner), name)
          val values = actualParams.indices
            .map(n => '{ $args.productElement(${ Expr(n) }) }.asTerm)
            .toList
          val extraValue =
            if (lastIsVarArgs) remaining.asTerm :: Nil
            else Nil
          Apply(f, values ::: extraValue).asExprOf[Unit]
        }

        def mainImpl(args: List[List[Tree]])(using Quotes) = {
          val arg = args.head.head.asExprOf[Array[String]]
          '{
            entrypoint.proceed(
              $parserExpr,
              (t, remaining) => ${ userEntrypoint('t, 'remaining) },
              $arg.toSeq
            )
          }
        }

        def mainMethod(owner: Symbol) = {
          val mainSymbol = Symbol.newMethod(
            owner,
            "main",
            MethodType(List("args"))(
              _ => List(TypeRepr.of[Array[String]]),
              _ => TypeRepr.of[Unit]
            ),
            Flags.Static,
            Symbol.noSymbol
          )
          List(mainSymbol)
        }

        // inspired by https://github.com/lampepfl/dotty/pull/16392
        // and https://github.com/lampepfl/dotty/pull/16534/files#diff-542c48c1b615eb1f49a9702a5df4f750df3e63c2784f9e742080b14b75db6ec5

        val parents = List(TypeTree.of[Object])
        val optionsSymbol = Symbol.newModule(
          Symbol.spliceOwner,
          Symbol.freshName(s"$$method$$$name"),
          Flags.EmptyFlags,
          Flags.EmptyFlags,
          parents.map(_.tpe),
          mainMethod,
          Symbol.noSymbol
        )
        val optionsCls = optionsSymbol.moduleClass

        val clsDef = {
          val runSym = optionsCls.declaredMethod("main").head
          val runDef =
            DefDef(runSym, args => Some(mainImpl(args)(using optionsSymbol.asQuotes).asTerm))
          ClassDef(optionsCls, parents, body = List(runDef))
        }

        val modVal = {
          val newCls = Apply(Select(New(TypeIdent(optionsCls)), optionsCls.primaryConstructor), Nil)
          ValDef(optionsSymbol, Some(newCls))
        }

        List(modVal, clsDef, tree)
      case _ =>
        report.error("Annotation only supported on `def`")
        List(tree)
  }
}

object entrypoint {
  def add[H: ArgParser, T <: Tuple](
    parser: Parser[T],
    name: String,
    default: () => Option[H]
  ): Parser[H *: T] =
    parser.add[H](name, default())
  def help[T](parser: Parser[T]) = Help[T](args = parser.args)
  def proceed(
    parser: Parser[Tuple],
    call: (Tuple, Seq[String]) => Unit,
    args: Seq[String]
  ): Unit = {
    // scala.scalajs.js.Dynamic.global.require("process").env.selectDynamic("PATH")
    val printArgs = sys.env.get("CASEAPP_METHOD_PRINT_ARGS").contains("true")
    if (printArgs)
      ???
    else {
      val t = CaseApp.process[Tuple](args)(using parser, help(parser))
      call(t._1, t._2.all)
    }
  }
}
