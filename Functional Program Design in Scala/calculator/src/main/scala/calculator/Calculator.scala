package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.map {
      case (name, exprSignal) => (name, Signal(eval(exprSignal(), namedExpressions)))
    }

  override def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double =
    evalUtil(expr, references, Set())

  def evalUtil(expr: Expr, references: Map[String, Signal[Expr]], callStack: Set[String])(using Signal.Caller): Double = expr match
    case Literal(v) => v
    case Plus(a, b) => evalUtil(a, references, callStack) + evalUtil(b, references, callStack)
    case Minus(a, b) => evalUtil(a, references, callStack) - evalUtil(b, references, callStack)
    case Times(a, b) => evalUtil(a, references, callStack) * evalUtil(b, references, callStack)
    case Divide(a, b) => evalUtil(a, references, callStack) / evalUtil(b, references, callStack)
    case Ref(name) =>
      if callStack.contains(name) then Double.NaN
      else if references.contains(name) then evalUtil(references(name)(), references, callStack + name)
      else Double.NaN



  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
