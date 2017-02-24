package cs220.evaluator

/**
 * An EvaluationException represents a problem with an expression program.
 * An EvaluationException is thrown when there is a problem with
 * evaluating an expression program.
 */
class EvaluationException(msg: String) extends RuntimeException(msg)

/**
 * An EvaluationResult represents the result of an evaluation.
 */
case class EvaluationResult(value: Value, env: Environment)

/**
 * An AbstractEvaluator defines the operations that our evaluator
 * will use to evaluation [[Expr]] objects in an [[Environment]].
 */
abstract class AbstractEvaluator {
  /**
   * eval returns an [[EvaluationResult]] given an expression `expr` and
   * an environment `env`. It determines which of the other `eval` methods
   * to invoke based off of the type of [[Expr]].
   */
  def eval(expr: Expr, env: Environment): EvaluationResult

  /**
   * evalNumber evaluates a [[Number]] `num` in an [[Environment]] `env`.
   */
  def evalNumber(num: Number, env: Environment): EvaluationResult

  /**
   * evalVar evaluates a [[Var]] `v` in an [[Environment]] `env`.
   */
  def evalVar(v: Var, env: Environment): EvaluationResult

  /**
   * evalAdd evaluates an add expression in an [[Environment]] `env`.
   */
  def evalAdd(op: Add, env: Environment): EvaluationResult

  /**
   * evalSub evaluates an subtract expression in an [[Environment]] `env`.
   */
  def evalSub(op: Sub, env: Environment): EvaluationResult

  /**
   * evalMul evaluates a multiply expression in an [[Environment]] `env`.
   */
  def evalMul(op: Mul, env: Environment): EvaluationResult

  /**
   * evalDiv evaluates a divide expression in an [[Environment]] `env`.
   */
  def evalDiv(op: Div, env: Environment): EvaluationResult

  /**
   * evalAssign evaluates an assignment expression in an [[Environment]] `env`.
   */
  def evalAssign(op: Assign, env: Environment): EvaluationResult

  /**
   * evalProgram evaluates a program expression in an [[Environment]] `env`.
   */
  def evalProgram(prog: Program, env: Environment): EvaluationResult
}

// TODO: Part 5 - implement a simple evaluator.
class SimpleEvaluator extends AbstractEvaluator {
  def eval(expr: Expr, env: Environment): EvaluationResult = expr match{
    case Number(a) => evalNumber(new Number(a), env)
    case Var(a) => evalVar(new Var(a),env)
    case Add(a,b) => evalAdd(new Add(a,b), env)
    case Sub(a,b) => evalSub(new Sub(a,b), env)
    case Mul(a,b) => evalMul(new Mul(a,b), env)
    case Div(a,b) => evalDiv(new Div(a,b), env)
    case Assign(a,b) => evalAssign(new Assign(a,b), env)
    case Program(a) => evalProgram(new Program(a), env)
  }
  def evalNumber(num: Number, env: Environment): EvaluationResult = new EvaluationResult(new Value(num.value),env)
  def evalVar(v: Var, env: Environment): EvaluationResult = {
    if(env.lookup(v) == None)
      throw new EvaluationException("Teddy this ones on you")
    else new EvaluationResult(env.lookup(v).get.a, env)
  }

  def evalAdd(op: Add, env: Environment): EvaluationResult = new EvaluationResult(new Value(eval(op.left,env).value.i + (eval(op.right,env).value.i)), env)
  def evalSub(op: Sub, env: Environment): EvaluationResult = new EvaluationResult(new Value(eval(op.left,env).value.i - (eval(op.right,env).value.i)), env)
  def evalMul(op: Mul, env: Environment): EvaluationResult = new EvaluationResult(new Value(eval(op.left,env).value.i * (eval(op.right,env).value.i)), env)
  def evalDiv(op: Div, env: Environment): EvaluationResult = new EvaluationResult(new Value(eval(op.left,env).value.i / (eval(op.right,env).value.i)), env)
  def evalAssign(op: Assign, env: Environment): EvaluationResult = new EvaluationResult(eval(op.right,env).value, env.extend(op.left, eval(op.right, env).value))
  def evalProgram(prog: Program, env: Environment): EvaluationResult = {
    if(prog.exprs.isEmpty) {
      throw new EvaluationException("Talk to dan about this")
    }else{ prog.exprs match{
      case head::Nil => eval(head, env)
      case head::tail => evalProgram(new Program(tail), eval(head,env).env)
      //case Nil => evalNumber(new Number(0),env)
    }

    }
  }
}

/** A factory object for an evaluator. */
object Evaluator extends SimpleEvaluator
