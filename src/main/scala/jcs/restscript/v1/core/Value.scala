package jcs.restscript.v1.core

/**
  * Created by Jeremy on 6/20/2016.
  */
abstract class Value

case class IntValue(value: Int) extends Value
case class StringValue(s: String) extends Value
case class BoolValue(b: Boolean) extends Value
case class ListValue(vs: List[Value]) extends Value



case class LambdaValue(nameList: ListValue, body: Expression, lambdaEnv: Environment) extends Value {
    def app(expList: List[Expression], env: Environment) = {
        def envAcc(envCur: Environment, names: ListValue, exps: List[Expression]) : Environment
                = (names, exps)  match {
            case (ListValue(nExp :: nExps), bExp :: bExps) => (nExp, bExp.eval(env)) match {
                case (StringValue(s), v: Value) => envAcc(envCur + (s -> v), ListValue(nExps), bExps)
                case _ => throw MalformedException("Variable name not string")
            }
            case (ListValue(Nil), Nil) => envCur
            case (_, _) =>  throw MalformedException("Name Parameter mismatch")
        }
        body.eval(envAcc(lambdaEnv, nameList, expList))
    }
}

