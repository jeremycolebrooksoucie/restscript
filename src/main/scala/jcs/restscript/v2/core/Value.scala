package jcs.restscript.v2.core

/**
  * Created by Jeremy on 6/20/2016.
  */


abstract class Value

case class IntValue(value: Int) extends Value
case class StringValue(s: String) extends Value
case class BoolValue(b: Boolean) extends Value
case class ListValue(vs: List[Value]) extends Value {
    def toList = vs
}


/*
case class LambdaValue(nameList: ListValue, body: Expression, lambdaEnv: Environment) extends Value {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager ) = {
        def envAcc(envCur: Environment, names: ListValue, exps: List[Expression]) : Environment
                = (names, exps)  match {
            case (ListValue(nExp :: nExps), bExp :: bExps) => (nExp, bExp.eval(env, lookup)) match {
                case (StringValue(s), v: Value) => envAcc(envCur + (s -> v), ListValue(nExps), bExps)
                case _ => throw MalformedException("Variable name not string")
            }
            case (ListValue(Nil), Nil) => envCur
            case (_, _) =>  throw MalformedException("Name Parameter mismatch")
        }
        body.eval(envAcc(lambdaEnv, nameList, expList))
    }
}

*/

case class Closure(names: List[String], body: Expression, closureEnv: Environment) extends Value with ApplicableValue {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager) = {
        if (names.size != expList.size) throw new MalformedException("Closure evaluation: wrong num params")
        // extend closure environment with bound variables
        val newEnv: Environment = names.zip(expList).foldLeft(closureEnv)
                                    {case (e, (n, exp)) => e + (n -> lookup.putValue(exp.eval(env, lookup)))
                                     case _ => throw new MalformedException("Closure: binding parameters failed")}
        body.eval(newEnv, lookup)
    }
}