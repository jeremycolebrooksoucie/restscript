package jcs.restscript.v1.core

/**
  * Created by Jeremy on 6/20/2016.
  */

abstract class Expression {
    def eval(env: Environment): Value
}




case class Apply(exp: Expression, expList: List[Expression]) extends Expression {
    def eval(env: Environment): Value = exp.eval(env) match {
        case prim : Primitive  => prim.app(expList, env)
        case lambda : LambdaValue => lambda.app(expList, env)
        case _ => throw new MalformedException("Invalid apply")
    }
}

case class LiteralExp(v: Value) extends Expression {
    // testing functions to ease in expression construction
    def this(n: Int)    = { this(IntValue(n)) }
    def this(s: String)  = { this(StringValue(s)) }
    def this(b: Boolean) = { this(BoolValue(b))}

    def eval(env: Environment) = v
}

case class VariableExp(name: String) extends Expression {
    def eval(env: Environment) = env(name)
}

case class PrimitiveExp(prim: Primitive) extends Expression{
    def eval(env: Environment) = prim
}

