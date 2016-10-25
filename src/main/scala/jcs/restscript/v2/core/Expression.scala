package jcs.restscript.v2.core

/**
  * Created by Jeremy on 6/20/2016.
  */

abstract class Expression {
    def eval(env: Environment, lookup: VariableManager): Value
}



/*
 * Expression for the application of the result of exp to explist
 * exp should resolve to either primitive or closure
 */
case class Apply(exp: Expression, expList: List[Expression]) extends Expression {
    def eval(env: Environment, lookup: VariableManager): Value = exp.eval(env, lookup) match {
        case applicableValue : ApplicableValue => applicableValue.app(expList, env, lookup)
        /*case prim : Primitive  => prim.app(expList, env, lookup )
        case c: Closure => c.app(expList, env, lookup)*/
        case _ => throw new MalformedException("Invalid apply")
    }
}

/*
 * Expression that resolves to a single literal value set explicitly during instantiation
 */
case class LiteralExp(v: Value) extends Expression {
    //  functions to ease in expression construction
    def this(n: Int)    = { this(IntValue(n)) }
    def this(s: String)  = { this(StringValue(s)) }
    def this(b: Boolean) = { this(BoolValue(b))}
    def this(ps: List[Value]) = {this(ListValue(ps))}
    def eval(env: Environment, lookup: VariableManager) = v
}

/*
 * Expression that resolves to a value looked up from its environment
 */
case class VariableExp(name: String) extends Expression {
    def eval(env: Environment, lookup: VariableManager) = lookup.getValue(env.getOrElse(name,
                throw new MalformedException("Variable Exp: Value " + name + " not found in environment")))
}

/*
 * Resolves to primitive value set on instantiation
 */
case class PrimitiveExp(prim: Primitive) extends Expression{
    def eval(env: Environment, lookup: VariableManager) = prim
}


/*
 * If p resolves to true value or non-zero value, resolves to e1,
 * If p resolves to false value or 0 value, resolves to e2
 * Short circuits (so safe to terminate recursion with)
 */
case class IfElseExp(p: Expression, e1: Expression, e2: Expression) extends Expression{
    def eval(env: Environment, lookup: VariableManager) = p.eval(env, lookup) match {
        case BoolValue(true)  => e1.eval(env, lookup)
        case BoolValue(false) => e2.eval(env, lookup)
        case IntValue(0)      => e2.eval(env, lookup)
        case IntValue(_)     => e1.eval(env, lookup)
        case _ => throw MalformedException("IfElseExp predicate not bool/int")
    }
}

/*
 * Evaluates body in new environment generated from binding exps in nameExps to corresponding names
 * Entries are generated in lookup for all bindings before corresponding expressions are evaluated
 * All expressions are evaluated before any resulting values are bound
 * This allows for recursion and things
 */
case class LetExp(nameExps: List[(String, Expression)], body: Expression) extends Expression {
    def eval(env: Environment, lookup: VariableManager) = {
        val (names, exps) = nameExps.unzip
        // generate a new environment with references for freshly bound variables
        val newEnv: Environment = names.foldLeft(env) { (e, n) => e + (n -> lookup.getFreshKey()) }
        // figure out what expressions evaluate to
        val boundValues = exps.map {_.eval(newEnv, lookup)}
        // actually bind resulting values into lookup at corresponding keys
        for ((s, v) <- names zip boundValues)
            lookup.putWithKey(newEnv(s), v)

        body.eval(newEnv, lookup)
    }
}

/*
 * Resolves to a closure value with params and body set during instantiation and environment set during evaluation
 */
case class LambdaExp(params: List[String], body: Expression) extends Expression {
    def eval(env: Environment, lookup: VariableManager) = Closure(params, body, env)
}

/*
 * Resolves to a list value containing resolved es
 */
case class ListExpression(es: List[Expression]) extends Expression {
    def eval(env: Environment, lookup: VariableManager) = new ListValue(es.map(_.eval(env, lookup)))
}