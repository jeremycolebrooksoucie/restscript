package jcs.restscript.v1.core

/**
  * Created by Jeremy on 6/20/2016.
  *
  */

abstract class Primitive() extends Value {
    def app(expList: List[Expression], env: Environment): Value
}

case class IfElse() extends Primitive {
    def app(expList: List[Expression], env: Environment) = expList match {
        case List(boolExp, e1, e2) => boolExp.eval(env) match {
            case BoolValue(true) => e1.eval(env)
            case BoolValue(false) => e2.eval(env)
            case IntValue(0)  => e2.eval(env)
            case IntValue(_) => e1.eval(env)

            case _ => throw new MalformedException("IfElse non bool predicate")
        }
        case _ => throw new MalformedException("IfElse bad number params")
    }
}

/**
  * Arithmetic Primitives
  */
case class Mult() extends Primitive {
    def app(expList: List[Expression], env: Environment) = expList.map(_.eval(env)) match{
        case IntValue(v1) :: IntValue(v2)  :: Nil => IntValue(v1 * v2)
        case _ => throw new MalformedException("Invalid multiplication application")
    }
}

case class Add() extends Primitive {
    def app(expList: List[Expression], env: Environment) = expList.map(_.eval(env)) match{
        case IntValue(v1) :: IntValue(v2)  :: Nil => IntValue(v1 + v2)
        case _ => throw new MalformedException("Invalid multiplication application")
    }
}

/**
  *
  */
//case class Let() extends Primitive {

//}

case class LetSingle() extends Primitive {
    def app(expList: List[Expression], env: Environment) = expList match {
        case List(nameExp, valExp, restCalculation) => (nameExp.eval(env), valExp.eval(env)) match {
            case (StringValue(name), (v : Value)) => restCalculation.eval(env + (name -> v))
            case _ => throw new MalformedException("Bad LetSingle var binding")
        }
        case _ => throw new MalformedException("Bad LetSingle form")
    }

    //expList.map(_.eval()) match {
    //    case StringValue(name) :: (v : Value) ::
}

case class Lambda() extends Primitive {
    def app(expList: List[Expression], env: Environment) = expList match  {
        case List(paramList, body) => paramList.eval(env) match {
            case vs: ListValue => LambdaValue(vs, body, env)
            case _ => throw new MalformedException("Lambda definition: parameter list does eval to ListValue")
        }
        case _ => throw new MalformedException("Lambda definition: too many parameters")
    }
    /// LambdaValue(expList.map(_eval(env)), env)
}


case class ListCreate() extends Primitive {
    def app(expList: List[Expression], env: Environment) = ListValue(expList.map(_.eval(env)))
}