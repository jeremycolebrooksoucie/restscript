package jcs.restscript.v2.core

/**
  * Created by Jeremy on 6/20/2016.
  *
  */


object Primitive {
    val primLookup : Map[String, Primitive] = Map(
        "*" -> Mult(),
        "+" -> Add(),
        "/" -> Div(),
        "-" -> Subtract(),
        "cons" -> Cons(),
        "car" -> Car(),
        "cdr" -> Cdr()

    )

    def get(primString: String) : Primitive = primLookup(primString)
    def isValid(primString: String) : Boolean = primLookup.keySet.contains(primString)
}


abstract class Primitive() extends Value with ApplicableValue {
    //def app(expList: List[Expression], env: Environment, lookup: VariableManager): Value
}

/*
case class IfElse() extends Primitive {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager) = expList match {
        case List(boolExp, e1, e2) => boolExp.eval(env, lookup) match {
            case BoolValue(true) => e1.eval(env, lookup)
            case BoolValue(false) => e2.eval(env, lookup)
            case IntValue(0)  => e2.eval(env, lookup)
            case IntValue(_) => e1.eval(env, lookup)

            case _ => throw new MalformedException("IfElse non bool predicate")
        }
        case _ => throw new MalformedException("IfElse bad number params")
    }
}
*/
/**
  * Arithmetic Primitives
  */
case class Mult() extends Primitive {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager) = expList.map(_.eval(env, lookup)) match{
        case IntValue(v1) :: IntValue(v2)  :: Nil => IntValue(v1 * v2)
        case _ => throw new MalformedException("Invalid multiplication application")
    }
}

case class Add() extends Primitive {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager) = expList.map(_.eval(env, lookup)) match{
        case IntValue(v1) :: IntValue(v2)  :: Nil => IntValue(v1 + v2)
        case _ => throw new MalformedException("Invalid addition application")
    }
}

case class Div() extends Primitive {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager) = expList.map(_.eval(env, lookup)) match{
        case IntValue(v1) :: IntValue(v2)  :: Nil => IntValue(v1 / v2)
        case _ => throw new MalformedException("Invalid division application")
    }
}

case class Subtract() extends Primitive {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager) = expList.map(_.eval(env, lookup)) match{
        case IntValue(v1) :: IntValue(v2)  :: Nil => IntValue(v1 - v2)
        case _ => throw new MalformedException("Invalid subtract application")
    }
}

case class Cons() extends Primitive {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager) = expList.map(_.eval(env, lookup)) match {
        case List(head : Value, ListValue(tail)) => ListValue(head :: tail)
    }
}

case class Car() extends Primitive {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager) = expList.map(_.eval(env, lookup)) match {
        case List(ListValue(head :: tail)) => head
    }
}


case class Cdr() extends Primitive {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager) = expList.map(_.eval(env, lookup)) match {
        case List(ListValue(head :: tail)) => ListValue(tail)
    }
}



/**
  *
  */

/*
case class Let() extends Primitive {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager) = {
        def bindNames(name: StringValue, exp: )
        expList match {
            case List(pairs, restCalculation) => pairs.eval(env, lookup) match {
                case ListValue(ListValue(n:: e:: Nil) :: rest) =>
            }
        }
    }

}
*/
//}
/*
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
*/

/*
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
*/

/*
case class ListCreate() extends Primitive {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager)
        = ListValue(expList.map(_.eval(env, lookup)))
}
*/
