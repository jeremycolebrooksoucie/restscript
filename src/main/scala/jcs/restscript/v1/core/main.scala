package jcs.restscript.v1.core

/**
  * Created by Jeremy on 6/21/2016.
  */

object main extends App{
    val env: Environment = Map("x" -> IntValue(5))
    println("Beginning tests")
    val exp: Expression = Apply(PrimitiveExp(LetSingle()),
                            new LiteralExp("y") :: new LiteralExp(11) ::
                            Apply(PrimitiveExp(Mult()), new LiteralExp(5) :: new LiteralExp(10) :: Nil ) :: Nil)

    val exp2: Expression = Apply(PrimitiveExp(Mult()), VariableExp("x") ::  VariableExp("x") :: Nil)

    val exp3: Expression = Apply(PrimitiveExp(LetSingle()),
        new LiteralExp("y") :: new LiteralExp(11) ::
            Apply(PrimitiveExp(Mult()), new VariableExp("x") :: new VariableExp("y") :: Nil ) :: Nil)


    println(exp)
    println(exp.eval(env))
    println(exp2.eval(env))
    println(exp3.eval(env))

    val lambdaTest = LambdaValue(ListValue(List(new LiteralExp("a").eval(emptyEnv), new LiteralExp("b").eval(emptyEnv))),
                            Apply(PrimitiveExp(Mult()), List(VariableExp("a"), VariableExp("b"))), emptyEnv)

    val env2: Environment = Map("lam" -> lambdaTest)
    val exp4 = Apply(VariableExp("lam"), List(new LiteralExp(5), new LiteralExp(8)))
    println(exp4.eval(env2))


    val IfElseTestTrue = Apply(PrimitiveExp(IfElse()), List(new LiteralExp(true), new LiteralExp(5), exp4))
    val IfElseTestFalse = Apply(PrimitiveExp(IfElse()), List(new LiteralExp(false), new LiteralExp(5), exp4))

    println(IfElseTestTrue.eval(env2))
    println(IfElseTestFalse.eval(env2))


    val IfElseTestTrueInt = Apply(PrimitiveExp(IfElse()), List(new LiteralExp(1), new LiteralExp(5), exp4))
    val IfElseTestFalseInt = Apply(PrimitiveExp(IfElse()), List(new LiteralExp(0), new LiteralExp(5), exp4))

    println(IfElseTestTrueInt.eval(env2))
    println(IfElseTestFalseInt.eval(env2))



    /*lazy val factorial = LambdaValue(ListValue(List(new LiteralExp("i").eval(emptyEnv))),
        Apply(PrimitiveExp(IfElse()),
              List(VariableExp("i"),
                  Apply(PrimitiveExp(Mult()),
                        List(VariableExp("i"),
                             Apply(VariableExp("factorial"),
                                   List(Apply(PrimitiveExp(Add()), List(VariableExp("i"),
                                                          new LiteralExp(-1)) : List[Expression]))))),
                  new LiteralExp(1))),
               factTestEnv)
    val factTestEnv = Map("factorial" -> factorial)

    val factTest = Apply(VariableExp("factorial"), List(new LiteralExp(10)))

    println(factTest.eval(factTestEnv))*/

    val listTest = Apply(PrimitiveExp(ListCreate()), List(new LiteralExp(10), new LiteralExp(15)))
    println(listTest.eval(emptyEnv))

}
