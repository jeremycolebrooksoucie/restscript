package jcs.restscript.v1.core.tests

import jcs.restscript.v1.core._
import org.scalatest.FunSuite
/**
  * Created by Jeremy on 6/24/2016.
  */
class PrimitiveTests extends FunSuite{
    /**
      * Multiplication Tests
      */
    test("Primitive Multiplication should multiply two expressions yielding IntValues") {

        val multExp: Expression = Apply(PrimitiveExp(Mult()),
                                        List(LiteralExp(IntValue(5)),  LiteralExp(IntValue(10))))
        assert(multExp.eval(emptyEnv) == IntValue(50))
    }

    test("Primitive multiplication of expressions that do not yield IntValues should fail") {
        intercept[MalformedException] {
            val multExp: Expression = Apply(PrimitiveExp(Mult()),
                                            List(LiteralExp(BoolValue(true)),  LiteralExp(IntValue(10))))
            multExp.eval(emptyEnv)
        }
    }

    test("Primitive Multiplication with too many parameters should fail") {
        intercept[MalformedException] {
            val multExp: Expression = Apply(PrimitiveExp(Mult()),
                List(LiteralExp(IntValue(4)),  LiteralExp(IntValue(10)), LiteralExp(IntValue(1))))
            multExp.eval(emptyEnv)
        }
    }

    /**
      * Addition Tests
      */
    test("Primitive Addition should add two expressions yielding IntValues") {

        val addExp: Expression = Apply(PrimitiveExp(Add()),
                                        List(LiteralExp(IntValue(5)),  LiteralExp(IntValue(10))))
        assert(addExp.eval(emptyEnv) == IntValue(15))
    }

    test("Primitive Addition of expressions that do not yield IntValues should fail") {
        intercept[MalformedException] {
            val addExp: Expression = Apply(PrimitiveExp(Add()),
                                            List(LiteralExp(BoolValue(true)),  LiteralExp(IntValue(10))))
            addExp.eval(emptyEnv)
        }
    }

    test("Primitive Addition with too many parameters should fail") {
        intercept[MalformedException] {
            val addExp: Expression = Apply(PrimitiveExp(Add()),
                List(LiteralExp(IntValue(4)),  LiteralExp(IntValue(10)), LiteralExp(IntValue(1))))
            addExp.eval(emptyEnv)
        }
    }

    /**
      * IfElse Tests
      */
    test("IfElse should evaluate first expression with BoolValue(true) predicate") {
        val IfElseExp = Apply(PrimitiveExp(IfElse()),
                              List(LiteralExp(BoolValue(true)),
                                   LiteralExp(IntValue(100)), LiteralExp(StringValue("Hello"))))
        assert(IfElseExp.eval(emptyEnv) == IntValue(100))
    }

    test("IfElse should evaluate second expression with BoolValue(false) predicate") {
        val IfElseExp = Apply(PrimitiveExp(IfElse()),
            List(LiteralExp(BoolValue(false)),
                LiteralExp(IntValue(100)), LiteralExp(StringValue("Hello"))))
        assert(IfElseExp.eval(emptyEnv) == StringValue("Hello"))
    }

    test("IfElse should evaluate first expression with IntValue(n) where n is non 0 predicate") {
        val IfElseExp = Apply(PrimitiveExp(IfElse()),
            List(LiteralExp(IntValue(10)),
                LiteralExp(IntValue(100)), LiteralExp(StringValue("Hello"))))
        assert(IfElseExp.eval(emptyEnv) == IntValue(100))
    }

    test("IfElse should evaluate second expression with IntValue(0) predicate") {
        val IfElseExp = Apply(PrimitiveExp(IfElse()),
            List(LiteralExp(IntValue(0)),
                LiteralExp(IntValue(100)), LiteralExp(StringValue("Hello"))))
        assert(IfElseExp.eval(emptyEnv) == StringValue("Hello"))
    }

    test("IfElse should fail with predicate expression that does not evaluate to bool/int") {
        intercept[MalformedException] {
            val IfElseExp = Apply(PrimitiveExp(IfElse()),
                List(LiteralExp(StringValue("weee")),
                    LiteralExp(IntValue(100)), LiteralExp(StringValue("Hello"))))
            IfElseExp.eval(emptyEnv)
        }
    }

    test("IfElse should fail with more than 3 parameters") {
        intercept[MalformedException] {
            val IfElseExp = Apply(PrimitiveExp(IfElse()),
                List(LiteralExp(IntValue(0)), LiteralExp(IntValue(110)),
                    LiteralExp(IntValue(100)), LiteralExp(StringValue("Hello"))))
            IfElseExp.eval(emptyEnv)
        }
    }
}
