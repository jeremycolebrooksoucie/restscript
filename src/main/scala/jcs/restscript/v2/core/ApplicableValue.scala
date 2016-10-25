package jcs.restscript.v2.core

/**
  * Created by Jeremy on 9/7/2016.
  */
trait ApplicableValue {
    def app(expList: List[Expression], env: Environment, lookup: VariableManager): Value
}
