package jcs.restscript.v2.core

import scala.collection.mutable.Map

/**
  * Created by Jeremy on 6/25/2016.
  */
class VariableManager {
    private var nextIndex: Int = 0
    private var varMap: Map[ValueKey, Value] = Map()



    def getFreshKey(): ValueKey = {
        val key = new ValueKey(nextIndex)
        nextIndex = nextIndex + 1
        key
    }

    def putValue(v: Value): ValueKey = {
        val key = this.getFreshKey()
        varMap.put(key, v)
        key
    }

    def putWithKey(k: ValueKey, v: Value): Unit =  varMap.put(k, v)

    def getValue(k: ValueKey) = varMap.getOrElse(k, throw new MalformedException("Variable not found"))


}

class ValueKey(private val i: Int)