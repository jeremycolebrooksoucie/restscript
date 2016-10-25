package jcs.restscript.v1.core

import scala.collection.mutable.Map
/**
  * Created by Jeremy on 6/25/2016.
  */
class VariableManager {
    private var nextIndex: Int = 0
    private var varMap: Map[Int, Value] = Map()

    def getKey(): ValueKey = {
        val key = new ValueKey(nextIndex)
        nextIndex = nextIndex + 1
        key
    }



}

class ValueKey(private val i: Int)