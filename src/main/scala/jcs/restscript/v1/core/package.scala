package jcs.restscript.v1

import scala.collection.immutable.Map

/**
  * Created by Jeremy on 6/24/2016.
  */
package object core {
    type Environment = Map[String, Value]
    val emptyEnv: Environment = Map()
}
