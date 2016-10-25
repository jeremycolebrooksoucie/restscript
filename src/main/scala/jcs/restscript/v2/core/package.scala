package jcs.restscript.v2

import scala.collection.immutable.Map

/**
  * Created by Jeremy on 6/24/2016.
  */
package object core {
    type Environment = Map[String, ValueKey]
    val emptyEnv: Environment = Map()
}
