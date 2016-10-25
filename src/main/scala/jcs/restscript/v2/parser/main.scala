package jcs.restscript.v2.parser


import com.lambdaworks.jacks.JacksMapper
import jcs.restscript.v2.core._


/**
  * TODO HERE
  *
  * Lambda Expression Parsing
  * Finish implementing primitive stuff
  *
  *
  */


/**
  * Created by Jeremy on 6/25/2016.
  */
object main extends App{
    var lookup: VariableManager = new VariableManager()

    //val a  = Parser.parseFromString()
    val raw = """{"Hello": ["a", {"*": ["a"]}]}"""

    val LiteralInt = """{"Int":"5"} """
    val IfElse = """ {"IfElse": [{"Int": "0"}, {"Int": "5"}, {"Int": "10"} ]}  """
    val IfElse2 = """ {"IfElse": ["#0", "#5", "#10" ]}  """

    val IfElseNested = """ {"IfElse": [ {"IfElse": ["$t", "#5", "#f"]}  , {"Int": "5"}, {"Int": "10"} ]} """

    val LetTest = """{"Let": [{"x": "#5"}, "$x"]} """"

    val multTest = """ {"*" : ["#5", "#5"]} """

    val letMultTest = """ {"Let" : [{"x": "#5", "y" : "#4"}, {"*" : ["$x", "$y"]}]}  """

    val timeTwoLambda =
        """
           {"Let" : [
                {"time2" :
                    {"Lambda" :
                        [["y"],
                         {"*" : ["#2", "$y"]}]},
                 "x" : "#5"
                },
                {"$time2" : ["$x"]}
            ]}
        """

    val closureTest =
        """
           {"Let" : [
                {"timeN" :
                    {"Lambda" :
                        [["x"],
                         {"$sym" : ["$x", "$x"]}]},
                 "x" : "#5",
                 "sym" : "*"
                },
                {"$timeN" : ["#2"]}
            ]}
        """

    val listTest1 =
        """
          {"List" : ["#1", "#2", "#3", "4"]}
        """

    val listTestCar =
        """
          {"cons" : ["hello", {"List" : ["#1", "#2", "#3", "4"]}]}

        """

    println(timeTwoLambda)


    //val map = JSON.parseFull(raw)
    //println(map)
    val a = new Resolver(listTestCar)
    //print(a)
    println(a.resolve().eval(emptyEnv, lookup))


}
