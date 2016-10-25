package jcs.restscript.v2.parser
import com.lambdaworks.jacks.JacksMapper
import jcs.restscript.v2.core._


/**
  * Created by Jeremy on 6/25/2016.
  */
class Resolver(rawJson: String) {
    def resolve() = {
        //val out = JacksMapper.readValue[Map[String, Any]](raw)
        val out = JacksMapper.readValue[Map[String, Any]](rawJson)

        convertToExp(expand(out))

    }

    def convertToExp(token: JsonObject): Expression = {
        token match {
            case JsonApplication("Int", s: String) => new LiteralExp(s.toInt)
            case JsonApplication("IfElse", List(e1, e2, e3))
                    => IfElseExp(convertToExp(expand(e1)), convertToExp(expand(e2)), convertToExp(expand(e3)))
            case JsonApplication("Let", List(bindings, body))
                    => LetExp(expandLetBindings(bindings), convertToExp(expand(body)))
            case JsonApplication("Lambda", List(params, body))
                    => LambdaExp(expandLambdaParams(params), convertToExp(expand(body)))
            case JsonApplication("List", members : List[Any]) => ListExpression(members.map(expand _ andThen convertToExp _))


            case JsonApplication(prim,  es : List[Any]) if Primitive.isValid(prim)
                    => Apply(PrimitiveExp(Primitive.get(prim)), es.map(expand _ andThen convertToExp _))
            case JsonSingle(prim) if Primitive.isValid(prim) => PrimitiveExp(Primitive.get(prim))



            case JsonSingle(s) if s.length != 0 && s.charAt(0) == '#' => s.drop(1) match {
                case "t" => new LiteralExp(true)
                case "f" => new LiteralExp(false)
                case numStr => new LiteralExp(s.drop(1).toInt)
            }
            case JsonSingle(s) if s.length != 0 && s.charAt(0) == '$' => VariableExp(s.drop(1))
            case JsonSingle(s) => new LiteralExp(s)

            case JsonApplication(s, ps : List[Any]) if s.length != 0 && s.charAt(0) == '$'
                => new Apply(VariableExp(s.drop(1)),
                             ps.map(expand _ andThen convertToExp _))
            //case JsonSingle(s) => new LiteralExp(0)
        }
    }



    def expand(e: Any): JsonObject= e match{
        case map: Map[String, Any] => expandMap(map)
        case single: String => expandSingle(single)
        //case arr: List[Any] =>

    }

    def expandSingle(single: String): JsonSingle = JsonSingle(single)

    def expandMap(e: Map[String, Any]): JsonApplication = e.unzip match {
        case (List(cmd), List(params)) => JsonApplication(cmd, params)
    }


    def expandLetBindings(bindings: Any) : List[(String, Expression)]= {
        bindings match {
            case map: Map[String, Any] => {
                val (names, rawExps) = map.unzip
                val exps = rawExps.map(expand(_)).map(convertToExp(_))
                (names zip exps).toList

            }
        }
    }

    def expandLambdaParams(params: Any) : List[String] = {
        params match {
            case ps : List[String] => ps
        }
    }
}


abstract class JsonObject
case class JsonApplication(command: String, params: Any) extends JsonObject
case class JsonSingle(single: String) extends JsonObject
//case class JsonArray(arr: List[JsonObject])
//case class JsonToken(command: String, params: Any)
