package utils

import org.w3.banana._
import org.w3.banana.diesel._
import org.w3.banana.jena._
import org.w3.banana.syntax._
import play.api.libs.json._
import play.utils.UriEncoding
//import play.api.libs.functional.syntax.toFunctionalBuilderOps
//import play.api.libs.functional.syntax.functionalCanBuildApplicative

object JSON2RDF extends RDFModule with JenaModule {

  import Ops._
  
  def toRDF(jsonStr: String, out: Output): String = {
  
    
//    def extractValue(key: String) = (Json.parse(jsonStr) \ key).asOpt[String].get
    
//    def extractValueFromObj(obj: JsValue, key: String) = (obj \ key).asOpt[String]
    
    
    def valueToLiteral(value: JsValue): Option[Rdf#Literal] = {
      value match {
        case s: JsString => Some(Literal(s.asOpt[String].get))
        case n: JsNumber => Some(Literal(n.asOpt[Int].get.toString()))
        case v: JsValue => Some(Literal(v.toString()))
        case a: JsArray => Some(Literal(a.toString()))
        case _ => None
      }
    }
    
    def jsObj2RDF(subj: String, jsObj: JsValue) = {
      out.mappings map {
        m => {
//          val value = extractValueFromObj(jsObj, m.key)
          val value = valueToLiteral(jsObj \ m.key)
          if (value.isEmpty) Graph.empty
          else Graph( Triple( URI(subj), URI(m.prop), value.get ) )
        }
      } reduceLeft(_ union _) union Graph( Triple(URI(subj), rdf.typ, URI(out.entityClass)) )
    }
    
    import models.STNPrefix
    
    def extractId(obj: JsValue, mappings: List[OutputMapping]): Option[String] = {
      if (mappings.isEmpty) {
        None
      } else {
        val head::tail = mappings
        if (head.prop == STNPrefix[Rdf].id.toString()) {
          val idValue = (obj \ head.key) match {
            case s: JsString => s.asOpt[String].get
            case n: JsNumber => n.asOpt[Int].get.toString()
          }
          
          Some(UriEncoding.encodePathSegment(idValue, "UTF-8"))
        }
        else extractId(obj, tail)
      }
    }
    
    
    import models.STNHttpPrefix
    
    val jsonData =
      if (out.rootKey.isEmpty) {
        Json.parse(jsonStr)
      } else {
        (Json.parse(jsonStr) \ out.rootKey.get)
      }
    
    val graph = if (out.representationFormat == STNHttpPrefix[Rdf].JSONArray.toString()) {
      val array = jsonData.asOpt[JsArray]
      
      if (!array.isEmpty && !array.get.value.isEmpty) {
        val result = array.get.value map {
          o =>
            jsObj2RDF("http://example.com/" + extractId(o, out.mappings).get, o)
        } reduceLeft(_ union _)
        Some(result)
      } else {
        None
      }
    } else if (out.representationFormat == STNHttpPrefix[Rdf].JSONRepresentation.toString()) {
      val obj = jsonData.asOpt[JsValue]
      if (!obj.isEmpty) Some(jsObj2RDF("http://example.com/" + extractId(obj.get, out.mappings).get, obj.get)) else None
    } else {
      None
    }
    
//    val graph = out.mappings map {
//      m => Graph( Triple( URI(""), URI(m.prop), Literal(extractValue(m.key)) ) )
//    } reduceLeft(_ union _) union Graph( Triple(URI(""), rdf.typ, URI(out.entityClass)) )
    
    if (!graph.isEmpty) {
      TurtleWriter.asString(graph.get, "") getOrElse sys.error("Couldn't serialize the graph.")
    } else ""
  }
}