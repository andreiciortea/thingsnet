package utils

import scala.util._

import org.w3.banana._
import org.w3.banana.diesel._
import org.w3.banana.jena._
import org.w3.banana.syntax._
import play.api.libs.json._
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.functional.syntax.functionalCanBuildApplicative


object Parser extends RDFModule with SparqlGraphModule with TurtleReaderModule with JenaModule {

val jsonData = """
  {
    "name": "Doug Williams",
    "profile_sidebar_border_color": "87BC44",
    "profile_sidebar_fill_color": "F2F8FC",
    "profile_background_tile": false,
    "created_at": "Sun Mar 18 06:42:26 +0000 2007",
    "profile_image_url": "http://a0.twimg.com/profile_images/1947332700/dougw_avatar_can2_normal.jpg",
    "location": "San Francisco, CA",
    "profile_link_color": "0000FF",
    "follow_request_sent": false,
    "is_translator": false,
    "id_str": "1401881",
    "entities": {
      "description": {
        "urls": [
   
        ]
      }
    },
    "default_profile": false,
    "url": null,
    "favourites_count": 2620,
    "contributors_enabled": true,
    "utc_offset": -28800,
    "id": 1401881,
    "profile_image_url_https": "https://si0.twimg.com/profile_images/1947332700/dougw_avatar_can2_normal.jpg",
    "listed_count": 890,
    "profile_use_background_image": true,
    "profile_text_color": "000000",
    "protected": false,
    "lang": "en",
    "followers_count": 306556,
    "geo_enabled": true,
    "time_zone": "Pacific Time (US & Canada)",
    "notifications": false,
    "verified": false,
    "profile_background_image_url_https": "https://si0.twimg.com/profile_background_images/2752608/twitter_bg_grass.jpg",
    "profile_background_color": "9AE4E8",
    "description": "Partnerships at @twitter",
    "profile_background_image_url": "http://a0.twimg.com/profile_background_images/2752608/twitter_bg_grass.jpg",
    "friends_count": 1284,
    "statuses_count": 11873,
    "default_profile_image": false,
    "status": {
      "coordinates": null,
      "truncated": false,
      "created_at": "Sun Aug 26 21:58:35 +0000 2012",
      "favorited": false,
      "id_str": "239844310982459392",
      "entities": {
        "urls": [
   
        ],
        "media": [
          {
            "media_url_https": "https://p.twimg.com/A1QZIU3CAAAnCkS.jpg",
            "expanded_url": "http://twitter.com/dougw/status/239844310982459392/photo/1",
            "sizes": {
              "large": {
                "w": 816,
                "resize": "fit",
                "h": 612
              },
              "medium": {
                "w": 600,
                "resize": "fit",
                "h": 450
              },
              "small": {
                "w": 340,
                "resize": "fit",
                "h": 255
              },
              "thumb": {
                "w": 150,
                "resize": "crop",
                "h": 150
              }
            },
            "id_str": "239844310986653696",
            "url": "http://t.co/1uG4mhaB",
            "id": 239844310986653696,
            "type": "photo",
            "indices": [
              45,
              65
            ],
            "media_url": "http://p.twimg.com/A1QZIU3CAAAnCkS.jpg",
            "display_url": "pic.twitter.com/1uG4mhaB"
          }
        ],
        "hashtags": [
   
        ],
        "user_mentions": [
   
        ]
      },
      "in_reply_to_user_id_str": null,
      "text": "Warm day by the Bay. Watching boats sail by. http://t.co/1uG4mhaB",
      "contributors": null,
      "id": 239844310982459392,
      "retweet_count": 0,
      "in_reply_to_status_id_str": null,
      "geo": null,
      "retweeted": false,
      "in_reply_to_user_id": null,
      "possibly_sensitive": false,
      "place": null,
      "possibly_sensitive_editable": true,
      "source": "<a>Camera on iOS</a>",
      "in_reply_to_screen_name": null,
      "in_reply_to_status_id": null
    },
    "show_all_inline_media": true,
    "screen_name": "dougw",
    "following": false
  }
  """

val template = """
  @base <http://www.twitter.com/> .
  @prefix stn: <http://purl.org/stn/core#> .
  @prefix stn-ops: <http://purl.org/stn/operations#> .
  @prefix stn-http: <http://purl.org/stn/http#> .
  @prefix http: <http://www.w3.org/2011/http#> .
  
  <#twitterAccount>
    a stn:UserAccount ;
    stn:id _:userAccountId ;
    stn:name _:displayedName ;
    stn:description _:description .

  <#twitterAccountJSONRepresentation>
    a stn-http:JSONRepresentation ;
    stn-ops:representationOf <#twitterAccount> ;
    stn-ops:contains
        [ a stn-ops:UserAccountID ;
            stn-ops:key "screen_name" ;
            stn-ops:value _:userAccountId ;
        ] ;
    stn-ops:contains
        [ a stn-ops:DisplayedName ;
            stn-ops:key "name" ;
            stn-ops:value _:displayedName ;
        ] ;
    stn-ops:contains
        [ a stn-ops:Description ;
            stn-ops:key "description" ;
            stn-ops:value _:description ;
        ] .
    
    <#getMyAccount>
      a stn-ops:GetMyUserAccount ;
      stn-ops:implementedAs
          [ a stn-http:STNRequest ;
              http:methodName "GET" ;
              http:requestURI "/account/verify_credentials.json" ;
              stn-http:requiresAuth true ;
          ] ;
      stn-ops:hasOutput <#twitterAccountJSONRepresentation> .
    """

val newtemplate = """
  @base <http://www.twitter.com/> .
  @prefix stn: <http://purl.org/stn/core#> .
  @prefix stn-ops: <http://purl.org/stn/operations#> .
  @prefix stn-http: <http://purl.org/stn/http#> .
  @prefix http: <http://www.w3.org/2011/http#> .
  
  <#twitterAccountJSONMapping>
    a stn-http:JSONRepresentation ;
    stn-ops:representationOf [ a stn:UserAccount ] ;
    stn-ops:contains [
            a stn-http:Mapping ;
            stn-http:paramKey "screen_name" ;
            stn-http:stnTerm stn:id ;
        ] ;
    stn-ops:contains [
            a stn-http:Mapping ;
            stn-http:paramKey "name" ;
            stn-http:stnTerm stn:name ;
        ] ;
    stn-ops:contains [
            a stn-http:Mapping ;
            stn-http:paramKey "description" ;
            stn-http:stnTerm stn:description ;
        ] .

  <#getMyAccount>
    a stn-ops:GetMyUserAccount ;
    stn-ops:implementedAs [
            a stn-http:AuthSTNRequest ;
            http:methodName "GET" ;
            http:requestURI "/account/verify_credentials.json" ;
        ] ;
    stn-ops:hasOutput <#twitterAccountJSONMapping> .
  """

  import Ops._
  import SparqlOps._

//  val graph = TurtleReader.read(template, "") getOrElse sys.error("Couldn't read the STN spec.");
  val graph = TurtleReader.read(newtemplate, "") getOrElse sys.error("Couldn't read the STN spec.");
  val sparqlEngine = SparqlGraph(graph)
  
  def extractOutputSpec = {
    val query = """
      |prefix stn: <http://purl.org/stn/core#>
      |prefix stn-ops: <http://purl.org/stn/operations#>
      |prefix stn-http: <http://purl.org/stn/http#>
      |prefix http: <http://www.w3.org/2011/http#>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |prefix http: <http://www.w3.org/2011/http#>
      |
      |SELECT ?opUri ?output ?outputFormat
      |WHERE {
      |  ?opUri stn-ops:hasOutput ?output .
      |  ?output rdf:type ?outputFormat .
      |}""".stripMargin
    
    val row = sparqlEngine.executeSelect(SelectQuery(query)).getOrFail().toIterable.toList(0)
    
    val opUri = row("opUri")  getOrElse sys.error("") toString()
    val outputUri = row("output")  getOrElse sys.error("") toString()
    val outputFormat = row("outputFormat")  getOrElse sys.error("") toString()
    
    println("opUri: " + opUri)
    println("outputUri: " + outputUri)
    println("outputFormat: " + outputFormat)
    
    (opUri, outputUri, outputFormat)
  }
  
  def extractFromJSON(outputUri: String) = {
    val query = """
      |prefix stn: <http://purl.org/stn/core#>
      |prefix stn-ops: <http://purl.org/stn/operations#>
      |prefix stn-http: <http://purl.org/stn/http#>
      |prefix http: <http://www.w3.org/2011/http#>
      |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |prefix http: <http://www.w3.org/2011/http#>
      |
      |SELECT ?entityClass ?paramClass ?paramKey ?stnProp
      |WHERE {
      |  ?outputUri stn-ops:representationOf [ a ?entityClass ] .
      |  
      |  ?outputUri stn-ops:contains [ 
      |        a ?paramClass ;
      |        stn-http:paramKey ?paramKey ;
      |        stn-http:stnTerm ?stnProp ;
      |    ] .
      |}""".stripMargin
    
    val bindings = Map(
        "outputUri" -> URI(outputUri)
      )
    
    println("extracting key-value pairs from JSON")
    
    val rows = sparqlEngine.executeSelect(SelectQuery(query), bindings).getOrFail().toIterable.toList
    
    println("#rows: " + rows.size)
    
    val graph = Graph.empty
    
    
    def extractValue(key: String) = (Json.parse(jsonData) \ key).asOpt[String].get
    
    def addTriple(graph: Rdf#Graph, entityClass: String, paramClass: String, prop: String, key: String) = {
      ( ( 
        graph union Graph( Triple( URI(""), rdf.typ, URI(entityClass) ) ) ) 
          union Graph( Triple( URI(""), URI(prop), Literal(extractValue(key)) ) ) )
        
    }
    
    val extractedGraph = rows map {
      row => {
        val entityClass = row("entityClass") getOrElse sys.error("") toString()
        val paramClass = row("paramClass") getOrElse sys.error("") toString()
        val prop = row("stnProp") getOrElse sys.error("") toString()
        val key = row("paramKey").flatMap(_.as[String]) getOrElse sys.error("")
        
        println("entityClass: " + entityClass)
        println("paramClass: " + paramClass)
        println("prop: " + prop)
        println("key: " + key)
        
        val newGraph = addTriple(graph, entityClass, paramClass, prop, key)
        
        println(TurtleWriter.asString(addTriple(graph, entityClass, paramClass, prop, key), "") getOrElse sys.error("Couldn't serialize the graph."))
        
        newGraph
      }
    } reduceLeft(_ union _)
    
    TurtleWriter.asString(extractedGraph, "") getOrElse sys.error("Couldn't serialize the graph.")
  }
  
  def parse: String = {
    
    val (opUri, outputUri, outputFormat) = extractOutputSpec
    
    import models.STNHttpPrefix
    val stnHttp = STNHttpPrefix[Rdf]
    
    if (outputFormat equals stnHttp.JSONRepresentation.toString()) {
      extractFromJSON(outputUri)
    } else {
      "Unknown format"
    }
    
  }

}