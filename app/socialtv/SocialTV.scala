package socialtv


import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import org.w3.banana._
import org.w3.banana.jena._
import play.api.libs.ws._
import utils._
import models.STNOpsPrefix
import scala.concurrent.duration.DurationInt


object SocialTV extends RDFModule with JenaModule {

  import Ops._
  import models.STNPrefix
  import models.STNOpsPrefix
  
  
  def runTestSuite(client: STNClient, ops: List[(String, String, Map[String,String])], 
      out: List[(String, String)]): List[(String, String)] = {
    
    if (ops.isEmpty) {
      out
    } else {
      val head::tail = ops
      
      import scala.concurrent.Await
      import scala.concurrent.duration._
      runTestSuite(client, tail, 
          out :+ (head._1, Await.result[String](client.runOperation(head._2, head._3), 5000 millis))
        )
    }
    
  }
  
  def buildOutput(outList: List[(String, String)], output: String): Future[String] = {
    if (outList.isEmpty) {
      Future(output)
    }
    else {
      val head::tail = outList
      buildOutput(tail, output + head._1 + "\n" + head._2 + "\n=====\n")
    }
  }
  
  def runThingsNetTestSuite(stn: STNClient) = {
    stn.platform map {
      platform => println(platform)
    }
    
    buildOutput(
      runTestSuite(stn,
        List(
          ("CreateUserAccount",
            STNOpsPrefix[Rdf].CreateUserAccount toString(),
            Map(STNOpsPrefix[Rdf].SocialThingClass.toString() -> "http://www.example.com/#SocialTV",
              STNOpsPrefix[Rdf].SocialThingOwner.toString() -> "http://www.example.com/#John",
              STNOpsPrefix[Rdf].DisplayedName.toString() -> "John's TV")
            ),
          ("WhoIsAgent",
            STNOpsPrefix[Rdf].WhoIsAgent toString(),
            Map(STNOpsPrefix[Rdf].AgentURI.toString() -> "http://api.mymanufacturer.com/tvs/874260#thing")
            ),
          ("GetFriends", STNOpsPrefix[Rdf].GetConnectionsFrom toString(),
              Map(STNOpsPrefix[Rdf].UserAccountURI.toString() -> "http://localhost:9000/users/e754d7f8-3cfe-440b-9409-ed21f40af8f3")
            ),
          ("Create connection",
              STNOpsPrefix[Rdf].CreateConnectionTo toString(),
              Map(STNOpsPrefix[Rdf].AgentURI.toString() -> "http://www.example.com/#thing")
            ),
          ("WhoIsAgent",
            STNOpsPrefix[Rdf].WhoIsAgent toString(),
            Map(STNOpsPrefix[Rdf].AgentURI.toString() -> "http://api.mymanufacturer.com/tvs/874260#thing")
            ),
          ("Delete connection",
              STNOpsPrefix[Rdf].DeleteConnectionTo toString(),
              Map(STNOpsPrefix[Rdf].AgentURI.toString() -> "http://www.example.com/#thing")
            ),
          ("WhoIsAgent",
            STNOpsPrefix[Rdf].WhoIsAgent toString(),
            Map(STNOpsPrefix[Rdf].AgentURI.toString() -> "http://api.mymanufacturer.com/tvs/874260#thing")
            ),
          ("DeleteUserAccount", STNOpsPrefix[Rdf].DeleteUserAccount toString(), Map()),
          ("WhoIsAgent",
            STNOpsPrefix[Rdf].WhoIsAgent toString(),
            Map(STNOpsPrefix[Rdf].AgentURI.toString() -> "http://api.mymanufacturer.com/tvs/874260#thing")
            )
        ),
        List()
      ),
      ""
    )
  }
  
  def runTwitterTestSuite(stn: STNClient) = {
    stn.platform map {
      platform => println(platform)
    }
    
    buildOutput(
      runTestSuite(stn,
        List(
          ("GetMyUserAccount", STNOpsPrefix[Rdf].GetMyUserAccount toString(), Map()),
          ("GetUserAccount", STNOpsPrefix[Rdf].GetUserAccount toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "swotdev")
            ),
          ("GetFriends", STNOpsPrefix[Rdf].GetConnectionsFrom toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "swotwm")
            ),
          ("Follow", STNOpsPrefix[Rdf].CreateConnectionTo toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "swotdoor")
            ),
          ("GetFriends", STNOpsPrefix[Rdf].GetConnectionsFrom toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "swotwm")
            ),
          ("Unfollow", STNOpsPrefix[Rdf].DeleteConnectionTo toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "swotdoor")
            ),
          ("GetFriends", STNOpsPrefix[Rdf].GetConnectionsFrom toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "swotwm")
            ),
          ("GetFollowers", STNOpsPrefix[Rdf].GetConnectionsTo toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "swotwm")
            )
        ),
        List()
      ),
      ""
    )
  }
  
  def runFacebookTestSuite(stn: STNClient) = {
    stn.platform map {
      platform => println(platform)
    }
    
    buildOutput(
      runTestSuite(stn,
        List(
          ("GetUserAccount", STNOpsPrefix[Rdf].GetUserAccount toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "1550387481863557")
            ),
          ("GetConnectionsTo", STNOpsPrefix[Rdf].GetConnectionsTo toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "1550387481863557")
            ),
          ("GetConnectionsFrom", STNOpsPrefix[Rdf].GetConnectionsFrom toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "1550387481863557")
            )
        ),
        List()
      ),
      ""
    )
  }
  
  def run8tracksTestSuite(stn: STNClient) = {
    stn.platform map {
      platform => println(platform)
    }
    
    buildOutput(
      runTestSuite(stn,
        List(
          ("GetUserAccount", STNOpsPrefix[Rdf].GetUserAccount toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "13284802")
            ),
          ("GetConnectionsTo", STNOpsPrefix[Rdf].GetConnectionsTo toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "13284802")
            ),
          ("GetConnectionsFrom", STNOpsPrefix[Rdf].GetConnectionsFrom toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "13284802")
            ),
          ("Unfollow", STNOpsPrefix[Rdf].DeleteConnectionTo toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "7888920")
            ),
          ("GetConnectionsFrom", STNOpsPrefix[Rdf].GetConnectionsFrom toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "13284802")
            ),
          ("Follow", STNOpsPrefix[Rdf].CreateConnectionTo toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "7888920")
            ),
          ("GetConnectionsFrom", STNOpsPrefix[Rdf].GetConnectionsFrom toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> "13284802")
            )
        ),
        List()
      ),
      ""
    )
  }
  
  
  def retrieveOutConnections(stn: STNClient, accountUri: String) = {
    stn.runOperation(STNOpsPrefix[Rdf].GetOutConnections.toString(),
            Map(STNOpsPrefix[Rdf].UserAccountURI.toString() -> accountUri)
      )
  }
  
  def retrieveProfileAndExtractConnections(stn: STNClient, agentUri: String) = {
    val result = stn.runOperation(STNOpsPrefix[Rdf].WhoIsAgent.toString(),
                      Map(STNOpsPrefix[Rdf].AgentURI.toString() -> agentUri)
      )
    
    result map {
      graph =>
        val accountUri = 
          TurtleParser.getOne(graph, "?accountUri a stn:UserAccount ; stn:heldBy ?agentUri", 
              "accountUri", Map("?agentUri" -> URI(agentUri)))
        
        if (!accountUri.isEmpty) {
          TurtleParser.getListOfOne(graph, "?accountUri stn:connectedTo ?connectionUri", 
              "connectionUri", true, Map("?accountUri" -> URI(accountUri.get)))
        } else {
          None
        }
    }
  }
  
  def fetchAndExtractThings(stn: STNClient, accountUri: String) = {
    println("Retrieving account for: " + accountUri)
    val result = stn.fetchURL(accountUri)
    
    result map {
      response =>
          val things = TurtleParser.getListOfOne(response.body, "?accountUri stn:heldBy [ stn:owns ?thingUri ]", 
              "thingUri", true, Map("?accountUri" -> URI(accountUri)))
          println("Found things: " + things)
          things
    }
  }
  
  def selectSocialTVs(stn: STNClient, things: List[String], found: List[String]): Future[List[String]] = {
    if (things.isEmpty) Future { found }
    else {
      val head::tail = things
      stn.fetchURL(head) flatMap {
        result =>
          if (TurtleParser.getOne(result.body, 
              "?thingUri a <http://www.example.com/#SocialTV>", "thingUri").isEmpty) {
            selectSocialTVs(stn, tail, found)
          } else {
            selectSocialTVs(stn, tail, found :+ head)
          }
      }
    }
  }
  
  def discoverTVs(stn: STNClient, accounts: List[String], discovered: List[String]): Future[List[String]] = {
    if (accounts.isEmpty) {
      Future { discovered }
    } else {
      val head::tail = accounts
      fetchAndExtractThings(stn, head) flatMap {
        results =>
          if (results.isEmpty) discoverTVs(stn, tail, discovered)
          else {
            selectSocialTVs(stn, results.get.toList, List()) flatMap {
              tvs => discoverTVs(stn, tail, discovered ++ tvs)
            }
          }
      }
    }
  }
  
  
  case class OnlineAccount(stnSpec: Option[String], 
      accountUri: Option[String], accountId: Option[String], description: Option[String])
  
  def extractAccountsFromWebID(webId: String): Future[List[OnlineAccount]] = {
    (new STNClient("http://localhost:9000/assets/stnspecs/thingsnet.ttl")).fetchURL(webId) map {
      response =>
        val accounts = TurtleParser.getListOfMany(response.body, 
                          "?webId stn:holds ?accountUri ." +
                          "?accountUri stn:hostedBy ?platform ." +
                          "OPTIONAL { ?accountUri stn:id ?id } ." +
                          "OPTIONAL { ?accountUri stn:description ?desc } ." ,
                          List(("platform", true), ("accountUri", true), ("id", false), ("desc", false)), 
                          Map("?webId" -> URI(webId))
                        )

        accounts map {
          a =>
//            if (!a(1).isEmpty && Validator.isValidUri(a(1).get)) {
              OnlineAccount(a(0), a(1), a(2), a(3))
//            } else {
//              OnlineAccount(a(0), None, a(2))
//            }
        }
    }
  }
  
  def extractConnectionsFromAccount(account: OnlineAccount) = {
    val client = new STNClient(account.stnSpec.getOrElse("http://localhost:9000/assets/stnspecs/thingsnet.ttl"))
    
    val connectionsGraph = 
        if (account.stnSpec.isEmpty) {
          client.fetchURL(account.accountUri.get) map {
            response => response.body
          }
        } else if (account.accountId.isEmpty) {
          println("checking acc: " + account)
          client.runOperation(STNOpsPrefix[Rdf].GetConnectionsFrom toString(),
              Map(STNOpsPrefix[Rdf].UserAccountURI.toString() -> account.accountUri.get)
            )
        } else {
          client.runOperation(STNOpsPrefix[Rdf].GetConnectionsFrom toString(),
              Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> account.accountId.get)
            )
        }
    
    connectionsGraph map {
      graph =>
        if (account.accountId.isEmpty) {
          val accounts = TurtleParser.getListOfOne(graph, 
                              "?accountUri stn:connectedTo ?connectionUri", 
                              "connectionUri", true, 
                              Map("?accountUri" -> URI(account.accountUri.get))
                            )
          if (!accounts.isEmpty) accounts.get map {
              a => OnlineAccount(None, Some(a), None, None)
            } toList
          else List()
        } else {
          // TODO: here I am assuming that networks that use local IDs are closed
          val accounts = TurtleParser.getListOfMany(graph, 
                              "?accountUri a stn:UserAccount ." +
                              "OPTIONAL { ?accountUri stn:id ?id . } " +
                              "OPTIONAL { ?accountUri stn:description ?desc } ",
                              List(("id", false), ("desc", false)),
                              Map()
                            )
//          println("GRAPH: " + graph)
//          println("ACCS: " + accounts)
          if (!accounts.isEmpty) accounts map {
            a =>
              OnlineAccount(account.stnSpec, None, a(0), a(1))
          } toList
          else List()
        }
    }
  }
  
  def extractAndMergeConnections(accounts: List[OnlineAccount], 
        connections: List[OnlineAccount]): Future[List[OnlineAccount]] = {
      
    if (accounts.isEmpty) {
      Future(connections)
    } else {
      val head::tail = accounts
      extractConnectionsFromAccount(head) flatMap {
        conns =>
          println("found conns: " + conns)
          extractAndMergeConnections(tail, connections ++ conns)
      }
    }
  }
  
  def extendFriendAccountList(friendAccounts: List[OnlineAccount], 
      extendedList: List[OnlineAccount]): Future[List[OnlineAccount]] = {
    
    if (friendAccounts.isEmpty) {
      Future(extendedList)
    } else {
      val head::tail = friendAccounts
      if (!head.accountId.isEmpty && !head.description.isEmpty && head.description.get.startsWith("http://")) {
        extractAccountsFromWebID(head.description.get) flatMap {
          accs => {
            println("EXTENDED: " + accs)
            extendFriendAccountList(tail, (extendedList :+ head) ++ accs)
          }
        }
      } else if (!head.accountId.isEmpty && head.accountId.get == "1475781352713478") {
        // Facebook's test harness provides access to only a few fields for test users, so we have to hardcode this. 
        extractAccountsFromWebID("http://localhost:9004/users/9ed8f9cf-abae-4973-9109-68a3e8989686#John") flatMap {
          accs => {
            println("EXTENDED: " + accs)
            extendFriendAccountList(tail, (extendedList :+ head) ++ accs)
          }
        }
      } else {
        extendFriendAccountList(tail, extendedList :+ head)
      }
    }
  }
  
  def isSocialTV(thingWebId: String) = {
    (new STNClient("http://localhost:9000/assets/stnspecs/thingsnet.ttl")).fetchURL(thingWebId) map {
        result =>
          if (TurtleParser.getOne(result.body, 
              "<" + thingWebId + ">" + "a <http://www.example.com/#SocialTV>", "thingUri").isEmpty) {
            true
          } else {
            false
          }
      }
  }
  
  def filterThingWebIDs(thingWebIds: List[String], results: List[String]): Future[List[String]] = {
    if (thingWebIds.isEmpty) {
      Future(results)
    } else {
      val head::tail = thingWebIds
      
      isSocialTV(head) flatMap {
        result => if (result) {
          filterThingWebIDs(tail, results :+ head)
        } else {
          filterThingWebIDs(tail, results)
        }
      }
    }
  }
  
  def findSocialTVs(accList: List[OnlineAccount], tvs: List[String]): Future[List[String]] = {
    if (accList.isEmpty) {
      Future(tvs)
    } else {
      val head::tail = accList
      
      // TODO: currently using the local ID to determine if it is an STN account or not
      if (head.accountId.isEmpty && !head.accountUri.isEmpty) {
        println("account URI: " + head.accountUri.get)
        (new STNClient("http://localhost:9000/assets/stnspecs/thingsnet.ttl"))
          .fetchURL(head.accountUri.get) flatMap {
          
          response =>
            val things = TurtleParser.getListOfOne(response.body, 
                              "?accountUri stn:heldBy [ stn:owns ?thingUri ]", 
                              "thingUri", true,
                              Map("?accountUri" -> URI(head.accountUri.get))
                            )
                            
            println("THINGS: " + things)
            
            if (!things.isEmpty) {
              filterThingWebIDs(things.get.toList, List()) flatMap {
                socialTVs => findSocialTVs(tail, tvs ++ socialTVs)
              }
            } else {
              findSocialTVs(tail, tvs)
            }
        }
      } else {
        findSocialTVs(tail, tvs)
      }
    }
  }
  
  def aggregateMovieData(accounts: List[OnlineAccount], data: List[String]): Future[List[String]] = {
    if (accounts.isEmpty) Future(data)
    else {
      val head::tail = accounts
      val client = new STNClient(head.stnSpec.get)
      client.getOperationDescription(STNOpsPrefix[Rdf].GetUserAccountFeed toString()) flatMap {
        op => 
          if (!op.isEmpty) {
            client.runOperation(STNOpsPrefix[Rdf].GetUserAccountFeed toString(),
                      Map(STNOpsPrefix[Rdf].UserAccountID.toString() -> head.accountId.get)
                    ) flatMap {
                         dataObj =>
                           aggregateMovieData(tail, data :+ dataObj)
                       }                            
          } else aggregateMovieData(tail, data)
      }
    }
  }
  
  def extractMovieDataForTV(tvWebId: String) = {
    extractAccountsFromWebID(tvWebId) flatMap {
      accounts => {
        println("accounts: " + accounts)
        aggregateMovieData(accounts, List()) map {
          l => l.toString()
        }
      }
    }
  }
  
  
  def run(): Future[String] = {
    
    import utils.TurtleParser
    
    println("[SocialTV] hello world !")
    
//    runThingsNetTestSuite(new STNClient("http://localhost:9000/assets/stnspecs/thingsnet.ttl"))
//    runTwitterTestSuite(new STNClient("http://localhost:9000/assets/stnspecs/twitter.ttl"))
//    runFacebookTestSuite(new STNClient("http://localhost:9000/assets/stnspecs/facebook.ttl"))
//    run8tracksTestSuite(new STNClient("http://localhost:9000/assets/stnspecs/8tracks.ttl"))
    
    val client = new STNClient("http://localhost:9000/assets/stnspecs/thingsnet.ttl")
    
    // Registering to Dave's STN Box
    
    val regResult = client.runOperation(
            STNOpsPrefix[Rdf].CreateUserAccount toString(),
            Map(STNOpsPrefix[Rdf].SocialThingClass.toString() -> "http://www.example.com/#SocialTV",
              STNOpsPrefix[Rdf].SocialThingOwner.toString() -> "http://localhost:9000/users/e754d7f8-3cfe-440b-9409-ed21f40af8f3#Dave",
              STNOpsPrefix[Rdf].DisplayedName.toString() -> "Dave's TV")
            )

    regResult map {
      result => println("Registration: " + result)
    }
    
    // Extracting all of Dave's accounts
    
    val accounts = extractAccountsFromWebID("http://localhost:9000/users/e754d7f8-3cfe-440b-9409-ed21f40af8f3#Dave") map {
      list =>
        println("Dave's accounts: " + list)
        list
    }
    
    // Extracting all social relations of Dave
    
    val friends = accounts flatMap {
      list => extractAndMergeConnections(list, List())
    }
    
    // Extending the list with all accounts owned by his friends
    
    val extededFriendAccountList = friends flatMap {
      accList => extendFriendAccountList(accList, List())
    }
    
    // Searching friends for social TVs 
    
    val socialTVs = extededFriendAccountList flatMap {
      accList => findSocialTVs(accList, List())
    }
    
    // Adding connections
    
    socialTVs map {
      tvWebIdList => 
        tvWebIdList map {
          tvWebId => client.runOperation(STNOpsPrefix[Rdf].CreateConnectionTo toString(),
              Map(STNOpsPrefix[Rdf].AgentURI.toString() -> "http://www.example.com/#thing")
            )
        }
    }
    
    // Retrieving movie data
    
    val movieData = socialTVs flatMap {
      tvWebIdList =>
        tvWebIdList map {
          tvWebId => extractMovieDataForTV(tvWebId)
        } reduceLeft( (d1: Future[String], d2: Future[String])  
                        =>  (d1 flatMap { d1S => d2 map { d2S => d1S + "\n\n" + d2S} })
                      )
    }
    
    // Build output
    
    socialTVs flatMap {
      tv => movieData map {
        data =>
          "Discovered TVs:\n" + tv.toString() + "\n\n\nMovie data (last 24h):\n" + data
      }
    }
  }
}