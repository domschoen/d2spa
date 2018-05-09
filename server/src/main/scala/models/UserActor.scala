package models

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.event.LoggingReceive
import akka.actor.ActorRef
import akka.actor.Props

import scala.xml.Utility
import play.api.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.ws._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._


case class DataResponse(js: String)



/* This actor represent any request i.e a page in the browser display the graphs. (A new such actor is created if we refresh the page in the browser)*/
class UserActor(uid: String, out: ActorRef) extends Actor with ActorLogging {
  var didSayHello = false


  def receive = LoggingReceive {
    // to the browser
    case DataResponse(js: String) => {
      log.debug("Refresh browser: " + uid + " with data: " + js)
      out ! js
    }
    // from the browser
    case js: JsValue => {
        println("Received from client: " + js)
      if (!didSayHello) {
        didSayHello = true
        println("Will send hello to client")

      }
    }
    case other => log.error("unhandled: " + other)
  }
  
  /*def graphTypeFromString(s: String) : GraphType = {
    
  }*/
}

object UserActor {
  def props(uid: String)(out: ActorRef) = Props(new UserActor(uid, out))
}


/*
receive

    case TableValuesResponse(task, rawData, teamRows) => {
      log.debug("Receive TableValuesResponse")
      processTableValuesResponse(task,rawData,teamRows)
    }



    DataFetcher.wsRequest().post(q).map(tableValuesProcessing(_, task, r, regexs)).pipeTo(self)

  def tableValuesProcessing(response: WSResponse, task: String, rawData: RawData, regexs: List[Regex]) : TableValuesResponse = {
    log.debug("tableValuesProcessing")
    val resp = response.asInstanceOf[NingWSResponse]
    val velocity = rawData.velocity
    val sprints = rawData.sprintNames
    var teamrows = List[RawTeamRow]()
    var sprintValRest = rawData.transposed

    val teamsFromV1 = resp.json.asInstanceOf[JsArray].value(0).asInstanceOf[JsArray]
    val v2TeamValues = teamsFromV1.value
    log.debug("Count " + v2TeamValues.size)
    for (rObj <- v2TeamValues) {
      val rs = rObj.toString()

      def repeatRegexs(exposed: String, counter: Int, rgxs: List[Regex]): String = {
        rgxs match {
          case Nil => exposed
          case x :: xs => {
            val newS = x.replaceAllIn(exposed, "count" + counter)
            repeatRegexs(newS,counter+1,xs)
          }
        }

      }
      log.debug("rs " + rs)

  def processTableValuesResponse(task: String, rawData: RawData, teamRows: List[RawTeamRow]): Unit = {
     val response = if (task.equals(VelocityData)) {
       val teamWithVelo = teamRows.filter(tv => {
         val piVals = tv.piValues.reverse


 */