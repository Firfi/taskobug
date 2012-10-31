package ru.megaplan.jira.plugins.taskobug.resource

import javax.ws.rs._
import javax.ws.rs.core.{Response, Context, MediaType}
import javax.servlet.http.HttpServletRequest
import ru.megaplan.jira.plugins.taskobug.{TimeUtil, TaskobugManager}
import com.atlassian.jira.user.util.UserManager
import org.joda.time.{Period, DateTime}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.databind.ObjectMapper
import java.io.StringWriter
import ru.megaplan.jira.plugins.taskobug.util.LogHelper

/**
 * Created with IntelliJ IDEA.
 * User: firfi
 * Date: 25.10.12
 * Time: 17:24
 * To change this template use File | Settings | File Templates.
 */
@Path ("/gadget")
@Produces(Array(MediaType.APPLICATION_JSON))
class TaskobugResource(taskobugManager: TaskobugManager, userManager: UserManager) extends LogHelper {
  @GET
  @Path("/generate")
  def generate(@Context request: HttpServletRequest,
                @QueryParam ("usersAndPercents") usersAndPercents: String,
                @QueryParam("daysBefore") @DefaultValue("2") daysBefore: String): Response = {
    val uap = usersAndPercents.split(",").map(up => {
      val splitted = up.trim.split(":")
      (splitted.head.trim, splitted.tail.headOption.getOrElse("0").trim.toFloat)
    }).toMap
    val users = uap.map {
      case (login, percent) => {
        val user = userManager.getUser(login)
        val filteredPercent: Int = {
          if (percent < 0) {
            0
          } else if (percent > 0 && percent < 1) {
            100*percent.toInt
          } else if (percent > 100) 100
          else percent.toInt
        }
        (user, filteredPercent)
      }
    }.filter {
      case (user, percent) => {
        user != null
      }
    }

    def getTimeTable(dayBefore: Int) = {
      taskobugManager.getTimeTable(
        users,
        (TimeUtil.startOfDay(DateTime.now).minusDays(math.abs(dayBefore))).toDate,
        (TimeUtil.startOfDay(DateTime.now).plusDays(1)).toDate
      )
    }

    val body = {
      val tt =  getTimeTable(daysBefore.toInt)
      if (tt._1.size < daysBefore.toInt) {
        getTimeTable(daysBefore.toInt + (daysBefore.toInt - tt._1.size))
      } else {
        tt
      }
    }

    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)
    val sw = new StringWriter()
    mapper.writeValue(sw, body)
    Response.ok(sw.toString).build
  }

  @GET
  @Path("/validate")
  def validate(@QueryParam("daysBefore") @DefaultValue("2") daysBefore: String): Response = {
    if (daysBefore.toInt <= 0) {
      log.warn("invalid daysBefore value : " + daysBefore)
      Response.status(Response.Status.BAD_REQUEST).build()
    }
    Response.ok.build
  }
}
