package ru.megaplan.jira.plugins.taskobug.resource

import javax.ws.rs._
import scala.Array
import core.{Response, Context, MediaType}
import ru.megaplan.jira.plugins.taskobug.util.LogHelper
import javax.servlet.http.HttpServletRequest
import ru.megaplan.jira.plugins.taskobug.BugobugManager
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import java.io.StringWriter

/**
 * Created with IntelliJ IDEA.
 * User: firfi
 * Date: 30.10.12
 * Time: 13:38
 * To change this template use File | Settings | File Templates.
 */
@Path ("/bugobug")
@Produces(Array(MediaType.APPLICATION_JSON))
class BugobugResource(bugobugManager: BugobugManager) extends LogHelper {

  @GET
  @Path("/generate")
  def generate(@Context request: HttpServletRequest, @QueryParam ("releasePattern") releasePattern: String) = {
    val body = bugobugManager.getBugobugTable(releasePattern, "MP")
    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)
    val sw = new StringWriter()
    mapper.writeValue(sw, body)
    Response.ok(sw.toString).build
  }

  @GET
  @Path("/validate")
  def validate(@QueryParam ("releasePattern") releasePattern: String): Response = {
    Response.ok.build
  }

}
