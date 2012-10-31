package ru.megaplan.jira.plugins.taskobug.util

import org.apache.log4j.Logger

/**
 * Created with IntelliJ IDEA.
 * User: firfi
 * Date: 25.10.12
 * Time: 14:59
 * To change this template use File | Settings | File Templates.
 */
trait LogHelper {
  private val loggerName = this.getClass.getName
  lazy val log = Logger.getLogger(loggerName)
}
