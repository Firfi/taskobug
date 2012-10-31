package ru.megaplan.jira.plugins.taskobug.util

import com.atlassian.core.util.DateUtils

/**
 * Created with IntelliJ IDEA.
 * User: firfi
 * Date: 31.10.12
 * Time: 15:21
 * To change this template use File | Settings | File Templates.
 */
object TimeConversions {
  implicit def long2prettyLong(l: Long): PrettyLong = new PrettyLong(l)
  class PrettyLong(l: Long) {
    def prettyDuration: String = {
      DateUtils.getDurationString(l)
    }
  }
}
