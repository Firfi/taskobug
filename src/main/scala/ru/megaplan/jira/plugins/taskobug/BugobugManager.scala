package ru.megaplan.jira.plugins.taskobug

import com.atlassian.jira.bc.issue.search.SearchService
import com.atlassian.jira.jql.builder.JqlQueryBuilder
import com.atlassian.jira.issue.CustomFieldManager
import util.{TimeConversions, LogHelper}
import com.atlassian.jira.security.JiraAuthenticationContext
import com.atlassian.jira.web.bean.PagerFilter
import collection.JavaConversions._
import collection.immutable.TreeMap
import com.atlassian.jira.issue.worklog.WorklogManager


/**
 * Created with IntelliJ IDEA.
 * User: firfi
 * Date: 30.10.12
 * Time: 13:43
 * To change this template use File | Settings | File Templates.
 */
trait BugobugManager {

  val RedColor = "#ff0000"
  val OrangeColor = "#ffbf00"
  val GreenColor = "#00ff00"

  val NOTEAM = "Нет команды"

  type TeamName = String
  type FixedNum = Long
  type NonFixedNum = Long
  type Percent = Int
  type Color = String
  type Timespent = Long
  type PrettyTimespent = String
  type DetailsRow = (FixedNum, NonFixedNum, (Percent, Option[Color]), PrettyTimespent)
  type SumRow = DetailsRow
  val releaseCfName = "Найдено в версии"
  val teamCfName = "Команда"
  def getBugobugTable(releasePattern: String, projectKey: String): Option[(Map[TeamName, DetailsRow], SumRow)]
}

class BugobugManagerImpl(searchService: SearchService,
                         customFieldManager: CustomFieldManager,
                         jiraAuthenticationContext: JiraAuthenticationContext,
                         worklogManager: WorklogManager)
  extends BugobugManager with LogHelper {

  private def getPercentViolation(p: Int, isSum: Boolean): (Int, Option[Color]) = {
    val c = {
      if (!isSum) {
        if (p < 50) {
          RedColor
        } else if (p < 75) {
          OrangeColor
        } else {
          GreenColor
        }
      } else {
        if (p < 75) {
          RedColor
        } else {
          GreenColor
        }
      }
    }
    (p, Some(c))
  }

  def getBugobugTable(releasePattern: String, projectKey: String) = {
    val releaseCf = customFieldManager.getCustomFieldObjectByName(releaseCfName)
    val teamCf = customFieldManager.getCustomFieldObjectByName(teamCfName)
    def countPercents(c: Long, o: Long): Int = {
      val sum: Float = c + o
      if (sum == 0) 0
      else (o.toFloat/(sum/100)).toInt
    }
    if (releaseCf == null) {
      log.error("can't find customfield with name : " + releaseCfName)
      Option.empty
    } else if (teamCf == null) {
      log.error("can't find customfield with name : " + teamCfName)
      Option.empty
    } else {
      val initiator = jiraAuthenticationContext.getLoggedInUser
      val builder = JqlQueryBuilder.newBuilder()
      val allQuery = builder.where().
        project(projectKey).and().customField(releaseCf.getIdAsLong).like(releasePattern).
        buildQuery()
      val issues = searchService.search(initiator, allQuery, PagerFilter.getUnlimitedFilter).getIssues
      val details = issues.groupBy {
        i => {
          val teamValue = i.getCustomFieldValue(teamCf)
          if (teamValue == null) {
            NOTEAM
          } else {
            teamValue.toString
          }
        }
      } map {
        case (team, ix) => {
          val (c, o, t) = ix.foldLeft((0L: FixedNum, 0L: NonFixedNum, 0L: Timespent)) {
            case ((f, n, ts), i) => {
              val timespent = worklogManager.getByIssue(i).foldLeft(0L) {
                (s, w) => {
                  s + w.getTimeSpent
                }
              }
              if ("6" == i.getStatusObject.getId) {
                (f + 1, n, ts + timespent)
              } else {
              (f, n + 1, ts + timespent)
              }
            }
          }
          val p = {
            val rawPercents = getPercentViolation(countPercents(c, o), isSum = false)
            if (NOTEAM != team) {
              rawPercents
            } else {
              (rawPercents._1, None)
            }
          }
          (team, (c, o, p, t))
        }
      }
      val sums = {
        val (c, o, spent) = details.foldLeft((0L: FixedNum, 0L: NonFixedNum, 0L: Timespent)) {
          case ((fa, na, ts), (_, (f, n, _, t))) => {
            (fa + f, na + n, ts + t)
          }
        }
        (c, o, getPercentViolation(countPercents(c, o), isSum = true), spent)
      }
      val sortedDetails = TreeMap(details.toArray:_*) {
        new Ordering[TeamName] {
          def compare(x: TeamName, y: TeamName) = {
            if (x == NOTEAM) 1
            else if (y == NOTEAM) -1
            else {
              x.compareTo(y)
            }
          }
        }
      }

      type NotPrettyRow = (FixedNum, NonFixedNum, (Percent, Option[Color]), Timespent)
      import TimeConversions.long2prettyLong

      def toPrettyTimespents(r: NotPrettyRow) = {
        (r._1, r._2, r._3, r._4.prettyDuration)
      }

      Some((sortedDetails.mapValues(toPrettyTimespents), toPrettyTimespents(sums)))
    }
  }
}
