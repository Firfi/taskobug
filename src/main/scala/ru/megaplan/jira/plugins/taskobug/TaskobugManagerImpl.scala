package ru.megaplan.jira.plugins.taskobug

import javax.xml.bind.annotation.{XmlAccessorType, XmlRootElement, XmlAccessType}
import java.util.Date
import com.atlassian.crowd.embedded.api.User
import org.ofbiz.core.entity._
import java.sql.Timestamp
import com.atlassian.jira.ComponentManager

import scala.collection.JavaConversions._
import util.{TimeConversions, LogHelper}
import com.atlassian.jira.issue.worklog.WorklogManager
import com.atlassian.jira.config.IssueTypeManager
import com.atlassian.jira.issue.IssueManager
import com.atlassian.jira.user.util.UserManager
import com.atlassian.jira.issue.issuetype.IssueType
import org.joda.time.DateTime
import com.atlassian.core.util.DateUtils
import com.atlassian.jira.issue.comparator.UserNameComparator
import collection.immutable.TreeMap
import org.joda.time.format.DateTimeFormat

/**
 * Created with IntelliJ IDEA.
 * User: firfi
 * Date: 25.10.12
 * Time: 13:57
 * To change this template use File | Settings | File Templates.
 */



trait TaskobugManager {

  val secondsInHour = 60*60
  val secondsInDay = 24*secondsInHour

  val dateFormat = "dd.MM.yy"

  val necessaryTypes = List("Story", "Task", "Bug", "Technical Task")


  def getTimeTable(users: Map[User, Int], start: Date, end: Date): TaskobugResult


  object Conversions {

    implicit def redBugTuple2Timespent(tup: (Timespent, RedBug)): Timespent = tup._1
    implicit def int2TimeableInt(i: Int): TimeableInt = new TimeableInt(i)
    implicit def usersAndPercentsToUsers(uap: Map[User, Int]): List[User] = uap.keys.toList
    implicit def map2orderMap[A,B](m: Map[A,B]) = new OrderMap(m)


    class TimeableInt(i: Int) {
      def hours: Int = i*secondsInHour
    }
    class OrderMap[A, B](m: Map[A, B]) {
      def orderMap(implicit ordering: Ordering[A]): TreeMap[A, B] = {
        TreeMap(m.toArray:_*)(ordering)
      }
    }
  }

  type DayName = String
  type IssueTypeName = String
  type UserName = String
  type UserFullName = String
  type Timespent = Long
  type PrettyTimespent = String
  type RedBug = Boolean
  type RedSum = Boolean
  type DayDetails = Map[IssueTypeName, Map[UserName, (PrettyTimespent, RedBug)]]
  type TaskobugResult = (
    Map[DayName, (DayDetails, Map[UserName, (PrettyTimespent, RedSum)])]
    ,
    Map[UserName, UserFullName])

}

class TaskobugManagerImpl(val worklogManager: WorklogManager,
                          val issueTypeManager: IssueTypeManager,
                          val issueManager: IssueManager,
                          val userManager: UserManager
                           ) extends TaskobugManager with LogHelper {

  import Conversions.int2TimeableInt
  import Conversions.usersAndPercentsToUsers

  private def countSumViolations(sums: Map[UserName, Timespent]): Map[UserName, (Timespent, RedSum)] = {
    sums.mapValues(spent => {
      (spent, {spent < 7.hours})
    })
  }

  private def countRedBugViolations(constraints: Map[UserName, Int])(
    typeStat: Map[IssueTypeName, Map[UserName, Timespent]]
  ): Map[IssueTypeName, Map[UserName, (Timespent, RedBug)]] = {
    typeStat.map {
      case (typeName, stat) => {
        (typeName, stat.map {
          case (user, spent) => {
            (user, (spent, {
              if ("Bug" == typeName) {
                val constraint = constraints.getOrElse(user, Int.MaxValue)
                if (constraint == 0) false
                else spent > {
                  val h = 8.hours
                  h/100 * constraint
                } || spent == 0
              }
              else false
            }))
          }
        })
      }
    }
  }



  def getTimeTable(usersAndPercents: Map[User, Int], start: Date, end: Date) = {
    val userNames = usersAndPercents.map(_._1.getName).toList

    object PrettyTimespentOrdering extends Ordering[PrettyTimespent] {
      def compare(x: PrettyTimespent, y: PrettyTimespent) = {
        def parse(s: PrettyTimespent) = {
          DateTime.parse(s, DateTimeFormat.forPattern(dateFormat))
        }
        val (dx, dy) = (parse(x), parse(y))
        dx.compareTo(dy)
      }
    }
    val worklogs = getWorklogs(usersAndPercents, start, end)
    import Conversions.map2orderMap
    def getResultWithoutViolations(logs: Seq[GenericValue]) = {
      implicit object UserNameOrdering extends Ordering[UserName] {
        def compare(x: TaskobugManagerImpl.this.type#UserName, y: TaskobugManagerImpl.this.type#UserName) = {
          val xpos = userNames.indexOf(x)
          val ypos = userNames.indexOf(y)
          //positions can't be -1 here
          xpos.compareTo(ypos)
        }
      }
      worklogs.groupBy({ w =>
        TimeUtil.startOfDay(w.getTimestamp("startdate"))
      }).mapValues(_.groupBy{ w =>
        issueManager.getIssueObject(w.getLong("issue")).getIssueTypeObject.getName
      }).mapValues(_.mapValues(v => {
        v.groupBy( w => {
          w.getString("author")
        }).orderMap
      })).mapValues(_.mapValues(_.mapValues(
        _.foldLeft(0L){(acc, w) => {
          acc + w.getLong("timeworked")
        }}
      )
      ))
    }
    val resultWithoutViolations = getResultWithoutViolations(worklogs)
    val resultWithoutViolationAndEmptyUsersAndNecessaryTypes = resultWithoutViolations.mapValues(dayStat => {
      type LocalResultType = Map[IssueTypeName, Map[UserName, Timespent]]
      val marginalTypesWithUsers: LocalResultType = {
        val marginalTypes = {
          val existTypes = dayStat.map {
            case (t: IssueTypeName, _) => {
              t
            }
          }.toList
          necessaryTypes.foldLeft(List.empty[String]) {
            (marginals, necessaryType) => {
              if (!existTypes.contains(necessaryType)) {
                necessaryType :: marginals
              } else {
                marginals
              }
            }
          }
        }
        marginalTypes.map {
          mt => {
            val typeStat = {
              userNames.map {
                name => {
                  (name, 0L)
                }
              }.toMap
            }
            (mt, typeStat)
          }
        }.toMap
      }

      val currentTypesWithUsers: LocalResultType = dayStat.mapValues(v => {
        val acc = v.foldLeft((Map.empty[UserName, Timespent], usersAndPercents.keys)) {
          (acc, info) => {
            type Acc = (Map[UserName, Timespent], Iterable[User])
            def eatTail(arg: (Acc, (UserName, Timespent))): Acc = {
              arg match {
                case ((res, all), (userName, timeSpent)) => {
                  if (userName == all.head.getName) {
                    (res + (userName->timeSpent), all.tail)
                  } else {
                    eatTail((res + (all.head.getName->0L), all.tail), userName->timeSpent)
                  }
                }
              }
            }
            eatTail(acc, info)
          }
        }
        acc._1 ++ acc._2.map(_.getName->0L)
      })

      import scalaz._
      import Scalaz._
      (currentTypesWithUsers |+| marginalTypesWithUsers).orderMap

    })

    val resultWithBugViolations = resultWithoutViolationAndEmptyUsersAndNecessaryTypes.mapValues(countRedBugViolations(usersAndPercents.map {
      case (u, cons) => (u.getName, cons)
    }))

    val resultWithSumViolations = resultWithBugViolations.mapValues(dayStat => {
      val dayStatByUser = dayStat.foldLeft(Map.empty[UserName, Timespent]) {
        (acc, typeStat) => {
          val (_, spentByUser) = typeStat
          spentByUser.foldLeft(acc) {
            (innerAcc, userSpent) => {
              val name = userSpent._1
              import Conversions.redBugTuple2Timespent
              val spent = userSpent._2
              val currentSpent = acc.getOrElse(name, 0L)
              innerAcc.updated(name, currentSpent + spent)
            }
          }
        }
      }
      (dayStat, countSumViolations(dayStatByUser))
    })




    val prettyResult = resultWithSumViolations.mapValues(v => {
      import TimeConversions.long2prettyLong
      ( v._1.mapValues(_.mapValues(v => {
        (v._1.prettyDuration, v._2)
      }))
      ,
      v._2.mapValues(v => {
        (v._1.prettyDuration, v._2)
      }))
    }).map {
      case (dateTime, other) => {
        (dateTime.toString(dateFormat), other)
      }
    }.orderMap(PrettyTimespentOrdering)

    val headerNames = userNames.foldLeft(Map.empty[String, String].orderMap) {
      (map, name) => {
        map + (name->userManager.getUserObject(name).getDisplayName)
      }
    }

    (prettyResult, headerNames)

  }



  def getWorklogs(users: List[User], start: Date, end: Date): Seq[GenericValue] = {
    val startExpr = new EntityExpr("startdate", EntityOperator.GREATER_THAN_EQUAL_TO, new Timestamp(start.getTime))
    val endExpr = new EntityExpr("startdate", EntityOperator.LESS_THAN, new Timestamp(end.getTime))
    val authorConds = new EntityConditionList(
      users.map(u => new EntityExpr("author", EntityOperator.EQUALS, u.getName)),
      EntityOperator.OR
    )
    val entityConds = List(startExpr, endExpr, authorConds)
    val order = List("startdate", "issue", "author")
    ComponentManager.getComponent(classOf[DelegatorInterface]).findByAnd("Worklog", entityConds, order)
  }
}

object TimeUtil {
  def startOfDay(o: Object) = {
    val d = o match {
      case d: DateTime => {
       d
      }
      case _ => {
        new DateTime(o)
      }
    }
    d.withHourOfDay(0).withMinuteOfHour(0).withSecondOfMinute(0)
  }
}
