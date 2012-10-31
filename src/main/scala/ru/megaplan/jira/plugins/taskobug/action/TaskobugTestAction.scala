package ru.megaplan.jira.plugins.taskobug.action

import com.atlassian.jira.web.action.JiraWebActionSupport
import ru.megaplan.jira.plugins.taskobug.TaskobugManager
import java.util.Date
import com.atlassian.jira.user.util.UserManager

/**
 * Created with IntelliJ IDEA.
 * User: firfi
 * Date: 25.10.12
 * Time: 16:03
 * To change this template use File | Settings | File Templates.
 */
class TaskobugTestAction(taskobugManager: TaskobugManager, userManager: UserManager) extends JiraWebActionSupport {
  @scala.reflect.BeanProperty
  var res = "ok"

  override def doExecute = {
    val rs = taskobugManager.getTimeTable(Map(userManager.getUser("admin")->50), new Date(new Date().getTime - 1005001), new Date())
    super.doExecute()
  }
}
