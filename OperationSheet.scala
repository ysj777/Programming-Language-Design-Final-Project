
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.io.StdIn.{readLine, readInt}
import scala.sys.process._
import scala.util.control.Breaks._


object OperationSheet extends App {
  
  class Sheet(var ownerName: String, var sheetName: String, var userList: ArrayBuffer[Map[String, String]], var content: ArrayBuffer[ArrayBuffer[Any]])
  
  class  OperationSheet {
    
    var currentUser: String = "admin"
    val users: ArrayBuffer[Map[String, String]] = ArrayBuffer(
      Map("user_name" -> "admin", "account" -> "admin", "password" -> "admin"),
      Map("user_name" -> "user", "account" -> "user", "password" -> "user")
    )
    
    val sheets: ArrayBuffer[Sheet] = ArrayBuffer(
      new Sheet("admin", "sheet_A", ArrayBuffer(), ArrayBuffer(ArrayBuffer(1.0), ArrayBuffer(2.0, 3.0))),
      new Sheet("user", "sheet_U", ArrayBuffer(Map("user_name" -> "admin", "access_right" -> "ReadOnly")), ArrayBuffer())
    )
    
    var currentSheetIndex: Option[Int] = None
    
    var isProcess = true
    while (isProcess) {
      // Print the Menu List to use
      println(s"""
      |Hello, Can I help you? ${currentUser}
      |---------------Menu---------------
      |1. Create a user.
      |2. Login.
      |3. Logout.
      |4. Create a sheet
      |5. Check the sheet 
      |6. Access the sheet
      |
      |0. Exit the program
      |====================================
      |""".stripMargin)
      isProcess = action()
    }

    def action(): Boolean = {
      val selectOption = readLine("Please enter your action option(number please): ")
      selectOption match {
        case "1" => createUser()
        case "2" => login()
        case "3" if isLogin() => logout()
        case "4" if isLogin() => createSheet()
        case "5" if isLogin() =>
          checkSheet()
          val sheetIndex = readLine("Please enter the sheet ID number to check the content(-1 to exit.): ")
          printSheetContent(sheetIndex)
        case "6" if isLogin() => accessSheet()
        case "0" => return false
        case _ => println("Please enter the correct option!")
      }
      readLine("Press any key to continue...")
      //"clear".!
      Seq("cmd", "/c", "cls").!
      true
    }

    def createUser(): Unit = {
      println("""
      |Now have something need you to do:
      |Please enter you User Name, Account and Password to create a new user.
      |""".stripMargin)

      val userName = readLine("User Name: ")
      val account = readLine("Account: ")
      val password = readLine("Password: ")

      // Check the repeated user name and account
      var is_double = false
      if (users.exists(user => user("user_name") == userName || user("account") == account)) {
        println(s"Already have the user name or account!")
        is_double = true
      } else {
        users += Map("user_name" -> userName, "account" -> account, "password" -> password)
        currentUser = userName
        println("Create a new user successfully! You are logging now!")
      }
    }

    def login(): Unit = {
      println("Please enter your account and password to login.")
      val account = readLine("Account: ")
      val password = readLine("Password: ")

      breakable {
      for (user <- users) {
        if (user("account") == account && user("password") == password) {
          currentUser = user("user_name")
          println(s"Welcome ${user("user_name")} to login!")
          break
        }
      }
    }
    }

    def logout(): Unit = {
      println(s"Logout successfully! See you next time, $currentUser!")
      currentUser = null
    }

    def createSheet(): Unit = {
      val sheetName = readLine("Please enter the sheet name: ")
      sheets += new Sheet(currentUser, sheetName, ArrayBuffer(), ArrayBuffer())
      println(s"Create a new sheet '$sheetName' for $currentUser successfully!")
    }

    def checkSheet(showUser: Boolean = true): Unit = {
      println()
      for ((sheet, index) <- sheets.zipWithIndex) {
        if (sheet.ownerName == currentUser) {
          println(s"Sheet ID number: $index, Sheet Name: ${sheet.sheetName}")
        }

        if (showUser) {
          for (user <- sheet.userList) {
            if (user("user_name") == currentUser) {
              println(s"Sheet ID number: $index, Sheet Name: ${sheet.sheetName}, Sheet Access Right: ${user("access_right")}")
            }
          }
        }
      }
    }

    def accessSheet(): Unit = {
      //"clear".!
      Seq("cmd", "/c", "cls").!
      while (true) {
        // Print the Sheet Menu List to use
        println(s"""
        |Now access in: ${if (currentSheetIndex.isDefined) sheets(currentSheetIndex.get).sheetName else "None"}
        |---------------Sheet Menu---------------
        |1. Select the sheet
        |4. change access right of the sheet
        |
        |# have selected the sheet
        |2. Print the sheet content
        |3. change_value
        |
        |0. Exit the Sheet Menu
        |====================================
        |""".stripMargin)
        val selectOption = readLine("Please enter your action option(number please): ")
        selectOption match {
          case "1" => selectSheet()
          case "2" if isSelectSheet() => printSheetContent(currentSheetIndex.get.toString)
          case "3" if isSelectSheet() => changeValue()
          case "4" => changeAccessRight()
          case "0" => return
          case _ => println("Please enter the correct option!")
        }
        readLine("Press any key to continue...")
        //"clear".!
        Seq("cmd", "/c", "cls").!
      }
    }

    def selectSheet(): Unit = {
      checkSheet()
      val sheetIndex = readLine("Please enter the sheet ID number to check the content(-1 to exit.): ")
      if(sheets.length - 1 < sheetIndex.toInt) {
        println("The sheet is not exist.")
        return
      }
      printSheetContent(sheetIndex)
      if (sheetIndex == "-1") {
        currentSheetIndex = None
        println("Now you are not access any sheet.")
      } else {
        currentSheetIndex = Some(sheetIndex.toInt)
        println(s"Now you are successfully access the sheet: ${sheets(currentSheetIndex.get).sheetName}")
      }
    }
    
    def eval(expression: String): Double = {
      // 將表達式拆分成操作數和操作符
      val tokens = expression.split("[+\\-*/]").toList
      if (tokens.length == 1) {
        try {
          return tokens.head.toDouble // 如果只有一個操作數，直接返回
        } catch {
          case _: NumberFormatException => return Double.NaN// 轉換失敗，返回 None
        }
      }
      val operator = expression.find(c => "+-*/".contains(c))
      
      // 檢查表達式是否有效
      if (tokens.length != 2 || operator.isEmpty) return Double.NaN
      
      // 轉換操作數為 Double 並計算結果
      val operand1 = tokens.head.toDouble
      val operand2 = tokens.tail.head.toDouble
      val result = operator.get match {
        case '+' => operand1 + operand2
        case '-' => operand1 - operand2
        case '*' => operand1 * operand2
        case '/' => if (operand2 != 0) operand1 / operand2 else Double.NaN
      }
        
      result
    }
    def checkAccessRight(): Boolean = {
      if (currentSheetIndex.isEmpty) {
        println("Please select the sheet first!")
        return false
      }
      
      val currentSheet = sheets(currentSheetIndex.get)
      
      if (currentSheet.ownerName == currentUser) {
        return true
      }
      
      currentSheet.userList.find(_("user_name") == currentUser) match {
        case Some(user) if user("access_right") == "ReadWrite" => true
        case _ => 
          println("You do not have the access right to change the value!")
          false
      }
    }
  
    def changeValue(): Unit = {
      if (!checkAccessRight()) return
      printSheetContent(currentSheetIndex.get.toString)
      val rowIndex = readLine("Please enter the row number to change the value: ").toInt
      val columnIndex = readLine("Please enter the column number to change the value: ").toInt
      val value = readLine("Please enter the new value: ")

      // 檢查是否需要新增行
      while (rowIndex >= sheets(currentSheetIndex.get).content.length) {
        sheets(currentSheetIndex.get).content += ArrayBuffer()
      }

      // 檢查是否需要新增列
      while (columnIndex >= sheets(currentSheetIndex.get).content(rowIndex).length) {
        sheets(currentSheetIndex.get).content(rowIndex) += ""
      }

      try {
        // 嘗試將 value 轉換為數字類型
        val transformerValue = eval(value)
        sheets(currentSheetIndex.get).content(rowIndex)(columnIndex) = transformerValue
      } catch {
        case _: NumberFormatException =>
          // 如果轉換失敗，將原始值存儲
          sheets(currentSheetIndex.get).content(rowIndex)(columnIndex) = value
      }

      println("Change the value successfully!")
      //"clear".!
      Seq("cmd", "/c", "cls").!
      printSheetContent(currentSheetIndex.get.toString)
    }

    def changeAccessRight(): Unit = {
      checkSheet(showUser = false)
      if (!checkAccessRight()) return
      var sheetIndex = readLine("Please enter the sheet ID number to change the access right (-1 to exit.): ")
      while (!sheetIndex.matches("-?\\d+")) {
        println("Invalid input. Please enter a number.")
        sheetIndex = readLine("Please enter the sheet ID number to change the access right (-1 to exit.): ")
      }

      if (sheetIndex == "-1") return

      println(s"This is the user list of the sheet: ${sheets(sheetIndex.toInt).userList}")

      val userName = readLine("Please enter the username you want to add: ")
      if (!isUserExist(userName) && userName != currentUser) {
        println("The user is not exist!")
        return
      }

      val accessRight = readLine("Please enter the access right (ReadOnly, ReadWrite): ")
      if (accessRight != "ReadOnly" && accessRight != "ReadWrite") {
        println("Invalid input. Please enter 'ReadOnly' or 'ReadWrite'.")
        return
      }

      if (!sheets(sheetIndex.toInt).userList.exists(_("user_name") == userName)) {
        println("Now we added the user to the user list.")
        sheets(sheetIndex.toInt).userList += Map("user_name" -> userName, "access_right" -> accessRight)
      } else {
        println("Now we changed the access right of the user.")
        for (user <- sheets(sheetIndex.toInt).userList if user("user_name") == userName) {
          user("access_right") = accessRight
        }
      }

      println(s"This is the new user list of the sheet: ${sheets(sheetIndex.toInt).userList}")
    }

    def isSelectSheet(): Boolean = {
      if (currentSheetIndex.isEmpty) {
        println("Please select the sheet first!")
        return false
      }
      true
    }

    def isLogin(): Boolean = {
      if (currentUser == null) {
        println("Please login first!")
        return false
      }
      true
    }

    def printSheetContent(sheetIndex: String): Unit = {
      try {
        if (sheetIndex == "-1" || sheetIndex == null) return
        val sheet = sheets(sheetIndex.toInt)
        if (sheet.content.isEmpty) {
          println("The sheet content is empty.")
          return
        }

        val maxCols = sheet.content.map(_.length).max
        for (row <- sheet.content) {
          while (row.length < maxCols) {
            row += ""
          }
        }

        val headers = "" +: (0 until maxCols).map(i => s"Col $i")
        val table = sheet.content
        val tableWithIndex = (0 until table.length).map(i => s"Row $i" +: table(i).toSeq)
        val tableWithHeaders = headers +: tableWithIndex

        println(tabulate(tableWithHeaders))
      } catch {
        case _: Exception => println("Please enter the correct sheet ID number!")
      }
    }

    def isUserExist(userName: String): Boolean = {
      users.exists(_("user_name") == userName)
    }

    def tabulate(table: Seq[Seq[Any]]): String = {
      val lengths = for (row <- table) yield row.map(_.toString.length)
      val maxLengths = lengths.transpose.map(_.max)
      val rows = for (row <- table) yield {
        val cells = for ((item, index) <- row.zipWithIndex) yield {
          val itemStr = item.toString
          itemStr + " " * (maxLengths(index) - itemStr.length)
        }
        cells.mkString(" | ")
      }
      val separator = maxLengths.map("-" * _).mkString("-+-")
      rows.mkString("\n", s"\n$separator\n", "\n")
    }
  }

  new  OperationSheet()
}
