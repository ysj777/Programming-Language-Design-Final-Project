
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.io.StdIn.{readLine, readInt}
import scala.sys.process._
import scala.util.control.Breaks._


object OperationSheet extends App {
  
  // Define the Sheet class to store the sheet information
  class Sheet(var ownerName: String, var sheetName: String, var userList: ArrayBuffer[Map[String, String]], var content: ArrayBuffer[ArrayBuffer[Any]])
  
  class  OperationSheet {

    // Define the current user and the user list
    var currentUser: String = "admin"
    val users: ArrayBuffer[Map[String, String]] = ArrayBuffer(
      Map("user_name" -> "admin", "account" -> "admin", "password" -> "admin"),
      Map("user_name" -> "user", "account" -> "user", "password" -> "user")
    )
    
    // Define the sheet list and the current sheet index
    val sheets: ArrayBuffer[Sheet] = ArrayBuffer(
      new Sheet("admin", "sheet_A", ArrayBuffer(), ArrayBuffer(ArrayBuffer(1.0), ArrayBuffer(2.0, 3.0))),
      new Sheet("user", "sheet_U", ArrayBuffer(Map("user_name" -> "admin", "access_right" -> "ReadOnly")), ArrayBuffer())
    )
    var currentSheetIndex: Option[Int] = None
    
    // Define the main process
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

    // Define the action function to process the user's action
    def action(): Boolean = {
      // Read the user's action option
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

      // Clear the screen(for Unix/Linux)
      //"clear".!

      // Clear the screen(for windows)
      Seq("cmd", "/c", "cls").!

      true
    }

    // Define the function to create a new user
    def createUser(): Unit = {
      println("""
      |Now have something need you to do:
      |Please enter you User Name, Account and Password to create a new user.
      |""".stripMargin)

      // Read the user's information
      val userName = readLine("User Name: ")
      val account = readLine("Account: ")
      val password = readLine("Password: ")

      // Check the repeated user name and account
      var is_double = false
      if (users.exists(user => user("user_name") == userName || user("account") == account)) {
        println(s"Already have the user name or account!")
        is_double = true
      } 
      else {
        users += Map("user_name" -> userName, "account" -> account, "password" -> password)
        currentUser = userName
        println("Create a new user successfully! You are logging now!")
      }
    }

    // Define the function to login
    def login(): Unit = {

      // Read the user's account and password
      println("Please enter your account and password to login.")
      val account = readLine("Account: ")
      val password = readLine("Password: ")

      // Check the user's account and password
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

    // Define the function to logout
    def logout(): Unit = {
      println(s"Logout successfully! See you next time, $currentUser!")
      currentUser = null
    }

    // Define the function to create a new sheet
    def createSheet(): Unit = {
      // Read the sheet name
      val sheetName = readLine("Please enter the sheet name: ")
      sheets += new Sheet(currentUser, sheetName, ArrayBuffer(), ArrayBuffer())
      println(s"Create a new sheet '$sheetName' for $currentUser successfully!")
    }

    // Define the function to check the sheet that the user can access
    def checkSheet(showUser: Boolean = true): Unit = {
      println()
      for ((sheet, index) <- sheets.zipWithIndex) {

        // Check if currentUser is the sheet owner or in the sheet's user list
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

    // Define the function to access the sheet
    def accessSheet(): Unit = {

      // Clear the screen(for Unix/Linux)
      //"clear".!

      // Clear the screen(for windows)
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

        // Read the user's action option
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

        // Clear the screen(for Unix/Linux)
        //"clear".!

        // Clear the screen(for windows)
        Seq("cmd", "/c", "cls").!
      }
    }

    // Define the function to select the sheet
    def selectSheet(): Unit = {
      //print the sheet list that currentuser can see
      checkSheet()
      //read the sheet index
      val sheetIndex = readLine("Please enter the sheet ID number to check the content(-1 to exit.): ")

      //check the sheet is exist or not
      if(sheets.length - 1 < sheetIndex.toInt) {
        println("The sheet is not exist.")
        return
      }
      //print the sheet content
      printSheetContent(sheetIndex)

      if (sheetIndex == "-1") {
        currentSheetIndex = None
        println("Now you are not access any sheet.")
      } 

      else {
        currentSheetIndex = Some(sheetIndex.toInt)
        println(s"Now you are successfully access the sheet: ${sheets(currentSheetIndex.get).sheetName}")
      }
    }
    
    // Define the function to evaluate the expression
    def eval(expression: String): Double = {
      // split the expression by operator
      val tokens = expression.split("[+\\-*/]").toList
      if (tokens.length == 1) {
        try {
          return tokens.head.toDouble // if the expression is a number, return the number
        } catch {
          case _: NumberFormatException => return Double.NaN // if the expression is not a number, return NaN 
        }
      }

      // find the operator in the expression
      val operator = expression.find(c => "+-*/".contains(c))
      
      // check the expression is valid or not
      if (tokens.length != 2 || operator.isEmpty) return Double.NaN
      
      // calculate the result
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

    // Define the function to check the access right
    def checkAccessRight(): Boolean = {

      // check the current sheet is selected or not
      if (currentSheetIndex.isEmpty) {
        println("Please select the sheet first!")
        return false
      }
      
      val currentSheet = sheets(currentSheetIndex.get)

      // check the current user is the owner of the sheet
      if (currentSheet.ownerName == currentUser) {
        return true
      }

      // check the current user is in the user list of the sheet
      currentSheet.userList.find(_("user_name") == currentUser) match {
        case Some(user) if user("access_right") == "ReadWrite" => true
        case _ => 
          println("You do not have the access right to change the value!")
          false
      }
    }
    
    // Define the function to change the value
    def changeValue(): Unit = {
      if (!checkAccessRight()) return
      printSheetContent(currentSheetIndex.get.toString)

      // read the row index, column index and new value
      val rowIndex = readLine("Please enter the row number to change the value: ").toInt
      val columnIndex = readLine("Please enter the column number to change the value: ").toInt
      val value = readLine("Please enter the new value: ")

      // check the row index
      while (rowIndex >= sheets(currentSheetIndex.get).content.length) {
        sheets(currentSheetIndex.get).content += ArrayBuffer()
      }

      // check the column index
      while (columnIndex >= sheets(currentSheetIndex.get).content(rowIndex).length) {
        sheets(currentSheetIndex.get).content(rowIndex) += ""
      }

      try {
        // turn the value to double
        val transformerValue = eval(value)
        sheets(currentSheetIndex.get).content(rowIndex)(columnIndex) = transformerValue
      } catch {
        case _: NumberFormatException =>
          // if the value is not a number, set the value directly
          sheets(currentSheetIndex.get).content(rowIndex)(columnIndex) = value
      }

      println("Change the value successfully!")
      // Clear the screen(for Unix/Linux)
      //"clear".!

      // Clear the screen(for windows)
      Seq("cmd", "/c", "cls").!
      
      printSheetContent(currentSheetIndex.get.toString)
    }

    // Define the function to change the access right
    def changeAccessRight(): Unit = {
      checkSheet(showUser = false)

      // check the access right of current user
      if (!checkAccessRight()) return

      // read the sheet index, user name and access right
      var sheetIndex = readLine("Please enter the sheet ID number to change the access right (-1 to exit.): ")
      while (!sheetIndex.matches("-?\\d+")) {
        println("Invalid input. Please enter a number.")
        sheetIndex = readLine("Please enter the sheet ID number to change the access right (-1 to exit.): ")
      }
      if (sheetIndex == "-1") return
      println(s"This is the user list of the sheet: ${sheets(sheetIndex.toInt).userList}")

      //read the user name
      val userName = readLine("Please enter the username you want to add: ")
      if (!isUserExist(userName) && userName != currentUser) {
        println("The user is not exist!")
        return
      }

      //read the access right
      val accessRight = readLine("Please enter the access right (ReadOnly, ReadWrite): ")
      if (accessRight != "ReadOnly" && accessRight != "ReadWrite") {
        println("Invalid input. Please enter 'ReadOnly' or 'ReadWrite'.")
        return
      }

      //chage the access right of user name
      if (!sheets(sheetIndex.toInt).userList.exists(_("user_name") == userName)) {
        println("Now we added the user to the user list.")
        sheets(sheetIndex.toInt).userList += Map("user_name" -> userName, "access_right" -> accessRight)
      } 
      else {
        println("Now we changed the access right of the user.")
        for (user <- sheets(sheetIndex.toInt).userList if user("user_name") == userName) {
          user("access_right") = accessRight
        }
      }

      println(s"This is the new user list of the sheet: ${sheets(sheetIndex.toInt).userList}")
    }

    // Define the function to check the current sheet is selected or not
    def isSelectSheet(): Boolean = {
      if (currentSheetIndex.isEmpty) {
        println("Please select the sheet first!")
        return false
      }
      true
    }

    // Define the function to check the current user is login or not
    def isLogin(): Boolean = {
      if (currentUser == null) {
        println("Please login first!")
        return false
      }
      true
    }

    // Define the function to print the sheet content
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
    // Define the function to check the user is exist or not
    def isUserExist(userName: String): Boolean = {
      users.exists(_("user_name") == userName)
    }

    // Define the function to tabulate the content inside the sheet
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
