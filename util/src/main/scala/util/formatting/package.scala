package util

package object formatting {

  import build._
  import Console._

  val sbtVersion: String    = BuildInfo.sbtVersion
  val scalaVersion: String  = BuildInfo.scalaVersion
  val buildInfo             = s"BuildInfo: sbt.version = $sbtVersion, scala.version = $scalaVersion"
  val buildInfoLong: String = BuildInfo.toString

  def javaRuntimeInfo: String = {
    val javaVendor  = System.getProperty("java.vendor")
    val javaVersion = System.getProperty("java.version")
    s"Java Runtime: $javaVendor, $javaVersion"
  }

  def printHeader(
      text: String,
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): Unit =
    println(header(text, width, leading, trailing, fill))

  def header(
      text: String,
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): String = {
    s"""|${textInLine(text, width, leading, trailing, fill, BLUE)}
        |${textInLine(s"$javaRuntimeInfo", width, leading, trailing, fill, BLUE)}
        |${textInLine(s"$buildInfo", width, leading, trailing, fill, BLUE)}""".stripMargin
  }

  def printTextInLine(
      text: String,
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500",
      color: String = ""
  ): Unit =
    println(textInLine(text, width, leading, trailing, fill, color))

  def textInLine(
      text: String,
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500",
      color: String = ""
  ): String = {
    val coloredText = if (color.isEmpty) text else s"$color$text$RESET"
    val frontPad    = fill * 10
    val startLength = (10 + text.length() + 2)
    val endLength   = if (startLength > width) 0 else width - startLength
    val endPad      = fill * (endLength + 9) // add 9 to adjust color escape chars
    s"$leading$frontPad $coloredText $endPad$trailing"
  }

  def printFooter(
      text: String,
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): Unit =
    println(footer(text, width, leading, trailing, fill))

  def footer(
      text: String,
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): String =
    textInLine(text, width, leading, trailing, fill, BLUE)

  def printLine(
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): Unit =
    println(line(width, leading, trailing, fill))

  def line(
      width: Int = 80,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): String = {
    val line = fill * width
    s"$leading$line$trailing"
  }

  def dash(
      width: Int,
      fill: String = "\u2500"
  ): String =
    line(width, "", "", fill)

  def objectNameSimple(scalaObject: java.lang.Object): String = {
    val cn = scalaObject.getClass.getSimpleName
    cn.substring(0, cn.length() - 1)
  }

  def objectName(scalaObject: java.lang.Object): String = {
    val cn = scalaObject.getClass.getName
    cn.substring(0, cn.length() - 1)
  }

  def printHeaderWithProgramName(scalaObject: java.lang.Object): Unit =
    printHeader(objectName(scalaObject))

  def printProgramNameInLine(scalaObject: java.lang.Object): Unit =
    printTextInLine(objectName(scalaObject))

  def printGreen(): Unit   = print(GREEN)
  def printRed(): Unit     = print(RED)
  def printBlue(): Unit    = print(BLUE)
  def printYellow(): Unit  = print(YELLOW)
  def printCyan(): Unit    = print(CYAN)
  def printMagenta(): Unit = print(MAGENTA)
  def printReset(): Unit   = print(RESET)

  implicit class StringSyntax(private val what: String) extends AnyVal {

    def boxed(width: Int = 80): String =
      s"${line(width)}\n$what\n${line(width)}"

    def colored(escape: String): String =
      s"$escape$what${Console.RESET}"

    def red: String =
      what.colored(Console.RED)

    def green: String =
      what.colored(Console.GREEN)

    def blue: String =
      what.colored(Console.BLUE)

    def reset: String =
      what.colored(Console.RESET)
  }
}
