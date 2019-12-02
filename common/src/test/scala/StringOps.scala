package sculptor.common

trait StringOps {

  implicit class StringOps(s: String) {
    def fix: String = {
      s.stripMargin.replaceAll("(\r\n)|(\r)", "\n").replaceAll("^\\w+\n", "\n")
    }
  }

}
