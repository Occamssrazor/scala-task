object PalindromeDates {

  def isPalindrome(dateStr: String): Boolean = {
    val cleanDate = dateStr.replaceAll("\\D", "")
    cleanDate == cleanDate.reverse
  }

  def formatDate(year: Int, month: Int, day: Int): String = f"$year%04d$month%02d$day%02d"

  def isLeapYear(year: Int): Boolean = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)

  private def daysInMonth(year: Int, month: Int): Int = month match {
    case 2 => if (isLeapYear(year)) 29 else 28
    case 4 | 6 | 9 | 11 => 30
    case _ => 31
  }

  private def nextDate(date: (Int, Int, Int)): (Int, Int, Int) = {
    val (year, month, day) = date
    if (day < daysInMonth(year, month)) (year, month, day + 1)
    else if (month < 12) (year, month + 1, 1)
    else (year + 1, 1, 1)
  }

  def findNextPalindromicDates(count: Int, startDate: (Int, Int, Int) = (2020, 2, 2)): List[(Int, Int, Int)] = {
    Iterator.iterate(startDate)(nextDate)
      .filter { case (y: Int, m: Int, d: Int) => isPalindrome(formatDate(y, m, d)) }
      .take(count)
      .toList
  }

  def main(args: Array[String]): Unit = {
    val nextDates = findNextPalindromicDates(15)
    nextDates.foreach { case (year, month, day) =>
      println(f"$year%04d-$month%02d-$day%02d")
    }
  }
}