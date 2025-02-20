import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PalindromeDatesTest extends AnyFlatSpec with Matchers {

  "isPalindrome" should "return true for palindromic dates" in {
    PalindromeDates.isPalindrome("20200202") shouldBe true
    PalindromeDates.isPalindrome("20200802") shouldBe false
  }

  "formatDate" should "format dates correctly" in {
    PalindromeDates.formatDate(2024, 1, 1) shouldBe "20240101"
  }

  "isLeapYear" should "correctly identify leap years" in {
    PalindromeDates.isLeapYear(2020) shouldBe true
    PalindromeDates.isLeapYear(2021) shouldBe false
    PalindromeDates.isLeapYear(2024) shouldBe true
  }
  "findNextPalindromicDates" should "return correct palindromic dates and their quantity" in {
    PalindromeDates.findNextPalindromicDates(3, (2020, 2, 2)) shouldBe List((2020, 2, 2), (2021, 12, 2), (2030, 3, 2))
  }
}