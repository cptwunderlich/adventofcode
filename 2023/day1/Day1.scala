import scala.io.StdIn.readLine
import scala.annotation.tailrec

@main
def main =
  val res = solution
  println(s"Part 1: ${res._1}, Part 2: ${res._2}")

def solution =
  Iterator.continually(readLine())
    .takeWhile(s => s != null && !s.isEmpty())
    .map(s => (part1(s), part2(s)))
    .reduce((x, y) => (x._1 + y._1, x._2 + y._2))

def part1(s: String) =
  mkValue(s.filter(_.isDigit))

def part2(s: String) =
  mkValue(toDigits(s))

def mkValue(digits: String) =
  (digits.take(1) + digits.takeRight(1)).toIntOption.getOrElse(0)

def toDigits(s: String): String =
  toDigitsImpl("", s)

@tailrec
def toDigitsImpl(acc: String, s: String): String =
  if s.isEmpty() then acc
  else
    val accNew =
      if s.startsWith("one") then acc + "1"
      else if s.startsWith("two") then acc + "2"
      else if s.startsWith("three") then acc + "3"
      else if s.startsWith("four") then acc + "4"
      else if s.startsWith("five") then acc + "5"
      else if s.startsWith("six") then acc + "6"
      else if s.startsWith("seven") then acc + "7"
      else if s.startsWith("eight") then acc + "8"
      else if s.startsWith("nine") then acc + "9"
      else if s.head.isDigit then acc + s.head
      else acc

    toDigitsImpl(accNew, s.tail)
