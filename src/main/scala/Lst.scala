package Lst
import scala.math._
import scala.compiletime.ops.string
import scala.annotation.tailrec
enum Compared:
  case Lt, Gt, Eq

enum Lst[+A]:
  case Nil
  case Cns(head: A, tail: Lst[A])

  override def toString: String =
    def rec(sb: StringBuilder, as: Lst[A]): String =
      as match
        case Nil =>
          sb.append("]").result
        case Cns(h, t) =>
          rec(
            sb
              .append(h)
              .append(if t == Nil then "" else ", "),
            t
          )
    rec(new StringBuilder("["), this)

  def len: Int =
    def rec(list: Lst[A], n: Int = 0): Int =
      list match
        case Nil             => n
        case Cns(head, tail) => rec(tail, n + 1)
    rec(this)

  def reverse: Lst[A] =
    def rec[A](list: Lst[A], reversed: Lst[A] = Nil): Lst[A] =
      list match
        case Nil             => reversed
        case Cns(head, tail) => rec(tail, Cns(head, reversed))
    rec(this)

  def getLeft: Lst[A] =
    def rec[A](
        list: Lst[A] = this,
        len: Double = floor(this.len / 2),
        n: Int = 0,
        left: Lst[A] = Nil
    ): Lst[A] =
      if n < len then {
        list match
          case Nil             => left
          case Cns(head, tail) => rec(tail, len, n + 1, Cns(head, left))
      } else left
    rec().reverse

  def getRight: Lst[A] =
    def rec[A](
        list: Lst[A] = this,
        len: Double = floor(this.len / 2),
        n: Int = 0,
        right: Lst[A] = Nil
    ): Lst[A] =
      if n < len then {
        list match
          case Nil             => right
          case Cns(head, tail) => rec(tail, len, n + 1)
      } else {
        list match
          case Nil             => right
          case Cns(head, tail) => rec(tail, len, n + 1, Cns(head, right))
      }
    rec().reverse

  def getHead: A =
    this match
      case Cns(head, tail) => head
      case Nil             => throw new RuntimeException("NO HEAD!")

  def getInits: Lst[Lst[A]] =
    def rec[A](list: Lst[A], inits: Lst[Lst[A]] = Nil): Lst[Lst[A]] =
      list match
        case Nil => inits
        case Cns(head, tail) =>
          if inits != Nil then rec(tail, Cns(Cns(head, inits.getHead), inits))
          else rec(tail, Cns(Cns(head, Nil), inits))
    def rev[A](
        list: Lst[Lst[A]] = rec(this),
        reversed: Lst[Lst[A]] = Nil
    ): Lst[Lst[A]] =
      list match
        case Nil             => reversed
        case Cns(head, tail) => rev(tail, Cns(head.reverse, reversed))
    rev()

  def exists[A](
      p: (A, A, A) => Boolean,
      add: A,
      eq: A,
      list: Lst[A] = this
  ): Boolean =
    list match
      case Nil => false
      case Cns(head, tail) =>
        if p(head, add, eq) then true else exists(p, add, eq, tail)

  def merge[A](left: Lst[A], right: Lst[A], p: (A, A) => Compared): Lst[A] =
    (left, right) match
      case (left, Nil)  => left
      case (Nil, right) => right
      case (Cns(leftH, leftT), Cns(rightH, rightT)) =>
        if p(rightH, leftH) == Compared("Gt") then
          Cns(leftH, merge(leftT, right, p))
        else Cns(rightH, merge(left, rightT, p))

  def mergeSort[A](p: (A, A) => Compared, list: Lst[A] = this): Lst[A] =
    if list.len == 0 || list.len == 1 then list
    else { merge(mergeSort(p, list.getLeft), mergeSort(p, list.getRight), p) }

object Lst {
  def apply[A](list: A*): Lst[A] =
    list.foldRight(Nil: Lst[A]) { case (head, tail) => Cns(head, tail) }
}
object Compared {
  def apply(param: String): Compared =
    param match
      case "Lt" => Lt
      case "Gt" => Gt
      case _    => Eq
}
@main def main() =
  println(
    Lst(1, 2, 3, 4, 5, 6, 7).exists(
      { (el: Int, add: Int, eq: Int) =>
        if el + add == eq then true else false
      },
      1,
      9
    )
  )
  println(Lst(4, 2, 5, 1, 3, 7, 8, 6).mergeSort({ (a: Int, b: Int) =>
    if a > b then Compared("Gt")
    else if a < b then Compared("Lt")
    else Compared("Eq")
  }))
