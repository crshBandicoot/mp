package Lst
import scala.math._
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
    def rec(list: Lst[A], inits: Lst[Lst[A]] = Nil): Lst[Lst[A]] =
      list match
        case Nil => inits
        case Cns(head, tail) =>
          if inits != Nil then rec(tail, Cns(Cns(head, inits.getHead), inits))
          else rec(tail, Cns(Cns(head, Nil), inits))
    def rev(
        list: Lst[Lst[A]] = rec(this),
        reversed: Lst[Lst[A]] = Nil
    ): Lst[Lst[A]] =
      list match
        case Nil             => reversed
        case Cns(head, tail) => rev(tail, Cns(head.reverse, reversed))
    this match
      case Nil => Lst(Lst())
      case _   => rev()

  def exists[A](
      p: (A, A, A) => Boolean,
      add: A,
      eq: A,
      list: Lst[A] = this
  ): Boolean =
    def rec(
        p_f: (A, A, A) => Boolean,
        add_f: A,
        eq_f: A,
        list_f: Lst[A] = list
    ): Boolean =
      list_f match
        case Nil => false
        case Cns(head, tail) =>
          if p_f(head, add_f, eq_f) then true else rec(p_f, add_f, eq_f, tail)
    rec(p, add, eq)

  def merge[A](left: Lst[A], right: Lst[A], p: (A, A) => Compared): Lst[A] =
    def rec(
        left_f: Lst[A],
        right_f: Lst[A],
        p_f: (A, A) => Compared
    ): Lst[A] =
      (left_f, right_f) match
        case (left_f, Nil)  => left_f
        case (Nil, right_f) => right_f
        case (Cns(leftH, leftT), Cns(rightH, rightT)) =>
          if p_f(rightH, leftH) == Compared("Gt") then
            Cns(leftH, rec(leftT, right_f, p_f))
          else Cns(rightH, merge(left_f, rightT, p_f))
    rec(left, right, p)

  def mergeSort[A](p: (A, A) => Compared, list: Lst[A] = this): Lst[A] =
    def rec(p_f: (A, A) => Compared, list_f: Lst[A] = list): Lst[A] =
      if list_f.len == 0 || list_f.len == 1 then list_f
      else { merge(rec(p, list_f.getLeft), rec(p, list_f.getRight), p_f) }
    rec(p, list)

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
