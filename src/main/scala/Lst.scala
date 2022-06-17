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

  def split: (Lst[A], Lst[A]) =
    def rec[A](list: Lst[A] = this, mid: Double = floor(this.len / 2), cnt: Int = 0, left: Lst[A] = Nil, right: Lst[A] = Nil): (Lst[A], Lst[A]) =
      if cnt < mid then {
        list match
          case Nil             => (left.reverse, right.reverse)
          case Cns(head, tail) => rec(tail, mid, cnt + 1, Cns(head, left), right)}
          else {
        list match
          case Nil             => (left.reverse, right.reverse)
          case Cns(head, tail) => rec(tail, mid, cnt + 1, left, Cns(head, right))}
    rec()

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
    def rev(list: Lst[Lst[A]] = rec(this), reversed: Lst[Lst[A]] = Nil): Lst[Lst[A]] =
      list match
        case Nil             => reversed
        case Cns(head, tail) => rev(tail, Cns(head.reverse, reversed))
    this match
      case Nil => Lst(Lst())
      case _   => rev()

  def exists[A](predicate: (A) => Boolean, mylist: Lst[A] = this): Boolean =
    def rec(func: (A) => Boolean, list: Lst[A] = mylist): Boolean =
      list match
        case Nil => false
        case Cns(head, tail) => if func(head) then true else rec(func, tail)
    rec(predicate)

  def merge[A](l: Lst[A], r: Lst[A], comparator: (A, A) => Compared): Lst[A] =
    def rec(left: Lst[A], right: Lst[A], comp: (A, A) => Compared): Lst[A] =
      (left, right) match
        case (left, Nil)  => left
        case (Nil, right) => right
        case (Cns(leftHead, leftTail), Cns(rightHead, rightTail)) =>
          if comp(rightHead, leftHead) == Compared.Gt then
            Cns(leftHead, rec(leftTail, right, comp))
          else Cns(rightHead, merge(left, rightTail, comp))
    rec(l,r, comparator)

  def mergeSort[A](comparator: (A, A) => Compared, mylist: Lst[A] = this): Lst[A] =
    def rec(comp: (A, A) => Compared, list: Lst[A] = mylist): Lst[A] =
      if list.len == 0 || list.len == 1 then list
      else {val (left, right) = list.split
            merge(rec(comp, left), rec(comp, right), comp)}
    rec(comparator)

object Lst {
  def apply[A](list: A*): Lst[A] =
    list.foldRight(Nil: Lst[A]) { case (head, tail) => Cns(head, tail) }}
