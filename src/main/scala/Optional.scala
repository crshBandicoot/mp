package com.tkroman.kpi.y2022.l1

enum Optional[+A]:
  case None
  case Some(x: A)

def fold[A, B](a: Optional[A], z: B, f: (B, A) => B) =
  a match {
    case Optional.None    => z
    case Optional.Some(x) => f(z, x)
  }

enum Tree[+A]:
  case Branch(l: Tree[A], r: Tree[A])
  case Leaf(a: A)

enum Set[+A]:
  case Empty
  case NonEmpty private (a: A, rest: Set[A])
object Set:
  def makeSet[A](xs: A*): Set[A] = ???

case class Bag[A] private (private val map: Map[A, Int])
object Bag:
  def makeBag[A](xs: A*): Bag[A] = ???

// https://en.wikipedia.org/wiki/Binary_search_tree
case class BSTree private (l: Option[BSTree], r: Option[BSTree], v: Int)
object BSTree:
  def makeBst(xs: Int*): BSTree = ???

enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])

enum IntOpCode:
  case Add, Mul

enum BoolOpCode:
  case And, Or

enum IntExpr:
  case Lit(n: Int)
  case Op(opCode: IntOpCode, l: IntExpr, r: IntExpr)

enum BoolExpr:
  case Lit(b: Boolean)
  case Op(opCode: BoolOpCode, l: BoolExpr, r: BoolExpr)

enum Nat:
  case Zero
  case Succ(n: Nat)

case class Rational(num: Int, denom: Int)

enum Compared:
  case Lt, Gt, Eq // < > =

enum RecEntry[A]:
  case Flat(a: A)
  case Nested(as: List[RecEntry[A]])

@main def run() =
  println("Hello")
