package fpinscala.exercises.laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList_1: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList_1

  def toList: List[A] =
    def loop(l: LazyList[A], acc: List[A]): List[A] = l match
      case Empty      => acc.reverse
      case Cons(h, t) => loop(t(), h() :: acc)

    loop(this, Nil)

  def foldRight[B](
      z: => B
  )(
      f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if f(h()) then Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _                   => Empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case l                   => l

  def takeWhile_1(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile_1(p))
    case _                    => Empty

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a, b) => if p(a) then cons(a, b) else empty)

  def forAll_1(p: A => Boolean): Boolean = this match
    case Empty                => true
    case Cons(h, t) if p(h()) => t().forAll_1(p)
    case _                    => false

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption_1: Option[A] = this match
    case Empty      => None
    case Cons(h, _) => Option(h())

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a, b) => if f(a) then cons(a, b) else b)

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _                        => None
    }

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _                    => None
    }

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)) {
      case (Cons(a, t), Cons(aa, tt)) => Some(f(a(), aa()), t() -> tt())
      case _                          => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Cons(a, t), Empty)        => Some(Some(a()) -> None, t() -> Empty)
      case (Empty, Cons(aa, tt))      => Some(None -> Some(aa()), Empty -> tt())
      case (Cons(a, t), Cons(aa, tt)) => Some(Some(a()) -> Some(aa()), t() -> tt())
      case _                          => None
    }

  def zip[B](that: LazyList[B]): LazyList[(A, B)] =
    zipWith(that)(_ -> _)

  def startsWith[A](s: LazyList[A]): Boolean =
    zipAll(s).takeWhileViaUnfold(_(1).isDefined).forAll { case (aa, bb) =>
      aa == bb
    }

  def tails: LazyList[LazyList[A]] =
    unfold(this) {
      case Empty          => None
      case l @ Cons(_, t) => Some((l, t()))
    }.append(LazyList(empty[A]))

  def hasSubsequence[A](s: LazyList[A]): Boolean =
    tails.exists(_.startsWith(s))

  def scanRight[A2 >: A](init: A2)(f: (A, A2) => A2): LazyList[A2] =
    foldRight(init -> LazyList(init)) { (a, b0) =>
      // b0 is passed by-name and used in by-name args in f and cons.
      // So use lazy val to ensure only one evaluation..
      lazy val b1 = b0
      val b2      = f(a, b1._1) // activate the recurtion
      // f(1, f(2, f(3, f(4, 0))))
      (b2, cons(b2, b1._2))
      // (4, cons(4, LazyList(0))
      // (7, cons(7, cons(4, LazyList(0))))
      // (9, cons(9, cons(7, cons(4, LazyList(0)))))
      // (10, cons(10, cons(9, cons(7, cons(4, cons(0, empty))))))
    }._2

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def loop(current: Int, next: Int): LazyList[Int] =
      cons(current, loop(next, current + next))
    loop(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None         => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))((current, next) => Some((current, (next, current + next))))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(s => Some((a, ())))

  lazy val onesViaUnfold: LazyList[Int] = unfold(())(s => Some((1, ())))

  @main def printScanRight: Unit =
    println(LazyList(1, 2, 3, 4).scanRight(0)(_ + _).toList)
