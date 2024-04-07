package tasks.typeclasses

import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.*
import u03.Sequences.Sequence.*

import scala.annotation.tailrec

/*  Exercise 5:
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others...
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a data structure T[A]
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: " + a)

  @tailrec
  def logAllSequence[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAllSequence(t)
    case _          => ()

  @tailrec
  def logAllOptional[A](opt: Optional[A]): Unit = opt match
    case Just(a) => log(a); logAllOptional(Empty())
    case _       => ()

  trait Traversable[T[_]]:
    def foreach[A](el: T[A])(f: A => Unit): Unit

  given Traversable[Sequence] with
    def foreach[A](el: Sequence[A])(f: A => Unit): Unit = el match
      case Cons(h, t) => f(h); foreach(t)(f)
      case _          => ()

  given Traversable[Optional] with
    def foreach[A](el: Optional[A])(f: A => Unit): Unit = el match
      case Just(a) => f(a)
      case _       => ()

  def logAll[A, T[_]: Traversable](el: T[A])(logger: A => Unit): Unit =
    val traversable = summon[Traversable[T]]
    traversable.foreach(el)(logger)

@main def tryTraversable(): Unit =
  import Ex5Traversable.{*, given}

  val seq1 = Cons(10, Cons(20, Cons(30, Nil())))
  logAllSequence(seq1)
  logAll(seq1)(log)
  logAll(seq1)(println(_))

  val seq2 = Cons("10", Cons("20", Cons("30", Nil())))
  logAllSequence(seq2)
  logAll(seq2)(log)
  logAll(seq2)(println(_))

  val opt1 = Just(10)
  logAllOptional(opt1)
  logAll(opt1)(log)
  logAll(opt1)(println(_))

  val op2 = Just("10")
  logAllOptional(op2)
  logAll(op2)(log)
  logAll(op2)(println(_))
