package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.Sequences.Sequence
import Sequence.*
import it.unibo.pps.u03.Optionals.Optional, Optional.*

/*  Exercise 5:
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)

  trait Traversable[T[_]]:
    def forEach[A](ta: T[A])(f: A => Unit): Unit

  def logAll[T[_] : Traversable, A](ta: T[A]): Unit =
    val trav = summon[Traversable[T]]
    trav.forEach(ta)(log)

  def applyAll[T[_] : Traversable, A](ta: T[A])(f: A => Unit): Unit =
    val trav = summon[Traversable[T]]
    trav.forEach(ta)(f)

  given Traversable[Sequence] with
    def forEach[A](seq: Sequence[A])(f: A => Unit): Unit = seq match
      case Cons(h, t) =>
        f(h)
        forEach(t)(f)
      case Nil() => ()

  given Traversable[Optional] with
    def forEach[A](opt: Optional[A])(f: A => Unit): Unit = opt match
      case Optional.Just(a) => f(a)
      case Empty => ()


  
