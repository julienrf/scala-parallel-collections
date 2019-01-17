/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection.parallel

import scala.collection.{Set, SetOps}
import scala.language.higherKinds

/** A template trait for parallel sets. This trait is mixed in with concrete
 *  parallel sets to override the representation type.
 *
 *  $sideeffects
 *
 *  @tparam T    the element type of the set
 *  @define Coll `ParSet`
 *  @define coll parallel set
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParSetLike[T,
                 +CC[X] <: ParIterable[X],
                 +Repr <: ParSet[T],
                 +Sequential <: Set[T] with SetOps[T, Set, Sequential]]
extends /*GenSetLike[T, Repr]
   with*/ ParIterableLike[T, CC, Repr, Sequential]
  with (T => Boolean)
  with Equals
{ self =>

  // --- Members previously inherited from GenSetLike
  def contains(elem: T): Boolean
  final def apply(elem: T): Boolean = contains(elem)

  def subsetOf(that: ParSet[T]): Boolean = this.forall(that)

  /** Compares this set with another object for equality.
    *
    *  '''Note:''' This operation contains an unchecked cast: if `that`
    *        is a set, it will assume with an unchecked cast
    *        that it has the same element type as this set.
    *        Any subsequent ClassCastException is treated as a `false` result.
    *  @param that the other object
    *  @return     `true` if `that` is a set which contains the same elements
    *              as this set.
    */
  override def equals(that: Any): Boolean = that match {
    case that: ParSet[_] =>
      (this eq that) ||
        (that canEqual this) &&
          (this.size == that.size) &&
          (try this subsetOf that.asInstanceOf[ParSet[T]]
          catch { case ex: ClassCastException => false })
    case _ =>
      false
  }

  // Careful! Don't write a Set's hashCode like:
  //    override def hashCode() = this map (_.hashCode) sum
  // Calling map on a set drops duplicates: any hashcode collisions would
  // then be dropped before they can be added.
  // Hash should be symmetric in set entries, but without trivial collisions.
  override def hashCode()= scala.util.hashing.MurmurHash3.unorderedHash(this, "ParSet".hashCode)

  def canEqual(other: Any): Boolean = true
  // ---

  def empty: Repr

  // note: should not override toSet (could be mutable)

  def union(that: Set[T]): Repr = sequentially {
    _ union that
  }

  def union(that: ParSet[T]): Repr = sequentially {
    _ union that.seq
  }

  def diff(that: Set[T]): Repr = sequentially {
    _ diff that
  }

  def diff(that: ParSet[T]): Repr = sequentially {
    _ diff that.seq
  }
}
