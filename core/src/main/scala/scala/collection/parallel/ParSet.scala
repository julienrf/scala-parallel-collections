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
package collection
package parallel

import scala.collection.generic._

/** A template trait for parallel sets.
 *
 *  $sideeffects
 *
 *  @tparam T    the element type of the set
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParSet[T]
   extends GenericParTemplate[T, ParSet]
   with ParIterable[T]
   with ParSetLike[T, ParSet, ParSet[T], Set[T]]
{ self =>

  override def empty: ParSet[T] = mutable.ParHashSet[T]()

  //protected[this] override def newCombiner: Combiner[T, ParSet[T]] = ParSet.newCombiner[T]

  override def companion: GenericParCompanion[ParSet] = ParSet

  override def stringPrefix = "ParSet"
}

object ParSet extends ParSetFactory[ParSet] {
  def newCombiner[T]: Combiner[T, ParSet[T]] = mutable.ParHashSetCombiner[T]

//  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParSet[T]] = new GenericCanCombineFrom[T]
}
