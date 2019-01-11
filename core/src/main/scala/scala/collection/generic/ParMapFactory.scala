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
package generic

import scala.collection.parallel.ParMap
import scala.collection.parallel.ParMapLike
import scala.collection.parallel.Combiner
import scala.collection.mutable.Builder
import scala.language.higherKinds

/** A template class for companion objects of `ParMap` and subclasses thereof.
 *  This class extends `TraversableFactory` and provides a set of operations
 *  to create `$Coll` objects.
 *
 *  @define coll parallel map
 *  @define Coll `ParMap`
 *  @define factoryInfo
 *    This object provides a set of operations needed to create `$Coll` values.
 *  @author Aleksandar Prokopec
 *  @since 2.8
 */
abstract class ParMapFactory[CC[X, Y] <: ParMap[X, Y] with ParMapLike[X, Y, CC[X, Y], _]]
extends GenMapFactory[CC]
   with GenericParMapCompanion[CC] {

  type MapColl = CC[_, _]

  /** The default builder for $Coll objects.
   *  @tparam K      the type of the keys
   *  @tparam V      the type of the associated values
   */
  override def newBuilder[K, V]: Builder[(K, V), CC[K, V]] = newCombiner[K, V]

  /** The default combiner for $Coll objects.
   *  @tparam K     the type of the keys
   *  @tparam V     the type of the associated values
   */
  def newCombiner[K, V]: Combiner[(K, V), CC[K, V]]

  class CanCombineFromMap[K, V] extends CanCombineFrom[CC[_, _], (K, V), CC[K, V]] {
    def apply(from: MapColl) = from.genericMapCombiner[K, V].asInstanceOf[Combiner[(K, V), CC[K, V]]]
    def apply() = newCombiner[K, V]
  }

}

