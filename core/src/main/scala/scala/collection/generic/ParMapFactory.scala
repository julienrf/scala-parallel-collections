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
//import scala.collection.mutable.Builder
import scala.language.{higherKinds, implicitConversions}

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
abstract class ParMapFactory[CC[X, Y] <: ParMap[X, Y] with ParMapLike[X, Y, CC, CC[X, Y], _]]
extends /*GenMapFactory[CC]
   with*/ GenericParMapCompanion[CC] {

  type Coll = MapColl

  /** A collection of type $Coll that contains given key/value bindings.
    *  @param elems   the key/value pairs that make up the $coll
    *  @tparam K      the type of the keys
    *  @tparam V      the type of the associated values
    *  @return        a new $coll consisting key/value pairs given by `elems`.
    */
  def apply[K, V](elems: (K, V)*): CC[K, V] = (newCombiner[K, V] ++= elems).result()

  type MapColl = CC[_, _]

//  /** The default builder for $Coll objects.
//   *  @tparam K      the type of the keys
//   *  @tparam V      the type of the associated values
//   */
//  def newBuilder[K, V]: mutable.Builder[(K, V), CC[K, V]] = newCombiner[K, V]

  /** The default combiner for $Coll objects.
   *  @tparam K     the type of the keys
   *  @tparam V     the type of the associated values
   */
  def newCombiner[K, V]: Combiner[(K, V), CC[K, V]]

  class CanCombineFromMap[K, V] extends CanCombineFrom[CC[_, _], (K, V), CC[K, V]] {
    def apply(from: MapColl) = from.genericMapCombiner[K, V].asInstanceOf[Combiner[(K, V), CC[K, V]]]
    def apply() = newCombiner[K, V]
  }

  implicit def toFactory[K, V]: Factory[(K, V), CC[K, V]] = ParMapFactory.toFactory(this)

}

object ParMapFactory {
  /**
    * Implicit conversion for converting any `ParFactory` into a sequential `Factory`.
    * This provides supports for the `to` conversion method (eg, `xs.to(ParArray)`).
    */
  implicit def toFactory[K, V, CC[X, Y] <: ParMap[X, Y] with ParMapLike[X, Y, CC, CC[X, Y], _]](
    parFactory: ParMapFactory[CC]
  ): Factory[(K, V), CC[K, V]] =
    new ToFactory[K, V, CC](parFactory)

  @SerialVersionUID(3L)
  private class ToFactory[K, V, CC[X, Y] <: ParMap[X, Y] with ParMapLike[X, Y, CC, CC[X, Y], _]](
    parFactory: ParMapFactory[CC]
  ) extends Factory[(K, V), CC[K, V]] with Serializable {
    def fromSpecific(it: IterableOnce[(K, V)]): CC[K, V] = (parFactory.newCombiner[K, V] ++= it).result()
    def newBuilder: mutable.Builder[(K, V), CC[K, V]] = parFactory.newCombiner
  }

}