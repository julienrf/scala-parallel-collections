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

package scala.collection.parallel

import scala.language.implicitConversions
import scala.{collection => sc}
import scala.collection.{mutable => scm, immutable => sci/*, concurrent => scc*/}

/** Extension methods for `.par` on sequential collections. */
object CollectionConverters extends CollectionConvertersLowPriority {

  // TODO Use IsSeqLike, IsIterableLike, etc.
  // Iterable

  implicit class IterableIsParallelizable[A](private val coll: sc.Iterable[A]) extends AnyVal with sc.CustomParallelizable[A, ParIterable[A]] {
    def seq = coll
    override def par = coll match {
//      case coll: sc.Set[_] => new SetIsParallelizable(coll.asInstanceOf[sc.Set[A]]).par
//      case coll: sc.Map[_, _] => new MapIsParallelizable(coll.asInstanceOf[sc.Map[_, _]]).par.asInstanceOf[ParIterable[A]]
      case coll: sci.Iterable[A] => new ImmutableIterableIsParallelizable(coll).par
      case coll: scm.Iterable[A] => new MutableIterableIsParallelizable(coll).par
      case coll: sc.Parallelizable[_, _] => coll.asInstanceOf[sc.Parallelizable[A, ParIterable[A]]].par
      case _ => ParIterable.newCombiner[A].fromSequential(seq) // builds ParArray, same as for scm.Iterable
    }
  }

  implicit class MutableIterableIsParallelizable[A](private val coll: scm.Iterable[A]) extends AnyVal with sc.CustomParallelizable[A, mutable.ParIterable[A]] {
    def seq = coll
    override def par = coll match {
      case coll: scm.Seq[A] => new MutableSeqIsParallelizable(coll).par
//      case coll: scm.Set[A] => new MutableSetIsParallelizable(coll.asInstanceOf[scm.Set[A]]).par
//      case coll: scm.Map[_, _] => new MutableMapIsParallelizable(coll.asInstanceOf[scm.Map[_, _]]).par.asInstanceOf[mutable.ParIterable[A]]
      case coll: sc.Parallelizable[_, _] => coll.asInstanceOf[sc.Parallelizable[A, mutable.ParIterable[A]]].par
      case _ => mutable.ParIterable.newCombiner[A].fromSequential(seq) // builds ParArray
    }
  }

  implicit class ImmutableIterableIsParallelizable[A](private val coll: sci.Iterable[A]) extends AnyVal with sc.CustomParallelizable[A, immutable.ParIterable[A]] {
    def seq = coll
    override def par = coll match {
      case coll: sci.Seq[A] => new ImmutableSeqIsParallelizable(coll).par
//      case coll: sci.Set[A] => new ImmutableSetIsParallelizable(coll).par
//      case coll: sci.Map[_, _] => new ImmutableMapIsParallelizable(coll.asInstanceOf[sci.Map[_, _]]).par.asInstanceOf[immutable.ParIterable[A]]
      case coll: sc.Parallelizable[_, _] => coll.asInstanceOf[sc.Parallelizable[A, immutable.ParIterable[A]]].par
      case _ => immutable.ParIterable.newCombiner[A].fromSequential(seq) // builds ParVector
    }
  }

  // Seq
  implicit def seqIsParallelizable[A](coll: sc.Seq[A]): sc.Parallelizable[A, ParSeq[A]] = coll match {
    case it: scm.Seq[A] => new MutableSeqIsParallelizable(it)
    case it: sci.Seq[A] => new ImmutableSeqIsParallelizable(it)
    case _ => throw new IllegalArgumentException("Unexpected type "+coll.getClass.getName+" - every scala.collection.Seq must be a scala.collection.mutable.Seq or scala.collection.immutable.Seq")
  }

  implicit class MutableSeqIsParallelizable[A](private val coll: scm.Seq[A]) extends AnyVal with sc.CustomParallelizable[A, mutable.ParSeq[A]] {
    def seq = coll
    override def par = coll match {
      case coll: scm.ArraySeq[A] => new MutableArraySeqIsParallelizable(coll).par
      case coll: scm.ArrayBuffer[A] => new MutableArrayBufferIsParallelizable(coll).par
      case coll: sc.Parallelizable[_, _] => coll.asInstanceOf[sc.Parallelizable[A, mutable.ParSeq[A]]].par
      case _ => mutable.ParSeq.newCombiner[A].fromSequential(seq)
    }
  }

  implicit class MutableArraySeqIsParallelizable[T](private val coll: scm.ArraySeq[T]) extends AnyVal with sc.CustomParallelizable[T, mutable.ParArray[T]] {
    def seq = coll
    override def par = mutable.ParArray.handoff(coll.array.asInstanceOf[Array[T]], coll.length)
  }

  implicit class MutableArrayBufferIsParallelizable[T](private val coll: scm.ArrayBuffer[T]) extends AnyVal with sc.CustomParallelizable[T, mutable.ParArray[T]] {
    def seq = coll
    override def par = mutable.ParArray.handoff[T](coll.array.asInstanceOf[Array[T]], coll.size)
  }

  // immutable.Seq

  implicit class ImmutableSeqIsParallelizable[A](private val coll: sci.Seq[A]) extends AnyVal with sc.CustomParallelizable[A, immutable.ParSeq[A]] {
    def seq = coll
    override def par = coll match {
      case coll: sci.Vector[_] => new VectorIsParallelizable(coll.asInstanceOf[sci.Vector[A]]).par
      case coll: sci.Range => new RangeIsParallelizable(coll).par.asInstanceOf[immutable.ParSeq[A]]
      case _ => immutable.ParSeq.newCombiner[A].fromSequential(seq)
    }
  }

  implicit class RangeIsParallelizable(private val coll: sci.Range) extends AnyVal with sc.CustomParallelizable[Int, immutable.ParRange] {
    def seq = coll
    override def par = new immutable.ParRange(coll)
  }

  implicit class VectorIsParallelizable[T](private val coll: sci.Vector[T]) extends AnyVal with sc.CustomParallelizable[T, immutable.ParVector[T]] {
    def seq = coll
    override def par = new immutable.ParVector(coll)
  }

  // Set

//  implicit class SetIsParallelizable[A](private val coll: sc.Set[A]) extends AnyVal with CustomParallelizable[A, ParSet[A]] {
//    def seq = coll
//    override def par = coll match {
//      case coll: sci.Set[_] => new ImmutableSetIsParallelizable(coll.asInstanceOf[sci.Set[A]]).par
//      case coll: scm.Set[_] => new MutableSetIsParallelizable(coll.asInstanceOf[scm.Set[A]]).par
//      case coll: Parallelizable[_, _] => coll.asInstanceOf[Parallelizable[A, ParSet[A]]].par
//      case _ => ParSet.newCombiner[A].fromSequential(seq)
//    }
//  }
//
//  implicit class ImmutableSetIsParallelizable[A](private val coll: sci.Set[A]) extends AnyVal with CustomParallelizable[A, immutable.ParSet[A]] {
//    def seq = coll
//    override def par = coll match {
//      case coll: sci.HashSet[_] => new ImmutableHashSetIsParallelizable(coll.asInstanceOf[sci.HashSet[A]]).par
//      case coll: Parallelizable[_, _] => coll.asInstanceOf[Parallelizable[A, immutable.ParSet[A]]].par
//      case _ => immutable.ParSet.newCombiner[A].fromSequential(seq)
//    }
//  }
//
//  implicit class MutableSetIsParallelizable[A](private val coll: scm.Set[A]) extends AnyVal with CustomParallelizable[A, mutable.ParSet[A]] {
//    def seq = coll
//    override def par = coll match {
//      case coll: scm.HashSet[_] => new MutableHashSetIsParallelizable(coll.asInstanceOf[scm.HashSet[A]]).par
//      case coll: Parallelizable[_, _] => coll.asInstanceOf[Parallelizable[A, mutable.ParSet[A]]].par
//      case _ => mutable.ParSet.newCombiner[A].fromSequential(seq)
//    }
//  }
//
//  implicit class MutableHashSetIsParallelizable[T](private val coll: scm.HashSet[T]) extends AnyVal with CustomParallelizable[T, mutable.ParHashSet[T]] {
//    def seq = coll
//    override def par = new mutable.ParHashSet(coll.hashTableContents)
//  }
//
//  implicit class ImmutableHashSetIsParallelizable[T](private val coll: sci.HashSet[T]) extends AnyVal with CustomParallelizable[T, immutable.ParHashSet[T]] {
//    def seq = coll
//    override def par = immutable.ParHashSet.fromTrie(coll)
//  }

  // Map

//  implicit class MapIsParallelizable[K, V](private val coll: sc.Map[K, V]) extends AnyVal with CustomParallelizable[(K, V), ParMap[K, V]] {
//    def seq = coll
//    override def par = coll match {
//      case coll: sci.Map[_, _] => new ImmutableMapIsParallelizable(coll.asInstanceOf[sci.Map[K, V]]).par
//      case coll: scm.Map[_, _] => new MutableMapIsParallelizable(coll.asInstanceOf[scm.Map[K, V]]).par
//      case coll: Parallelizable[_, _] => coll.asInstanceOf[Parallelizable[(K, V), ParMap[K, V]]].par
//      case _ => ParMap.newCombiner[K, V].fromSequential(seq)
//    }
//  }
//
//  implicit class ImmutableMapIsParallelizable[K, V](private val coll: sci.Map[K, V]) extends AnyVal with CustomParallelizable[(K, V), immutable.ParMap[K, V]] {
//    def seq = coll
//    override def par = coll match {
//      case coll: sci.HashMap[_, _] => new ImmutableHashMapIsParallelizable(coll.asInstanceOf[sci.HashMap[K, V]]).par
//      case coll: Parallelizable[_, _] => coll.asInstanceOf[Parallelizable[(K, V), immutable.ParMap[K, V]]].par
//      case _ => immutable.ParMap.newCombiner[K, V].fromSequential(seq)
//    }
//  }
//
//  implicit class MutableMapIsParallelizable[K, V](private val coll: scm.Map[K, V]) extends AnyVal with CustomParallelizable[(K, V), mutable.ParMap[K, V]] {
//    def seq = coll
//    override def par = coll match {
//      case coll: scm.HashMap[_, _] => new MutableHashMapIsParallelizable(coll.asInstanceOf[scm.HashMap[K, V]]).par
//      case coll: scc.TrieMap[_, _] => new ConcurrentTrieMapIsParallelizable(coll.asInstanceOf[scc.TrieMap[K, V]]).par
//      case coll: Parallelizable[_, _] => coll.asInstanceOf[Parallelizable[(K, V), mutable.ParMap[K, V]]].par
//      case _ => mutable.ParMap.newCombiner[K, V].fromSequential(seq)
//    }
//  }
//
//  implicit class ImmutableHashMapIsParallelizable[K, V](private val coll: sci.HashMap[K, V]) extends AnyVal with CustomParallelizable[(K, V), immutable.ParHashMap[K, V]] {
//    def seq = coll
//    override def par = immutable.ParHashMap.fromTrie(coll)
//  }
//
//  implicit class MutableHashMapIsParallelizable[K, V](private val coll: scm.HashMap[K, V]) extends AnyVal with CustomParallelizable[(K, V), mutable.ParHashMap[K, V]] {
//    def seq = coll
//    override def par = new mutable.ParHashMap[K, V](coll.hashTableContents)
//  }
//
//  implicit class ConcurrentTrieMapIsParallelizable[K, V](private val coll: scc.TrieMap[K, V]) extends AnyVal with CustomParallelizable[(K, V), mutable.ParTrieMap[K, V]] {
//    def seq = coll
//    override def par = new mutable.ParTrieMap(coll)
//  }

  // Other

  implicit class ArrayIsParallelizable[T](private val a: Array[T]) extends AnyVal with sc.CustomParallelizable[T, mutable.ParArray[T]] {
    def seq = a // via ArrayOps
    override def par = mutable.ParArray.handoff(a)
  }
}

trait CollectionConvertersLowPriority { self: CollectionConverters.type =>

  // Generic

//  implicit def genSetLikeIsParallelizable[A, Repr](coll: sc.Set[A]): Parallelizable[A, ParSet[A]] = coll match {
//    case coll: Parallelizable[_, _] => coll.asInstanceOf[Parallelizable[A, ParSet[A]]].par
//    case it: sc.Set[_] => new SetIsParallelizable(it.asInstanceOf[sc.Set[A]])
//    case coll => throw new IllegalArgumentException("Unexpected type "+coll.getClass.getName+" - every scala.collection.GenSetLike must be Parallelizable or a scala.collection.Set")
//  }
//
//  implicit def genMapLikeIsParallelizable[K, V, Repr](coll: sc.Map[K, V]): Parallelizable[(K, V), ParMap[K, V]] = coll match {
//    case coll: Parallelizable[_, _] => coll.asInstanceOf[Parallelizable[(K, V), ParMap[K, V]]].par
//    case it: sc.Map[_, _] => new MapIsParallelizable(it.asInstanceOf[sc.Map[K, V]])
//    case coll => throw new IllegalArgumentException("Unexpected type "+coll.getClass.getName+" - every scala.collection.GenMapLike must be Parallelizable or a scala.collection.Map")
//  }
}
