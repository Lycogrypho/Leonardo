package it.grypho.scala.leonardo
package core

import scala.collection.IterableOps


/** `Some` of every value when none is missing, in the collection type it was given; `None` as
 *  soon as one is.
 *
 *  One definition for every package (F_0080): `optimize` and `statistics` each carried a private
 *  copy, over `List` and over `Vector`.
 *
 *  @tparam A  the element type
 *  @tparam CC the collection type, kept in the result
 *  @param xs  the optional values
 *  @return the values, or `None` if any is absent
 */
private[leonardo] def sequence[A, CC[X] <: IterableOps[X, CC, CC[X]]](xs: CC[Option[A]]): Option[CC[A]] =
  Option.when(xs.forall(_.isDefined))(xs.collect { case Some(a) => a })
