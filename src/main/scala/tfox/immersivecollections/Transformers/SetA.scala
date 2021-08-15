package tfox.immersivecollections.Transformers

import cats.Applicative
import tfox.immersivecollections.syntax.applicative._
import cats.syntax.applicative._
import cats.syntax.functor._

object SetA {
  class SetAOps[F[_]: Applicative, A](private val set: F[Set[A]]) {
    def intersect(other: F[Set[A]]): F[Set[A]] =
      set.productWith(other)(_ intersect _)

    def union(other: F[Set[A]]): F[Set[A]] =
      set.productWith(other)(_ union _)
  }
}

