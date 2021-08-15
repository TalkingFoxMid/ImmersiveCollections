package tfox.immersivecollections.Syntax

import cats.Applicative
import cats.syntax.all._
object ApplicativeSyntax {
  implicit class ApplicativeCore[F[_]: Applicative, A](private val a: F[A]) {
    def product[B](other: F[B]):F[(A, B)] = {
      Applicative[F].product(a, other)
    }
    def ap[B](ff: F[A => B]): F[B] =
      Applicative[F].ap(ff)(a)

    def productWith[B, C](other: F[B])(f: (A, B) => C) : F[C]=
      Applicative[F].product(a, other).map {
        case (a, b) => f(a, b)
      }
  }
}
