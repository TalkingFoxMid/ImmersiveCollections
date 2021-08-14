package tfox.immersivecollections.Instances
import cats.{Applicative, Eval, FlatMap, Functor, Id, Monad, Traverse, instances}
import cats.syntax.functor._

object SetInstances {
  implicit val traverse: Traverse[Set] with FlatMap[Set] = new Traverse[Set] with FlatMap[Set] {
    override def traverse[G[_], A, B](fa: Set[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Set[B]] =
      fa.foldLeft(Applicative[G].pure(Set.empty[B])) {
        case (a, b)=> Applicative[G].product(a, f(b))
          .map {
            case (value, b) => value + b
          }
      }

    override def foldLeft[A, B](fa: Set[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    override def foldRight[A, B](fa: Set[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.foldLeft(lb)((a,b) => f(b,a))

    override def flatMap[A, B](fa: Set[A])(f: A => Set[B]): Set[B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Set[Either[A, B]]): Set[B] =
      f(a).collect { case Right(value) => value}
  }
}
