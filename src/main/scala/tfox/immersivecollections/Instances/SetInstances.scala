package tfox.immersivecollections.Instances
import cats.{Applicative, Eval, Functor, Id, Monad, Traverse, instances}
import cats.syntax.functor._

object SetInstances {
  implicit val traverse: Traverse[Set] = new Traverse[Set] {
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
  }
}
