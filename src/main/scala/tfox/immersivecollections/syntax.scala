package tfox.immersivecollections

import tfox.immersivecollections.Syntax.ApplicativeSyntax

object syntax {
  implicit val applicative: ApplicativeSyntax.type = ApplicativeSyntax
}
