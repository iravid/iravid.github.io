---
date: '2017-11-10'
title: 'On encoders, contravariant functors and contravariant applicatives'
---

Functors and applicatives are very common in the functional programming
landscape. They arise naturally in various computational contexts -
optionality (`Option`), possible failure (`Either[A, ?]`), effect
suspension (`IO`) and so forth.

All of these computational contexts are functors; the practical meaning
is that given a function `A => B` (forall `A`, `B`), we can lift it to
operate within the context - `Option[A] => Option[B]`. That is the
essential meaning of having an instance of `Functor`: the ability to
lift ordinary functions to fancy functions that operate in a special
context.

We'll use these data types throughout the post:

```scala
case class Username(name: String)
case class UserId(id: Int)
case class User(id: UserId, name: Username)
```

Here's a short example to illustrate this, using `cats` and
`cats-effect`:

```scala
import cats.effect.IO

trait DB {
  def retrieveInt(): IO[Int]
}

val testDB = new DB {
  def retrieveInt(): IO[Int] = IO(0)
}
```

Our `DB` interface only provides an `IO[Int]` output; it has no
knowledge of our `UserId` data type. Luckily, since `IO` has a `Functor`
instance, we can lift the `UserId.apply` function, and compose it with
`DB.retrieveInt`:

```scala
import cats.implicits._, cats.effect.implicits._
// import cats.implicits._
// import cats.effect.implicits._

import cats.Functor
// import cats.Functor

val original = UserId(_)
// original: Int => UserId = <function1>

val lifted = Functor[IO].lift(original)
// lifted: cats.effect.IO[Int] => cats.effect.IO[UserId] = <function1>

val retrievedUserId = lifted(testDB.retrieveInt())
// retrievedUserId: cats.effect.IO[UserId] = IO$1971858114
```

The essence of being able to `lift` ordinary functions means that having
a `Functor` instance allows us to utilize our ordinary functions in
special contexts.

Let's turn to another data type: a JSON decoder. We'll use `play-json`
in this post. The decoder type, `Reads[A]`, is morally equivalent to a
function:

```scala
type Reads[A] = String => JsResult[A]
type JsResult[A] = Either[JsError, A]
```

A JSON decoder receives a `String` and returns a possible error value.
We can write a functor for his data type:

```scala
import play.api.libs.json._

implicit val decoderFunctor = new Functor[Reads] {
  def map[A, B](fa: Reads[A])(f: A => B): Reads[B] = 
    Reads {
      fa.reads(_).map(f)
    }
}
```

You'll notice that we just reused the fact that `JsResult[A]` has a
`Functor` instance. Now, going back to our original example of `String`
and `UserId`, we can parse a `UserId` given a `Reads[String]` and a
`String => UserId`:

```scala
val idDecoder = Reads.of[Int].map(UserId(_))
// idDecoder: play.api.libs.json.Reads[UserId] = play.api.libs.json.Reads$$anon$8@4193da4d
```

What happens when we need to combine two parsed values?

```scala
val nameDecoder = Reads.of[String].map(Username(_))
// nameDecoder: play.api.libs.json.Reads[Username] = play.api.libs.json.Reads$$anon$8@e0c03bd
```

We have a function `(UserId, Username) => User`, and two instances of
`Reads`. So we need a function of the form:

```scala
def lift2(f: (UserId, Username) => User): (JsDecoder[UserId], JsDecoder[Username]) => JsDecoder[User]
```

Meaning, a function that lifts a function in two arguments, to a decoder
that combines two decoders. This notion is available with the
`Applicative` typeclass. `cats` doesn't provide the `lift2` function
directly, but we have `map2` at our disposal, which is similar.

The `Applicative` instance for `Reads` is available in
[play-json-cats](https://github.com/iravid/play-json-cats) so we'll
elide it for brevity, but let's see how `map2` is used:

```scala
import cats.Applicative
// import cats.Applicative

import com.iravid.playjsoncats.implicits._
// import com.iravid.playjsoncats.implicits._

val userDecoder = Applicative[Reads].map2(idDecoder, nameDecoder)(User(_, _))
// userDecoder: play.api.libs.json.Reads[User] = play.api.libs.json.Reads$$anon$8@21bf1b1f
```

Let's now consider the dual of `Reads`: `Writes`. This, too, is just a
glorified function, of the form `A => String`. Very similar to `Reads`,
only this time the arrow is reversed and the possibility for failure is
elided. The arrow being reversed is significant: it means that we can't
compose the encoder with functions of `A => B` as the encoder must be
**supplied** with an `A`. Thus, we can't have a regular `Functor` for
this.

But if we have a function that **produces** an `A`, we can pre-compose
it with the encoder. Let's try to develop an intuition for what we can
do with this through an example with a simplified `Writes` definition:

```scala
type PseudoWrites[A] = A => String

val stringWrites: PseudoWrites[Int] = _.toString

val userIdToString: UserId => Int = _.id
```

Here, we have a `Writes[Int] = Int => String`, and a function `UserId =>
Int`. Composing these functions together, we get `UserId => String` -
which is a `Writes[UserId]`! So if we know how to encode a simple type
`A` as a String, and we know how to encode a complex type `B` as an `A`,
we can encode `B` as `String` for free.

This notion is encoded in the contravariant functor typeclass:

```scala
trait Contravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

// specialized for Reads:
def contramap[A, B](fa: Reads[A])(f: B => A): Reads[B]
```

If we flip the argument order and curry the arguments, we can get
something similar to `lift`:

```scala
def contralift[A, B](f: B => A): Writes[A] => Writes[B]
```

Notice how `contralift` flips the arrows in the lifted function; this is
the notion of contravariance in a functor. As a side note, the "regular"
functor is actually called a covariant functor.

How is this useful, you ask? Well, libraries such as `play-json` usually
come preloaded with encoders and decoders for the primitive types. Using
the contravariant functor, we can succinctly derive an encoder for a
wrapper type (again using
[play-json-cats](https://github.com/iravid/play-json-cats/)):

```scala
val intWrites = Writes.of[Int]
val userIdWrites: Writes[UserId] = intWrites.contramap(_.id)

val stringWrites = Writes.of[String]
val usernameWrites: Writes[Username] = stringWrites.contramap(_.name)
```

Continuing along, can we generalize this to more than one `Writes`, as
we did with `Applicative`? Asking differently, if we know how to encode
a `UserId` and a `Username`, do we know how to encode the `User` data
type?

Let's see what's the signature we're looking for:

```scala
def contramap2(fa: JsEncoder[UserId], fb: JsEncoder[Username])(f: User => (UserId, Username)): JsEncoder[User]
```

The typeclass that describes this operation is called `Divide`,
originating in Edward Kmett's
[contravariant](https://hackage.haskell.org/package/contravariant/)
package; here's how it looks like in Scala:

```scala
trait Divide[F[_]] extends Contravariant[F] {
  def divide[A, B, C](fb: F[A], fc: F[B])(f: C => (A, B)): F[C]
}
```

The division wording comes from the fact that we are dividing a big
problem (the notion of encoding a `User`) into smaller problems that are
solvable (encoding a `UserId` and a `Username`).

This typeclass not available in cats
[yet](HTTps://github.com/typelevel/cats/issues/1935), but we do have
`contramap` available on the cartesian builders, which acts as `divide`;
we just need an instance of `Cartesian` for our `Writes`:

```scala
import cats.Cartesian
// import cats.Cartesian

implicit val cartesian: Cartesian[Writes] = new Cartesian[Writes] {
  def product[A, B](fa: Writes[A], fb: Writes[B]): Writes[(A, B)] =
    Writes { case (a, b) =>
      Json.arr(fa.writes(a), fb.writes(b))
    }
}
// cartesian: cats.Cartesian[play.api.libs.json.Writes] = $anon$1@430e9d37

val userWrites: Writes[User] = (userIdWrites |@| usernameWrites).contramap(u => (u.id, u.name))
// userWrites: play.api.libs.json.Writes[User] = play.api.libs.json.Writes$$anon$6@16ca9e06
```

Do note that our choice of combining the output of the individual
`Writes` instances as a `JsArray` is quite arbitrary. If we combine more
than one `Writes` instances, our types line up, but we get nested
arrays:

```scala
implicit val tripleWrites: Writes[(Int, Int, Int)] = 
  (Writes.of[Int] |@| Writes.of[Int] |@| Writes.of[Int]).tupled
// tripleWrites: play.api.libs.json.Writes[(Int, Int, Int)] = play.api.libs.json.Writes$$anon$6@4682b468

tripleWrites.writes((1, 1, 1)).toString
// res6: String = [1,[1,1]]
```

This typeclass instance is, in fact, unlawful. Instances of the
`Cartesian` typeclass are required to uphold the associativity law -
`(a product b) product
c` must be equal to `a product (b product c)`. In our case, the two
formulations would result in encoders which produce different JSON
arrays:

```scala
implicit val intWriter = Writes.of[Int]
// intWriter: play.api.libs.json.Writes[Int] = play.api.libs.json.DefaultWrites$IntWrites$@7388d94d

implicit val sideA: Writes[(Int, Int, Int)] = 
  (intWriter product (intWriter product intWriter)).contramap {
    case (a, b, c) => (a, (b, c))
  }
// sideA: play.api.libs.json.Writes[(Int, Int, Int)] = play.api.libs.json.Writes$$anon$6@189c12c4

implicit val sideB: Writes[(Int, Int, Int)] = 
  ((intWriter product intWriter) product intWriter).contramap {
    case (a, b, c) => ((a, b), c)
  }
// sideB: play.api.libs.json.Writes[(Int, Int, Int)] = play.api.libs.json.Writes$$anon$6@6e3a479f

sideA.writes((1, 1, 1)).toString
// res7: String = [1,[1,1]]

sideB.writes((1, 1, 1)).toString
// res8: String = [[1,1],1]
```

A more principled way of combining encoders is required. We could
introspect the resulting `JsValue` in the composed encoder and flatten
the resulting JSON array, but what would happen though if we would like
to preserve nesting in products? This is not an easy problem. [Sam
Halliday](https://twitter.com/fommil) has been tackling the same issues
in his [scalaz-deriving](https://gitlab/fommil/stalactite) project.
We'll follow along to see what solutions he'll discover :-)

We can summarize by saying that the laws can be successfully upheld when
the target datatype for the typeclass is a lawful semigroup (e.g.
`(a |+| b) |+| c
<-> a |+| (b |+| c)`). `JsValue` under the JSON array concatenation as
formulated here is not.

Other useful, lawful instances often show up when dealing with type
constructors that are isomorphic to functions that consume the data. For
example:

-   `Equal[A]`, which is isomorphic to `(A, A) => Boolean`;
-   `Order[A]`, which is isomorphic to `(A, A) => LT/EQ/GT`;
-   `Predicate[A]` - which is just a fancy name for `A => Boolean`.

Admittedly, these aren't as common as the covariant functors, but
they're quite useful for reducing boilerplate.

When starting out this post, I had hoped that I would figure out a good
way to build up encoder instances for product types out of the instances
of their fields, but we have not managed to achieve that lawfully. I
intend on a following post to look into the relationship between
encoders (contravariant functors), decoders (covariant functors) and
profunctors - functors that are contravariant on one type parameter and
covariant on the other. Should be interesting!
