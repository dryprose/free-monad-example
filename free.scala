object FreeExample extends FreeExample
trait FreeExample {
  sealed trait Free[F[_],A] {
    def pure(a: A): Free[F,A] = Value(a)
    def flatMap[B](k: A => Free[F,B]): Free[F,B] = this match {
      case Value(a) =>
        k(a)
      case Request(cmd, k1) =>
        Request(cmd, k1 andThen (_ flatMap k))
    }
    def map[B](f: A => B): Free[F,B] =
      flatMap(f andThen (Value(_)))

    def foldMap[G[_]:Monad](f: F ~> G): G[A] = {
      val G = implicitly[Monad[G]]
      this match {
        case Value(a) =>
          G.pure(a)
        case Request(cmd, k) =>
          G.flatMap(f(cmd))(k andThen (_.foldMap(f)))
      }
    }

  }
  final case class Value[F[_],A](a: A)
      extends Free[F,A]
  final case class Request[F[_],R,A](command: F[R],
                                     callback: R => Free[F,A]) extends Free[F,A]


  sealed trait ConvoOp[R]
  case class Ask(s: String) extends ConvoOp[String]
  case class Tell(s: String) extends ConvoOp[Unit]

  type Convo[R] = Free[ConvoOp,R]

  implicit def lift[F[_],R](command: F[R]): Free[F,R] =
    Request(command, (x:R) => Value(x))

  val prog: Convo[Unit] = for {
    lang <- Ask("What's your favorite language?")
    _ <- Tell(s"$lang rocks!")
  } yield ()

  trait ~>[F[_],G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  object ProdConvo extends (ConvoOp ~> Id) {
    def apply[R](fa: ConvoOp[R]): Id[R] = fa match {
      case Ask(s) =>
        println(s)
        Id(readLine)
      case Tell(s) =>
        Id(println(s))
    }
  }


  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A,B](m: F[A])(k: A => F[B]): F[B]
    def map[A,B](m: F[A])(f: A => B): F[B] = flatMap(m)(f andThen pure)

  }

  sealed case class Id[A](a: A)
  implicit val IdMonad: Monad[Id] = new Monad[Id] {
    def pure[A](a: A) = Id(a)
    def flatMap[A,B](m: Id[A])(k: A => Id[B]): Id[B] =
      m match { case Id(a) => k(a) }
  }

  prog.foldMap(ProdConvo)

  type CannedResponses = Map[String,String]
  case class TestConvoLog[A](a: A, convo: List[String])
  type ConvoTester[A] = CannedResponses => TestConvoLog[A]

  object TestConvo extends (ConvoOp ~> ConvoTester) {
    def apply[A](fa: ConvoOp[A]): ConvoTester[A] = fa match {
      case Ask(s) =>
        (m) => {
          val response = m.getOrElse(s, "I don't know, man.")
          TestConvoLog(response, List(s, response))
        }
      case Tell(s) =>
        (m) => TestConvoLog((), List(s))
    }
  }

  implicit val ConvoTesterMonad: Monad[ConvoTester] = new Monad[ConvoTester] {
    def pure[A](a: A): ConvoTester[A] = (_) => TestConvoLog(a, List())
    def flatMap[A,B](ma: ConvoTester[A])(k: A => ConvoTester[B]) = (m) => {
      val TestConvoLog(a, log1) = ma(m)
      val TestConvoLog(b, log2) = k(a)(m)
      TestConvoLog(b, log1 ++ log2)
    }
  }

  val cannedResponses = Map(("What's your favorite language?" -> "Haskell"))
  prog.foldMap(TestConvo)
}

object Example extends App with FreeExample
