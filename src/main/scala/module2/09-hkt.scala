package module2

import module2.higher_kinded_types.Bindable.BindableSyntax

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}


  def tuplef[F[_] : Bindable, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    Bindable[F].flatMap(fa)(a => Bindable[F].map(fb)(b => (a,b)))


  trait Bindable[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  object Bindable {
    def apply[F[_]](implicit ev: Bindable[F]): Bindable[F] = ev

    implicit class BindableSyntax[F[_], A](value: F[A]) {
      def map[B](f: A => B)(implicit ev: Bindable[F]): F[B] = Bindable[F].map(value)(f)

      def flatMap[B](f: A => F[B])(implicit ev: Bindable[F]): F[B] = Bindable[F].flatMap(value)(f)
    }
  }

  def tupleBindable[F[_]: Bindable, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  implicit val optBindable: Bindable[Option] = new Bindable[Option] {

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  implicit val listBindable: Bindable[List] = new Bindable[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }




  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r3: Option[(Int, Int)] = tupleBindable(optA, optB)
  val r4 = println(tupleBindable(list1, list2))


  val r1 = println(tuplef(optA, optB))
  val r2 = println(tuplef(list1, list2))

}