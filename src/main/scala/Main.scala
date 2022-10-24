trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

trait Applicative[F[_]] extends Functor[F]:

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def unit[A](a: => A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  ): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(a => a)
    // fas.foldRight(unit(List[A]()))((fa, fas) => map2(fa, fas)(_ :: _))
    //            ^^^^^^^^^^^^^^^
    //                F[List[A]]
    //                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //                             (F[A], F[List[A]]) => F[List[A]]

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    if (n <= 0) {
      unit(List[A]())
    } else {
      map2(fa, replicateM(n - 1, fa))(_ :: _)
    }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))
