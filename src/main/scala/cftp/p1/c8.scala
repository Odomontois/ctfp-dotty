package ctfp
package p1.c8
import p1.c7.{Functor, Const}
//snippet01
trait Bifunctor[F[_, _]]
    def bimap[A, B, C, D](f: A => C)(g: B => D): F[A, B] => F[C, D] = 
        first(f) compose second(g)

    def[A, B, C, D](fab: F[A, B]) bmap (f: A => C)(g: B => D):  F[C, D] = bimap(f)(g)(fab)

    def first[A, B, C](f: A => C): F[A, B] => F[C, B] = bimap(f)(identity)
    
    def second[A, B, D](g: B => D): F[A, B] => F[A, D] = bimap(identity[A])(g)

//snippet02
given Bifunctor[[A, B] =>> (A, B)] 
    override def bimap[A, B, C, D](f: A => C)(g: B => D) = 
        (x, y) => (f(x), g(y))


trait Snippet3
    def bimap[A, B, C, D](f: A => C)(g: B => D): ((A, B)) => (C, D)

//snippet4
given Bifunctor[Either]
    override def bimap[A, B, C, D](f: A => C)(g: B => D) = 
        case Left(x)  => Left(f(x))
        case Right(y) => Right(g(y))

//snippet5
type Id[+A] = A

//snippet6
given Functor[Id]
    def fmap[A, B](f: A => B) = f

trait Snippet7
    enum Option[+A]
        case None
        case Some(a: A)

trait Snippet8
    type Option[+A] = Either[Const[Unit, A], Id[A]]

//snippet09

opaque type BiComp[bf[_, _], fu[_], gu[_]] = [A, B] =>> bf[fu[A], gu[B]]

object BiComp
    def apply[bf[_, _], fu[_], gu[_], A, B](bc: bf[fu[A], gu[B]]): BiComp[bf,fu,gu][A, B] = bc

    //snippet 10
    given[bf[_, _], fu[_], gu[_]](using bf: Bifunctor[bf], fu: Functor[fu], gu: Functor[gu]
        ) as Bifunctor[BiComp[bf, fu, gu]] 
        override def bimap[A, B, C, D](f: A => C)(g: B => D) = bf.bimap(fu.fmap(f))(gu.fmap(g))
        

    extension on[bf[_, _], fu[_], gu[_], A, B](bc: BiComp[bf, fu, gu][A, B])
        def value : bf[fu[A], gu[B]] = bc

case class BiComp1[bf[_, _], fu[_], gu[_], A, B](value: bf[fu[A], gu[B]])


given[bf[_, _], fu[_], gu[_]](using Bifunctor[bf], Functor[fu], Functor[gu]
    ) as Bifunctor[[A, B] =>> BiComp1[bf, fu, gu, A, B]] 
    override def bimap[A, B, C, D](f: A => C)(g: B => D) = bc => BiComp1(bc.value.bmap(_.mapf(f))(_.mapf(g)))


type Snippet11[bf[_, _], fu[_], gu[_], A, B] = bf[fu[A], gu[B]]


trait Snippet12[A, B, A1, B1]
    val f1: A => A1
    val f2: B => B1

trait Snippet13[bf[_, _], fu[_], gu[_], A, B, A1, B1]
    def bimap(f: fu[A] => fu[A1])(g: gu[B] => gu[B1]): bf[fu[A], gu[B]] => bf[fu[A1], gu[B1]]

trait Snippet13X
    enum Option[+A] derives Functor
        case None
        case Some(a: A)


// snippet14
enum Tree[+A] derives Functor
    case Leaf(a: A)        
    case Node(l: Tree[A], r: Tree[A])

given Functor[Tree]
    def fmap[A, B](f: A => B) = 
        case Tree.Leaf(a)    => Tree.Leaf(f(a))
        case Tree.Node(l, r) => Tree.Node(fmap(f)(l), fmap(f)(r)) 

// snippet16
type Writer[A] = (A, String)

trait Snippet17
    def[A, B, C] (m1: A => Writer[B]) >=> (m2: B => Writer[C]) : A => Writer[C] = x =>
        val (y, s1) = m1(x)
        val (z, s2) = m2(y)
        (z, s1 + s2)

trait Snippet18
    def pure[A](x: A): Writer[A] = (x, "")

trait Snippet19 extends Snippet17 with Snippet18
    def fmap[A, B](f: A => B): Writer[A] => Writer[B] = 
        identity[Writer[A]] >=> (x => pure(f(x)))


type Snippet20[R] = [A] =>> R => A

// snippet21
type Reader[R] = [A] =>> R => A

// snippet22
given[R] as Functor[Reader[R]]
    def fmap[A, B](f: A => B) = f.compose

// snippet23
type Op[R] = [A] =>> A => R

trait Snipept24[R]
    def fmap[A, B](f: A => B): (A => R) => (B => R)

//snippet25
trait Contravariant[F[_]]
    def contramap[A, B](f: B => A): F[A] => F[B]

//snippet26
given[R] as Contravariant[Op[R]]
    def contramap[A, B](f: B => A) = f.andThen

//snippet27
def  flip[A, B, C](f: (A, B) => C): (B, A) => C = (b, a) => f(a, b)

//snippet28
trait Snippet28[R]
    def contramap[A, B]: (B => A, Op[R][A]) => Op[R][B] = flip(_ compose _)

//snippet29
trait Profunctor[F[_, _]]
    def dimap[A, B, C, D](f: A => B)(g: C => D): F[B, C] => F[A, D] = 
        lmap(f) compose rmap(g)

    def lmap[A, B, C](f: A => B): F[B, C] => F[A, C] = dimap(f)(identity)
    def rmap[A, B, C](g: B => C): F[A, B] => F[A, C] = dimap(identity[A])(g)
        
//snippet30
given Profunctor[Function1]
    override def dimap[A, B, C, D](ab: A => B)(cd: C => D) = cd compose _ compose ab
    override def lmap[A, B, C](f: A => B) = f.andThen
    override def rmap[A, B, C](g: B => C) = g.compose

