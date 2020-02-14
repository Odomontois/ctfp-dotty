package ctfp
package p1.c7

object Snippet1
    enum Option[+A]
        case Some(a: A)
        case None

trait Snippet2[A, B]
    val f: A => B

trait Snippet3[A, B] extends Snippet2[A, B]
    val f1: Option[A] => Option[B] = 
        case None    => None
        case Some(x) => Some(f(x)) 
        
trait Snippet4
    def fmap[A, B](f: A => B): Option[A] => Option[B]

trait Snippet5
    def fmap1[A, B] (f: A => B)(o: Option[A]) :  Option[B]


trait Snippet6 extends Snippet4
    def [A, B] (o: Option[A]) fmap (f: A => B) = o match
        case None    => None
        case Some(x) => Some(f(x))

trait Snippet7
    def identity[A](x: A) = x

trait Snippet8 extends Snippet4 with Snippet7
    fmap(identity) === identity

trait PreSnippet9[B, C]
    val g: B => C

trait Snippet9[A, B, C] extends Snippet4 with Snippet2[A, B] with PreSnippet9[B, C]
    fmap(g compose f) === (fmap(g) compose fmap(f))

//snippet10    
trait Eq[A]
    def (x: A) === (y: A): Boolean

//snippet11
case class Point(x: Float, y: Float)

//snippet12
given Eq[Point] 
    def (p: Point) === (q: Point) =
        p.x == q.x && p.y == q.y

//snippet13
trait Functor[F[_]] 
    def fmap [A, B] (f: A => B): F[A] =>  F[B]

    def[A, B] (fa: F[A]) mapf (f: A => B) : F[B] = fmap(f)(fa)

import scala.deriving.Mirror

object Functor
    type Arb
    inline given derived[C[_]](using m: Mirror.Of[C[Arb]]) as Functor[C] = null

//snippet14
given Functor[Option]
    def fmap[A, B] (f: A => B) =
        case None    => None
        case Some(x) => Some(f(x))

trait Snippet15
    enum List[+A]
        case Nil
        case Cons(head: A, tail: List[A])

trait Snippet16 extends Snippet15
    def fmap[A, B](f: A => B): List[A] => List[B]

trait Snippet17 extends Snippet16   
    def fmap[A, B](f: A => B) = 
        case List.Cons(head, tail) => 
            List.Cons(f(head), fmap(f)(tail))

//snippet18
given Functor[List]
    def fmap[A, B] (f: A => B) =
        case Nil    => Nil
        case x :: t => f(x) :: fmap(f)(t)

type Snippet19[A, B] = Function1[A, B]

type Snippet20[A] = [B] =>> A => B

trait Snipept21[R]
    def fmap[A, B](f: A => B): (R => A) => (R => B)

//snippet22
given [R] as Functor[[A] =>> R => A]
    def fmap[A, B] (f: A => B) = g => f compose g

trait Snippet23[R]
    def fmap[A, B] (f: A => B) = (g : R => A) => f.compose[R](g)

//snippet24
given funcFunctor[R] as Functor[[A] =>> R => A]
    def fmap[A, B] (f: A => B) = f.compose


trait Snippet25
    val nats: LazyList[Int] = LazyList.from(1)

//snippet26
type Const[+C, +A] = C

trait Snippet27[C]
    def fmap[A, B](f: A => B): Const[C, A] => Const[C, B]

object Snippet28
    given [C] as Functor[[A] =>> Const[C, A]]
        def fmap[A, B](f: A => B) = identity

//alt snippet 26
enum Const1[C, +A]
    case of[C](c: C) extends Const1[C, Nothing]

// alt snippet28
given [C] as Functor[[A] =>> Const1[C, A]]
    def fmap[A, B](f: A => B) = 
        case c@Const1.of(_) => c

trait Snippet29
    def maybeTail[A]: List[A] => Option[List[A]] =
        case Nil     => None
        case _ :: xs => Some(xs)

trait Snippet30
    def square(x: Int) = x * x

    val mis: Option[List[Int]] = Some(List(1, 2, 3))

    val mis2: Option[List[Int]] = mis.mapf(_.mapf(square))

trait Snippet31 extends Snippet30
    override
    val mis2: Option[List[Int]] = 
        (summon[Functor[List]].fmap[Int, Int] andThen summon[Functor[Option]].fmap)(square)(mis)

trait Snippet32[F[_]]
    def fmap[A, B]: (A => B) => (F[A] => F[B])

trait Snippet33
    val square: Int => Int

type Snippet34 = List[Int] => List[Int]

type Snippet35 = Option[List[Int]] => Option[List[Int]]

    










        