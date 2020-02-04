package ctfp
package p1.c6

//snippet1
def swap[A, B]: ((A, B)) => (B, A) = 
    case (x, y) => (y, x)

type Snippet2[A, B, C] = ((A, B), C)
type Snippet3[A, B, C] = (A, (B, C))

//snippet4
def alpha[A, B, C]: (((A, B), C)) => ((A, (B, C))) = 
    case ((x, y), z) => (x, (y, z))

//snippet5
def alphaInv[A, B, C]: ((A, (B, C))) => (((A, B), C)) = 
    case (x, (y, z)) => ((x, y), z)

type Snippet6[A] = (A, Unit)

//snippet6
def rho[A]: ((A, Unit)) => A = 
    case (x, ()) => x

//snippet7
def rhoInv[A]: A => (A, Unit) =
    x => (x, ())

trait Snippet9
    enum Pair[A, B]
        case P(a: A, b: B)

trait Snippet10 extends Snippet9
    val stmt: Pair[String, Boolean] =
        Pair.P("This statement is", false)

//snippet11
case class Pair[A, B](a: A, b: B)

object Snippet12
    val stmt = ("This statement is", false)

//snippet13
case class Stmt(s: String, b: Boolean)


trait Snippet14
    val startsWithSymbol: ((String, String, Int)) => Boolean = 
        case (name, symbol, _) => name startsWith symbol

//snippet15  
case class Element(
    name: String,
    symbol: String,
    atomicNumber: Int
)

//snippet16
val tupleToElem: ((String, String, Int)) => Element = 
  case (n, s, a) => Element(n, s, a)

//snippet17
val elemToTuple: Element => (String, String, Int) =
  e => (e.name, e.symbol, e.atomicNumber)  

trait Snippet18
    val atomicNumber: Element => Int

object Snippet19    
    def startsWithSymbol(e: Element): Boolean =
        e.name.startsWith(e.symbol)

object Snippet20    
    def startsWithSymbol(e: Element): Boolean =
        e.name startsWith e.symbol

object Snippet21
    enum Either[+A, +B]
        case Left(v: A)
        case Right(v: B)

object Snippet22
    enum OneOfThree[+A, +B, +C]
        case Sinistral(v: A)
        case Medial(v: B)
        case Dextral(v: C)

type Snippet23[A] = Either[A, Nothing]

object Snippet24
    enum Color
        case Red, Green, Blue

object Snippet25
    enum Bool
        case True, False

object Snippet26
    enum Option[+A]
        case None
        case Some(a: A)

object Snippet27
    case object NoneType

object Snippet28
    case class SomeType[+A](a: A)

object Snippet29
    type Option[+A] = Either[Unit, A]

object Snippe30
    enum List[+A]
        case Nil
        case Cons(head: A, tail: List[A])

object Snippet31    
    def optionTail[A](l: List[A]): Option[List[A]] = l match
        case List.Nil => None
        case List.Cons(_, t) => Some(t)

type Snippet32[A, B, C] = (A, Either[B, C])
type Snippet33[A, B, C] = Either[(A, B), (A, C)]

//snippet34

def prodToSum[A, B, C](p : (A, Either[B, C])): Either[(A, B), (A, C)] = p._2 match 
    case Left(y) => Left((p._1, y))
    case Right(z) => Right((p._1, z))

//snippet35
def sumToProd[A, B, C](e: Either[(A, B), (A, C)]): (A, Either[B, C]) = e match 
  case Left((x, y)) => (x, Left(y))
  case Right((x, z)) => (x, Right(z))

//snippet36
val prod1: (Int, Either[String, Float]) = (2, Left("Hi!"))

object Snippet37
    enum List[+A]
        case Nil
        case Cons(head: A, tail: List[A])
  

    



    