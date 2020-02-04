package ctfp
package p1.c5

trait Snippet1
    def absurd[A]: Nothing => A

trait Snippet2
    def unit[A]: A => Unit = _ => ()

trait Snippet3
    def yes[A]: A => Boolean = _ => true

trait Snippet4 
    def no[A]: A => Boolean = _ => false

trait PreSnippet5[A, B]
    val f: A => B
    val g: B => A

trait Snippet5[A, B] extends PreSnippet5[A, B]
    (f compose g) === identity
    (g compose f) === identity

trait Snippet6 
    def fst[A, B]: ((A, B)) => A = 
        case (x, y) => x

trait Snippet7
    def snd[A, B]: ((A, B)) => B = 
        case (x, y) => y

trait Snippet8
    def fst[A, B]: ((A, B)) => A = _._1
    def snd[A, B]: ((A, B)) => B = _._2

trait Snippet9[A, B, C]
    val p: C => A
    val q: C => B

trait Snippet10 extends Snippet9[Int, Boolean, Int]
    val p: Int => Int = x => x
    val q: Int => Boolean = _ => true

trait Snippet11 
    def p: ((Int, Int, Boolean)) => Int = _._1
    def q: ((Int, Int, Boolean)) => Boolean = _._3

trait PreSnippet12[A, B, C, D]
    def p1: D => A
    def q1: D => B
    def m:  D => C


trait Snippet12[A, B, C, D] extends Snippet9[A, B, C] with PreSnippet12[A, B, C, D]
    p1 === (p compose m)
    q1 === (q compose m)

trait Snippet13
    val m: Int => (Int, Boolean) = x => (x, true)   
    
trait Snippet14 extends Snippet8 with Snippet9[Int, Boolean, Int] with Snippet13
   val p = x => fst(m(x)) === x
   val q = x => snd(m(x)) === true     

trait Snippet15
    val m: ((Int, Int, Boolean)) => (Int, Boolean) = 
        case (x, _, b) => (x, b)

trait PreSnippet16[A, B, C] 
    val m1 : ((A, B)) => C

trait Snippet16[A, B, C] extends Snippet8 with Snippet9[A, B, C] with PreSnippet16[A, B, C]
    fst === (p compose m1)
    snd === (q compose m1)

trait Snippet17 extends Snippet16[Int, Boolean, (Int, Int, Boolean)]
    val m1: ((Int, Boolean)) => (Int, Int, Boolean) = 
        case (x, b) => (x, x, b)

trait Snippet18 extends Snippet16[Int, Boolean, (Int, Int, Boolean)]
    val m1: ((Int, Boolean)) => (Int, Int, Boolean) = 
        case (x, b) => (x, 42, b)

trait Snippet19[A, B, C] extends Snippet16[A, B, C]
    val m: C => (A, B) = x => (p(x), q(x))

trait Snippet20
    def factorizer[A, B, C]: (C => A, C => B) => (C => (A, B)) =
        (p, q) => x => (p(x), q(x))

trait Snippet21[A, B, C]
    val i: A => C
    val j: B => C

trait PreSnippet2[A, B, C, D]
    val m: C => D
    val i1: A => D
    val j1: B => D

trait Snippet22[A, B, C, D] extends Snippet21[A, B, C] with PreSnippet2[A, B, C, D]
    i1 === (m compose i)
    j1 === (m compose j)

object Snippet23
    enum Contact
        case PhoneNum(num: Int)
        case EmailAddr(addr: String)

import Snippet23.Contact

object Snippet24
    val helpdesk: Contact = Contact.PhoneNum(2222222)

object Snippet25
    enum Either[+A, +B]
        case Left(v: A)
        case Right(v: B)

object Snippet26
    def factorizer[A, B, C]: (A => C, B => C) => Either[A, B] => C =
        (i, j) => 
            case Left(a) => i(a)
            case Right(b) => j(b)

trait PreSnippet27[A, B, C]
    val m: C => (A, B)
            
trait Snippet27[A, B, C] extends Snippet8 with Snippet9[A, B, C] with PreSnippet27[A, B, C]
    p === (fst compose m)
    q === (snd compose m)

trait Snippet28[A, B, C] extends Snippet27[A, B, Unit]
    p(()) === fst(m(()))
    q(()) === snd(m(()))



    

    