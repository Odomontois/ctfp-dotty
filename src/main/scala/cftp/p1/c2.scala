package ctfp
package p1.c2

trait Snippet1
    val x: BigInt

trait Snippet2
    val f: Boolean => Boolean

trait Snippet3 extends Snippet2
    val f: Boolean => Boolean = x => ???

trait Snippet4 extends Snippet2
    val f: Boolean => Boolean = ???

trait Snippet5
    val fact = (n: Int) => (1 to n).product

trait Snippet6
    def absurd[A]: Nothing => A

trait Snippet7
    val f44: Unit => BigInt = _ => 44

trait Snippet8
    val fInt: BigInt => Unit = x => ()

trait Snippet9
    val fInt: BigInt => Unit = _ => ()

trait Snippet10
    def unit[A]: A => Unit = _ => ()

object Snippet11
    enum Bool
        case True, False



