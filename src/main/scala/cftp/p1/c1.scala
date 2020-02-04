package ctfp
package p1.c1

trait Snippet1[A, B]
    val f: A => B

trait Snippet2[B, C]
    val g: B => C

trait Snippet3[A, B, C] extends Snippet1[A, B] with Snippet2[B, C]
    g compose f

trait Snippet4[A, B, C, D]
    val f: A => B
    val g: B => C
    val h: C => D

    (h compose (g compose f)) === ((h compose g) compose f) === (h compose g compose f)


trait Snippet5
    def identity[A]: A => A = a => a

trait Snippet6[A, B] extends Snippet5 with Snippet1[A, B]
    (f compose identity[A]) === f
    (identity[B] compose f) === f



    