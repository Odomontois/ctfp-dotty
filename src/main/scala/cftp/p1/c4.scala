package ctfp 
package p1.c4

//snippet1
type Writer[A] = (A, String)


type Snippet2[A, B] = A => Writer[B]

trait Snippet3
    def [A, B, C](ab: A => Writer[B]) >=> (bc: B => Writer[C]): A => Writer[C]

trait Snippet4 extends Snippet3
    def [A, B, C](ab: A => Writer[B]) >=> (bc: B => Writer[C]) = a =>
        val (b, s1) = ab(a)
        val (c, s2) = bc(b)
        (c, s1 + s2)

trait Snippet5 
    def pure[A](x: A): Writer[A] = (x, "")

trait Snippet6    
    val upCase: String => Writer[String] =
        s => (s.toUpperCase, "upCase ")

    val toWords: String => Writer[List[String]] =
        s => (s.split(' ').toList, "toWords ")

trait Snippet7 extends Snippet4 with Snippet6
    val process: String => Writer[List[String]] = 
        upCase >=> toWords
