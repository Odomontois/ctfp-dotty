package ctfp
package p1.c3

//snippet 1
trait Monoid[M] 
  def combine(x: M, y: M): M
  def empty: M


//snippet 2
given Monoid[String]
    def empty = ""
    def combine(x: String, y: String) = x + y
