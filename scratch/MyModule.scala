object MyModule {
  def abs(n :Int) : Int =
    if (n < 0) - n
    else n

  def factorial(n: Int) : Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  // not tail position
  def fibNoTail(n: Int) : Int = {
    def go(n: Int): Int =
      if (n <= 2) 1
      else go(n-1) + go(n-2)
    go(n)
  }


  def fib(n: Int) : Int = {
    def go(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else go(n - 1, cur, prev + cur)
    go(n, 0, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }
  private def formatAbs(x: Int) = {
    val msg = "The abs value of %d is %d"
    msg.format(x, abs(x))
  }


  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    loop(0)
  }

  def isOrdered(a: Int, b: Int): Boolean = a <= b

  def curry[A, B, C](f:(A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]) : Unit =
    println(formatAbs(-42))
}
