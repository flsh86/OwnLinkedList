abstract class MyList[+A] {
  def head(): A
  def tail(): MyList[A]
  def isEmpty(): Boolean
  def add[B >: A](element: B): MyList[B]
  def putToString(): String
  def map[B](transformer: A => B): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def ++[B >: A](list: MyList[B]): MyList[B]
}

object EmptyList extends MyList {
  override def head(): Nothing = throw new NoSuchElementException
  override def tail(): Nothing = throw new NoSuchElementException
  override def isEmpty(): Boolean = true
  override def add[A >: Nothing](element: A): MyList[A] = Cons(element, EmptyList)
  override def putToString(): String = ""
  override def map[A](element: Nothing => A): MyList[A] = EmptyList
  override def filter(predicate: Nothing => Boolean): MyList[Nothing] = EmptyList
  override def ++[A >: Nothing](list: MyList[A]): MyList[A] = EmptyList
  override def flatMap[A](transformer: Nothing => MyList[A]): MyList[A] = EmptyList
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  override def head(): A = h
  override def tail(): MyList[A] = t
  override def isEmpty(): Boolean = false
  override def add[B >: A](element: B): MyList[B] = Cons(element, this)
  override def putToString(): String =
    if(t.isEmpty()) h.toString
    else h.toString + " " + tail().putToString()

  override def map[B](transformer: A => B): MyList[B] = Cons(transformer(h) , t.map(transformer))
  override def filter(predicate: A => Boolean): MyList[A] =
    if(predicate(h)) Cons(h, t.filter(predicate))
    else t.filter(predicate)

//  override def ++[B >: A](list: MyList[B]): MyList[B] = Cons(h, t.++(list))
  override def ++[B >: A](list: MyList[B]): MyList[B] = Cons(h, t ++ list)

  override def flatMap[B](transformer: A => MyList[B]): MyList[B] = transformer(h) ++ t.flatMap(transformer)
}

object Test extends App {
  val listOfInts: MyList[Int] = Cons(1, Cons(2, Cons(3, EmptyList)))
  print("List of Ints: ")
  println(listOfInts.putToString())

  print("List of DoubleInts: ")
  val listOfDoubleInts: MyList[Int] = listOfInts.map(_ * 2)
  println(listOfDoubleInts.putToString())

  print("IDK: ")
  println(listOfInts.flatMap(elem => Cons(elem, Cons(elem + 1, EmptyList))).putToString())
  //  val a: MyList[Int] = Cons(2, Cons(1, EmptyList))
//  println(a.putToString())
//
//  val b: MyList[Int] = a.map()
//  println(b.putToString())
//  val doubler: Function1[Int, Int] = new Function[Int, Int] {
//    override def apply(v1: Int): Int = v1 * 2
//  }
//
//  val betterDoubler: Int => Int = x => x + 2
//
//  val evenBetterDoubler: Int => Int = _ + 2
//
//  val b: MyList[Int] = a.map(evenBetterDoubler)
//
//  val c: MyList[Int] = a.map(_ + 2)
}