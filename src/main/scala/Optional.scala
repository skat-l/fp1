package com.tkroman.kpi.y2022.l1

sealed trait Nat
case object Zero extends Nat
case class Succ(n: Nat) extends Nat

//цієї функції не було в списку завдань але вона нам необхідна для розрахунку множення
def add(a: Nat, b: Nat): Nat =
  a match {
    case Zero => b
    case Succ(n) => add(n, Succ(b))
  }

//a * b
def mul(a: Nat, b: Nat): Nat =
  a match{
    case Zero => Zero
    case Succ(n) => add(b, mul(n, b))
  }

//a^b
def pow(a: Nat, b: Nat): Nat =
  b match{
    case Zero => Succ(Zero)
    case Succ(n) => mul(a, pow(a, n))
  }

def toInt(nat: Nat): Int =
  nat match{
    case Zero => 0
    case Succ(nat) => 1 + toInt(nat)
  }

// В завданні було прописано fromInt(n: Nat): Int
// Але мені здається що тут виникла описка і тут має бути fromInt(n: Int): Nat
// Якщо ж, там має бути так як зазначено у самий перший раз будь ласка розпишіть детальніше що має робити ця функція
def fromInt(n: Int): Nat = {
  var i = 0
  var res: Nat = Zero

  for (i <- 1 to n) {
    res = Succ(res)
  }

  res
}

//(a^b) mod n
def modPow(a: Nat, b: Nat, n: Nat): Nat = {
  n match{
    case Zero => Zero
    case Succ(n) => fromInt( toInt(pow(a,b)) % (toInt(n) + 1) )
  }
}


@main def run() =

  println(
      toInt( Succ(Succ(Succ(Succ(Zero)))) )
  )

  println(
    toInt(fromInt(3))
  )

  println(
    toInt(add(fromInt(2), fromInt(3)))
  )

  println(
    toInt(mul(fromInt(2), fromInt(3)))
  )

  println(
    toInt(pow(fromInt(2), fromInt(5)))
  )

  println(
    toInt(modPow(fromInt(3), fromInt(4), fromInt(7)))
  )


