package scala3_2

import scala.annotation.targetName

object homework1 {
  extension (x: String)
    def ++(y: String): Int = StringBuilder(x).append(y).toString().toInt

    @main def part1Ex(): Unit ={
      println("1" ++ "33")
    }
}

object homework2 {
  object Completions {
    enum CompletionArg {
      case StringValue(s: String)
      case IntValue(i: Int)
      case FloatValue(f: Float)
      //см приведенную ссылку
    }

    object CompletionArg {
      given fromString: Conversion[String, CompletionArg] = StringValue(_)

      given fromInt: Conversion[Int, CompletionArg] = IntValue(_)

      given fromFloat: Conversion[Float, CompletionArg] = FloatValue(_)
    }
    import CompletionArg.*
    def complete[T](arg: CompletionArg) = arg match
      case StringValue(s) => s
      case IntValue(i) => i.toString
      case FloatValue(f) => f.toString
  }


  @main def part2Ex(): Unit ={
    println(Completions.complete("String"))
    println(Completions.complete(1))
    println(Completions.complete(7f))
  }
}


object homework3 {

  object MyMath {
    opaque type Logarithm = Double

    object Logarithm {
      //см приведенную ссылку
      def apply(d: Double): Logarithm = math.log(d)

      def safe(d: Double): Option[Logarithm] =
        if d > 0.0 then Some(math.log(d)) else None

    }

    extension (x: Logarithm)
      def toDouble: Double = math.exp(x)
      def +(y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
      def *(y: Logarithm): Logarithm = x + y
  }


  @main def part3Ex(): Unit ={
    import MyMath.Logarithm

    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2

  }
}