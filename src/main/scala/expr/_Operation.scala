package it.grypho.scala.leonardo
package expr

import parser.Environment

abstract class _Operation(a: _Expression, b: _Expression)(implicit env: Environment) extends _Expression


case class Sum(a: _Expression, b: _Expression)(implicit env: Environment) extends _Operation(a, b)
{

  override def toString: String = f"($a + $b)"

  override def eval(): Either[_Expression, Double] =
  {
    val a_val = a.eval()
    val b_val = b.eval()


    a_val match
    {
      case Left(x)  => b_val match
      {
        case Left(y)  => Left(Sum(x, y))
        case Right(y) => Left(Sum(x, _Number(y)))
      }
      case Right(x) => b_val match
      {
        case Left(y)  => Left(Sum(_Number(x), y))
        case Right(y) => _Number(x + y).eval()
      }

    }
  }
}

//no need for subtraction, it's just the sum with the opposite
//case class Difference(a: Expr, b: expr) extends _Operation(a, b)
case class Product(a: _Expression, b: _Expression)(implicit env: Environment) extends _Operation(a, b)
{

  override def toString: String = f"($a * $b)"

  override def eval(): Either[_Expression, Double] =
  {
    val a_val = a.eval()
    val b_val = b.eval()

    a_val match
    {
      case Left(x)  => b_val match
      {
        case Left(y)  => Left(Product(x, y))
        case Right(y) => Left(Product(x, _Number(y)))
      }
      case Right(x) => b_val match
      {
        case Left(y)  => Left(Product(_Number(x), y))
        case Right(y) => _Number(x * y).eval()
      }

    }

  }
}

case class Ratio(a: _Expression, b: _Expression)(implicit env: Environment) extends _Operation(a, b)
{

  override def toString: String = f"($a / $b)"

  override def eval(): Either[_Expression, Double] =
  {
    val a_val = a.eval()
    val b_val = b.eval()

    a_val match
    {
      case Left(x)  => b_val match
      {
        case Left(y)  => Left(Ratio(x, y))
        case Right(y) => Left(Ratio(x, _Number(y)))
      }
      case Right(x) => b_val match
      {
        case Left(y)  => Left(Ratio(_Number(x), y))
        case Right(y) => _Number(x / y).eval()
      }

    }
  }
}
