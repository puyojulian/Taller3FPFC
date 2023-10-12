/*
Autores:  Julian Ernesto Puyo Mora <julian.puyo@correounivalle.edu.co> <202226905>
          Sebastian Orrego Marin <orrego.sebastian@correounivalle.edu.co> <201941144>
N° Taller: 03
Fecha de realización: 12/10/2023
*/

package object ManiobrasTrenes {
  type Vagon = Any
  type Tren = List[Vagon]
  type Estado = (Tren, Tren, Tren)
  trait Movimiento
  case class Uno(n: Int) extends Movimiento
  case class Dos(n: Int) extends Movimiento

  type Maniobra = List[Movimiento]

  def aplicarMovimiento(e: Estado, m: Movimiento): Estado = m match {
    case Uno(n) =>
      if (n > 0) {
        if (e._1.nonEmpty)
          (e._1 take (e._1.length - n), (e._1 drop (e._1.length - n)) ++ e._2, e._3)
        else
          (e._1, e._2, e._3)
      } else if (n < 0)
        if (n.abs==e._2.length)
          if (e._2.nonEmpty)
            (e._1 ++ (e._2 take (n.abs+1)), e._2 drop n.abs, e._3)
          else
            (e._1, e._2, e._3)
        else
          if (e._2.nonEmpty)
            ( e._1 ++ (e._2 take (n.abs)), e._2 drop n.abs, e._3)
          else
            (e._1, e._2, e._3)
      else
        (e._1, e._2, e._3)
    case Dos(n) =>
      if (n > 0)
        if (e._1.nonEmpty)
          (e._1 take (e._1.length - n), e._2, (e._1 drop (e._1.length - n)) ++ e._3)
        else
          (e._1, e._2, e._3)
      else if (n < 0)
        if (n.abs==e._3.length)
          if (e._3.nonEmpty)
            (e._1++ (e._3 take (n.abs+1)), e._2, e._3 drop n.abs)
          else
            (e._1, e._2, e._3)
        else
          if (e._3.nonEmpty)
            (e._1 ++ (e._3 take (n.abs)), e._2, e._3 drop n.abs)
          else
            (e._1, e._2, e._3)
      else
        (e._1, e._2, e._3)
  }

  def aplicarMovimientos(e: Estado, movs: Maniobra): List[Estado] = {
    def listaDeEstados(estado: Estado, maniobra: Maniobra): List[Estado] = {
      maniobra match {
        case x :: xs => aplicarMovimiento(estado, x) :: listaDeEstados(aplicarMovimiento(estado, x), xs)
        case Nil => Nil
      }
    }
    e::listaDeEstados(e,movs)
  }

  def definirManiobra(t1: Tren, t2: Tren): Maniobra = {

    def listaDeMovimientos(e: Estado, v: Vagon): List[Movimiento] = {
      t2.indexOf(v) == e._1.indexOf(v) match {
        case true => e._3.nonEmpty match {
          case true => Dos(-e._3.length) :: listaDeMovimientos(aplicarMovimiento(e, Dos(-e._3.length)), v)
          case false => Nil
        }
        case false => {
          e._1 contains v match {
            case true => e._1.indexOf(v) == e._1.indexOf(e._1.last) match {
              case true => Uno(1) :: listaDeMovimientos(aplicarMovimiento(e, Uno(1)), v)
              case false => {
                val cuantos = e._1.length - (e._1.indexOf(v) + 1)
                Dos(cuantos) :: listaDeMovimientos(aplicarMovimiento(e, Dos(cuantos)), v)
              }
            }
            case false => (e._2 contains v) match {
              case true => e._1.length == t2.indexOf(v) match {
                case true => Uno(-1) :: listaDeMovimientos(aplicarMovimiento(e, Uno(-1)), v)
                case false => {
                  val cuantos = e._1.length - t2.indexOf(v)
                  Dos(cuantos) :: listaDeMovimientos(aplicarMovimiento(e, Dos(cuantos)), v)
                }
              }
            }
          }
        }
      }
    }

    val estado = (t1, Nil, Nil)

    def listarMovimientos(tren: Tren, e: Estado): Maniobra = {
      tren match {
        case x :: xs => {
          val listaMovimientos = listaDeMovimientos(e, x: Vagon)
          listaMovimientos ++ listarMovimientos(xs, aplicarMovimientos(e, listaMovimientos).last)
        }
        case Nil => Nil
      }
    }

    listarMovimientos(t2, estado)
  }
}