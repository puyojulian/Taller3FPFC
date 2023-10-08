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

  def aplicarMovimiento(e: Estado, m: Movimiento): Estado = {
    def moverUno(n: Int, estado: Estado): Estado = {
      if (n > 0)
        if (n <= estado._1.length)
          moverUno(n - 1, (estado._1.init, estado._1.last :: estado._2, estado._3))
        else
          moverUno(estado._1.length - 1, (estado._1.init, estado._1.last :: estado._2, estado._3))
      else if (n < 0)
        if (n.abs <= estado._2.length)
          moverUno(n + 1, (estado._1 ++ List(estado._2.head), estado._2.tail, estado._3))
        else
          moverUno(estado._2.length - 1, (estado._1 ++ List(estado._2.head), estado._2.tail, estado._3))
      else
        estado
    }

    def moverDos(n: Int, estado: Estado): Estado = {
      if (n > 0)
        if (n <= estado._1.length)
          moverDos(n - 1, (estado._1.init, estado._2, estado._1.last :: estado._3))
        else
          moverDos(estado._1.length - 1, (estado._1.init, estado._2, estado._1.last :: estado._3))
      else if (n < 0)
        if (n.abs <= estado._3.length)
          moverDos(n + 1, (estado._1 ++ List(estado._3.head), estado._2, estado._3.tail))
        else
          moverDos(estado._3.length - 1, (estado._1 ++ List(estado._3.head), estado._2, estado._3.tail))
      else estado
    }

    m match {
      case Uno(n) => moverUno(n, e)
      case Dos(n) => moverDos(n, e)
    }
  }

  def aplicarMovimiento1(e: Estado, m: Movimiento): Estado = m match {
    case Uno(n) => if (n > 0) {
      (e._1 take (e._1.length - n), e._2 ++ e._1 drop (e._1.length - n), e._3)
    } else if (n < 0) {
      (e._1 ++ e._2 take n.abs, e._2 drop n.abs, e._3)
    } else
      (e._1, e._2, e._3)
    case Dos(n) => if (n > 0) {
      (e._1 take (e._1.length - n), e._2, e._3 ++ e._1 drop (e._1.length - n))
    } else if (n < 0) {
      (e._1 ++ e._3 take n.abs, e._2, e._3 drop n.abs)
    } else {
      (e._1, e._2, e._3)
    }
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
  List(Uno(0), Dos(0))
}


}