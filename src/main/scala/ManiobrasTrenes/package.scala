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
    new Estado(Nil,Nil,Nil)
  }

  def aplicarMovimientos(e: Estado, movs: Maniobra): List[Estado] = {
    List(new Estado(Nil,Nil,Nil))
  }

  def definirManiobra(t1: Tren, t2: Tren): Maniobra = {
    List(Uno(0), Dos(0))
  }
}