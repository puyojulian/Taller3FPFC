/*
Autores:  Julian Ernesto Puyo Mora <julian.puyo@correounivalle.edu.co> <202226905>
          Sebastian Orrego Marin <orrego.sebastian@correounivalle.edu.co> <201941144>
N° Taller: 03
Fecha de realización: 12/10/2023
*/

import ManiobrasTrenes._

val e1 = (List('a','b', 'c', 'd'), Nil, Nil)
val e2= aplicarMovimiento (e1,Uno(2))
val e3 = aplicarMovimiento (e2,Dos(3))
val e4 = aplicarMovimiento (e3, Dos(-1))
val e5 = aplicarMovimiento (e4,Uno(-2))
val e6 = aplicarMovimiento (e5,Dos(-1))

val e = (List('a','b'),List('c'), List('d'))
aplicarMovimientos(e, List(Uno(1), Dos(1), Uno(-2)))

val m1 = (List(1, 2, 3, 4, 5), Nil, Nil)
aplicarMovimientos(m1, List(Uno(2), Dos(3), Dos(-1), Uno(-2), Dos(-1)))
aplicarMovimientos(m1, List(Uno(5), Uno(-1), Dos(0), Dos(4), Uno(3)))
aplicarMovimientos(m1, List(Dos(3), Dos(-2), Uno(1), Uno(4), Dos(2)))
aplicarMovimientos(m1, List(Dos(0), Uno(6), Dos(2), Dos(-1), Uno(-1)))

val e = (List('a', 'b'), List('c'), List('d'))
aplicarMovimientos(e, List(Uno(1), Dos(1), Uno(-2)))

definirManiobra(List('a', 'b', 'c', 'd') , List('d', 'b', 'c', 'a'))
aplicarMovimientos((List('a', 'b', 'c', 'd'),Nil,Nil),definirManiobra(List('a', 'b', 'c', 'd') , List('d', 'b', 'c', 'a')))

definirManiobra(List(1, 2, 3, 4, 5) , List(5, 4, 3, 2, 1))
aplicarMovimientos((List(1, 2, 3, 4, 5),Nil,Nil),definirManiobra(List(1, 2, 3, 4, 5) , List(5, 4, 3, 2, 1)))