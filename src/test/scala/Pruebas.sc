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

val e2= aplicarMovimiento1 (e1,Uno(2))
val e3 = aplicarMovimiento1 (e2,Dos(3))
val e4 = aplicarMovimiento1 (e3, Dos(-1))
val e5 = aplicarMovimiento1 (e4,Uno(-2))
val e6 = aplicarMovimiento1 ((List('a'),List('c'),List('b','d')),Dos(-2))
val e6 = aplicarMovimiento1 ((List('a'),List('c', 'd'),List('b')),Uno(-2))

aplicarMovimientos(e1, List(Uno(2), Dos(3), Dos(-1), Uno(-2), Dos(-1)))
val e = (List('a', 'b'), List('c'), List('d'))
aplicarMovimientos(e, List(Uno(1), Dos(1), Uno(-2)))
//definirManiobra(List('a', 'b', 'c', 'd') , List('d', 'b', 'c', 'a'))


