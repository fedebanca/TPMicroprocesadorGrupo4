module MicroEntrega1 where

import Text.Show.Functions

--  EJERCICIO 3.1  --

type Acumulador = Int
type ContadorDePrograma = Int
type Memoria = [Int]
type MensajeDeError = String

data Microprocesador = Microprocesador {a::Acumulador, b::Acumulador, pC::ContadorDePrograma, datos::Memoria, ultimoError::MensajeDeError} deriving (Show)

--   El criterio utilizado para modelar el tipo de dato microprocesador fue el de un constructor, 
-- ya que un microprocesador consta de varios elementos organizados y estructurados. Estructura 
-- que hay que respetar para el correcto uso del microprocesador.
--   Para la memoria se uso una lista de enteros ya que era muy tedioso y poco eficiente el uso de una tupla, 
-- tanto para la declaracion de las 1024 posicines, como para el manejo de todas las posiciones, 
-- teniendo que usar 1024 accesos distintos.

xt8088 = Microprocesador 0 0 0 memoriaVacia ""

fp20 = Microprocesador 7 24 0 memoriaVacia ""

at8086 = Microprocesador 0 0 0 memoriaDel1al20 ""

memoriaVacia :: Memoria
memoriaVacia = replicate 1024 0

memoriaDel1al20 :: Memoria
memoriaDel1al20 = [1..20] ++ replicate 1004 0

--  EJERCICIO 3.2  --

nop :: Microprocesador -> Microprocesador
nop = incrementarPC

incrementarPC :: Microprocesador -> Microprocesador
incrementarPC microprocesador = microprocesador {pC= pC microprocesador + 1}

--  EJERCICIO 3.2.2  --
--  tresPosicionesPC = nop.nop.nop

--  para lograr este punto interviene el concepto de composición  --

--  EJERCICIO 3.3  --

lodv :: Int->Microprocesador->Microprocesador
lodv valor = nop.cargarAcumuladorConValor valor

cargarAcumuladorConValor :: Int->Microprocesador->Microprocesador
cargarAcumuladorConValor valor microprocesador = microprocesador {a=valor}

swap :: Microprocesador->Microprocesador
swap = nop.intercambiarAyB

intercambiarAyB :: Microprocesador->Microprocesador
intercambiarAyB microprocesador = microprocesador {b=a microprocesador, a=b microprocesador}

add :: Microprocesador->Microprocesador
add = nop.sumarAyB

sumarAyB :: Microprocesador->Microprocesador
sumarAyB microprocesador = microprocesador {a= suma microprocesador, b=0}

suma :: Microprocesador->Int
suma microprocesador = a microprocesador + b microprocesador

--  EJERCICIO 3.3.2 --
--  sumarNconM n m microprocesador = add.(lodv m).swap.(lodv n)
--  sumar10con22 = add.(lodv 22).swap.(lodv 10) 


--  EJERCICIO 3.4  --

divide :: Microprocesador->Microprocesador
divide = nop.dividirAporB

dividirAporB :: Microprocesador->Microprocesador
dividirAporB microprocesador 
  | b microprocesador /= 0 = correctaDivision microprocesador 
  | otherwise = errorEnLaDivision microprocesador

correctaDivision :: Microprocesador->Microprocesador
correctaDivision microprocesador = microprocesador {a = div (a microprocesador) (b microprocesador), b = 0}

errorEnLaDivision :: Microprocesador->Microprocesador
errorEnLaDivision microprocesador = microprocesador { ultimoError = "DIVISION BY ZERO"}

str :: Int->Int->Microprocesador->Microprocesador
str posicion valor= nop.(guardarEnMemoria posicion) valor
--str = nop.guardarEnMemoria

guardarEnMemoria :: Int->Int->Microprocesador->Microprocesador
guardarEnMemoria posicion valor microprocesador = microprocesador { datos = modificarPosicionN posicion valor microprocesador}

modificarPosicionN :: Int->Int->Microprocesador->[Int]
modificarPosicionN posicion valor microprocesador = (take (posicion - 1) (datos microprocesador)) ++ [valor] ++ (drop posicion (datos microprocesador))

lod :: Int->Microprocesador->Microprocesador
lod posicion = nop.cargarAcumuladorConMemoria posicion

cargarAcumuladorConMemoria :: Int->Microprocesador->Microprocesador
cargarAcumuladorConMemoria posicion microprocesador = microprocesador { a =  elementoNdeMemoria posicion microprocesador}

elementoNdeMemoria :: Int->Microprocesador->Int
elementoNdeMemoria posicion microprocesador = (datos microprocesador) !!(posicion-1)

-- EJERCICIO 3.4.2  --
-- dividir2por0 = divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)
-- dividirNporM n m = divide.(lod 1).swap.(lod 2).(str 2 m).(str 1 n)

--  EJERCICIO 5.1  --

programCounter :: Microprocesador->Int
programCounter = mostrarPC

acumuladorA :: Microprocesador->Int
acumuladorA = mostrarA

acumuladorB :: Microprocesador->Int
acumuladorB = mostrarB

memoria :: Microprocesador->[Int]
memoria = mostrarMemoria

mensajeError :: Microprocesador->String
mensajeError = mostrarError

mostrarA :: Microprocesador->Int
mostrarA microprocesador = a microprocesador

mostrarB :: Microprocesador->Int
mostrarB microprocesador = b microprocesador

mostrarPC :: Microprocesador->Int
mostrarPC microprocesador = pC microprocesador

mostrarMemoria :: Microprocesador->[Int]
mostrarMemoria microprocesador = datos microprocesador

mostrarError :: Microprocesador->String
mostrarError microprocesador = ultimoError microprocesador