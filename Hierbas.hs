module Hierbas where

import Text.Show.Functions
import Data.List

type Nombre = String
type Edad = Float
type Peso = Float
type Enfermedades = [String]

data Raton = UnRaton {
    nombre :: Nombre,
    edad :: Edad,
    peso :: Peso,
    enfermedades :: Enfermedades
    } deriving (Show)

--modelado de ratones

cerebro :: Raton
cerebro = UnRaton {
    nombre = "cerebro",
    edad = 9,
    peso = 0.2,
    enfermedades = ["brucelosis","sarampión","tuberculosis"]
}

bicenterrata  :: Raton
bicenterrata  = UnRaton {
    nombre = "bicenterrata ",
    edad = 256,
    peso = 0.2,
    enfermedades = []
}

huesudo  :: Raton
huesudo  = UnRaton {
    nombre = "huesudo ",
    edad = 4,
    peso = 10,
    enfermedades = ["altaObesidad","sinusitis"]
}

--modelado de hierbas

type Hierba = (Raton -> Raton)

hierbaBuena :: Hierba
hierbaBuena raton = rejuvenecer (sqrt (edad raton)) raton

rejuvenecer :: Edad -> Raton -> Raton
rejuvenecer anios raton = raton {edad = anios}

hierbaVerde :: String -> Hierba
hierbaVerde terminacion raton = raton {enfermedades = filter (terminaCon terminacion) (enfermedades raton)}

terminaCon :: String -> String -> Bool
terminaCon terminacion enfermedad =  

alcachofa :: Hierba
alcachofa raton = perderPeso ((peso raton) * (coeficiente raton)) raton

coeficiente :: Raton -> Float
coeficiente raton | (peso raton) > 2 = 0.1
                  | otherwise = 0.05

perderPeso :: Peso -> Raton -> Raton
perderPeso pesoAPerder raton | (peso raton) > pesoAPerder = raton {peso = (peso raton) - pesoAPerder}
                             | otherwise = raton {peso = 0}

hierbaZort :: Hierba
hierbaZort = (cambiarNombreA "pinky").(rejuvenecer 0).perderEnfermedades

perderEnfermedades :: Raton -> Raton
perderEnfermedades raton = raton {enfermedades = []}

cambiarNombreA :: Nombre -> Raton -> Raton
cambiarNombreA nuevoNombre raton = raton {nombre = nuevoNombre}

hierbaDelDiablo :: Hierba
hierbaDelDiablo = (perderPeso 0.1).(eliminarEnfermedades 10)

eliminarEnfermedades :: Int -> Raton -> Raton
eliminarEnfermedades cantidadDeLetrasMax raton = raton {enfermedades = (filter ((<=10).length) (enfermedades raton))}

--medicamentos
type Medicamento = Raton -> Raton

pondsAntiAge :: Medicamento
pondsAntiAge = hierbaBuena.hierbaBuena.hierbaBuena.alcachofa

--reduceFatFast :: Int -> Hierba -> Medicamento
--reduceFatFast 0 hierbaVerde = hierbaVerde
--reduceFatFast potencia hierbaVerde = reduceFatFast (potencia-1) ((.)alcachofa)

--pdepCilina :: [String] -> 
--pdepCilina sufijosInfecciosas =
--sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

-- 4 experimentos 
--a
cantidadIdeal :: (Num a, Enum a) => (a -> Bool) -> a
cantidadIdeal condicion = head (filter condicion [1..])
--b
lograEstabilizar :: Medicamento -> [Raton] -> Bool
lograEstabilizar medicamento ratones = ningunoConSobrepeso(medicarRatones) && menosDe3Enfermedades(medicarRatones)

medicarRatones :: Medicamento -> [Raton]
medicarRatones medicamento ratones = foldl1 medicamento ratones

ningunoConSobrepeso :: [Raton] -> Bool
ningunoConSobrepeso ratones = all ((<1).peso) ratones

menosDe3Enfermedades :: [Raton] -> Bool
menosDe3Enfermedades ratones = all ((<3).(length enfermedades))
--c
cantidadIdeal (lograEstabilizar (reduceFatFast potencia hierbaVerde) comunidad) 

--5
{-a
En el caso en que no logre estabilizar a toda la comunidad, si obtendremos respuesta ya que dejará de iterar 
cuando encuentre al primer caso que no cumpla con la condición requerida.
Nunca podremos saber si la condición se cumple para todos ya que nunca dejará de probar a menos que encuentre un caso falso
-}

{-b
Si es verdadero, lo sabremos en cuanto encuentre al primer ratón cumpla con pesar 2kg y tener 4 enfermedades
En cambio si es falso, nunca lo sabremos ya que continuará iterando infinitamente, a menos que encuentre un caso verdadero
-}

--6
{-a
simplemente hay que agregar la hierba, no es necesario cambiar ninguna de las otras funciones, a menos que 
cambie la definición de algún medicamento debido a la nueva hierba
-}

{-b
El concepto que está involucrado en la pregunta anterior es 
tengo 3 capas de funciones, las principales
-}

{-c
Si se quiere poner el peso en libras hay que cambiar todas las funciones que hagan cálculos o comparaciones
con el peso ya que están puestas en kg
Ej: si un ratón pesa más de 2kg, hay que transformar el 2 a libras.
-}