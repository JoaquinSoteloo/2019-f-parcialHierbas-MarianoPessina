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

--hierbaVerde :: String -> Hierba
--hierbaVerde terminacion raton = (terminaCon terminacion raton)



alcachofa :: Hierba
alcachofa raton = perderPeso ((peso raton) * (coeficiente raton)) raton

coeficiente :: Raton -> Float
coeficiente raton | peso raton > 2 = 0.2
                  | otherwise = 0.1

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


