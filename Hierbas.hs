module Hierbas where

import Text.Show.Functions
import Data.List

type Nombre = String
type Edad = Float
type Peso = Float
type Enfermedades = []

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
    enfermedades = [brucelosis,sarampión,tuberculosis]
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
    enfermedades = [altaObesidad,sinusitis]
}

