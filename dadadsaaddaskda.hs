import Text.Show.Functions

-- Dominio
type Atraccion = String

data Ciudad = Ciudad
  { nombre :: String,
    anioFundacion :: Int,
    atracciones :: [Atraccion],
    costoDeVida :: Float
  }
  deriving (Show, Eq, Ord)

-- Ciudades de prueba
baradero :: Ciudad
baradero = Ciudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 150

nullish :: Ciudad
nullish = Ciudad "Nullish" 1800 [] 140

caletaolivia :: Ciudad
caletaolivia = Ciudad "Caleta Olivia" 1901 ["El Gorosito", "Faro Costanera"] 120

maipu :: Ciudad
maipu = Ciudad "Maipu" 1878 ["Fortin kakel"] 115

azul :: Ciudad
azul = Ciudad "Azul" 1832 ["Teatro Espanol", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"] 190

-- Punto 1: Valor de una ciudad
-- Valor de una ciudad
valorCiudad :: Ciudad -> Float
valorCiudad unaCiudad
  | (< 1800) . anioFundacion $ unaCiudad = 5 * fromIntegral (1800 - anioFundacion unaCiudad)
  | null . atracciones $ unaCiudad = (2 *) . costoDeVida $ unaCiudad
  | otherwise = (3 *) . costoDeVida $ unaCiudad

-- Punto 2: Caraterísticas de las ciudades
-- Alguna atracción copada
esVocal :: Char -> Bool
esVocal unaLetra = unaLetra `elem` "aeiouAEIOU"

esAtraccionCopada :: Atraccion -> Bool
esAtraccionCopada = esVocal . head

tieneAtraccionCopada :: Ciudad -> Bool
tieneAtraccionCopada = any esAtraccionCopada . atracciones

-- Ciudad sobria
esSobria :: Int -> Ciudad -> Bool
esSobria cantidadDeLetras = all ((> cantidadDeLetras) . length) . atracciones

-- Ciudad con nombre raro
tieneNombreRaro :: Ciudad -> Bool
tieneNombreRaro = (< 5) . length . nombre

-- Punto 3: Eventos
type Evento = Ciudad -> Ciudad

-- Funciones genéricas para modificar distintos campos del data Ciudad
modificarAtracciones :: [Atraccion] -> Ciudad -> Ciudad
modificarAtracciones nuevasAtracciones unaCiudad = unaCiudad {atracciones = nuevasAtracciones}

modificarCostoDeVida :: Float -> Ciudad -> Ciudad
modificarCostoDeVida nuevoCosto unaCiudad = unaCiudad {costoDeVida = nuevoCosto}

modificarNombre :: String -> Ciudad -> Ciudad
modificarNombre nuevoNombre unaCiudad = unaCiudad {nombre = nuevoNombre}

-- Función para no repetir lógica en cuanto a la suma o resta de porcentajes de costo de vida
modificarPorcentaje :: Float -> Ciudad -> Float
modificarPorcentaje porcentaje unaCiudad = costoDeVida unaCiudad + (costoDeVida unaCiudad * porcentaje / 100)

-- Sumar una nueva atracción
agregarAtraccion :: Atraccion -> Evento
agregarAtraccion nuevaAtraccion unaCiudad = modificarCostoDeVida (modificarPorcentaje 20 unaCiudad) (modificarAtracciones (nuevaAtraccion : atracciones unaCiudad) unaCiudad)

-- Crisis
crisis :: Evento
crisis unaCiudad
  | null . atracciones $ unaCiudad = modificarCostoDeVida (modificarPorcentaje (-10) unaCiudad) unaCiudad
  | otherwise = modificarCostoDeVida (modificarPorcentaje (-10) unaCiudad) (modificarAtracciones (init $ atracciones unaCiudad) unaCiudad)

-- Remodelación
remodelacion :: Float -> Evento
remodelacion porcentaje unaCiudad = modificarCostoDeVida (modificarPorcentaje porcentaje unaCiudad) (modificarNombre ("New " ++ nombre unaCiudad) unaCiudad)

-- Reevaluación
reevaluacion :: Int -> Evento
reevaluacion cantidadDeLetras unaCiudad
  | esSobria cantidadDeLetras unaCiudad = modificarCostoDeVida (modificarPorcentaje 10 unaCiudad) unaCiudad
  | otherwise = modificarCostoDeVida (costoDeVida unaCiudad - 3) unaCiudad

-- Punto 4: La Transformación No Para
-- Para aplicar todos los eventos en este orden en la consola se debe hacer la siguiente invocación
-- ghci> (reevaluacion cantidadDeLetras . remodelacion porcentaje . crisis . agregarAtraccion nuevaAtraccion) ciudad
-- o bien utilizar la siguiente función:
aplicarTodosLosEventos :: Int -> Float -> Atraccion -> Ciudad -> Ciudad
aplicarTodosLosEventos cantidadDeLetras porcentaje nuevaAtraccion = reevaluacion cantidadDeLetras . remodelacion porcentaje . crisis . agregarAtraccion nuevaAtraccion

-- Punto 5: Un año para recordar
-- 5.1 Los años pasan...
data Anio = Anio
  { numero :: Int,
    eventos :: [Evento]
  }
  deriving (Show)

-- Años de prueba
anio2015 :: Anio
anio2015 = Anio 2015 []

anio2021 :: Anio
anio2021 = Anio 2021 [crisis, agregarAtraccion "playa"]

anio2022 :: Anio
anio2022 = Anio 2022 [crisis, remodelacion 5, reevaluacion 7]

anio2023 :: Anio
anio2023 = Anio 2023 [crisis, agregarAtraccion "parque", remodelacion 10, remodelacion 20]

-- Función que aplica los eventos de una lista de eventos a una ciudad
aplicarEventos :: Ciudad -> [Evento] -> Ciudad
aplicarEventos ciudad eventos = foldr ($) ciudad eventos