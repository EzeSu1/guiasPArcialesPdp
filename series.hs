import Text.Show.Functions ()

-- enunciado= https://www.utnianos.com.ar/foro/attachment.php?aid=20205

data Serie = UnSerie
  { nombre :: String,
    actores :: [Actor],
    presupuestoAnual :: Float,
    cantidadTemporadas :: Int,
    rating :: Float,
    cancelada :: Bool
  }
  deriving (Show)

data Actor = UnActor
  { nombreActor :: String,
    sueldoAnual :: Float,
    restriccion :: [String]
  }
  deriving (Show)

paulrudd :: Actor
paulrudd = UnActor "Paul Rudd" 41000000 ["no actuar en bata", " comer ensalada de rúcula todos los días"]

{-a. Saber si la serie está en rojo, esto es si el presupuesto no alcanza a cubrir lo
que quieren cobrar todos los actores.
b. Saber si una serie es problemática, esto ocurre si tienen más de 3 actores o
actrices con más de 1 restricción-}
-- 1a

estaRojo :: Serie -> Bool
estaRojo serie = alcanzaPresupuesto (presupuestoAnual serie) (presupuestoActores (actores serie))

presupuestoActores :: [Actor] -> Float
presupuestoActores actores = sum . map sueldoAnual $ actores

alcanzaPresupuesto :: Float -> Float -> Bool
alcanzaPresupuesto presupuesto sueldoActores = presupuesto > sueldoActores

esProblematica :: Serie -> Bool
esProblematica serie = (> 3) . tieneMasdeXRestriccion 1 $ (actores serie)

tieneMasdeXRestriccion :: Int -> [Actor] -> Int
tieneMasdeXRestriccion cantidad actores = length . filter ((> cantidad) . length) . map restriccion $ actores

-- 2a
type Productor = Serie -> Serie

favoritismos :: [Actor] -> Productor
favoritismos actoresFavoritos serie = agregarActores actoresFavoritos . quitarActores 2 $ serie

quitarActores :: Int -> Serie -> Serie
quitarActores cantidad serie = serie {actores = drop cantidad (actores serie)}

agregarActores :: [Actor] -> Serie -> Serie
agregarActores actoresNuevos serie = serie {actores = actoresNuevos ++ actores serie}

-- 2b
timburton :: Productor
timburton serie = agregarActores [johnnydepp, helenabonham] . quitarActores 2 $ serie

johnnydepp :: Actor
johnnydepp = UnActor "Johnny depp" 20000000 []

helenabonham :: Actor
helenabonham = UnActor "Helena Bonham" 15000000 []

-- 2c
gatopardeitor :: Productor
gatopardeitor serie = serie

estireitor :: Productor
estireitor serie = modificarTemporadas (2 * cantidadTemporadas serie) serie

modificarTemporadas :: Int -> Serie -> Serie
modificarTemporadas nuevaCantidad serie = serie {cantidadTemporadas = nuevaCantidad}

desespereitor :: Productor -> Productor -> Productor
desespereitor productor1 productor2 serie = productor1 . productor2 $ serie

canceleitor :: Float -> Productor
canceleitor cifra serie
  | bajoelRating cifra serie && estaRojo serie = seCancela serie
  | otherwise = serie

bajoelRating :: Float -> Serie -> Bool
bajoelRating cifra serie = rating serie < cifra

seCancela :: Serie -> Serie
seCancela serie = serie {cancelada = True}

{-3. Calcular el bienestar de una serie, en base a la sumatoria de estos conceptos:
- Si la serie tiene estimadas más de 4 temporadas, su bienestar es 5, de lo contrario
es (10 - cantidad de temporadas estimadas) * 2
- Si la serie tiene menos de 10 actores, su bienestar es 3, de lo contrario es (10 -
cantidad de actores que tienen restricciones), con un mínimo de 2
Aparte de lo mencionado arriba, si la serie está cancelada, su bienestar es 0 más
allá de cómo diesen el bienestar por longitud y por reparto.
-}
bienestarTemporadas :: Serie -> Int
bienestarTemporadas serie
  | cantidadTemporadas serie > 4 = 5
  | otherwise = (10 - cantidadTemporadas serie) * 2

bienestarActores :: Serie -> Int
bienestarActores serie
  | length (actores serie) > 10 = 5
  | otherwise = (10 - tieneMasdeXRestriccion 2 (actores serie)) * 2

bienestarTotal :: Serie -> Int
bienestarTotal serie = bienestarTemporadas serie + bienestarActores serie

bienestarDeUnaSerie :: Serie -> Int
bienestarDeUnaSerie serie
  | cancelada serie = 0
  | otherwise = bienestarTotal serie

-- 4
aplicarProductores :: [Serie] -> [Productor] -> [Serie]
aplicarProductores series productores = map (elMasEfectivo productores) series

elMasEfectivo :: [Productor] -> Serie -> Serie
elMasEfectivo [productor] serie = productor serie
elMasEfectivo (productor1 : productor2 : productores) serie
  | (bienestarDeUnaSerie . productor1 $ serie) > (bienestarDeUnaSerie . productor2 $ serie) = elMasEfectivo (productor1 : productores) serie
  | otherwise = elMasEfectivo (productor2 : productores) serie
elMasEfectivo [] serie = serie

nada :: Productor
nada = id

actoresInfinitos :: [Actor]
actoresInfinitos = cycle [johnnydepp, helenabonham]

serieA :: Serie
serieA = UnSerie "bla" actoresInfinitos 1231 13 31 False

---- 6  Saber si una serie es controvertida,
-- que es cuando no se cumple que cada actor de la lista cobra más que el siguiente.

serieControVertida :: Serie -> Bool
serieControVertida serie = cobraMasQueAnterior (actores serie)

cobraMasQueAnterior :: [Actor] -> Bool
cobraMasQueAnterior [] = True
cobraMasQueAnterior [actor] = True
cobraMasQueAnterior (actor1 : actor2 : actores) = sueldoAnual actor1 > sueldoAnual actor2 && cobraMasQueAnterior (actor2 : actores)