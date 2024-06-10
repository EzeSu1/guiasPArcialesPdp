import Text.Show.Functions ()

data Turista = UnTurista
  { cansancio :: Int,
    stress :: Int,
    viajasolo :: Bool,
    idiomas :: [String]
  }
  deriving (Show)

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya turista
  | viajasolo turista = modificarCansansio (-5) turista
  | otherwise = modificarStress (-1) turista

modificarCansansio :: Int -> Turista -> Turista
modificarCansansio variacion turista = turista {cansancio = cansancio turista + variacion}

modificarStress :: Int -> Turista -> Turista
modificarStress variacion turista = turista {stress = stress turista + variacion}

apreciarAlgunElementoPaisaje :: String -> Excursion
apreciarAlgunElementoPaisaje algo unTurista = modificarStress (-length algo) unTurista

salirAHablarIdiomaEspecifico :: String -> Excursion
salirAHablarIdiomaEspecifico idioma personaje = estaacompaniado . agregarIdioma idioma $ personaje

agregarIdioma :: String -> Turista -> Turista
agregarIdioma idioma unTurista = unTurista {idiomas = idioma : idiomas unTurista}

estaacompaniado :: Turista -> Turista
estaacompaniado unTurista = unTurista {viajasolo = False}

caminarCiertosMinutos :: Int -> Excursion
caminarCiertosMinutos cantidad turista = salirAHablarIdiomaEspecifico "aleman" . modificarStress (-div cantidad 4) $ turista

data Marea = Tranquila | Moderada | Fuerte

paseoEnbarco :: Marea -> Excursion
paseoEnbarco Tranquila turista = apreciarAlgunElementoPaisaje "mar" . caminarCiertosMinutos 10 $ turista
paseoEnbarco Moderada turista = turista
paseoEnbarco Fuerte turista = modificarStress 6 . modificarCansansio 10 $ turista

ana :: Turista
ana = UnTurista 0 21 False ["espanol"]

beto :: Turista
beto = UnTurista 15 15 True ["aleman"]

cathi :: Turista
cathi = UnTurista 15 15 True ["aleman", "catalan"]

-- 2

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista = modificarStressPorcentual (-10) . excursion $ turista

modificarStressPorcentual :: Int -> Turista -> Turista
modificarStressPorcentual porciento turista = modificarStress (div (porciento * stress turista) 100) turista

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista

esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = deltaExcursionSegun (length . idiomas) turista excursion > 0

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista excursiones = filter (esDesestresante turista) excursiones

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista excursion = (<= -3) . deltaExcursionSegun stress turista $ excursion

{-

Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria
de la espiritualidad recibida de cada turista a quienes les resultó convincente el tour.
La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour.

Implementar y contestar en modo de comentarios o pruebas por consola
Construir un tour donde se visiten infinitas playas.
¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.
-}

type Tour = [Excursion]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnbarco Tranquila, excursion, caminarCiertosMinutos 120]

tourCompleto :: Tour
tourCompleto = [caminarCiertosMinutos 20, apreciarAlgunElementoPaisaje "cascada", caminarCiertosMinutos 40, irALaPlaya, salirAHablarIdiomaEspecifico "melmacquiano"]

islaVecina :: Marea -> Tour
islaVecina marea = [paseoEnbarco marea, hacerExcursionSegunMarea marea, paseoEnbarco marea]

hacerExcursionSegunMarea :: Marea -> Excursion
hacerExcursionSegunMarea Fuerte = apreciarAlgunElementoPaisaje "lago"
hacerExcursionSegunMarea _ = irALaPlaya

hacerTour :: Tour -> Turista -> Turista
hacerTour tour turista = foldr ($) turista tour

propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente turista tours = any (flip esConvincente turista) tours

esConvincente :: Tour -> Turista -> Bool
esConvincente tour turista = estaacompaniadoDespuesTour turista . excursionesDesestresantes turista $ tour

estaacompaniadoDespuesTour :: Turista -> Tour -> Bool
estaacompaniadoDespuesTour turista tour = not . viajasolo . hacerTour tour $ turista

buscarDesestresantes :: Tour -> Turista -> Bool
buscarDesestresantes tour turista = any (esDesestresante turista) tour

efectividadTour :: Tour -> [Turista] -> Int
efectividadTour tour turistas = sum . map (espiritualidad tour) . filter (esConvincente tour) $ turistas

espiritualidad :: Tour -> Turista -> Int
espiritualidad tour turista = -deltaRutina tour turista

deltaRutina :: Tour -> Turista -> Int
deltaRutina tour turista = deltaSegun stress (hacerTour tour turista) turista + deltaSegun cansancio (hacerTour tour turista) turista

-- tourInfinitas :: Tour
-- tourInfinitas = cycle [irALaPlaya]
playasEternas :: Tour
playasEternas = salidaLocal : repeat irALaPlaya

salidaLocal :: Excursion
salidaLocal = salirAHablarIdiomaEspecifico "melmacquiano"