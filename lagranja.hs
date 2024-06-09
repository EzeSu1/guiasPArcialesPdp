import Text.Show.Functions ()

-- consignas https://www.utnianos.com.ar/foro/attachment.php?aid=23797
data Animal = Animal
  { nombre :: String,
    tipoAnimal :: String,
    peso :: Float,
    edad :: Int,
    enfermo :: Bool,
    visitasMedicas :: [VisitaMedica]
  }
  deriving (Eq, Show)

data VisitaMedica = VisitaMedica
  { diasdeRecuperacion :: Int,
    costo :: Float
  }
  deriving (Eq, Show)

-- 1
laPasoMal :: Animal -> Bool
laPasoMal animal = any mayorA30Dias (visitasMedicas animal)

mayorA30Dias :: VisitaMedica -> Bool
mayorA30Dias visita = diasdeRecuperacion visita > 30

tieneNombreFalopa :: Animal -> Bool
tieneNombreFalopa animal = (== "i") . agarrarLaUltimaLetra . nombre $ animal

agarrarLaUltimaLetra :: String -> String
agarrarLaUltimaLetra nombre = take 1 . reverse $ nombre

-- gachi
pachi :: Animal
pachi = Animal "pachi" "Sagitario" 50 50 False []

dorothy :: Animal
dorothy = Animal "drothy" "Sagitario" 50 50 False []

-- 2
type Actividad = Animal -> Animal

engordar :: Float -> Actividad
engordar kilosComida animal
  | kilosComida > 10 = modificarPeso 5 animal
  | otherwise = modificarPeso (kilosComida / 2) animal

modificarPeso :: Float -> Animal -> Animal
modificarPeso cantidad animal = animal {peso = peso animal + cantidad}

revisacion :: VisitaMedica -> Actividad
revisacion visita animal
  | enfermo animal = darleVitaminas . agregarVisita visita $ animal
  | otherwise = animal

darleVitaminas :: Animal -> Animal
darleVitaminas animal = engordar 2 animal

agregarVisita :: VisitaMedica -> Animal -> Animal
agregarVisita visita animal = animal {visitasMedicas = visita : visitasMedicas animal}

visita1 :: VisitaMedica
visita1 = VisitaMedica 10 1000

festejoCumple :: Actividad
festejoCumple animal = hacerFiesta . agregarAnio 1 $ animal

hacerFiesta :: Actividad
hacerFiesta animal = modificarPeso (-1) animal

agregarAnio :: Int -> Animal -> Animal
agregarAnio anioAgregar animal = animal {edad = edad animal + anioAgregar}

chequeoDePeso :: Float -> Actividad
chequeoDePeso pesoMinimo animal
  | peso animal >= pesoMinimo = animal
  | otherwise = animal {enfermo = True}

type Proceso = [Actividad]

realizarProcesos :: Proceso -> Animal -> Animal
realizarProcesos actividades animal = foldr ($) animal actividades

-- 4

mejoraSustentable :: [Actividad] -> Animal -> Bool
mejoraSustentable actividades animal = mejoraRecursiva (peso animal) actividades animal

mejoraRecursiva :: Float -> [Actividad] -> Animal -> Bool
mejoraRecursiva _ [] _ = True
mejoraRecursiva pesoAnterior (actividad : actividades) animal = pesoMayorQueAnterior actividad animal pesoAnterior && subadePesoMenor4 actividad animal pesoAnterior && mejoraRecursiva (peso (actividad animal)) actividades (actividad animal)

pesoMayorQueAnterior :: Actividad -> Animal -> Float -> Bool
pesoMayorQueAnterior actividad animal pesoAnterior = (> pesoAnterior) . peso . actividad $ animal

subadePesoMenor4 :: Actividad -> Animal -> Float -> Bool
subadePesoMenor4 actividad animal pesoAnterior = peso (actividad animal) - pesoAnterior <= 3

-- 5a
devolver3PrimerosFalopas :: [Animal] -> [Animal]
devolver3PrimerosFalopas animales = take 3 . filtrarNombreFalopas $ animales

filtrarNombreFalopas :: [Animal] -> [Animal]
filtrarNombreFalopas animales = filter tieneNombreFalopa animales

animalesInfinitos :: [Animal]
animalesInfinitos =
  cycle
    [ Animal "pachi" "Sagitario" 50 50 False [],
      Animal "dorothy" "Sagitario" 50 50 False [],
      Animal "bobi" "Perro" 20 5 False [],
      Animal "loki" "Gato" 10 3 False [],
      Animal "tomi" "Hamster" 0.5 1 False [],
      Animal "pepito" "Pez" 0.1 1 False []
    ]
