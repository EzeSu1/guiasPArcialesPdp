import Text.Show.Functions ()

data Personaje = Personaje
  { nombre :: String,
    dinero :: Int,
    felicidad :: Int
  }
  deriving (Show)

homero :: Personaje
homero = Personaje "Homero " 50 100

skinner :: Personaje
skinner = Personaje "Skinner" 140 50

burns :: Personaje
burns = Personaje "Mr Burns" 1000 60

lisa :: Personaje
lisa = Personaje "Lisa" 100 50

modificarFelicidad :: Int -> Personaje -> Personaje
modificarFelicidad valor unPersonaje = unPersonaje {felicidad = verificarSiSonNegativos (felicidad unPersonaje) valor}

verificarSiSonNegativos :: Int -> Int -> Int
verificarSiSonNegativos valorAgregado valorOriginal
  | valorOriginal + valorAgregado > 0 = valorOriginal + valorAgregado
  | otherwise = 0

modificarDinero :: Int -> Personaje -> Personaje
modificarDinero valor unPersonaje = unPersonaje {dinero = verificarSiSonNegativos (dinero unPersonaje) valor}

type Actividad = Personaje -> Personaje

escuelaElemental :: Actividad
escuelaElemental unPersonaje
  | (== "Lisa") . nombre $ unPersonaje = modificarFelicidad 20 unPersonaje
  | otherwise = modificarFelicidad (-20) unPersonaje

comerDona :: Actividad
comerDona = modificarFelicidad 10 . modificarDinero (-10)

comercantDonas :: Int -> Actividad
comercantDonas cantidad unPersonaje
  | dinero unPersonaje >= 10 = comercantDonas (cantidad - 1) (comerDona unPersonaje)
  | otherwise = unPersonaje

trabajar :: String -> Actividad
trabajar trabajo unPersonaje = modificarDinero (length trabajo) unPersonaje

trabajarComoDirector :: Actividad
trabajarComoDirector = escuelaElemental . trabajar "escuelaElemental"

type Logro = Personaje -> Bool

type Criterio = Personaje -> Int

compararCon :: Criterio -> Personaje -> Int -> Bool
compararCon criterio unPersonaje valorAcomparar = criterio unPersonaje > valorAcomparar

serMillonario :: Logro
serMillonario unPersonaje = compararCon dinero unPersonaje (dinero burns)

alcanzaAlegrarse :: Int -> Logro
alcanzaAlegrarse nivelDeseado unPersonaje = compararCon felicidad unPersonaje nivelDeseado

verKrosti :: Logro
verKrosti unPersonaje = compararCon dinero unPersonaje 10

actividadResultaDecisivaParaLograrUnLogro :: Personaje -> Logro -> Actividad -> Bool
actividadResultaDecisivaParaLograrUnLogro personaje logro actividad = (not . logro) personaje || (logro . actividad) personaje

encontrarLaActivadadDecisiva :: [Actividad] -> Logro -> Personaje -> Personaje
encontrarLaActivadadDecisiva [] logro personaje = personaje
encontrarLaActivadadDecisiva (actividad : actividades) logro personaje
  | actividadResultaDecisivaParaLograrUnLogro personaje logro actividad = actividad personaje
  | otherwise = encontrarLaActivadadDecisiva actividades logro personaje

actividadesInfinitas :: [Actividad] -> [Actividad]
actividadesInfinitas actividades = cycle actividades