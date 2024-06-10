import Text.Show.Functions ()

type Gema = Personaje -> Personaje

data Guantelete = UnGuantelete
  { material :: String,
    gemas :: [Gema]
  }
  deriving (Show)

data Personaje = UnPersonaje
  { nombre :: String,
    edad :: Int,
    energia :: Float,
    habilidades :: [String],
    planetaActual :: String
  }
  deriving (Show)

type Universo = [Personaje]

chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo
  | puedeUsarse guantelete = reducirMitad universo
  | otherwise = universo

reducirMitad :: Universo -> Universo
reducirMitad universo = take (length universo `div` 2) universo

puedeUsarse :: Guantelete -> Bool
puedeUsarse guantelete = ((== 6) . length . gemas) guantelete && ((== "uru") . material) guantelete

esAptoPendex :: Universo -> Bool
esAptoPendex personajes = any ((< 45) . edad) personajes

energiaTotalUniverso :: Universo -> Float
energiaTotalUniverso personajes = sumatoriaEnergiaUniversos . losQuetienenMasdeXHabilidad 1 $ personajes

sumatoriaEnergiaUniversos :: Universo -> Float
sumatoriaEnergiaUniversos personajes = sum . map energia $ personajes

losQuetienenMasdeXHabilidad :: Int -> Universo -> Universo
losQuetienenMasdeXHabilidad cantidad personajes = filter ((> cantidad) . length . habilidades) personajes

lamente :: Float -> Gema
lamente cantidadEnergiaADebilitar personaje = modificarEnergia (-cantidadEnergiaADebilitar) personaje

modificarEnergia :: Float -> Personaje -> Personaje
modificarEnergia valor personaje = personaje {energia = energia personaje + valor}

elAlma :: String -> Gema
elAlma habilidadAquitar personaje = modificarEnergia (-10) . quitarHabilidad habilidadAquitar $ personaje

quitarHabilidad :: String -> Personaje -> Personaje
quitarHabilidad habilidad personaje = personaje {habilidades = filter (/= habilidad) (habilidades personaje)}

elEspacio :: String -> Gema
elEspacio planetaATranportar personaje = modificarEnergia (-20) . cambiarDePlaneta planetaATranportar $ personaje

cambiarDePlaneta :: String -> Personaje -> Personaje
cambiarDePlaneta planetaNuevo personaje = personaje {planetaActual = planetaNuevo}

quitarHabilidadesSiTieneMenosDe :: Int -> Personaje -> Personaje
quitarHabilidadesSiTieneMenosDe cantidad personaje
  | cantidadHabilidades < cantidad = personaje {habilidades = []}
  | otherwise = personaje
  where
    cantidadHabilidades = length . habilidades $ personaje

poder :: Gema
poder personaje = modificarEnergia (-energia personaje) . quitarHabilidadesSiTieneMenosDe 2 $ personaje

elTiempo :: Gema
elTiempo personaje = reducirEdad personaje

reducirEdad :: Personaje -> Personaje
reducirEdad personaje
  | edadReducido >= 18 = modificarEdad edadReducido personaje
  | otherwise = modificarEdad 18 personaje
  where
    edadReducido = div (edad personaje) 2

modificarEdad :: Int -> Personaje -> Personaje
modificarEdad edadNueva personaje = personaje {edad = edadNueva}

gemaLoca :: Gema -> Gema
gemaLoca gema personaje = gema . gema $ personaje

-- Punto 4: (1 punto) Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad
-- de “usar Mjolnir” y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.
guanteleteGoma :: Guantelete
guanteleteGoma = UnGuantelete "Goma" [elTiempo, elAlma "usar Mjolnir", gemaLoca (elAlma "programación en Haskell")]

-- Generar la función utilizar  que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las
-- gemas que lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.

utilizar :: [Gema] -> Gema
utilizar listaDeGemas destinatario = foldr ($) destinatario $ listaDeGemas

-- Punto 6: (2 puntos). Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete
-- y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima.
gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje guantelete = gemaMasPoderosaDe personaje $ gemas guantelete

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1 : gema2 : gemas)
  | energia (gema1 personaje) > energia (gema2 personaje) = gemaMasPoderosaDe personaje (gema2 : gemas)
  | otherwise = gemaMasPoderosaDe personaje (gema1 : gemas)

-- 7
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema : (infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas elTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3 . gemas) guantelete

punisher :: Personaje
punisher = UnPersonaje "The Punisher" 38 350 ["Disparar con de todo", "golpear"] "Tierra"