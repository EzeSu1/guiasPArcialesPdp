import Data.List (genericLength)

-- 1a

data Postre = Postre
  { sabores :: [String],
    peso :: Float,
    temperatura :: Float
  }
  deriving (Eq)

bizcochodefrutaycrema :: Postre
bizcochodefrutaycrema = Postre ["fruta", "crema"] 100 25

type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio postre = modificarPeso (-0.05 * peso postre) . calentarPostre $ postre

calentarPostre :: Postre -> Postre
calentarPostre postre = modificarTemperatura 1 postre

modificarTemperatura :: Float -> Postre -> Postre
modificarTemperatura temperaturaAgregado postre = postre {temperatura = temperatura postre + temperaturaAgregado}

modificarPeso :: Float -> Postre -> Postre
modificarPeso pesoModificar postre = postre {peso = peso postre + pesoModificar}

immobulus :: Hechizo
immobulus postre = congelarPostre postre

congelarPostre :: Postre -> Postre
congelarPostre postre = modificarTemperatura (-temperatura postre) postre

wingardium :: Hechizo
wingardium postre = modificarPeso (-0.1 * peso postre) . agregarSabor "concentrado" $ postre

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre = postre {sabores = sabor : sabores postre}

diffindo :: Float -> Hechizo
diffindo porcentaje postre = modificarPeso (-porcentaje * peso postre) postre

riddikulus :: String -> Hechizo
riddikulus nuevoSabor postre = agregarSabor (reverse nuevoSabor) postre

avadakedavra :: Hechizo
avadakedavra postre = quitarTodosLosSabores . immobulus $ postre

quitarTodosLosSabores :: Postre -> Postre
quitarTodosLosSabores postre = postre {sabores = []}

estaListoPostreHechizo :: Hechizo -> Postre -> Bool
estaListoPostreHechizo hechizo postre = listoPostre (hechizo postre)

listoPostre :: Postre -> Bool
listoPostre postre = pesaMasde0 postre && tieneAlgunSabor postre && noEstacongelado postre

tieneAlgunSabor :: Postre -> Bool
tieneAlgunSabor postre = not . null . sabores $ postre

noEstacongelado :: Postre -> Bool
noEstacongelado postre = (> 0) . temperatura $ postre

pesaMasde0 :: Postre -> Bool
pesaMasde0 postre = (> 0) . peso $ postre

tartademelaza :: Postre
tartademelaza = Postre ["melaza"] 50 0

-- 1d
type Postres = [Postre]

pesoPromedioPostresListos :: Postres -> Float
pesoPromedioPostresListos postres = promedioPostres . listaPesosPostreListo . filtrarPostresListos $ postres

promedioPostres :: [Float] -> Float
promedioPostres listaPesos = sum listaPesos / genericLength listaPesos

listaPesosPostreListo :: Postres -> [Float]
listaPesosPostreListo postres = map peso postres

filtrarPostresListos :: Postres -> Postres
filtrarPostresListos postres = filter listoPostre postres

-- 2
data Mago = Mago
  { hechizosAprendidos :: [Hechizo],
    cantidadHorrorcruxes :: Int
  }

asistirClase :: Mago -> Postre -> Hechizo -> Mago
asistirClase mago postre hechizo
  | hechizo postre == avadakedavra postre = modificarHorrorcrux 1 mago
  | otherwise = agregarHechizo hechizo mago

modificarHorrorcrux :: Int -> Mago -> Mago
modificarHorrorcrux cantidad mago = mago {cantidadHorrorcruxes = cantidadHorrorcruxes mago + cantidad}

agregarHechizo :: Hechizo -> Mago -> Mago
agregarHechizo hechizo mago = mago {hechizosAprendidos = hechizo : hechizosAprendidos mago}

-- B) Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al
-- postre con más cantidad de sabores luego de usarlo.
mejorHechizo :: Mago -> Postre -> Hechizo
mejorHechizo mago postre = compararHechizos (hechizosAprendidos mago) postre

compararHechizos :: [Hechizo] -> Postre -> Hechizo
compararHechizos [] _ = hechizoSinNada
compararHechizos [hechizo] _ = hechizo
compararHechizos (hechizo1 : hechizo2 : hechizos) postre
  | cantidadSabores (hechizo1 postre) > cantidadSabores (hechizo2 postre) = compararHechizos (hechizo1 : hechizos) postre
  | otherwise = compararHechizos (hechizo2 : hechizos) postre
  where
    cantidadSabores = length . sabores

hechizoSinNada :: Hechizo
hechizoSinNada postre = id postre

{-mejorHechizo :: Mago -> Postre -> Hechizo
mejorHechizo mago postre = tieneHechizo mago postre
mejorHechizo mago postre = foldr1 (compararHechizos postre) (hechizosAprendidos mago)

compararHechizos :: Postre -> Hechizo -> Hechizo -> Hechizo
compararHechizos postre hechizo1 hechizo2
  | cantidadSabor (hechizo1 postre) > cantidadSabor (hechizo2 postre) = hechizo1
  | otherwise = hechizo2
  where
    cantidadSabor = length . sabores
NOFUCNIONA PARA LISTA VACIA o LISTA De 1 elemento
    -}

hechizosInfinitos :: [Hechizo]
hechizosInfinitos = cycle [wingardium, diffindo 20, riddikulus "chocolate", avadakedavra]

magoInfinito :: Mago
magoInfinito = Mago hechizosInfinitos 10

postresInfinitos :: [Postre]
postresInfinitos = cycle [bizcochodefrutaycrema, tartademelaza]

-- b si con any, por la lazy evaluation ...
-- c no, porque no termina de evaluar toda la lista de hechizos
