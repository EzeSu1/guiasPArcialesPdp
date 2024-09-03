import Data.List (isInfixOf)

data Alfajor = Alfajor
  { capasderelleno :: [Relleno],
    peso :: Float,
    dulzor :: Float,
    nombre :: String
  }
  deriving (Show)
--sd
data Relleno = Relleno
  { nombredeRelleno :: String,
    precio :: Float
  }
  deriving (Eq, Show)

modificarPeso :: Float -> Alfajor -> Alfajor
modificarPeso pesoAgregar unAlfajor = unAlfajor {peso = verificarsiEsNegativo (peso unAlfajor) pesoAgregar}

modificarDulzor :: Float -> Alfajor -> Alfajor
modificarDulzor dulzorAgregar unAlfajor = unAlfajor {dulzor = verificarsiEsNegativo (dulzor unAlfajor) dulzorAgregar}

verificarsiEsNegativo :: Float -> Float -> Float
verificarsiEsNegativo datoModificarAlfajor valor
  | datoModificarAlfajor - valor > 0 = datoModificarAlfajor - valor
  | otherwise = 0

-- 1

dulcedeleche :: Relleno
dulcedeleche = Relleno "Dulce de leche" 12

mousse :: Relleno
mousse = Relleno "Mousse" 15

fruta :: Relleno
fruta = Relleno "Fruta" 10

jorgito :: Alfajor
jorgito = Alfajor [dulcedeleche] 80 8 "Jorgito"

havanna :: Alfajor
havanna = Alfajor [mousse, mousse] 60 12 "Havanna"

capitandelEspacio :: Alfajor
capitandelEspacio = Alfajor [dulcedeleche] 40 12 "Capitan del espacio"

-- 1b
coeficienteDulzorAlfajor :: Alfajor -> Float
coeficienteDulzorAlfajor alfajor = dulzor alfajor / peso alfajor

precioAlfajor :: Alfajor -> Float
precioAlfajor alfajor = 2 * peso alfajor + preciototalRelleno (capasderelleno alfajor)

preciototalRelleno :: [Relleno] -> Float
preciototalRelleno rellenos = sum . map (precio) $ rellenos

alfajorPotable :: Alfajor -> Bool
alfajorPotable alfajor = verificarcapas (capasderelleno alfajor) && coeficienteDulzorMayorA 0.1 alfajor

verificarcapas :: [Relleno] -> Bool
verificarcapas [] = False
verificarcapas [_] = True
verificarcapas (relleno : rellenos) = sonTodosDelmismoRelleno relleno rellenos

sonTodosDelmismoRelleno :: Relleno -> [Relleno] -> Bool
sonTodosDelmismoRelleno relleno = all (== relleno)

coeficienteDulzorMayorA :: Float -> Alfajor -> Bool
coeficienteDulzorMayorA numero alfajor = coeficienteDulzorAlfajor alfajor > numero

-- 222
type Produccion = Alfajor -> Alfajor

abaratarAlfajor :: Produccion
abaratarAlfajor = modificarDulzor (-7) . modificarPeso (-10)

renombrarAlfajor :: String -> Produccion
renombrarAlfajor nuevoNombre alfajor = alfajor {nombre = nuevoNombre}

agregarCapasdeRelleno :: Relleno -> Alfajor -> Alfajor
agregarCapasdeRelleno relleno unAlfajor = unAlfajor {capasderelleno = relleno : capasderelleno unAlfajor}

puedeSerAlfajorPremium :: Alfajor -> Alfajor
puedeSerAlfajorPremium alfajor
  | alfajorPotable alfajor = hacerPremium alfajor
  | otherwise = alfajor

hacerPremium :: Produccion
hacerPremium alfajor = renombrarAlfajor (nombre alfajor ++ " premium") . agregarCapasdeRellenoMismotipo (capasderelleno alfajor) $ alfajor

agregarCapasdeRellenoMismotipo :: [Relleno] -> Alfajor -> Alfajor
agregarCapasdeRellenoMismotipo [relleno] = agregarCapasdeRelleno relleno
agregarCapasdeRellenoMismotipo (relleno : _) = agregarCapasdeRelleno relleno

hacerPremiumUnaCiertaCantidadDeVeces :: Int -> Alfajor -> Alfajor
hacerPremiumUnaCiertaCantidadDeVeces gradosDePremium alfajor
  | gradosDePremium >= 0 = hacerPremiumUnaCiertaCantidadDeVeces (gradosDePremium - 1) (puedeSerAlfajorPremium alfajor)
  | otherwise = alfajor

jorgitito, jorgelin, capitanDelEspacioDeCostaACosta :: Alfajor
jorgitito = (renombrarAlfajor "Jorgitito" . abaratarAlfajor) jorgito
jorgelin = (renombrarAlfajor "Jorgelin" . agregarCapasdeRelleno dulcedeleche) jorgito
capitanDelEspacioDeCostaACosta = renombrarAlfajor "Capitan del espacio de costa a costa" . hacerPremiumUnaCiertaCantidadDeVeces 4 . abaratarAlfajor $ capitandelEspacio

-- capitanDelEspacioDeCostaACosta = renombrarAlfajor "Capitan del espacio de costa a costa" capitandelEspacio

-- 33

type Criterio = Alfajor -> Bool

data Cliente = Cliente
  { nombreDelCliente :: String,
    plataDisponible :: Float,
    alfajoresComprados :: [Alfajor],
    gustosPersonales :: [Criterio]
  }

contenganEnSuNombre :: String -> Criterio
contenganEnSuNombre nombreBuscado alfajor = nombreBuscado `isInfixOf` nombre alfajor

-- -}
pretencioso :: Criterio
pretencioso alfajor = contenganEnSuNombre "premium" alfajor

dulcero :: Criterio
dulcero = coeficienteDulzorMayorA 0.15

antirelleno :: Relleno -> Criterio
antirelleno rellenoQueNoSeQuiere alfajor = rellenoQueNoSeQuiere `notElem` (capasderelleno alfajor)

extrano :: Criterio
extrano = not . alfajorPotable

cualesLesGustan :: [Alfajor] -> Cliente -> [Alfajor]
cualesLesGustan alfajores cliente = leGustan alfajores (gustosPersonales cliente)

leGustan :: [Alfajor] -> [Criterio] -> [Alfajor]
leGustan alfajores gustosDelCliente = filter (cumplenConLosGustos gustosDelCliente) alfajores

cumplenConLosGustos :: [Criterio] -> Alfajor -> Bool
cumplenConLosGustos gustosPersonales alfajor = all (\gustoPersonal -> gustoPersonal alfajor) gustosPersonales

comprarAlfajor :: Alfajor -> Cliente -> Cliente
comprarAlfajor alfajor cliente
  | puedeComprarAlfajor alfajor cliente = modificarDinero (precioAlfajor alfajor) . agregarAlfajor alfajor $ cliente
  | otherwise = cliente
  where
    puedeComprarAlfajor alfajor cliente = plataDisponible cliente > (precioAlfajor alfajor)

modificarDinero :: Float -> Cliente -> Cliente
modificarDinero dineroAModificar uncliente = uncliente {plataDisponible = plataDisponible uncliente - dineroAModificar}

agregarAlfajor :: Alfajor -> Cliente -> Cliente
agregarAlfajor unalfajor uncliente = uncliente {alfajoresComprados = unalfajor : alfajoresComprados uncliente}

comprarAlfajorqueLegusten :: [Alfajor] -> Cliente -> Cliente
comprarAlfajorqueLegusten alfajores cliente = comprarAlfajores (cualesLesGustan alfajores cliente) $ cliente

comprarAlfajores :: [Alfajor] -> Cliente -> Cliente
comprarAlfajores alfajores cliente = foldl (flip comprarAlfajor) cliente alfajores
