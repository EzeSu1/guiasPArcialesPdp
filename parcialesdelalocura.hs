
data Investigador = Investidafor {
    nombre :: String,
    cordura :: Int,
    items :: [Item],
    sucesosEvitados :: [String]
}deriving(Show, Eq, Ord)

data  Item = Item {
    nombreItem :: String,
    valor :: Int 
} deriving (Show, Eq, Ord)

type Criterio  a =  Investigador -> a
maximoSegun :: (Ord a) => Criterio a -> Investigadores -> Investigador
maximoSegun criterio investigadores = foldr1 (mayorSegun criterio) investigadores

mayorSegun ::Ord a => Criterio a->Investigador-> Investigador-> Investigador
mayorSegun criterio investigador1 investigador2
    | criterio investigador1 > criterio investigador2 = investigador1
    | otherwise = investigador2
--1
hacerEnloquecerInvestigador :: Int -> Investigador -> Investigador
hacerEnloquecerInvestigador cantidadCordura unInvestigador = modificarCordura (-cantidadCordura) unInvestigador

modificarCordura :: Int -> Investigador -> Investigador
modificarCordura cantidadAmodificar unInvestigador = unInvestigador{ cordura = verificarSiEsNegativo cantidadAmodificar (cordura unInvestigador)}

verificarSiEsNegativo :: Int -> Int -> Int
verificarSiEsNegativo corduraAgregar cordura 
    |cordura + corduraAgregar > 0 = cordura + corduraAgregar
    |otherwise = 0

--1b
hallarItem :: Item -> Investigador -> Investigador
hallarItem unItem unInvestigador = hacerEnloquecerInvestigador (valor unItem). agregarItem unItem $ unInvestigador

agregarItem :: Item -> Investigador -> Investigador
agregarItem unItem unInvestigador = unInvestigador {items= unItem : items unInvestigador }

--2 
type Investigadores = [Investigador]
seEncuentraElItemInvestigadores :: String ->Investigadores -> Bool
seEncuentraElItemInvestigadores nombredelitem investigadores =  any (algunoTieneElItem nombredelitem) investigadores

algunoTieneElItem :: String -> Investigador-> Bool
algunoTieneElItem nombredelitem unInvestigador = any (tieneElItem nombredelitem) (items unInvestigador)
tieneElItem :: String -> Item -> Bool
tieneElItem nombredelitem item = (== nombredelitem) . take (length nombredelitem) . nombreItem $ item
--3 
liderActual :: (Ord a) => Criterio a -> Investigadores -> Investigador
liderActual criterio investigadores = maximoSegun criterio investigadores

potencial :: Investigador -> Int
potencial investigador  
    |totalmenteLoco investigador = 0
    |otherwise = cordura investigador * (sucesosTotales + 1) + maximum (map valor (items investigador))
        where sucesosTotales =length (sucesosEvitados investigador)
       

totalmenteLoco :: Investigador -> Bool
totalmenteLoco investigador = cordura investigador == 0
{-deltaSegun :: (a -> Int) -> (a -> Int) -> a -> Int -> Int
deltaSegun ponderacion transformacion valor = abs ((ponderacion . transformacion ) valor - ponderacion valor)

deltaCorduraTotal :: Int -> Investigadores -> Int
deltaCorduraTotal puntos investigadores = deltaSegun (sum . map cordura) ((map (hacerEnloquecerInvestigador puntos)) investigadores) puntos
-}
--falta un punto

data Suceso = Suceso {
    descripcion :: String,
    evitarSuceso :: SucesoEvitar,
    consecuencias :: [Consecuencia]
}
type SucesoEvitar = Investigadores -> Bool
type Consecuencia = Investigadores -> Investigadores
perderPrimerIntegrante :: Investigadores -> Investigadores
perderPrimerIntegrante  = drop 1 
suceso1 :: Suceso
suceso1 = Suceso "Despertar de un antiguo" (seEncuentraElItemInvestigadores "Necronomicon" ) [map (hacerEnloquecerInvestigador 10) , perderPrimerIntegrante]

suceso2 :: Suceso 
suceso2 = Suceso "Ritual en Innsmouth" (potencialmayor100) [hallarDagaMaldita, map (hacerEnloquecerInvestigador 2), map (hacerEnloquecerInvestigador 10), perderPrimerIntegrante]

potencialmayor100 :: Investigadores -> Bool
potencialmayor100 investigadores = (>100). potencial .liderActual potencial $ investigadores 


hallarDagaMaldita :: Investigadores -> Investigadores
hallarDagaMaldita (investigador : investigadores) = hallarItem dagaMaldita investigador : investigadores
hallarDagaMaldita [] = []

dagaMaldita :: Item
dagaMaldita = Item "Daga Maldita " 3

--6 
enfrentarSucesos :: Investigadores -> Suceso -> Investigadores
enfrentarSucesos investigadores suceso   
    |(evitarSuceso suceso) investigadores = map (agregarSucesosEvitados (descripcion suceso). hacerEnloquecerInvestigador 1 )  investigadores
    | otherwise = map (hacerEnloquecerInvestigador 1) (foldl (flip ($) ) investigadores (consecuencias suceso))

agregarSucesosEvitados :: String -> Investigador -> Investigador
agregarSucesosEvitados descripcion investigador = investigador {sucesosEvitados = descripcion: sucesosEvitados investigador  }

