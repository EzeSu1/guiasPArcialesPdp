import Text.Show.Functions

-- enunciado: https://docs.google.com/document/d/1vfmY4xOaGVMtDCixKtA6rDugwWM86ZJaL7K5vDKcksA/edit

type Tribu = [Fremen]

data Fremen = Fremen
  { nombre :: String,
    nivelToleranciaEspecia :: Float,
    seriesTitulos :: [String],
    cantidadReconocimientos :: Float
  }
  deriving (Eq, Ord, Show)

data Gusano = Gusano
  { longitud :: Float,
    nivelHidratacion :: Float,
    descripcion :: String
  }
  deriving (Show)

stilgar :: Fremen
stilgar = Fremen "Stilgar" 150 ["Guia"] 3

recibirRenoconocimiento :: Float -> Fremen -> Fremen
recibirRenoconocimiento otroReconocimiento fremen = fremen {cantidadReconocimientos = cantidadReconocimientos fremen + otroReconocimiento}

hayCantidatoSerElegido :: Fremen -> Bool
hayCantidatoSerElegido unFremen = buscarTitulo "Domador" (seriesTitulos unFremen) && toleranteEspeciaMayorA 100 unFremen

buscarTitulo :: String -> [String] -> Bool
buscarTitulo tituloBuscado titulos = any (== tituloBuscado) titulos

toleranteEspeciaMayorA :: Float -> Fremen -> Bool
toleranteEspeciaMayorA valor fremen = nivelToleranciaEspecia fremen > valor

elElegido :: Tribu -> Fremen
elElegido tribu = sacarelMayorReconocimiento . filtrarElMAyorElegido $ tribu

type Criterio = Fremen -> Float

filtrarElMAyorElegido :: Tribu -> [Fremen]
filtrarElMAyorElegido tribu = filter (hayCantidatoSerElegido) $ tribu

sacarelMayorReconocimiento :: [Fremen] -> Fremen
sacarelMayorReconocimiento fremens = foldl1 mayorReconocimiento fremens

mayorReconocimiento :: Fremen -> Fremen -> Fremen
mayorReconocimiento a b
  | cantidadReconocimientos a > cantidadReconocimientos b = a
  | otherwise = b

{-buscarReconocimientos :: [Fremen] -> [Fremen]
buscarReconocimientos fremens = ordenarPorreconocimiento fremens
ordenarPorreconocimiento :: [Fremen] -> [Fremen]
ordenarPorreconocimiento [] = []
ordenarPorreconocimiento [fremen] = [fremen]
ordenarPorreconocimiento (fremen1 : fremen2 : fremens)
  | cantidadReconocimientos fremen1 > cantidadReconocimientos fremen2 = buscarReconocimientos (fremen1 : fremens)
  | otherwise = buscarReconocimientos (fremen2 : fremens)
-}
criaGusano :: Gusano -> Gusano -> Gusano
criaGusano gusano1 gusano2 =
  Gusano
    { longitud = 0.1 * max (longitud gusano1) (longitud gusano2),
      nivelHidratacion = 0,
      descripcion = descripcion gusano1 ++ " - " ++ descripcion gusano2
    }

gusano4 :: Gusano
gusano4 = Gusano 10 5 "rojo con lunares"

gusano3 :: Gusano
gusano3 = Gusano 8 1 "dientes puntiagudos"

listaCrias :: [Gusano] -> [Gusano] -> [Gusano -> Gusano -> Gusano]
listaCrias lista1 lista2 = sacarCria (zip lista1 lista2)

sacarCria :: [(Gusano, Gusano)] -> [Gusano -> Gusano -> Gusano]
sacarCria [] = []
-- sacarCria lista = map (\progenitor -> criaGusano' progenitor) lista
sacarCria lista = map (\(g1, g2) -> criaGusano' (g1, g2)) lista

criaGusano' :: (Gusano, Gusano) -> Gusano -> Gusano -> Gusano
criaGusano' (g1, g2) = (\g1 g2 -> criaGusano g1 g2) -- segunCHAT GPT

-- 4
type Mision = Fremen -> Fremen

domarGusanoArena :: Gusano -> Mision
domarGusanoArena gusano fremen
  | toleranteEspeciaMayorA (longitud gusano - 1) fremen = modificarTolerancia 100 . agregarTitulo "Domandor" $ fremen
  | otherwise = modificarTolerancia (-0.1 * nivelToleranciaEspecia fremen) fremen

modificarTolerancia :: Float -> Mision
modificarTolerancia cantidadAaumentada unFremen = unFremen {nivelToleranciaEspecia = nivelToleranciaEspecia unFremen + cantidadAaumentada}

agregarTitulo :: String -> Mision
agregarTitulo tituloAgregar unFremen = unFremen {seriesTitulos = tituloAgregar : seriesTitulos unFremen}

destruirGusanoArena :: Gusano -> Mision
destruirGusanoArena unGusano unFremen
  | buscarTitulo "Domador" (seriesTitulos unFremen) && (not . toleranteEspeciaMayorA (longitud unGusano + 1) $ unFremen) = modificarTolerancia 100 . recibirRenoconocimiento 1 $ unFremen
  | otherwise = modificarTolerancia (-0.2 * nivelToleranciaEspecia unFremen) unFremen

simularMision :: Gusano -> (Gusano -> Mision) -> Tribu -> Tribu
simularMision gusano mision tribu = map (mision gusano) tribu
