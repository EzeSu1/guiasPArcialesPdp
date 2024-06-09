data Heroe = Heroe
  { epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
  }

data Artefacto = Artefacto
  { nombre :: String,
    rareza :: Int
  }

type Tarea = Heroe -> Heroe

-- 2
cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto newEpiteto unHeroe = unHeroe {epiteto = newEpiteto}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto unArtefacto unHeroe = unHeroe {artefactos = unArtefacto : artefactos unHeroe}

paseALaHistoria :: Heroe -> Heroe
paseALaHistoria unHeroe
  | reconocimiento unHeroe > 1000 = cambiarEpiteto "El mítico" unHeroe
  | reconocimiento unHeroe >= 500 = cambiarEpiteto "El magnífico" . agregarArtefacto lanzaDelOlimpo $ unHeroe
  | reconocimiento unHeroe >= 100 = cambiarEpiteto "Hoplita" . agregarArtefacto xiphos $ unHeroe
  | otherwise = unHeroe

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50

-- 3
encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto unArtefacto unHeroe = ganarReconocimiento (rareza unArtefacto) . agregarArtefacto unArtefacto $ unHeroe

ganarReconocimiento :: Int -> Heroe -> Heroe
ganarReconocimiento cantidad unHeroe = unHeroe {reconocimiento = cantidad + reconocimiento unHeroe}

escalarElOlimpo :: Tarea
escalarElOlimpo unHeroe = (agregarArtefacto "El relampago de Zeus") . desecharArtefactosComunes . triplicarRarezadeObjetos . ganarReconocimiento 500 $ unHeroe

triplicarRarezaArtefactos :: Tarea
triplicarRarezaArtefactos = cambiarArtefactos (map triplicarRarezaArtefacto)

triplicarRarezaArtefacto :: Artefacto -> Artefacto
triplicarRarezaArtefacto unArtefacto = unArtefacto {rareza = (* 3) . rareza $ unArtefacto}

desecharArtefactosComunes :: Tarea
desecharArtefactosComunes unHeroe = unHeroe {artefactos = filter (not . esComun) (artefactos unHeroe)}

esComun :: Artefacto -> Bool
esComun unArtefacto = rareza unArtefacto < 1000

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cantidadDeCuadras = cambiarEpiteto ("Gros" ++ replicate cantidadDeCuadras 'o')

data Bestia = Bestia
  { nombreBestia :: String,
    debilidad :: Debilidad
  }
  deriving (Show)

type Debilidad = Heroe -> Bool

cambiarArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
cambiarArtefactos modificador unHeroe = unHeroe {artefactos = modificador (artefactos unHeroe)}

matarAUnaBestia :: Bestia -> Tarea
matarAUnaBestia unaBestia unHeroe
  | (debilidad unaBestia) unHeroe = cambiarEpiteto ("El asesino de " ++ nombreBestia unaBestia) unHeroe
  | otherwise = cambiarEpiteto "El Cobarde" . cambiarArtefactos (drop 1) $ unHeroe
