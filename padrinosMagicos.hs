import Data.List ()
import Text.Show.Functions ()

-- 1
data Chico = Chico
  { nombre :: String,
    edad :: Int,
    habilidadades :: Habilidad,
    deseos :: [Deseo]
  }
  deriving (Show)

timmy :: Chico
timmy = Chico "Timmy" 10 ["mirar television", "jugar en la pc", "ser un supermodelo noruego"] [serMayor]

-- timmy = Chico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor]

pogo :: Chico
pogo = Chico "Pogo" 10 ["mirar television", "jugar en la pc"] [serGrosoEnNeedForSpeed]

type Habilidad = [String]

type Deseo = Chico -> Chico

aprenderHabilidades :: Habilidad -> Deseo
aprenderHabilidades habilidades unChico = agregarHabilidad habilidades unChico

agregarHabilidad :: [String] -> Chico -> Chico
agregarHabilidad lasHabilidades unChico = unChico {habilidadades = lasHabilidades ++ habilidadades unChico}

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed unChico = agregarHabilidad versionesDeNeedForSpeed unChico

versionesDeNeedForSpeed :: [String]
versionesDeNeedForSpeed = map (crearVersiones "jugar need for speed ") [1 ..]

crearVersiones :: String -> Int -> String
crearVersiones unaPalabra unNumero = unaPalabra ++ show unNumero

cambiarEdad :: (Int -> Int) -> Chico -> Chico
cambiarEdad nuevaEdad chico = chico {edad = nuevaEdad (edad chico)}

serMayor :: Deseo
serMayor unChico = unChico {edad = 18}

type PadrinoMagico = Chico -> Chico

wanda :: PadrinoMagico
wanda unChico = cambiarEdad (+ 1) . cumplirDeseo unChico $ (head (deseos unChico))

cumplirDeseo :: Chico -> Deseo -> Chico
cumplirDeseo unchico deseo = deseo unchico

cosmo :: PadrinoMagico
cosmo unChico = cambiarEdad (`div` 2) unChico

muffinMagico :: PadrinoMagico
muffinMagico unChico = foldl cumplirDeseo unChico (deseos unChico)

data Chica = UnaChica
  { nombreDeLaChica :: String,
    requisitoParaSalir :: Requisito
  }

type Requisito = Chico -> Bool

tieneHabilidad :: String -> Requisito
tieneHabilidad unaHabilidad unChico = unaHabilidad `elem` (habilidadades unChico)

esSuperMaduro :: Requisito
esSuperMaduro unChico = esMayorDeEdad unChico && tieneHabilidad "manejar" unChico

esMayorDeEdad :: Chico -> Bool
esMayorDeEdad unchico = edad unchico > 18

noEsTimmy :: Requisito
noEsTimmy chico = nombre chico /= "Timmy"

trixie, vicky :: Chica
trixie = UnaChica "Trixie Tang" noEsTimmy
vicky = UnaChica "Vicky" (tieneHabilidad "ser un supermodelo noruego")

type Pretendientes = [Chico]

quienConquistaA :: Chica -> Pretendientes -> Chico
quienConquistaA unaChica losPretendientes
  | hayUnoQueCumple unaChica losPretendientes = elPrimeroQueCumplaRequisitos unaChica losPretendientes
  | otherwise = last losPretendientes

elPrimeroQueCumplaRequisitos :: Chica -> Pretendientes -> Chico
elPrimeroQueCumplaRequisitos chica (pretendiente : pretendientes)
  | requisitoParaSalir chica pretendiente = pretendiente
  | otherwise = elPrimeroQueCumplaRequisitos chica pretendientes

hayUnoQueCumple :: Chica -> Pretendientes -> Bool
hayUnoQueCumple unaChica chicos = any (requisitoParaSalir unaChica) chicos

habilidadesProhibidas :: Habilidad
habilidadesProhibidas = ["enamorar", "matar", "dominar el mundo"]

-- infractoresDaRules

tieneHabilidadProhibida :: Chico -> Bool
tieneHabilidadProhibida unChico = any (`elem` habilidadesProhibidas) (tomarlasPrimeras5 unChico)

tomarlasPrimeras5 :: Chico -> Habilidad
tomarlasPrimeras5 unChico = take 5 (habilidadades (muffinMagico unChico))