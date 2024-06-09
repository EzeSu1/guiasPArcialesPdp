data Auto = Auto
  { color :: String,
    velocidad :: Int,
    distancia :: Int
  }
  deriving (Eq)

type Carrera = [Auto]

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = abs (distancia auto1 - distancia auto2) < 10

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = (not . tieneAlgunAutoCerca auto) carrera && vaGanando auto carrera

tieneAlgunAutoCerca :: Auto -> Carrera -> Bool
tieneAlgunAutoCerca auto carrera = any (estaCerca auto) carrera

vaGanando :: Auto -> Carrera -> Bool
vaGanando auto carrera = all (recorrioMasQue auto) carrera

recorrioMasQue :: Auto -> Auto -> Bool
recorrioMasQue auto1 auto2 = distancia auto1 > distancia auto2

-- Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que le van ganando.
puesto :: Auto -> Carrera -> Int
puesto auto = (1 +) . length . filter (not . recorrioMasQue auto)

hacerQueCorraTiempoDe :: Int -> Auto -> Auto
hacerQueCorraTiempoDe tiempo auto = modificarDistanciaRecorrida (velocidad auto * tiempo) auto

modificarDistanciaRecorrida :: Int -> Auto -> Auto
modificarDistanciaRecorrida distanciaNuevaRecogida auto = auto {distancia = distancia auto + distanciaNuevaRecogida}

type ModificadorDeVelocidad = Int -> Int

alterarVelocidad :: ModificadorDeVelocidad -> Auto -> Auto
alterarVelocidad modificador auto = modificarVelocidad (-modificador (velocidad auto) * velocidad auto) auto

modificarVelocidad :: Int -> Auto -> Auto
modificarVelocidad velocidadNueva auto = auto {velocidad = verificarQueNoSeaNegativo (velocidad auto) velocidadNueva}

bajarVelocidad :: Int -> ModificadorDeVelocidad -> Auto -> Auto
bajarVelocidad velocidadABajar modificador auto = alterarVelocidad modificador (modificarVelocidad (verificarQueNoSeaNegativo velocidadABajar (velocidad auto)) auto)

verificarQueNoSeaNegativo :: Int -> Int -> Int
verificarQueNoSeaNegativo velocidadABajar velocidadauto = max 0 (velocidadABajar - velocidadauto)

type PowerUp = Auto -> Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not . criterio) lista

terremoto :: PowerUp
terremoto autoQueGatillo = afectarALosQueCumplen (estaCerca autoQueGatillo) (modificarVelocidad 50)

miguelitos :: Int -> PowerUp
miguelitos velocidadABajar autoQueGatillo carrera = afectarALosQueCumplen (recorrioMasQue autoQueGatillo) (modificarVelocidad velocidadABajar) carrera

jetPack :: Int -> PowerUp
jetPack tiempo autoQueGatillo = afectarALosQueCumplen (== autoQueGatillo) (alterarVelocidad (\_ -> velocidad autoQueGatillo) . hacerQueCorraTiempoDe tiempo . alterarVelocidad (* 2))

{-miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la
velocidad los autos que se vean afectados por su uso. Los autos a afectar son aquellos a los cuales el auto
que gatilló el power up les vaya ganando.

jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet pack
tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.
Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el
tiempo indicado y finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.
Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo.

-}

-- 4

type Evento = Carrera -> Carrera

type Color = String

simularCarrera :: Carrera -> [Evento] -> [(Int, Color)]
simularCarrera carrera eventos = tabladePosciciones . ejecutarEventos eventos $ carrera

tabladePosciciones :: Carrera -> [(Int, Color)]
tabladePosciciones carrera = zip (map (flip puesto carrera) carrera) (map color carrera)

ejecutarEventos :: [Evento] -> Carrera -> Carrera
ejecutarEventos eventos carreraInicial = foldr ($) carreraInicial eventos

-- procesarEventos :: Carrera -> [Evento] -> Carrera
-- procesarEventos carreraInicial eventos = foldl (\carreraActual evento -> evento carreraActual) carreraInicial eventos

correnTodos :: Int -> Evento
correnTodos tiempo carrera = map (hacerQueCorraTiempoDe tiempo) carrera

usaPowerUp :: PowerUp -> Color -> Evento
usaPowerUp powerUp colorBuscado carrera = powerUp autoQueGatillaElPoder carrera
  where
    autoQueGatillaElPoder = find ((== colorBuscado) . color) carrera

find :: (c -> Bool) -> [c] -> c
find cond = head . filter cond
