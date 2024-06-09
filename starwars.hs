data Nave = Nave
  { nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: Poder
  }

type Accion = Nave -> Nave

type Poder = [Accion]

modificarAtaque :: Int -> Nave -> Nave
modificarAtaque nuevoAtaque unNave = unNave {ataque = verificarSiSonNegativos (ataque unNave) nuevoAtaque}

verificarSiSonNegativos :: Int -> Int -> Int
verificarSiSonNegativos actualNivel variacion
  | actualNivel + variacion > 0 = actualNivel + variacion
  | otherwise = 0

modificarDurabilidad :: Int -> Nave -> Nave
modificarDurabilidad nuevoDurabilidad unNave = unNave {ataque = verificarSiSonNegativos (durabilidad unNave) nuevoDurabilidad}

modificarEscudo :: Int -> Nave -> Nave
modificarEscudo nuevoEscudo unNave = unNave {ataque = verificarSiSonNegativos (ataque unNave) nuevoEscudo}

turbo :: Accion
turbo = modificarAtaque 25

xWing :: Nave
xWing = Nave "X Wing" 300 150 100 [reparacionDeEmergencia]

tieFighter :: Nave
tieFighter = Nave "TIE Fighter" 200 100 50 [turbo]

reparacionDeEmergencia :: Accion
reparacionDeEmergencia unNave = modificarAtaque (-30) . modificarDurabilidad 50 $ unNave

navedeDarthVader :: Nave
navedeDarthVader = Nave "Nave de Darth Vader" 500 300 200 [superTurbo, modificarDurabilidad (-45)]

superTurbo :: Accion
superTurbo unNave = repetirAcciones 3 turbo $ unNave

millenniumFalcon :: Nave
millenniumFalcon = Nave "Millennium Falcon" 1000 500 50 [reparacionDeEmergencia, modificarEscudo 100]

repetirAcciones :: Int -> Accion -> Nave -> Nave
repetirAcciones cantidad accion nave
  | cantidad > 0 = repetirAcciones (cantidad - 1) accion nave
  | otherwise = nave

-- 2
type Flota = [Nave]

durabilidadTotaldeLaFlota :: Flota -> Int
durabilidadTotaldeLaFlota unFlota = sum . map (durabilidad $) $ unFlota

flota1 :: Flota
flota1 = [navedeDarthVader, tieFighter]

-- 3

activarPoderDeLaNave :: Nave -> Nave
activarPoderDeLaNave nave = foldl (\nave accion -> accion nave) nave (poder nave)

estadoNaveLuegoDeSerAtacada :: Nave -> Nave -> Nave
estadoNaveLuegoDeSerAtacada naveAtacada naveAtacante = actualizarNaveAtacada (activarPoderDeLaNave naveAtacada) (activarPoderDeLaNave naveAtacante)

actualizarNaveAtacada :: Nave -> Nave -> Nave
actualizarNaveAtacada naveAtacada naveAtacante
  | escudo naveAtacada < ataque naveAtacante = modificarDurabilidad (-calcularDanioRecibida naveAtacante naveAtacada) naveAtacada
  | otherwise = naveAtacada

calcularDanioRecibida :: Nave -> Nave -> Int
calcularDanioRecibida naveAtacante naveAtacada = ataque naveAtacante - escudo naveAtacada

-- 4
estaFueradeCombate :: Nave -> Bool
estaFueradeCombate = (== 0) . durabilidad

-- 5
type Estrategia = Nave -> Bool

navesDebiles :: Estrategia
navesDebiles = (> 200) . escudo

navesPeligrosas :: Int -> Estrategia
navesPeligrosas valordado = (< valordado) . ataque

naveFueraDeCombate :: Nave -> Bool
naveFueraDeCombate = (== 0) . durabilidad

navesQueQuedarianFueraDeCombate :: Nave -> Estrategia
navesQueQuedarianFueraDeCombate naveAtacante naveAtacada = estaFueradeCombate . estadoNaveLuegoDeSerAtacada naveAtacante $ naveAtacada

mision :: Flota -> Nave -> Estrategia -> Flota
mision flota naveAtacante estrategia = map (realizarAtaqueSiCumpleEstrategia estrategia naveAtacante) flota

realizarAtaqueSiCumpleEstrategia :: Estrategia -> Nave -> Nave -> Nave
realizarAtaqueSiCumpleEstrategia estrategia naveAtacante navedeLaFlota
  | estrategia navedeLaFlota = estadoNaveLuegoDeSerAtacada navedeLaFlota naveAtacante
  | otherwise = navedeLaFlota

-- 6

durabilidadDespuesMision :: Flota -> Nave -> Estrategia -> Int
durabilidadDespuesMision flotaEnemiga nave estrategia = durabilidadTotaldeLaFlota . (mision flotaEnemiga nave) $ estrategia

cualMinimizaMision :: Flota -> Nave -> Estrategia -> Estrategia -> Flota
cualMinimizaMision flotaEnemiga nave estrategia1 estrategia2
  | durabilidadDespuesMision flotaEnemiga nave estrategia1 > durabilidadDespuesMision flotaEnemiga nave estrategia2 = mision flotaEnemiga nave estrategia1
  | otherwise = mision flotaEnemiga nave estrategia2

