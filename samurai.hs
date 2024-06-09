import Distribution.Fields (PError)

data Elemento = Elemento
  { tipo :: String,
    ataque :: (Personaje -> Personaje),
    defensa :: (Personaje -> Personaje)
  }

data Personaje = Personaje
  { nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
  }

modificarAnio :: Int -> Personaje -> Personaje
modificarAnio anio personaje = personaje {anioPresente = anio}

modificarSalud :: Float -> Personaje -> Personaje
modificarSalud saludAgregar personaje = personaje {salud = verificarSiEsNegativoSalud (salud personaje) saludAgregar}

verificarSiEsNegativoSalud :: Float -> Float -> Float
verificarSiEsNegativoSalud saludPersonaje saludAgregar
  | saludPersonaje + saludAgregar > 0 = saludPersonaje + saludAgregar
  | otherwise = 0

mandarAlanio :: Int -> Personaje -> Personaje
mandarAlanio anio personaje = modificarAnio anio personaje

meditar :: Personaje -> Personaje
meditar personaje = modificarSalud (salud personaje / 2) personaje

causarDanio :: Float -> Personaje -> Personaje
causarDanio cantiDanio personaje = modificarSalud (-cantiDanio) personaje

-- danioQueProduce :: Personaje -> Elemento -> Float, que retorne la diferencia entre la salud
-- inicial del personaje y la salud del personaje luego de usar el ataque del elemento sobre él.

esMalvado :: Personaje -> Bool
esMalvado personaje = any (tieneElemento "Maldad") (elementos personaje)

tieneElemento :: String -> Elemento -> Bool
tieneElemento elementoBuscado elemento = (== elementoBuscado) . tipo $ elemento

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje)

type Enemigos = [Personaje]

enemigosMortales :: Personaje -> Enemigos -> Enemigos
enemigosMortales personaje enemigos = filter (esEnemigoMortal personaje) enemigos

esEnemigoMortal :: Personaje -> Personaje -> Bool
esEnemigoMortal personaje enemigo = any (tieneAtaqueMortal personaje) (elementos enemigo)

tieneAtaqueMortal :: Personaje -> Elemento -> Bool
tieneAtaqueMortal personaje elemento = estaMuerto (ataque elemento personaje)

estaMuerto :: Personaje -> Bool
estaMuerto = ((== 0) . salud)

-- Definir concentracion de modo que se pueda obtener un elemento cuyo
-- efecto defensivo sea aplicar meditar tantas veces como el nivel de concentración
-- indicado y cuyo tipo sea "Magia".

noHacerNada :: a -> a
noHacerNada = id

concentracion :: Int -> Elemento
concentracion nivelDeConcentracion =
  Elemento
    { tipo = "Magia",
      ataque = noHacerNada,
      defensa = foldr1 (.) (replicate nivelDeConcentracion meditar) -- por ejemplo (concentracion 3) resultaría en meditar.meditar.meditar
    }

-- equivalente con composición y aplicación parcial para:
-- defensa = (\personaje -> iterate meditar personaje !! nivelDeConcentracion) }
-- otra versión super interesante:
-- defensa = foldr1 (.) (replicate nivelDeConcentracion meditar)
-- por ejemplo (concentracion 3) resultaría en meditar.meditar.meditar

-- Definir esbirrosMalvados que recibe una cantidad y retorna una lista con esa cantidad de esbirros
-- (que son elementos de tipo “Maldad” cuyo efecto ofensivo es causar un punto de daño).
esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad unEsbirro

unEsbirro :: Elemento
unEsbirro = Elemento "Maldad" (causarDanio 1) noHacerNada

-- Definir jack de modo que permita obtener un personaje que tiene 300 de salud,
-- que tiene como elementos concentración nivel 3 y
-- una katana mágica (de tipo "Magia" cuyo efecto ofensivo es causar 1000 puntos de daño) y vive en el año 200.

jack :: Personaje
jack =
  Personaje
    { nombre = "Jack",
      salud = 300,
      elementos = [concentracion 3, katanaMagica],
      anioPresente = 200
    }

katanaMagica :: Elemento
katanaMagica = Elemento "Magico" (causarDanio 1000) noHacerNada

aku :: Int -> Float -> Personaje
-- Este lo salteamos en clase por falta de tiempo, agrego una solución
-- Como se puede ver aku es recursivo porque tiene portalAlFuturoDesde como elemento
-- cuya defensa es generar un nuevo aku en otro año
aku anio saludInicial =
  Personaje
    { nombre = "Aku",
      salud = saludInicial,
      anioPresente = anio,
      elementos = concentracion 4 : portalAlFuturoDesde anio : esbirrosMalvados (100 * anio)
    }

portalAlFuturoDesde :: Int -> Elemento
portalAlFuturoDesde anio = Elemento "Magia" (mandarAlanio (anio + 2800)) (aku (anio + 2800) . salud)

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
  | estaMuerto atacante = (defensor, atacante)
  | otherwise = luchar proximoAtacante proximoDefensor
  where
    proximoAtacante = usarElementos ataque defensor (elementos atacante)
    proximoDefensor = usarElementos defensa atacante (elementos atacante)

-- Abstraemos cómo hacer para usar uno de los efectos de un conjunto de elementos sobre un personaje
usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos funcion personaje elementos = foldl afectar personaje (map funcion elementos)

afectar :: t1 -> (t1 -> t2) -> t2
afectar personaje funcion = funcion personaje

afectar' :: b -> (b -> c) -> c
afectar' = flip ($)

f :: (Eq t1, Num t2) => (t1 -> a1 -> (a2, a2)) -> (t2 -> t1) -> t1 -> [a1] -> [a2]
f x y z
  | y 0 == z = map (fst . x z)
  | otherwise = map (snd . x (y 0))