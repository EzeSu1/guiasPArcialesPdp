type Material = String

type Materiales = [String]

data Personaje = UnPersonaje
  { nombre :: String,
    puntaje :: Int,
    inventario :: Materiales
  }
  deriving (Show)

-- Craft:

-- Punto 1:

data Receta = Receta
  { nombreDelMaterial :: Material,
    materialesNecesarios :: Materiales,
    segundosEnCraftear :: Int
  }
  deriving (Show)

fogata, polloasado, sueter:: Receta 
fogata = Receta "fogata" ["madera", "fosforo"] 10
polloasado = Receta "pollo asado" ["fogata", "pollo"] 300
sueter = Receta "sueter" ["lana","agujas", "tinturas"] 600

craftearObjeto :: Receta -> Personaje -> Personaje
craftearObjeto receta personaje
    |tieneLosMaterialesNecesarios (materialesNecesarios receta) (inventario personaje)= modificarPersonaje (personajeSinMaterialesPorLaReceta personaje (materialesNecesarios receta)) receta
    |otherwise = modificarPuntos (-10) personaje

tieneLosMaterialesNecesarios :: Materiales -> Materiales -> Bool
tieneLosMaterialesNecesarios materialesnecesario materialesPersonaje = all (`elem` materialesPersonaje ) materialesnecesario
modificarPuntos :: Int -> Personaje -> Personaje
modificarPuntos puntosModificar personaje = personaje{puntaje = puntaje personaje - puntosModificar}

agregarInventario :: Material -> Personaje -> Personaje
agregarInventario material personaje= personaje{inventario = material : inventario personaje}

personajeSinMaterialesPorLaReceta :: Personaje -> Materiales -> Personaje
personajeSinMaterialesPorLaReceta personaje materiales = foldl sacarMaterialNecesarioDelInventario personaje materiales

modificarPersonaje :: Personaje -> Receta -> Personaje
modificarPersonaje personaje receta = modificarPuntos (10 * segundosEnCraftear receta) . agregarInventario (nombreDelMaterial receta) $ personaje

sacarMaterialNecesarioDelInventario :: Personaje -> Material -> Personaje

sacarMaterialNecesarioDelInventario personaje materialNecesario = personaje {inventario = quitarmaterialUsado materialNecesario (inventario personaje)}
quitarmaterialUsado :: Material -> Materiales -> Materiales
quitarmaterialUsado _ []= []
quitarmaterialUsado materialUsado (material : materiales)
    | material == materialUsado = materiales
    | otherwise = material : quitarmaterialUsado materialUsado materiales


----2222222222----

recetasQueDuplicanPuntaje :: [Receta]-> Personaje -> [Receta]
recetasQueDuplicanPuntaje recetas jugador = filter (siDuplicaPuntaje jugador) recetas

siDuplicaPuntaje :: Personaje -> Receta -> Bool
siDuplicaPuntaje jugador receta = puntaje (craftearObjeto receta jugador) >= 2 * puntaje jugador
craftearTodasLasRecetas :: [Receta] -> Personaje -> Personaje
craftearTodasLasRecetas recetas personaje = foldl (flip craftearObjeto) personaje recetas

craftearAlReves :: [Receta] -> Personaje -> Bool
craftearAlReves recetas jugador = puntaje (craftearTodasLasRecetas recetas jugador) < puntaje (craftearTodasLasRecetas (reverse recetas) jugador)

----333333

type Herramienta = Bioma -> Material

type Condicion = Materiales -> Bool

data Bioma = UnBioma
  { nombreDeBioma :: String,
    materialesDelBioma :: Materiales,
    condicionesParaMinar :: Condicion
  }

artico :: Bioma
artico = UnBioma "Artico" ["iglu", "hielo", "agua congelada"] (tenerElementoEnElInventario "sueter")

tenerElementoEnElInventario :: String -> Condicion
tenerElementoEnElInventario elementoRequirido materiales = elementoRequirido `elem` materiales

hacha :: Herramienta
hacha bioma = last (materialesDelBioma bioma)

espada :: Herramienta
espada bioma = head (materialesDelBioma bioma)

pico :: Int -> Herramienta
pico posicion bioma = obtenerMaterialEnBasePosicion posicion (materialesDelBioma bioma)

obtenerMaterialEnBasePosicion :: Int -> Materiales -> Material
obtenerMaterialEnBasePosicion posicion materialesDelBioma = materialesDelBioma !! posicion

minar :: Herramienta -> Bioma -> Personaje -> Personaje
minar herramienta bioma jugador
    | condicionesParaMinar bioma (inventario jugador) = (modificarPuntos 50 . agregarMaterial (herramienta bioma)) jugador
    | otherwise = jugador

agregarMaterial :: Material -> Personaje -> Personaje
agregarMaterial materialNuevo personaje = personaje {inventario = materialNuevo : inventario personaje}

