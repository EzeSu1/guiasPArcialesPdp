import Data.Char (isUpper)
import Data.List (genericLength)
import Text.Show.Functions ()

data Plomero = Plomero
  { nombre :: String,
    reparaciones :: [Reparaciones],
    dinero :: Float,
    cajaHerramientas :: Herramientas
  }
  deriving (Show)

type Herramientas = [Herramienta]

data Herramienta = Herramienta
  { nombreHerramienta :: String,
    precio :: Float,
    material :: Material
  }
  deriving (Show)

data Material = Hierro | Madera | Goma | Plastico deriving (Show)

llaveInglesa :: Herramienta
llaveInglesa = Herramienta "Llave Inglesa" 200 Hierro

martillo :: Herramienta
martillo = Herramienta "Martillo" 20 Madera

herramientasInfinitas :: Herramienta -> [Herramienta]
herramientasInfinitas unaHerramienta = iterate aumentarPrecio unaHerramienta

aumentarPrecio :: Herramienta -> Herramienta
aumentarPrecio unaHerramienta = unaHerramienta {precio = precio unaHerramienta + 1}

mario :: Plomero
mario = Plomero "Mario" [] 1200 [llaveInglesa, martillo]

wario :: Plomero
wario = Plomero "Wario" [] 0.50 (herramientasInfinitas llaveInglesa)

-- 2
tieneLaHerramienta :: String -> Plomero -> Bool
tieneLaHerramienta nombre unPlomero = any (buscarHerramienta nombre) (cajaHerramientas unPlomero)

buscarHerramienta :: String -> Herramienta -> Bool
buscarHerramienta nombreBuscado unaHerramienta = (== nombreBuscado) . (take (length nombreBuscado)) . nombreHerramienta $ unaHerramienta

esMalvado :: Plomero -> Bool
esMalvado unPlomero = (== "Wa") . (take 2) $ nombre unPlomero

puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar unaHerramienta unPlomero = precio unaHerramienta < dinero unPlomero

-- 3Saber si una herramienta es buena, cumpliendose solamente si tiene empuñadura
-- de hierro que sale más de $10000 o es un martillo con mango de madera o goma.
type Requisito = Herramienta -> Bool

esBuenalaHerremienta :: Herramienta -> Bool
esBuenalaHerremienta unaHerramienta = tieneUnMangoQueSaleTanto unaHerramienta || esUnMartilloConGomaOMadera unaHerramienta

esUnMartilloConGomaOMadera :: Requisito
esUnMartilloConGomaOMadera (Herramienta "Martillo" _ Goma) = True
esUnMartilloConGomaOMadera (Herramienta "Martillo" _ Madera) = True
esUnMartilloConGomaOMadera (Herramienta _ _ _) = False

tieneUnMangoQueSaleTanto :: Requisito
tieneUnMangoQueSaleTanto (Herramienta _ precio Hierro) = precio >= 10000
tieneUnMangoQueSaleTanto _ = False

-- 4Todo plomero necesita comprar una herramienta, cuando lo hace paga su precio y agrega la herramienta a las suyas. Solo sucede si puede pagarlo.

comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta unaHerramienta unPlomero
  | puedeComprar unaHerramienta unPlomero = agregarLaCajaDeHerramientas unaHerramienta . actualizarlaCartera (-precio unaHerramienta) $ unPlomero
  | otherwise = error "No alcanza"

actualizarlaCartera :: Float -> Plomero -> Plomero
actualizarlaCartera precio unPlomero = unPlomero {dinero = dinero unPlomero + precio}

agregarLaCajaDeHerramientas :: Herramienta -> Plomero -> Plomero
agregarLaCajaDeHerramientas unaHerramienta unPlomero = unPlomero {cajaHerramientas = unaHerramienta : cajaHerramientas unPlomero}

-- 5

data Reparaciones = Reparaciones
  { descripcion :: String,
    requerimiento :: Requerimiento
  }
  deriving (Show)

type Requerimiento = Plomero -> Bool

filtracionDeAgua :: Reparaciones
filtracionDeAgua = Reparaciones "Filtracion de agua" (tieneLaHerramienta "LLave Inglesa")

todasMayuscula :: String -> Bool
todasMayuscula descripcion = all isUpper descripcion

tieneMasde100 :: String -> Bool
tieneMasde100 descripcion = (>= 100) . length $ descripcion

esDificilLaRearacion :: Reparaciones -> Bool
esDificilLaRearacion unaReparacion = tieneMasde100 (descripcion unaReparacion) && todasMayuscula (descripcion unaReparacion)

saberPresupuesto :: Reparaciones -> Float
saberPresupuesto unaReparacion = (* 3) . genericLength $ (descripcion unaReparacion)

destornillador :: Herramienta
destornillador = Herramienta "Destonillador" 0 Plastico

intentarhacerUnaReparacion :: Plomero -> Reparaciones -> Plomero
intentarhacerUnaReparacion unPlomero unReparacion
  | requerimiento unReparacion unPlomero = hacerReparacion unReparacion unPlomero
  | otherwise = actualizarlaCartera 100 unPlomero

agregarReparacion :: Reparaciones -> Plomero -> Plomero
agregarReparacion reparacion plomero = plomero {reparaciones = reparacion : reparaciones plomero}

hacerReparacion :: Reparaciones -> Plomero -> Plomero
hacerReparacion unReparacion unPlomero = actualizarlaCartera 100 . agregarReparacion unReparacion . efectosporTipoPlomero unReparacion $ unPlomero

sacarPrimerElemento :: Plomero -> Herramienta
sacarPrimerElemento unPlomero = head (cajaHerramientas unPlomero)

sacarBuenas :: Plomero -> Herramientas
sacarBuenas unPlomero = filter esBuenalaHerremienta (cajaHerramientas unPlomero)

efectosporTipoPlomero :: Reparaciones -> Plomero -> Plomero
efectosporTipoPlomero unReparacion unPlomero
  | esMalvado unPlomero = agregarLaCajaDeHerramientas destornillador unPlomero
  | esDificilLaRearacion unReparacion = agregarLaCajaDeHerramientas (sacarBuenas unPlomero) unPlomero
  | otherwise = agregarLaCajaDeHerramientas (sacarPrimerElemento unPlomero) unPlomero

-- 7
hacerUnaJornadaLaboral :: Reparaciones -> Plomero -> Plomero
hacerUnaJornadaLaboral reparaciones plomero = foldl intentarHacerReparacion plomero reparaciones

-- 8
type Plomeros = [Plomero]

empleadosDespdeUnajornadaLaboral :: Plomeros -> Reparaciones -> Plomeros
empleadosDespdeUnajornadaLaboral plomeros reparaciones = map (hacerUnaJornadaLaboral reparaciones) plomeros

type Criterio = Plomero -> Float

elMasReparador :: Criterio
elMasReparador unPlomero = cantidadTotal (reparaciones unPlomero)

elMasInvertido :: Criterio
elMasInvertido unPlomero = cantidadTotal (cajaHerramientas unPlomero)

cantidadTotal :: [a] -> Float
cantidadTotal listadelPlomero = genericLength listadelPlomero

sacarSegunCriterio :: Criterio -> Plomeros -> Plomero
sacarSegunCriterio _ [] = error "Error"
sacarSegunCriterio _ [plomero] = plomero
sacarSegunCriterio criterio (plomero1 : plomero2 : plomero3)
  | criterio plomero1 > criterio plomero2 = sacarSegunCriterio criterio (plomero1 : plomero3)
  | otherwise = sacarSegunCriterio criterio (plomero2 : plomero3)

elempleadoMasReparador :: Plomeros -> Reparaciones -> Plomero
elempleadoMasReparador plomeros reparaciones = sacarSegunCriterio elMasReparador . (empleadosDespdeUnajornadaLaboral plomeros) $ reparaciones

elEmpleadoMasAdinerado :: Plomeros -> Reparaciones -> Plomero
elEmpleadoMasAdinerado plomeros reparaciones = sacarSegunCriterio dinero (empleadosDespdeUnajornadaLaboral plomeros reparaciones)

elEmpleadoQueMasInvertio :: Plomeros -> Reparaciones -> Plomero
elEmpleadoQueMasInvertio plomeros reparaciones = sacarSegunCriterio elMasInvertido (empleadosDespdeUnajornadaLaboral plomeros reparaciones)
