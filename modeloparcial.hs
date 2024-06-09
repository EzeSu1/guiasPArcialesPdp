import Data.List




data Sustancia = SustanciaSencilla Elementos | SustanciaCompuestos{
  
    nombreCompuestos :: String,
    componente :: [Componente],
    simbolo :: String,
    grupo1 :: Grupo

}deriving(Show, Eq)

data Elementos= Elementos{
    nombreSencillo :: String,
    simboloQuimico :: String,
    nroAtomico :: Int,
    grupo :: Grupo
}deriving(Show, Eq)

data Grupo =  Metal | NoMetal | Halogeno | GasNoble deriving(Show, Eq)

data Componente = Componente{
    sustancia :: Sustancia,
    cantidadMoleculas:: Int
}deriving(Show, Eq)

--1
hidrogeno :: Sustancia 
hidrogeno = SustanciaSencilla (Elementos "hidrogeno" "H" 1 NoMetal)
        

oxigeno :: Sustancia
oxigeno =  SustanciaSencilla (Elementos "oxigeno" "O" 8 NoMetal)

agua :: Sustancia
agua = SustanciaCompuestos "agua" [Componente hidrogeno 2, Componente  oxigeno 1 ] "h2o" NoMetal

--2
{-conduceBien :: Sustancia -> String
conduceBien xsustancia
        |
        |
        |otherwise = "No conduce bien"
    where
        grupoSustancia xsustancia
            |
            |otherwise = -}
electricidad :: [Grupo]
electricidad = [Metal, GasNoble]
calor :: [Grupo]
calor = [Metal, Halogeno]

conduceBien :: Sustancia -> String
conduceBien (SustanciaSencilla (Elementos _ _ _ Metal))= "Conduce bien electricidad y calor"
conduceBien (SustanciaCompuestos _ _ _ Metal)= "Conduce bien cualquiera de los criterios"
conduceBien (SustanciaSencilla (Elementos _ _ _ GasNoble)) = "Conduce bien electricidad"
conduceBien (SustanciaSencilla (Elementos _ _ _ Halogeno)) = "Conduce bien calor"
conduceBien (SustanciaCompuestos _ _ _ Halogeno) = "Conduce bien calor"
conduceBien _ = "No es buen conductor"

--3

funcionUro :: String -> String
funcionUro sustancia 
        |not.esVocal.last $ sustancia = sustancia ++ "uro"
        |otherwise = reverse (dropWhile esVocal (reverse sustancia))++ "uro"
            where 
                esVocal sustancia=  sustancia `elem` "aeiou"

unionNombres :: Sustancia -> String
unionNombres (SustanciaCompuestos nombreCompuestos1 _ _ _ ) = (funcionUro nombreCompuestos1) 
unionNombres (SustanciaSencilla (Elementos nombreSencillo1 _ _ _)) = (funcionUro nombreSencillo1) 
--4

combinarNombres :: Sustancia -> Sustancia ->String
combinarNombres sust1 (SustanciaCompuestos nombreC _ _ _) = (unionNombres sust1) ++ " de " ++ nombreC
combinarNombres sust1 (SustanciaSencilla (Elementos nombreS _ _ _) ) = (unionNombres sust1) ++ " de " ++ nombreS
 
--5
obtenerComponente (SustanciaCompuestos _ componente _ _) = componente
obtenerComponente sust1 = [Componente {sustancia= sust1 , cantidadMoleculas= 1 }  ]
obtenerSimbolo (SustanciaCompuestos _ _ simbolo1 _)= simbolo1
obtenerSimbolo (SustanciaSencilla (Elementos _ simbolo2 _ _))= simbolo2


mezclar :: Sustancia -> Sustancia -> Sustancia
mezclar sust1 sust2 = SustanciaCompuestos{
        nombreCompuestos = combinarNombres sust1 sust2,
        componente =(obtenerComponente sust1) ++ obtenerComponente sust2,
        simbolo =(obtenerSimbolo sust1) ++ obtenerSimbolo sust2,
        grupo1 = NoMetal
}

--6

formula (SustanciaSencilla (Elementos _ simbolo0 _ _))= simbolo0
formula (SustanciaCompuestos _ [Componente ( (SustanciaSencilla (Elementos _ simbolo0 _  _))) _] _ _  )= simbolo0
formula (SustanciaCompuestos _ componente _ _ ) = "(" ++ concatMap funcion componente ++ ")"
    where funcion (Componente sustancia cantMoleculas)
            |cantMoleculas ==1 = formula sustancia
            |otherwise = formula sustancia ++ show cantMoleculas
obtenerCantMoleculas :: Sustancia -> Int
obtenerCantMoleculas (SustanciaCompuestos _ [Componente _ cantMoleculas] _ _) = cantMoleculas 
obtenerCantMoleculas _= 1

--(SustanciaCompuestos _ [Componente _ cantMoleculas]_ _) = cantMoleculas