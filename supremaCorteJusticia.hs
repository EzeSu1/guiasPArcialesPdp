-- enunciado https://www.utnianos.com.ar/foro/attachment.php?aid=22360
import Data.List (intersect)

data Ley = Ley
  { tema :: String,
    presupuesto :: Int,
    partidos :: [String]
  }
  deriving (Eq)

-- 1
leyCannabis, leyEducacionSuperior, leyProfesionalizacion, leyTenis :: Ley
leyCannabis = Ley "Uso Medicinal de Cannabis" 5 ["Cambio de Todos"]
leyEducacionSuperior = Ley "Educacion Superior" 30 ["Docentes universitarios", "Partido de Centro Federal"]
leyProfesionalizacion = Ley "Profesionalizacion del Tenis" 1 ["Partido de Centro Federal", "Liga de Deportistas Autonomos", "Club Paleta Veloz"]
leyTenis = Ley "Tenis" 2 ["Liga de Deportistas Autonomos"]

lasLeyesSonCompatibles :: Ley -> Ley -> Bool
lasLeyesSonCompatibles ley1 ley2 = coincidenEnLosTemas (tema ley1) (tema ley2) && coincidenEnAlgunoPartido (partidos ley1) (partidos ley2)

coincidenEnAlgunoPartido :: [String] -> [String] -> Bool
coincidenEnAlgunoPartido ley1 ley2
  | not . null . intersect ley1 $ ley2 = False
  | otherwise = True

coincidenEnLosTemas :: String -> String -> Bool
coincidenEnLosTemas ley1 ley2
  | ley1 `elem` ley2 = True
  | otherwise = False
