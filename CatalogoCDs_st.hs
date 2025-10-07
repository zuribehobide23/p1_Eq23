-------------------------------------------------------------------------------
--  PRACTICA: Gestion de Catalogo de Contenido Digital     
--  PF  2025-2026

--  Num. del equipo registrado en la egela: Eq_23
-- Apellidos del primer integrante: Amunarriz Queiruga
-- Apellidos del segundo integrante: Behobide Zabalegui  
-------------------------------------------------------------------------------
-- GRUPO C: Desarrollo sobre Series
-------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
module CatalogoCD where
-- import Data.XXX

type Serie = (Titulo, NTemporadas, EpisodiosXTemporada, DuracionM, GeneroS, Edad )
type Titulo = [Char]
data GeneroS = Accion | Animacion | Comedia | Drama | Documental | SciFic | Suspense | Romance | Terror deriving (Show, Eq)
type Edad = Int                -- edad minima para consumir el contenido 
type NTemporadas = Int
type EpisodiosXTemporada = Int -- promedio
type DuracionM = Int           -- minutos, promedio de duracion de los episodios

-- ====================================
-- Funciones extractoras y auxiliares BASICAS: Series
-- ====================================

-- Extrae el titulo de la serie.
getTituloS :: Serie -> Titulo
getTituloS (x,_,_,_,_,_) = x

-- Extrae el num. de temporadas.
getTemporadas :: Serie -> NTemporadas
getTemporadas (_,x,_,_,_,_) = x

-- Extrae el num. de episodios
getEpisodiosXT :: Serie -> EpisodiosXTemporada
getEpisodiosXT (_,_,x,_,_,_) = x

-- Extrae la duracion por episodio.
getDuracionEp :: Serie -> DuracionM
getDuracionEp (_,_,_,x,_,_) = x

-- Extrae el genero de la serie AV.
getGeneroS :: Serie -> GeneroS
getGeneroS (_,_,_,_,x,_) = x

-- Extrae la edad minima recomendada.
getEdad :: Serie -> Edad
getEdad (_,_,_,_,_,x) = x

-- Titulo, Nº de Temporadas, y Edad minima de la serie, seguido de salto de linea
printSerie :: Serie -> String
printSerie (t,n,_,_,_,e) = t ++ "\n" ++ show (n) ++ "\n" ++ show (e) ++ "\n"

-- Imprime la lista completa de canciones (playlist), formateada
printSeries :: [Serie] -> IO ()
printSeries xs = putStr (concat (map printSerie xs))

{-
-- Implementacion del quicksort por clave
qsortBy :: Ord b => (a -> b) -> [a] -> [a]
-}


-- ====================================
-- Funciones principales sobre Series
-- ====================================

-- 1
-- Dado un listado de series, calcula el numero de series por genero
-- incluido en el mismo
contarNumSeriesXGenero:: [Serie]->[(GeneroS, Int)]
contarNumSeriesXGenero xs = foldl (\acc s -> encuentraGen s acc) [] xs

--2	

-- Dada la edad y un listado de series, selecciona todas las series cuya edad
-- recomendada sea igual o superior a la dada
seriesParaMayoresDe:: Edad -> [Serie]-> [Serie]
seriesParaMayoresDe e xs = filter (\(_,_,_,_,_,edad) -> edad >= e) xs



-- 3
-- Dado un numero de temporadas y un listado de series, extrae los títulos de 
-- las series que tienen a los sumo ese numero de temporadas
titulosSconPocasTemporadas:: NTemporadas -> [Serie] -> [Titulo]
titulosSconPocasTemporadas n xs = map getTituloS (filter (\s -> getTemporadas s == n) xs)


-- 4
-- Dado n el numero de series, dm la duracion maxima en minutos y un listado de 
-- series, selecciona n series del listado con duracion menor o igual a dm 
miSeleccionDeSeriesMasCortasQue:: Int -> DuracionM -> [Serie]-> [Serie]
miSeleccionDeSeriesMasCortasQue n dm xs = take n (filter (\s -> getDuracionEp s <= dm) xs)
--no se si se puede usar el take


-- 5
-- Dado un listado de series, determina la duración total (en minutos)
-- de todos los episodios de todas sus temporadas
totalMinutosCatalogo:: [Serie] -> DuracionM
totalMinutosCatalogo xs = foldl (\acc s -> acc + getDuracionEp s * getEpisodiosXT s) 0 xs


-- 6
-- Dado un listado de series, identifica el genero (de series) con el más series
generoSMasProlifico:: [Serie] -> GeneroS 
generoSMasProlifico xs = devolverMax (contarNumSeriesXGenero xs)

{-
-- 7	
-- Listado de series ordenado decrecientemente por número total de episodios
rankingSeriesPorNumTotalEpisodios:: [Serie] -> [(Serie, Int)]



-- 8 	
-- Listado de series ordenado crecientemente por duración total (en minutos), 
-- considerando todos los episodios de todas sus temporadas
rankingSeriesMasBreves:: [Serie]-> [(Serie, Int)]

-- 9
-- Dado un listado de series, identifica los generos (de serie) que NO estan 
-- representados (que faltan) con respecto al conjunto completo de generos definidos
generosSerieSinRepresentacion :: [Serie]->[GeneroS]

-}

-- =============================
-- Resto de funciones auxiliares (para gestionar el catalogo de series)
-- ============================
encuentraGen :: Serie -> [(GeneroS, Int)] -> [(GeneroS, Int)]
encuentraGen serie [] = [(getGeneroS serie, 1)]
encuentraGen serie ((g, n):xs)
    | g == getGeneroS serie = (g, n + 1) : xs
    | otherwise = (g, n) : encuentraGen serie xs


devolverMax :: [(GeneroS, Int)] -> GeneroS
devolverMax [] = error "Tiene q haber al menos una serie"
devolverMax xs = fst $ foldl1 (\acc x -> if snd x > snd acc then x else acc) xs
--No se si se puede usar el fst

-- ======================================
-- Catalogos/Listados de ejemplos: Datos de prueba de series
-- ======================================

s1 = ("El verano en el que me volvi bonita", 3, 9, 55, Romance, 16) :: Serie
s2 = ("Stranger Things", 5, 8, 80, Terror, 16) :: Serie
s3 = ("Paquita Salas", 3, 5, 30, Comedia, 12) :: Serie
s4 = ("Euphoria", 2, 8, 55, Drama, 18) :: Serie
s5 = ("Riverdale", 7, 20, 45, Drama, 16) :: Serie

misSeries::[Serie]
misSeries = [s1,s2,s3,s4,s5]


