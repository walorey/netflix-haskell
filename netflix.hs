data Serie = UnaSerie { 
    nombre :: String, 
    genero :: String,
    duracion :: Int,
    cantTemporadas :: Int,
    calificaciones :: [Int],
    esOriginalDeNetflis :: Bool
} deriving (Eq, Show)

tioGolpetazo = UnaSerie {
    nombre = "One punch man",
    genero = "Monito chino",
    duracion = 24,
    cantTemporadas = 1,
    calificaciones = [5,3,4,5,4],
    esOriginalDeNetflis = False
}

cosasExtranias = UnaSerie {
    nombre = "Strangers things",
    genero = "Misterio",
    duracion = 50,
    cantTemporadas = 2,
    calificaciones = [3,3],
    esOriginalDeNetflis = True
}

dbs = UnaSerie {
    nombre = "Dragon ball supah",
    genero = "Monito chino",
    duracion = 150,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False
}

espejoNegro = UnaSerie {
    nombre = "Black mirrow",
    genero = "Suspenso",
    duracion = 123,
    cantTemporadas = 4,
    calificaciones = [2],
    esOriginalDeNetflis = True
}

rompiendoMalo = UnaSerie {
    nombre = "Breaking Bad",
    genero = "Drama",
    duracion = 200,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False
}

treceRazonesPorque = UnaSerie {
    nombre = "13 reasons why",
    genero = "Drama",
    duracion = 50,
    cantTemporadas = 1,
    calificaciones = [3,3,3],
    esOriginalDeNetflis = True
}

-- PARTE 1

-- 1 Crear maraton

maratonAnime = [tioGolpetazo, dbs]

-- 2 Saber la cantidad de series del maraton

cuantasSeriesMaraton maraton = length maraton

-- 3 Saber si una serie es popular

esPopular serie = length (calificaciones serie) == 3 || length (calificaciones serie) > 3

-- 4 Saber si la serie vale la pena

serieValeLaPena serie = (cantTemporadas serie) > 1 && esPopular serie

-- 5 Saber si un maraton vale la pena

maratonValeLaPena maraton = (serieValeLaPena (head maraton) && serieValeLaPena (last maraton)) || elem (rompiendoMalo) maraton

-- 6 Saber si un maraton repunta al final

maratonRepunta maraton = not (maratonValeLaPena (primerMitad maraton) ) && maratonValeLaPena (segundaMitad maraton)

primerMitad maraton = take (length maraton `div` 2) maraton

segundaMitad maraton = drop (length maraton `div` 2) maraton

-- 7 calcular calificaciones

promediarCalificacion serie | length (calificaciones serie) ==0 = 0
                |otherwise = (sum (calificaciones serie)) `div` (length (calificaciones serie))

-- 8 obtener dispersion

dispersion serie | (promediarCalificacion serie) == 0 = 0
                 |otherwise = (maximum (calificaciones serie)) - (minimum (calificaciones serie))


-- 9 calificar serie

calificar serie calificacion = (calificaciones serie) ++ [calificacion]


-- 10 hypear serie

medioCalificaciones puntuaciones = (init.tail) puntuaciones

hypear serie | elem 1 (calificaciones serie) = (calificaciones serie)
             |otherwise = [(head (calificaciones serie)) +2] ++ medioCalificaciones(calificaciones serie) ++ [(last(calificaciones serie) +2)]







