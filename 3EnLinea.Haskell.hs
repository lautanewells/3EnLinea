import Data.Char (isNumber)
import Data.List (transpose)
import Data.Text (justifyLeft, pack, unpack)
type Tablero = [[Char]]

-- algoritmo para prevenir empates: https://stackoverflow.com/questions/46921528/detect-early-tie-in-connect-4
-- algoritmo para bot inteligente: https://es.wikipedia.org/wiki/Minimax

main :: IO()
main = do
    separador
    separador
    putStrLn "Proyecto Final de Algoritmos y Estructura de Datos - UCA 2022"
    putStrLn "Ludueña Saavedra Luciano"
    putStrLn "Ojeda Lautaro"
    putStrLn "Ojeda Leandro"
    separador
    separador
    -- para jugar a "X en linea", descomentar la siguiente linea
    -- putStrLn "Ingrese la cantidad de fichas consecutivas para ganar: (mayor a 2)"
    -- cantFichasSeguidas <- getLine
    putStrLn "Ingrese la altura del tablero (mayor a 2): "
    ingresoAltura <- getLine
    putStrLn "Ingrese el ancho del tablero (mayor a 2):"
    ingresoAncho <- getLine
    -- valida que el ancho/alto sea mayor o igual a 3
    if not (esValido ingresoAltura False) || not (esValido ingresoAncho False) then do
        putStrLn "Tablero Inválido. Intente nuevamente"
        main
    else do
        let ancho = read ingresoAncho :: Int
        let altura = read ingresoAltura :: Int
        -- para jugar a "X en linea", descomentar la siguiente linea
        -- let condicionVictoria = read cantFichasSeguidas :: Int
        let tablero = crearTablero ancho
        printTablero tablero altura
        -- para jugar a "X en linea", descomentar la siguiente linea
        -- movimientoJugador 'X' tablero ancho altura condicionVictoria
        movimientoJugador 'X' tablero ancho altura 3

movimientoJugador :: Char -> Tablero -> Int -> Int -> Int -> IO ()
movimientoJugador ficha tablero anchoMaximo alturaMaxima condicionVictoria = do
    putStrLn ("Jugando "++show condicionVictoria++" en Línea")
    putStrLn ("Turno del jugador " ++ [ficha] ++ ": (Ingrese -1 para terminar el programa):")
    ingreso <- getLine
    -- gracias Profe por esValido
    if not (esValido ingreso True) then do
                                        putStrLn ("Comando desconocido del Jugador " ++ [ficha] ++ ". Intente nuevamente")
                                        movimientoJugador ficha tablero anchoMaximo alturaMaxima condicionVictoria
    else do
        -- es valor numérico
        let col = read ingreso :: Int
        if col == -1 then
            return ()
        else do
            if col <= 0 || col > anchoMaximo then do
                putStrLn ("Movimiento inválido del jugador " ++ [ficha] ++ ". Intente nuevamente")
                movimientoJugador ficha tablero anchoMaximo alturaMaxima condicionVictoria
            else do
                let largo = largoColumna col tablero
                if (largo+1) > alturaMaxima then do
                    putStrLn ("Movimiento inválido del jugador (Columna llena) " ++ [ficha] ++ ". Intente nuevamente")
                    movimientoJugador ficha tablero anchoMaximo alturaMaxima condicionVictoria
                else do
                    let tableroModificado = actualizarTablero tablero col ficha
                    printTablero tableroModificado alturaMaxima
                    if haGanado tableroModificado col ficha condicionVictoria then do
                        mensajeFinal ("El jugador "++[ficha]++" ha ganado la partida!!!")
                        jugarOtraVez
                    else if haEmpatado tableroModificado anchoMaximo alturaMaxima  then do
                        mensajeFinal "No se pueden ingresar nuevas fichas en el tablero. El juego terminó en empate :/"
                        jugarOtraVez
                    else do
                        if ficha == 'X' then
                            movimientoJugador 'O' tableroModificado anchoMaximo alturaMaxima condicionVictoria
                        else
                            movimientoJugador 'X' tableroModificado anchoMaximo alturaMaxima condicionVictoria

jugarOtraVez :: IO ()
jugarOtraVez = do
    putStrLn "Desea jugar otra vez? S/N (se interpreta un valor distinto a S como interés en terminar la ejecución)"
    ingreso <- getLine
    if ingreso == "S" || ingreso == "s" then main
    else putStrLn "Juego Terminado"

crearTablero :: Int -> Tablero
crearTablero ancho =  [[] | x <- [1..ancho]]

largoColumna :: Int -> Tablero -> Int
largoColumna col tablero = length (tablero !! (col -1))

dibujarColNum :: Int -> Char
dibujarColNum x = if mod x 2 == 1 then head (show (div (x + 1) 2) ) else ' '

imprimirFila :: Int -> Tablero -> IO()
imprimirFila fila tablero = putStrLn ('|':concat ([ dibujarColumnas columna fila | columna <- tablero ]))

dibujarColumnas :: [Char] -> Int -> [Char]
dibujarColumnas columna fila = [ if length columna >= fila then columna!!(fila-1) else ' ', '|' ]

cantDeFichasEnConMasFichas :: Tablero -> Int
cantDeFichasEnConMasFichas tablero = maximum [ length columna | columna <- tablero ]

printTablero :: Tablero -> Int -> IO()
printTablero tablero 0 = do
    putStrLn [ '-' | x <- espacioDibujable tablero]
    putStrLn [ dibujarColNum x | x <- espacioDibujable tablero]
printTablero tablero fila = do
    imprimirFila fila tablero
    printTablero tablero (fila-1)

espacioDibujable :: Tablero -> [Int]
espacioDibujable tablero = [0..length tablero * 2]

actualizarTablero :: Tablero -> Int -> Char -> Tablero
actualizarTablero tablero i a = [ if c == i then columna ++ [a] else columna | (columna,c) <- zip tablero  [1..length tablero] ]

haEmpatado :: Tablero -> Int -> Int -> Bool
haEmpatado tablero ancho altura = length (concat tablero) == ancho * altura

separador :: IO ()
separador = putStrLn "------------------"

mensajeFinal :: String -> IO ()
mensajeFinal msj = do
    separador
    separador
    putStrLn msj
    separador
    separador


hayXenLinea :: [Char] -> Int -> Char -> Int -> Bool
hayXenLinea [] _ _ _ = False
hayXenLinea (x:linea) fichasSeguidas ficha condicionVictoria =
    if x == ficha then
        if (fichasSeguidas+1) == condicionVictoria then True
        else hayXenLinea linea (fichasSeguidas+1) ficha condicionVictoria
    else hayXenLinea linea 0 ficha condicionVictoria


esValido :: [Char] -> Bool -> Bool
esValido "" _ = False
-- si es True acepto -1 xq es usada en movimientoJugador
esValido "-1" True = True
-- si es False rechazo 0 1 y 2 por ser menores a 3 ya que es usada en main
esValido "0" False = False
esValido "1" False = False
esValido "2" False = False
esValido [x] bool = isNumber x
esValido (x:xs) bool  | isNumber x = esValido xs bool
                      | otherwise = False

haGanado :: Tablero -> Int -> Char -> Int -> Bool
haGanado tablero col ficha condicionVictoria = do
    -- let coords = encontrarCoordenadas col tablero
    let winByColumna = hayXenLinea (tablero!!(col-1)) 0 ficha condicionVictoria
    let winByFila = or [hayXenLinea  x 0 ficha condicionVictoria | x <- fillYFlipTablero tablero]
    -- let winByDiagonal = superAraña coords tablero
    -- evalua si cualquier diagonal del programa es ganadora 
    let winByAnyDiagonal = or (evalDiagonales (fillYFlipTablero tablero) ficha condicionVictoria)
    winByColumna || winByFila || winByAnyDiagonal

evalDiagonales :: Tablero -> Char -> Int -> [Bool]
evalDiagonales tablero ficha condicionVictoria =
     [hayXenLinea  x 0 ficha condicionVictoria | x <- obtenerDiagonales tablero]
     ++
     [hayXenLinea  x 0 ficha condicionVictoria | x <- obtenerDiagonales (flipTablero tablero)]

flipTablero :: Tablero -> Tablero
flipTablero = transpose . map reverse

-- TODO hacer un justifyLeft sobre strings para no usar pack y unpack
fillTablero :: Tablero -> Tablero
fillTablero tablero = [unpack (justifyLeft (cantDeFichasEnConMasFichas tablero) '#' (pack x)) | x <- tablero]

fillYFlipTablero :: Tablero -> Tablero
fillYFlipTablero tablero = flipTablero (fillTablero tablero)

obtenerDiagonales :: Tablero -> [[Char]]
obtenerDiagonales []       = []
obtenerDiagonales ([]:xss) = xss
obtenerDiagonales xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat [])
                                  ([]:obtenerDiagonales (map tail xss))