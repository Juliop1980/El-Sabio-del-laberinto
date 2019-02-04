{-|
Module      : Sabio
Descripcion : Permite interactuar con el usuario.
Creadores  : 14-10576 Leonardo Lopez 14-10820 Julio Perez
Este modulo contiene las funciones que reciben rutas e indican 
qué se encuentra al seguirlas.
-}

module Main (
    laberintoNuevo,
    paredAbierta,
    derrumbe,
    tesoroTomado,
    tesoroHallado,
    readLaberinto,
    writeLaberinto,
    main
) where

import Laberinto
import Data.Maybe
import Data.List
import Data.Maybe

-- | Función que reemplaza el laberinto en memoria con un laberinto vacío usando la ruta recibida para poblar el laberinto inicialmente.
laberintoNuevo :: [Direccion] -- ^ Recibe una ruta que puede ser seguida en el laberinto nuevo que debe suponerse terminada en un camino sin salida y no contener tesoros.
               -> Laberinto -- ^ Retorna un laberinto que toma la ruta recibida para poblar el laberinto.
laberintoNuevo [] = deadEnd
laberintoNuevo (x:xs) = connect deadEnd (Just (laberintoNuevo xs) )  x

-- | Función que recibe un laberinto, luego recorre el camino dado hasta alcanzar una pared (un Nothing) y retorna el laberinto alcanzable en esa dirección.
paredAbierta :: Laberinto -- ^ Recibe un laberinto.
             -> [Direccion] -- ^ Recibe un camino a tomar.
             -> Laberinto -- ^ Laberinto alcanzable en la dirección dada.
paredAbierta lab [] = lab
paredAbierta lab (x:xs) = connect lab camino x where
    c = if isNothing siguiente then nuevo else viejo
    camino = Just c
    siguiente = irRuta (Just lab) [x]
    viejo = paredAbierta (fromMaybe deadEnd siguiente) xs
    nuevo = laberintoNuevo xs

-- | Función que se recibe un laberinto y una dirección, se sigue la dirección y se elimina el laberinto alcanzable en la dirección dada.
derrumbe :: Laberinto -- ^ Recibe un laberinto.
         -> [Direccion] -- ^ Recibe un camino.
         -> Direccion -- ^ Recibe una direccion (Izquierda, Derecha, Recto).
         -> Laberinto -- ^ Retorna un nuevo laberinto sin el laberinto alcanzable en la direccion dada, al final del camino tomado.
derrumbe lab [] d = connect lab Nothing d
derrumbe lab (x:xs) d = connect lab camino x where
    camino = if isNothing siguiente then Nothing else Just seguir
    siguiente = irRuta (Just lab) [x]
    seguir = derrumbe (fromMaybe deadEnd siguiente) xs d

-- | Función que recibe un Laberinto y una ruta, reemplaza el tesoro al final de la ruta con el laberinto accesible pasando el tesoro si existe con un booleano indicando si en verdad hay algo despues.
tesoroTomado :: Laberinto -- ^ Recibe un laberinto.
             -> [Direccion] -- ^ Recibe una ruta con el tesoro al final
             -> (Maybe Laberinto, Bool) -- ^ Retorna una dupla con el laberinto que tiene el tesoro reemplazado con el sub-laberinto que  empieza al pasar el tesoro y un booleano indicando si efectivamente este laberinto no es vacio.
tesoroTomado (Tesoro descripcion lab) [] = (lab,True)
tesoroTomado lab [] = (Just lab, False)
tesoroTomado lab (x:xs) = respuesta where
    respuesta = if isNothing siguiente then (Nothing, False)
                else (Just (connect lab camino x) , valido)
    siguiente = irRuta (Just lab) [x]
    seguir = tesoroTomado (fromMaybe deadEnd siguiente) xs
    camino = fst seguir
    valido = snd seguir

-- | Función que carga un laberinto de un archivo.
readLaberinto :: FilePath -- ^ Recibe la direccion del archivo que contiene el laberinto.
              -> IO Laberinto -- ^ Retorna el laberinto leido marcado como IO.
readLaberinto filename = do
    contenido <- readFile filename
    return (fst(head (reads contenido :: [(Laberinto,String)])))

-- | Función que escribe un laberinto a un archivo.
writeLaberinto lab filename = do
    writeFile filename (show lab)

-- | Función que recibe un Laberinto y una ruta. Se agrega el tesoro como alcanzable desde la trifurcación inmediatamente anterior, y el laberinto anteriormente alcanzable desde la misma como alcanzable siguiendo recto desde el tesoro
tesoroHallado :: Laberinto  -- ^ Recibe el laberinto
              -> [Direccion] -- ^ Recibe la ruta del tesoro
              -> [Char] -- ^ Recibe una descripcion del Tesoro
              -> (Maybe Laberinto, Bool) -- ^ Retorna una dupla con el laberinto que tiene las nuevas opciones alcanzables y un booleano que indica si en verdad hay tesoro o no.
tesoroHallado _ [] _ = error "La ruta no puede ser vacía"
tesoroHallado _ _ [] = error "La descripcion no puede ser vacía"
tesoroHallado lab [d] descripcion = if tesoro then (Just lab,False)
                                    else (Just (connect lab t d), True)
    where
    seguir = irRuta (Just lab) [d]
    tesoro = if isNothing seguir then False else esTesoro (fromMaybe deadEnd seguir)
    t = Just (treasure descripcion seguir)
tesoroHallado lab (x:xs) descripcion = respuesta where
    respuesta = if isNothing siguiente then (Nothing, True)
                else if isNothing camino then (Nothing,True)
                     else (Just (connect lab camino x) , valido)
    siguiente = irRuta (Just lab) [x]
    seguir = tesoroHallado (fromMaybe deadEnd siguiente) xs descripcion
    camino = fst seguir
    valido = snd seguir
 
esTesoro (Tesoro _ _) = True
esTesoro _ = False


--Variables globales donde se mantienen los nombres de los laberintos y sus estructuras de datos
miprimerlab = laberintoNuevo [Derecha, Izquierda,Izquierda,Izquierda,Izquierda,Izquierda,Derecha,Derecha,Recto,Recto,Recto,Izquierda,Derecha,Izquierda,Derecha,Recto,Izquierda,Derecha,Recto,Recto,Izquierda]
listaNombresLabs = ["miprimerlab"]
listaRealLabs = [miprimerlab]

--realizarElecciones:: [Char] -> Maybe Laberinto -> IO Laberinto
--funcion que recibe la opcion de generar nuevo laberinto y hace las acciones correspondientes
realizarElecciones "1" Nothing =  do
    putStrLn "Introduzca la ruta que definira el nuevo laberinto:"
    ruta_aux <- getLine
    let ruta = fst(head(reads ruta_aux :: [([Direccion], String)]))
    return $ laberintoNuevo ruta

realizarElecciones "1" _ =  do
    putStrLn "Introduzca la ruta que definira el nuevo laberinto:"
    ruta_aux <- getLine
    let ruta = fst(head(reads ruta_aux :: [([Direccion], String)]))
    return $ laberintoNuevo ruta

--funcion que recibe la opcion de cargar un laberinto en memoria y hace las acciones correspondientes
realizarElecciones "2" Nothing = do
    putStrLn "Introduzca la ruta del archivo del cual se va a cargar el laberinto:"
    archivo <- getLine
    lab1 <- readLaberinto archivo
    return $ lab1

--funcion que recibe la opcion de cargar un laberinto de la memoria y hace las acciones correspondientes
realizarElecciones "2" _ = do
    putStrLn "Introduzca la ruta del archivo del cual se va a cargar el laberinto:"
    archivo <- getLine
    lab1 <- readLaberinto archivo
    return $ lab1

--funcion que recibe la opcion de reportar una pared abierta 
realizarElecciones "3" (Just lab) = do
    putStrLn "Introduzca el camino:"
    ruta_aux <- getLine
    let ruta = fst(head(reads ruta_aux :: [([Direccion], String)]))
    return $ paredAbierta lab ruta

--funcion que recibe la opcion de reportar un derrumbe 
realizarElecciones "4" (Just lab) = do
    putStrLn "Introduzca el camino:"
    camino_aux <- getLine
    let camino = fst(head(reads camino_aux :: [([Direccion], String)]))
    putStrLn "Introduzca la dirección:"
    direccion_aux <- getLine
    let direccion = fst(head(reads direccion_aux :: [(Direccion, String)]))
    return $ derrumbe lab camino direccion

--funcion que recibe la opcion de reportar un tesoro tomado 
realizarElecciones "5" (Just lab) = do
    putStrLn "Introduzca la ruta del tesoro:"
    ruta_aux <- getLine
    let ruta = fst(head(reads ruta_aux :: [([Direccion], String)]))
    let duplaTesoro = tesoroTomado lab ruta
    let lab1 = fst(duplaTesoro)
    let hayTesoro = snd(duplaTesoro)
    if hayTesoro == False then putStrLn "No hay tesoro" else return()
    return $ if hayTesoro then fromMaybe lab (lab1) else lab

--funcion que recibe la opcion de reportar un tesoro hallado
realizarElecciones "6" (Just lab) = do
    putStrLn "Introduzca la ruta del tesoro:"
    ruta_aux <- getLine
    let ruta = fst(head(reads ruta_aux :: [([Direccion], String)]))
    putStrLn "Introduzca la descripcion:"
    descripcion <- getLine
    let duplaTesoro = tesoroHallado lab ruta descripcion
    let lab1 = fst(duplaTesoro)
    if lab1 == Nothing then (putStrLn "Te estas metiendo en una pared") else return()
    let hayTesoro = snd(duplaTesoro)
    if hayTesoro == False then (putStrLn "Ya existe un tesoro ahi") else return()
    return $ (fromMaybe lab lab1)

--funcion que recibe la opcion de cargar un laberinto en memoria y hace las acciones correspondientes
realizarElecciones "7" (Just lab) = do
    putStrLn "Introduzca la ruta del archivo al cual se va a cargar el laberinto:"
    archivo <- getLine
    writeLaberinto lab archivo
    return $ lab

--funcion que recibe la opcion de preguntar ruta en la que si no se alcanza un tesoro o un camino sin salida se puede continuar una ruta o colocar una nueva
realizarElecciones "8" (Just lab) = do
    putStrLn "Introduzca la ruta a seguir:"
    ruta_aux <- getLine
    let ruta = fst(head(reads ruta_aux :: [([Direccion], String)]))
    let lab1 = irRuta (Just lab) ruta
    let hayTesoro = (esTesoro (fromMaybe deadEnd lab1))
    lab2 <- revisarLaberinto (fromMaybe deadEnd lab1) hayTesoro ruta (isNothing lab1)
    return $ lab

realizarElecciones _ (Just lab) = do
    putStrLn "Introduzca un número entre 1 y 8"
    return lab

revisarLaberinto lab1@(Tesoro descripcion _) True ruta _ = do
    putStrLn ("Se encontró un tesoro: "++descripcion)
    return $ lab1

revisarLaberinto lab1 False ruta False = do
    lab2 <- preguntarRuta lab1 ruta
    return $ lab2

revisarLaberinto lab1 False ruta True = do
    putStrLn "Te estas metiendo en una pared"
    return $ lab1

preguntarRuta lab ruta = do
    putStrLn "Desea continuar la ruta (1) o comenzar una ruta nueva (2)?"
    eleccion <- getLine
    lab2 <- continuarComenzarRuta eleccion lab ruta
    return $ lab2

--funcion que sirve para continuar o comenzar una nueva ruta segun la decision del jugador
continuarComenzarRuta "1" lab ruta = do
    putStrLn "Introduzca la ruta que desea tomar desde la ruta anterior: "
    ruta_aux <- getLine
    let ruta1 = fst(head(reads ruta_aux :: [([Direccion], String)]))
    let ruta2 = ruta ++ ruta1
    return $ fromMaybe deadEnd (irRuta (Just lab) ruta2)

continuarComenzarRuta "2" lab ruta = do
    putStrLn "Introduzca la nueva ruta que desea tomar: "
    ruta_aux <- getLine
    let ruta = fst(head(reads ruta_aux :: [([Direccion], String)]))
    return $ fromMaybe deadEnd (irRuta (Just lab) ruta)



preguntarOpciones lab = do
    putStrLn "Ingrese alguna de las siguientes opciones (Escriba el numero correspondiente):"
    putStrLn "1.Comenzar a hablar de un laberinto nuevo"
    putStrLn "2.Hablar de un laberinto de nombre conocido"
    putStrLn "3.Reportar pared abierta"
    putStrLn "4.Reportar derrumbe"
    putStrLn "5.Reportar tesoro tomado"
    putStrLn "6.Reportar tesoro hallado"
    putStrLn "7.Dar nombre al laberinto"
    putStrLn "8.Preguntar ruta"
    opcion <-getLine
    lab1 <- realizarElecciones opcion (Just lab)
    print lab1
    preguntarOpciones lab1

-- | Funcion principal que maneja la logica principal del juego 
main = do
    putStrLn "Bienvenido al juego de laberinto y el sabio. Deseas hablar de un laberinto nuevo (1) o cargar un laberinto conocido de la memoria (2)?"
    opcion <-getLine
    laberintoInicial <- realizarElecciones opcion Nothing
    print laberintoInicial
    preguntarOpciones laberintoInicial
