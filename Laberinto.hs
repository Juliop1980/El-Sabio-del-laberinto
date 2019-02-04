{-|
Module      : Laberinto
Descripcion : Contiene la estructura de datos Laberinto
Creadores  : 14-10576 Leonardo Lopez 14-10820 Julio Perez
Este modulo contiene las funciones que manejan la estructura de datos laberinto, 
ademas de la definicion del tipo de dato en si mismo.
-}

module Laberinto (
    Laberinto(Trifurcacion,Tesoro),
    Direccion(Izquierda,Recto,Derecha),
    deadEnd,
    treasure,
    connect,
    irIzquierda,
    irRecto,
    irDerecha,
    irRuta
) where
 
import Data.Maybe

-- | Estructura del laberinto.
data Laberinto =
    Trifurcacion (Maybe Laberinto) (Maybe Laberinto) (Maybe Laberinto)
    | Tesoro [Char] (Maybe Laberinto) deriving (Eq)
 
-- | Show de Laberinto.
instance Show Laberinto where
    show (Trifurcacion a b c) = ('(' : (mostrar a)) ++ ('|' : (mostrar b)) ++ ('|' : (mostrar c)) ++ ")"
        where
            mostrar x = if isNothing x then "#" else show (fromMaybe deadEnd x)
            izq = if isNothing a then "#" else show (fromMaybe deadEnd a)
            rec = if isNothing b then "#" else show (fromMaybe deadEnd b)
            der = if isNothing c then "#" else show (fromMaybe deadEnd c)
    show (Tesoro descripcion x) = descripcion++('/':lab)
        where
            lab = if isNothing x then "#" else show (fromMaybe deadEnd x)
 
-- | Read de Laberinto.
instance Read Laberinto where
    readsPrec _ s = readsLaberinto s


-- | Funcion que interpreta un laberinto mediante un String.
readsLaberinto :: String -- ^ Recibe una cadena de caracteres que sera interpretado.
               -> [(Laberinto, String)] -- ^ Retorna una lista con una dupla de elementos que definen al laberinto.
readsLaberinto ('(':s) = [ (Trifurcacion a b c , r) | (a, '|':p) <- readsLaberintoAux s,
                                                    (b, '|':q) <- readsLaberintoAux p,
                                                    (c, ')':r) <- readsLaberintoAux q ]
readsLaberinto ('#':xs) =  [(deadEnd,xs)]
readsLaberinto s =  if r /= [] then [(treasure descripcion (fst resto) , snd resto)] else error "Mal Formato"
    where
        descripcion = takeWhile (\c -> c/='/') s
        r = dropWhile (\c -> c/='/') s
        resto = head (readsLaberintoAux (tail r))

-- | Funcion auxiliar para interpretar un laberinto mediante un String.
readsLaberintoAux :: String -- ^ Recibe una cadena de caracteres que sera interpretado.
                  -> [(Maybe Laberinto, String)] -- ^ Retorna una lista con una dupla de elementos que definen al laberinto.
readsLaberintoAux ('#':xs) = [(Nothing,xs)]
readsLaberintoAux s = [ (Just x,y) | (x,y) <- readsLaberinto s ]

-- | Estructura de datos Direccion conformada por las tres direcciones posibles (Izquierda, Recto y Derecha).
data Direccion = Izquierda | Recto | Derecha deriving (Eq,Show,Read)

-- | Funcion que devuelve un camino sin salida
deadEnd :: Laberinto -- ^ Retorna una trifurcacion sin salida representado en Laberinto.
deadEnd = Trifurcacion Nothing Nothing Nothing

-- | Función que reciba un String con la descripción de un tesoro y un laberinto indicando qué encontrarán si pasan por alto el tesoro, y retorne el Tesoro.
treasure :: [Char] -- ^ Cadena de caracteres que representan la descripcion de un tesoro.
         -> Maybe Laberinto -- ^ Recibe un laberinto si es posible encontrarlo pasando de alto el tesoro.
         -> Laberinto -- ^ Retorna un tesoro en formato de laberinto
treasure descripcion Nothing = Tesoro descripcion Nothing
treasure descripcion lab = Tesoro descripcion lab
 
-- | Función que recibe una Trifurcacion, un laberinto y un indicador de cuál camino los relaciona (izquierda, derecha, recto), y retorna una Trifurcacion en la que se indique que dicho camino conduce a dicho laberinto.
connect :: Laberinto -- ^ Recibe una trifurcación en forma de laberinto.
        -> Maybe Laberinto -- ^ Recibe un laberinto.
        -> Direccion -- ^ Recibe la dirección que relaciona la trifurcación con el laberinto
        -> Laberinto -- ^ Retorna una trifurcación en forma de laberinto que representa el camino tomado para conectar la trifurcación con el laberinto
connect (Trifurcacion a b c) lab dir
    | dir == Izquierda = Trifurcacion lab b c
    | dir == Recto = Trifurcacion a lab c
    | dir == Derecha = Trifurcacion a b lab
connect (Tesoro descripcion a) lab dir = Tesoro descripcion (Just (connect (fromMaybe deadEnd a) lab dir))

-- | Funcion que maneja todas las rutas que se pueden seguir dada una posición en el laberinto.
irRuta :: Maybe Laberinto -- ^ Recibe un laberinto.
       -> [Direccion] -- ^ Recibe una ruta.
       -> Maybe Laberinto -- ^ Retorna el laberinto que se forma al término de la ruta recibida.
irRuta Nothing _ = Nothing
irRuta lab [] = lab
irRuta lab (Izquierda:ruta) = irRuta (irIzquierda lab) ruta
irRuta lab (Recto:ruta) = irRuta (irRecto lab) ruta
irRuta lab (Derecha:ruta) = irRuta (irDerecha lab) ruta
 
--irRuta lab@(Just (Trifurcacion a b c)) ruta
--    | head ruta == Izquierda = irRuta (irIzquierda lab) (tail ruta)
--    | head ruta == Recto = irRuta (irRecto lab) (tail ruta)
--    | head ruta == Derecha = irRuta (irDerecha lab) (tail ruta)
 
-- | Función que recibe un laberinto y retorna el laberinto que comienza al voltear a la izquierda.
irIzquierda :: Maybe Laberinto -- ^ Recibe un laberinto.
            -> Maybe Laberinto -- ^ Retorna laberinto al seguir la dirección izquierda.
irIzquierda Nothing = Nothing
irIzquierda (Just (Trifurcacion a _ _)) = a
irIzquierda (Just (Tesoro _ a)) = irIzquierda a
 
-- | Función que recibe un laberinto y retorna el laberinto que comienza al seguir recto.
irRecto :: Maybe Laberinto -- ^ Recibe un laberinto.
        -> Maybe Laberinto -- ^ Retorna laberinto al seguir recto.
irRecto Nothing = Nothing
irRecto (Just (Trifurcacion _ a _)) = a
irRecto (Just (Tesoro _ a)) = irRecto a
 
-- | Función que recibe un laberinto y retorna el laberinto que comienza al voltear a la derecha.
irDerecha :: Maybe Laberinto -- ^ Recibe un laberinto.
          -> Maybe Laberinto -- ^ Retorna laberinto al seguir la dirección derecha.
irDerecha Nothing = Nothing
irDerecha (Just (Trifurcacion _ _ a)) = a
irDerecha (Just (Tesoro _ a)) = irDerecha a
