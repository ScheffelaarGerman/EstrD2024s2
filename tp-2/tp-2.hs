-----RECURSION SOBRE LISTAS------
--1.1--
sumatoria :: [Int]  -> Int
sumatoria    []     =  0  
sumatoria    (n:ns) =  n + sumatoria ns

--1.2--
longitud :: [a]    -> Int
longitud    []     =  0
longitud    (x:xs) =  1 + longitud xs   

--1.3--
sucesores :: [Int]  -> [Int]
sucesores    []     =  []
sucesores    (n:ns) =  (n+1):sucesores ns
 
 --1.4--
conjuncion :: [Bool] -> Bool
conjuncion    []     =  True -- Es False si al menos un elemento es False.
conjuncion    (x:xs) =  x   &&  conjuncion  xs

--1.5--
disyuncion :: [Bool] -> Bool
disyuncion    []     = False -- Es True si al mens un elemento es True--
disyuncion    (b:bs) =  b || disyuncion bs 

--1.6--
aplanar :: [[a]]  -> [a]
aplanar    []     =  []
aplanar    (x:xs) =  x ++ aplanar xs

--1.7--
pertenece  :: Eq a => a -> [a]    -> Bool
pertenece             e    []     = False
pertenece             e    (x:xs) =  (e==x) || pertenece e xs   

--1.8--
apariciones :: Eq a => a-> [a]     -> Int
apariciones            _   []      =  0
apariciones            e   (x:xs)  = if (e==x)
                                      then 1 + apariciones e xs       
                                      else apariciones e xs
--1.9--
losMenoresA :: Int -> [Int]  -> [Int]
losMenoresA    _      []     =  []
losMenoresA    n      (x:xs) =  if (x<n)
                                   then x : losMenoresA n xs            
                                   else     losMenoresA n xs

--1.10--
lasDeMayorLongitud :: Int -> [[a]]  -> [[a]] 
lasDeMayorLongitud    _      []     =  []
lasDeMayorLongitud    n      (x:xs) =  if (longitud x > n )
                                        then x: lasDeMayorLongitud n xs
                                        else    lasDeMayorLongitud n xs

--1.11--
agregarAlFinal :: [a] -> a -> [a]
agregarAlFInal    []     e =  [e]
agregarAlFinal    (x:xs) e =   x : agregarAlFinal xs e

--1.12--
agregar ::  [a] ->  [a] ->  [a]
agregar     [ ]     ys  =   ys
agregar     (x:xs)  ys  =   x : agregar xs ys

--1.13--
reversa :: [a]    -> [a]
reversa    []     =  []
reversa    (x:xs) =  reversa xs ++ [x]

--1.14--
zipMaximos :: [Int]  -> [Int] -> [Int]
zipMaximos    []     ys       =  ys
zipMaximo    xs     []       =  xs
zipMaximo     (x:xs) (y:ys)   = max x y : zipMaximo xs ys

--1.15--
elMinimo :: Ord a => [a]   -> a 
--PRECONIDION: La lista no puede estar vacia.
elMinimo             [x]    = x 
elMinimo             (x:xs) = min x (elMinimo xs)


------RECURSION SOBRE NUMEROS------

--2.1--
factorial :: Int -> Int
factorial     0  =  1
factorial     n  =  n*factorial (n-1)

--2.2--
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva   0   = []
cuentaRegresiva   n   = n : cuentaRegresiva(n-1)

--2.3--
repetir :: Int -> a -> [a]
repetir    0      _ = []
repetir    n      x =  x: repetir (n-1) x

--2.4--
losPrimeros :: Int ->  [a]    -> [a]
losPrimeros    0       []     =  []
losPrimeros    _       []     =  []
losPrimeros    n       (x:xs) = x:losPrimeros (n-1) xs

--2.5--

sinLosPrimeros ::Int ->  [a]    -> [a]
sinLosPrimeros     0     []     = []
sinLosPrimeros     _     []     = []
sinLosPrimeros     n     (x:xs) = sinLosPrimeros (n-1) xs 


------REGISTROS------
--3.1--
data Persona =  P String Int
    deriving Show

juan = P " Juan" 21
ariel= P " Ariel" 15
leo  = P " Leo" 32

--
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA    n      []        =  []
mayoresA    n      (p:ps)    = if (edad p > n)
                                then  p: mayoresA n ps        
                                 else  mayoresA n ps    
edad :: Persona -> Int
edad    (P _ e) = e

--
promedioEdad :: [Persona] -> (Int, Int)
promedioEdad    []        =   error " La lista no puede ser vacia."
promedioEdad    ps        = divisionYResto  (sumatoria(edadesDe ps))  (longitud ps) 

edadesDe :: [Persona] -> [Int]
edadesDe    []        =  []
edadesDe   (p:ps)    = edad p : edadesDe ps 

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, (mod n m))

--
elMasViejo :: [Persona] -> Persona
--PRECONDIOCON: No puede ser una lista vacia
elMasViejo    []        = error "No puede ser una lista vacia"
elMasViejo    [p]       = p 
elMasViejo    (p:ps)    =  if edad p > edad (elMasViejo ps)
                            then p 
                            else elMasViejo ps   


--3.2--

data TipoDePokemon = Agua | Fuego | Planta        
    deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int
    deriving Show
data Entrenador = ConsEntrenador String [Pokemon]
    deriving Show

pokemonFuego = ConsPokemon Fuego 100
pokemonPlanta = ConsPokemon Planta 68
pokemonAgua = ConsPokemon Agua 75
pokemonAgua1 = ConsPokemon Agua 87

entrenador1 = ConsEntrenador "entrenador1" [pokemonAgua]
entrenador2 = ConsEntrenador "entrenador2" [ pokemonFuego]
entrenador3 = ConsEntrenador "entrenador3" [pokemonFuego, pokemonPlanta, pokemonAgua]

--
cantPokemon :: Entrenador             -> Int
cantPokemon    (ConsEntrenador _ pks) = longitud pks

--
cantPokemonDe :: TipoDePokemon -> Entrenador           -> Int
cantPokemonDe    tp             (ConsEntrenador _ pks) = cantidadDeEn tp pks

cantidadDeEn :: TipoDePokemon -> [Pokemon] -> Int
cantidadDeEn    _                []        = 0 
cantidadDeEn    tp               (p:ps)    = if (sonIgualTipo tp (tipoDe p))
                                              then 1 + cantidadDeEn tp ps
                                              else cantidadDeEn tp ps   

tipoDe :: Pokemon              -> TipoDePokemon 
tipoDe    (ConsPokemon  tp _ ) = tp

sonIgualTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonIgualTipo    Agua             Agua           = True
sonIgualTipo    Fuego            Fuego          = True
sonIgualTipo    Planta           Planta         = True
sonIgualTipo    _                _              = False                

--
cuantosDeTipo_De_LeGanaTodosLosDe_ :: TipoDePokemon -> Entrenador            -> Entrenador         -> Int
cuantosDeTipo_De_LeGanaTodosLosDe_    tp               (ConsEntrenador _ pks1) (ConsEntrenador _ pks2)  = cantidadDeTipo_De_QueGanaA_ tp pks1 pks2

cantidadDeTipo_De_QueGanaA_ :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cantidadDeTipo_De_QueGanaA_    _              []            _        = 0
cantidadDeTipo_De_QueGanaA_    tp             (p:pks1)     pks2  = unoSiCeroSino (sonIgualTipo tp (tipoDe p) && leGanaATodos p pks2) + cantidadDeTipo_De_QueGanaA_  tp pks1 pks2

unoSiCeroSino :: Bool -> Int
unoSiCeroSino    True  = 1
unoSiCeroSino    False = 0

leGanaATodos :: Pokemon ->  [Pokemon] -> Bool
leGanaATodos    _           []         =  False
leGanaATodos    pok         [p2]       = superaA (tipoDe pok) (tipoDe p2)
leGanaATodos    pok         (p2:pks2)     = superaA (tipoDe pok) (tipoDe p2) && leGanaATodos pok pks2

superaA :: TipoDePokemon -> TipoDePokemon -> Bool
superaA     Agua             Fuego         = True  
superaA     Fuego            Planta        = True
superaA     Planta           Agua          = True
superaA     _                _             = False     

--
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon   (ConsEntrenador _ poks) = hayDeTipoEn Agua poks && hayDeTipoEn Fuego poks && hayDeTipoEn Fuego poks

hayDeTipoEn :: TipoDePokemon -> [Pokemon] -> Bool
hayDeTipoEn    _                []        = False
hayDeTipoEn    tp               (p:ps)  = sonIgualTipo tp (tipoDe p) || hayDeTipoEn tp ps 


--3.3--
data Seniority = Junior | SemiSenior | Senior
    deriving Show
data Proyecto  = ConsProyecto String
    deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show
data Empresa = ConsEmpresa [Rol]
    deriving Show

empresa1= ConsEmpresa [empleado1, empleado2, empleado3, empleado6,empleado7, empleado8]
empresa2= ConsEmpresa [empleado4, empleado5, empleado6, empleado10, empleado11, empleado12]

proyecto1 = ConsProyecto "proyecto1"
proyecto2 = ConsProyecto "proeycto2"
proyecto3 = ConsProyecto "proyecto3"
proyecto4 = ConsProyecto "proeycto4"


empleado1 = Developer SemiSenior proyecto1
empleado2 = Management Junior proyecto1 
empleado3 = Developer  Senior proyecto1
empleado4 = Management SemiSenior proyecto2
empleado5 = Developer  Senior proyecto2
empleado6 = Developer  Senior proyecto2
empleado7 = Developer SemiSenior proyecto3
empleado8 = Management Senior proyecto3 
empleado9 = Developer  Senior proyecto3
empleado10 = Management Senior proyecto4
empleado11 = Developer  Junior proyecto4
empleado12 = Developer  SemiSenior proyecto4
--
proyectos :: Empresa -> [Proyecto]
proyectos    (ConsEmpresa rs) = proyectosDe rs

proyectosDe :: [Rol]  -> [Proyecto]
proyectosDe    []     = []
proyectosDe    (r:rs) = singularSi (proyecto r) (not (elProyectoEstaEnRoles (proyecto r) rs))  ++  proyectosDe rs 

singularSi :: p -> Bool  -> [p]
singularSi    _    False  = []
singularSi    p    True  = p:[]


proyecto :: Rol             -> Proyecto
proyecto   (Developer _ p)  = p 
proyecto   (Management _ p) = p 

elProyectoEstaEnRoles :: Proyecto-> [Rol]  -> Bool
elProyectoEstaEnRoles    _          []     =  False
elProyectoEstaEnRoles    p          (r:rs) = sonIgualesProy p (proyecto r)   || elProyectoEstaEnRoles p rs  

sonIgualesProy:: Proyecto          -> Proyecto       -> Bool
sonIgualesProy   (ConsProyecto p1) (ConsProyecto p2) = p1 == p2
sonIgualesProy   _                 _                 = False


---
losDevSenior :: Empresa        -> [Proyecto] -> Int
losDevSenior     (ConsEmpresa rs)  ps         = cantDeSeniorConProyectos rs ps 


cantDeSeniorConProyectos :: [Rol] -> [Proyecto] -> Int
cantDeSeniorConProyectos    []     _               = 0
cantDeSeniorConProyectos     (r:rs)   ps          = unoSiCeroSino(esSeniorityDev Senior r  && proyeEstaEnProyes (proyecto r) ps)  + cantDeSeniorConProyectos rs ps

proyeEstaEnProyes :: Proyecto -> [Proyecto] -> Bool
proyeEstaEnProyes _ [] = False
proyeEstaEnProyes pro (p:ps) = sonIgualesProy pro p || proyeEstaEnProyes pro ps
           
esSeniorityDev :: Seniority -> Rol              -> Bool
esSeniorityDev    sen          (Developer se _) = sonDeIgualSeniority sen se   
esSeniorityDev    _            _                = False

sonDeIgualSeniority :: Seniority -> Seniority   -> Bool
sonDeIgualSeniority    Junior       Junior      = True
sonDeIgualSeniority    SemiSenior   SemiSenior  = True
sonDeIgualSeniority    Senior       Senior      = True
sonDeIgualSeniority    _            _           = False

--
cantQueTrabajanEn :: [Proyecto] -> Empresa        -> Int
cantQueTrabajanEn    ps          (ConsEmpresa rs) = cantDeTrabEnProyes ps rs

cantDeTrabEnProyes :: [Proyecto] -> [Rol] -> Int
cantDeTrabEnProyes      _            []    = 0
cantDeTrabEnProyes      ps          (r:rs) = unoSiCeroSino(proyeEstaEnProyes (proyecto r) ps) + cantDeTrabEnProyes ps rs

--
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = asignarPorProyectos (proyectosDe rs) rs 

asignarPorProyectos :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
asignarPorProyectos [] _ = []
asignarPorProyectos (p:ps) rs = [(p, cantEmpleadosEnProyecto rs p)] ++ asignarPorProyectos ps rs

cantEmpleadosEnProyecto :: [Rol] -> Proyecto -> Int
cantEmpleadosEnProyecto [] _ = 0
cantEmpleadosEnProyecto (r:rs) p = unoSiCeroSino(sonIgualesProy (proyecto r) p) + cantEmpleadosEnProyecto rs p
{-- PRESETAR MUCHA ANTENCION doble pattern maching, miedo al booleano,patter maching anidado,  SIMEPRE EN SUBTAREA
data  Factura = Medialuna | Churro
data  Pedido = Bandeja
               |Simple Factura Int
               |Combinado Factura Int Factura Int
agregarfac :: Pedido ->               Factura    -> Pedido
agregarfac    Bandeja                  x         = Simple  x 1
agregarfac    (Simple f n)             x         = if esMismaFactura f x
                                                    then Simple      f    n+1
                                                    else Combinado   f n  x 1

agregarfac   (Combinado f1 n1 f2 n2)   x         =  if esMismaFactura f1 x
                                                     then Combinado f1 n1+1 f2  n2
                                                     else Combinado f1 n1   f2  n2+1
esMismaFactura :: Factura -> Factura -> Bool
esMismaFactura    Medialuna Medialuna = True
esMismaFactura    Churro   Churro     = True
esMismaFactura    _         _         = False     
--}