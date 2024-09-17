-------TIPOS RECURSIVOS SIMPLES ------
--1.1
data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

celda1 = CeldaVacia
celda2 = Bolita Rojo (Bolita Rojo (Bolita Rojo (Bolita Azul CeldaVacia)))
celda3 = Bolita Azul (Bolita Azul (Bolita Rojo CeldaVacia))
--
nroBolitas :: Color -> Celda            -> Int
nroBolitas    _        CeldaVacia       = 0
nroBolitas    c        (Bolita col cel) = unoSi (sonDelMismoColor c col  ) +  nroBolitas c cel 

unoSi :: Bool  -> Int
unoSi    True  = 1
unoSi    False = 0

sonDelMismoColor :: Color -> Color -> Bool
sonDelMismoColor    Azul     Azul  =  True
sonDelMismoColor    Rojo     Rojo  =  True
sonDelMismoColor    _        _     =  False
--
poner :: Color -> Celda -> Celda
poner    c        cel   = (Bolita c cel)
--
sacar :: Color -> Celda            -> Celda
sacar    _        CeldaVacia       = CeldaVacia
sacar    c        (Bolita col cel) = if ( sonDelMismoColor c col)
                                        then cel
                                        else sacar c (Bolita c cel)
ponerN :: Int -> Color -> Celda  -> Celda
ponerN     0      _       cel    =  cel
ponerN     n      c       cel    =  poner c (ponerN (n-1) c cel)


------CAMINO HACIA EL TESORO ------
--1.2
data Objeto = Cacharro | Tesoro
    deriving Show
data Camino  = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show
-- Variables--
camino1 = Cofre [Tesoro] Fin
camino2 = Nada (Cofre [Cacharro] Fin)
camino3 = Nada (Fin)
camino4 = Fin
camino5 = Nada (Nada (Nada (Nada(Nada(Cofre [Tesoro] Fin)))))
camino6 = Cofre [Cacharro, Tesoro](Cofre [Tesoro] (Cofre [Tesoro, Tesoro] Fin))
--
hayTesoro :: Camino          -> Bool 
hayTesoro    Fin             = False
hayTesoro    (Nada c)        = hayTesoro c
<<<<<<< HEAD
hayTesoro    (Cofre objts c) = existeAlMenosUnTesoroEn objts || hayTesoro c 

existeAlMenosUnTesoroEn :: [Objeto]  -> Bool
existeAlMenosUnTesoroEn    []        = False
existeAlMenosUnTesoroEn    (o : objs) = esTesoro o || existeAlMenosUnTesoroEn objs

esTesoro :: Objeto -> Bool
esTesoro    Tesoro = True
esTesoro    _      = False
=======
hayTesoro    (Cofre objts c) = existeTesoroEn objts || hayTesoro c 

existeTesoroEn:: [Objeto] -> Bool
existeTesoroEn   []       =  False
existeTesoroEn   (o: os)  =  sonIguales Tesoro o || existeTesoroEn os

sonIguales :: Objeto -> Objeto -> Bool
sonIguales    Tesoro    Tesoro = True
sonIguales    _         _      = False
>>>>>>> 10636e91f52d5f128fceb584ed8d683673352392
--
--PRECONDICION: Debe haber al menos un tesoro.
pasosHastaTesoro :: Camino              -> Int
pasosHastaTesoro    Fin                 =  error " Debe existir al menos un tesoro"
pasosHastaTesoro    (Nada c)             = 1 + pasosHastaTesoro c
<<<<<<< HEAD
pasosHastaTesoro    (Cofre objts c) = if (existeAlMenosUnTesoroEn objts)
=======
pasosHastaTesoro     (Cofre objts c) = if (existeTesoroEn objts)
>>>>>>> 10636e91f52d5f128fceb584ed8d683673352392
                                        then 0
                                        else 1 + pasosHastaTesoro c
--
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn    _      Fin              =  False
hayTesoroEn    0      c                =  hayTesoroAca  c
hayTesoroEn    n      (Nada c)         =  hayTesoroEn (n-1) c              
hayTesoroEn    n      (Cofre  objts c) =  hayTesoroEn (n-1) c

hayTesoroAca :: Camino          -> Bool
<<<<<<< HEAD
hayTesoroAca    (Cofre objts c) = existeAlMenosUnTesoroEn objts
hayTesoroAca    _               = False
---
--
alMenosNTesoros :: Int  ->Camino         -> Bool
alMenosNTesoros    0      _              = True
alMenosNTesoros    _      Fin            = False
alMenosNTesoros    n      (Nada c)       = alMenosNTesoros n c
alMenosNTesoros    n      (Cofre objs c) = alMenosNTesoros (n - unoSi (existeAlMenosUnTesoroEn objs)) c


--
cantTesorosEntre :: Int ->  Int ->  Camino  ->  Int
cantTesorosEntre     i       j        c     =   cantTesorosEnCamino (caminoDesde i (caminoHasta j c))

caminoDesde ::  Int ->  Camino           ->  Camino
caminoDesde     _       Fin              =  Fin
caminoDesde     0       c                =  c
caminoDesde     n       (Nada   c)       =  caminoDesde (n-1) c
caminoDesde     n       (Cofre _ c)      =  caminoDesde (n-1) c

caminoHasta ::  Int ->  Camino           ->  Camino
caminoHasta     _       Fin              =   Fin
caminoHasta     0       _                =   Fin
caminoHasta     n       (Nada   c)       =   Nada (caminoHasta (n-1) c)
caminoHasta     n       (Cofre objs c)   =   Cofre objs (caminoHasta (n-1) c)

cantTesorosEnCamino ::  Camino  ->  Int
cantTesorosEnCamino     Fin             =   0
cantTesorosEnCamino     (Nada   c)      =   cantTesorosEnCamino c
cantTesorosEnCamino     (Cofre objs c)  =  cantDeTesorosEn objs + cantTesorosEnCamino c

cantDeTesorosEn :: [Objeto] -> Int
cantDeTesorosEn    []       =  0
cantDeTesorosEn   (ob: obs)  =  unoSi(esTesoro ob) + cantDeTesorosEn obs
               
--

=======
hayTesoroAca    (Cofre objts c) = existeTesoroEn objts
hayTesoroAca    _               = False
---
--alMenosNTesoros :: Int  ->Camino        -> Bool
alMenosNTesoros    _      Fin            = False
alMenosNTesoros    n      (Nada c)       = alMenosNTesoros n c
alMenosNTesoros    n      (Cofre objs c) = cantTesorosEnCofre objs >= n || alMenosNTesoros (n - cantTesorosEnCofre objs) c 
 
cantTesorosEnCofre :: [Objeto] -> Int
cantTesorosEnCofre    []       =  0
cantTesorosEnCofre    (ob: obs)  =  unoSi(sonIguales Tesoro ob) + cantTesorosEnCofre obs
--

cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre    n1     n2     c      = cantDeTesorosHasta (n2-1)  c - cantDeTesorosHasta n1 c

cantDeTesorosHasta :: Int ->  Camino -> Int
cantDeTesorosHasta    0       _               = 0
cantDeTesorosHasta    _       Fin             = 0
cantDeTesorosHasta    n       (Nada c)        = cantDeTesorosHasta (n-1) c
cantDeTesorosHasta    n       (Cofre objts c) =  cantTesorosEnCofre objts  + cantDeTesorosHasta (n-1)  c                      
--


>>>>>>> 10636e91f52d5f128fceb584ed8d683673352392
------TIPOS ARBOREOS------
--2.1 ARBOLES BINARIOS ---

data Tree a= EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
--
arbol1 :: Tree Int
arbol1 =
    NodeT 5
        (NodeT 5
            (NodeT 5 EmptyT EmptyT)
            (NodeT 5 EmptyT EmptyT))
        EmptyT

arbol2 :: Tree Int
arbol2 =  
    NodeT 1
        (NodeT 2
            (NodeT 3 EmptyT EmptyT)
            (NodeT 4 EmptyT EmptyT))
        (NodeT 5
            (NodeT 6 EmptyT EmptyT)
            (NodeT 7 EmptyT EmptyT))
--    
sumarT :: Tree Int        -> Int
sumarT    EmptyT          = 0
sumarT    (NodeT x t1 t2) = x + sumarT  t1 + sumarT t2

--
sizeT :: Tree a           -> Int
sizeT    EmptyT           = 0
sizeT    (NodeT x t1  t2)  = 1 + sizeT t1 + sizeT t2   

--
mapDobleT :: Tree Int -> Tree Int
mapDobleT    EmptyT    = EmptyT
mapDobleT    (NodeT x t1 t2) = NodeT (x*2)  (mapDobleT t1) (mapDobleT t2)

--
perteneceT ::  Eq a => a -> Tree a          -> Bool
perteneceT            _    EmptyT          = False
perteneceT            e    (NodeT x t1 t2) = x==e || perteneceT e  t1 || perteneceT e t2

--
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT            _    EmptyT          = 0
aparicionesT            e    (NodeT x t1 t2) = unoSi (x==e) + aparicionesT e t1 + aparicionesT e t2

--
leaves ::  Tree a          -> [a]
leaves    EmptyT          = []
leaves    (NodeT x t1 t2) = singularSi x (esEmpty t1 && esEmpty t2) ++ leaves t1 ++ leaves t2

singularSi::  a -> Bool  -> [a]
singularSi    e    True  = e:[]
singularSi    _    _     = []

esEmpty:: Tree a -> Bool
esEmpty   EmptyT = True
esEmpty   _      = False

---
heightT :: Tree a -> Int
heightT    EmptyT = 0
heightT   (NodeT x t1 t2) =   1 +   max (heightT t1)  (heightT t2)

--
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = (NodeT x (mirrorT t2) (mirrorT t1))

--
toList :: Tree a         ->[a]
toList    EmptyT         = []
toList   (NodeT x t1 t2) = (toList t1) ++ x :  (toList t2) 

--
levelN :: Int -> Tree a -> [a]
levelN    _      EmptyT          = []
levelN    0      t               = listaDeElementoDeNodo t 
levelN    n      (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

listaDeElementoDeNodo :: Tree a          ->[a]
listaDeElementoDeNodo    EmptyT          = []
listaDeElementoDeNodo    (NodeT x t1 t2) = x:[]

--
listPerLevel :: Tree a         -> [[a]]
listPerLevel    EmptyT          = []
listPerLevel    (NodeT x t1 t2) = [x] : unirListas (listPerLevel t1) (listPerLevel t2)


unirListas :: [[a]] -> [[a]] -> [[a]]
unirListas xss [] = xss
unirListas [] yss = yss
unirListas (xs:xss) (ys:yss) = (xs ++ ys) : unirListas xss yss

--
ramaMasLarga :: Tree a       -> [a]
ramaMasLarga     EmptyT      =  []
ramaMasLarga (NodeT x t1 t2) =  if (heightT t1 > heightT t2)
                                 then x : ramaMasLarga t1
                                 else x : ramaMasLarga t2

--
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) = [x] : consACadaElemento x (todosLosCaminos t1) ++ consACadaElemento x (todosLosCaminos t2)

consACadaElemento :: a -> [[a]]    -> [[a]]
consACadaElemento    x    []       =  []
consACadaElemento    x    (xs:xss) =  (x:xs) : consACadaElemento x xss


------EXPREIOSNES ARITMETICAS------
--2.2
data ExpA = Valor Int 
          | Sum ExpA ExpA
          | Prod ExpA ExpA
          | Neg ExpA  
expresion1 = Valor 1
expresion2 = Valor 2
expresion3 = Sum expresion1 expresion2
expresion4 = Prod expresion1 expresion2
expresion5 = Neg (expresion1)
--2.1
<<<<<<< HEAD
eval :: ExpA                -> Int
eval    (Valor  n)          =   n
eval    (Sum    e1  e2)     =   eval e1   + eval    e2 
eval    (Prod   e1  e2)     =   eval e1   * eval    e2
eval    (Neg    e)         =   -   (eval e)
--2.2

simplificar ::  ExpA    ->  ExpA
simplificar    (Valor  n)          =   Valor    n
simplificar    (Sum    e1  e2)     =   simplificarSuma (simplificar e1)(simplificar e2)
simplificar    (Prod   e1  e2)     =   simplificarProducto (simplificar e1)(simplificar e2)
simplificar    (Neg    e)          =   simplificarNegativo (simplificar e)

simplificarSuma ::  ExpA        ->  ExpA    ->  ExpA
simplificarSuma     (Valor 0)   e2          =  e2
simplificarSuma     e1          (Valor 0)   =  e1
simplificarSuma     e1          e2          =  Sum e1 e2

simplificarProducto ::  ExpA        ->  ExpA       ->  ExpA
simplificarProducto     (Valor 0)       _          =   Valor 0
simplificarProducto     _              (Valor 0)   =   Valor 0
simplificarProducto     (Valor 1)       e2         =   e2
simplificarProducto     e1              (Valor 1)  =   e1   
simplificarProducto     e1              e2         =   Prod e1  e2

simplificarNegativo ::  ExpA        ->  ExpA
simplificarNegativo     (Neg e)     =   e
simplificarNegativo     e           =   (Neg e)







=======
eval ::  ExpA            -> Int
eval    (Valor n)        = n
eval    (Sum  exp1 exp2) = eval exp1 + eval exp2
eval    (Prod exp1 exp2) = eval exp1 * eval exp2
eval    (Neg  exp1)      = -(eval exp1)

--2.2
simplificar :: ExpA -> ExpA
simplificar (Valor n) = Valor n
simplificar (Sum e1 e2) = simplificacionSum (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2) = simplificaProd (simplificar e1) (simplificar e2)
simplificar (Neg e1) = simplificaNeg (simplificar e1)

simplificaNeg :: ExpA -> ExpA
simplificaNeg (Neg e1) = e1
simplificaNeg e1 = (Neg e1)

simplificacionSum :: ExpA -> ExpA -> ExpA
simplificacionSum e1 (Valor 0) = e1
simplificacionSum (Valor 0) e2 = e2
simplificacionSum e1 e2 = Sum e1 e2

simplificaProd :: ExpA -> ExpA -> ExpA
simplificaProd e1 (Valor 0) = (Valor 0)
simplificaProd (Valor 0) e2 = (Valor 0)
simplificaProd e1 (Valor 1) = e1
simplificaProd (Valor 1) e2 = e2
simplificaProd e1 e2 = Prod e1 e2
>>>>>>> 10636e91f52d5f128fceb584ed8d683673352392
