
-------NUMEROS ENTEROS-------

---2---
---2.1.A---
sucesor :: Int -> Int 
sucesor n= n+1
--2.1.B--
sumar :: Int -> Int -> Int
sumar n m = n+m
--2.1.C--
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, (mod n m))
--2.1.D--
maxDelPar :: (Int,Int) -> Int
maxDelPar (n,m) = if (n>m) then  n
                else m

--2.2--
--sumar (sucesor 4) (maxDelPar(divisionYResto 20 4))--
--sumar 1 (sucesor  (maxDelPar (divisionYResto 40 5)))
-- maxDelPar (divisionYResto (sumar 50 50)  (sucesor 9))
-- sucesor(maxDelPar(divisionYResto (sumar 9 9) 2))


-------TIPOS ENUMERATIVOS -------
--3.1--
data Dir = Norte | Este | Sur| Oeste
    deriving Show
--3.1.A--
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este  = Oeste
opuesto Sur   = Norte
opuesto Oeste = Este
--3.1.B--
iguales :: Dir -> Dir -> Bool
iguales Norte   Norte = True
iguales Este    Este  = True
iguales Sur     Sur   = True
iguales Oeste   Oeste = True
iguales _       _     = False
--3.1.C--
siguiente :: Dir -> Dir
--PRECONDICION Oeste No tiene siguiente
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente Oeste = error " Oeste no tiene siguinete."
 
---3.2--
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show
---3.2.A--
primerDia = Lunes
ultimoDia = Domingo
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)
--3.2.B--
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         =  False
--3.2.C--
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues    dia1 dia2 = numeroDe dia1 < numeroDe  dia2

numeroDe :: DiaDeSemana -> Int
numeroDe Lunes       = 1
numeroDe Martes      = 2
numeroDe Miercoles   = 3
numeroDe Jueves      = 4
numeroDe Viernes     = 5
numeroDe Sabado      = 6
numeroDe Domingo     = 7

--3.3.A--
negar :: Bool -> Bool
negar True  = not True 
negar False = not False
--3..3.B--
--3..3.B--
implica :: Bool -> Bool -> Bool
implica  False _ = True
implica  _     b = b
--3.3.C--
yTambien :: Bool -> Bool -> Bool
yTambien  False  _ = False
yTambien   _     b = b
--3.3.D--
oBien :: Bool -> Bool -> Bool
oBien True  _   = True
oBien _     b   = b


-------REGISTROS------
--4.1.--
data Persona = ConstPer String Int 
     deriving Show

german = ConstPer "German" 35
ariel = ConstPer "Ariel" 25
nombre :: Persona -> String
nombre (ConstPer n _ ) = n       
--
edad :: Persona -> Int
edad (ConstPer _ e) = e
---
crecer :: Persona -> Persona
crecer (ConstPer n e) = ConstPer n (e+1)
--
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoN (ConstPer n e) = ConstPer nuevoN e
--
esMayorQueOtra :: Persona -> Persona -> Bool
esMayorQueOtra  pers1 pers2 = edad pers1 > edad pers2 
--
laQueEsMayor :: Persona -> Persona -> Persona 
laQueEsMayor pers1  pers2=  if (esMayorQueOtra pers1 pers2 ) 
                                then
                                    pers1
                                else 
                                    pers2

--4.2--
data TipoDePokemon = Agua|Fuego|Planta
    deriving Show
data Pokemon = ConstPokemon TipoDePokemon Int
    deriving Show
pokemonFuego  = ConstPokemon Fuego  100
pokemonPlanta = ConstPokemon Planta 60
pokemonAgua   = ConstPokemon Agua   70
pokemonAgua2  = ConstPokemon Agua   80
data Entrenador = ConstEntr String Pokemon Pokemon
    deriving Show
entrenador1 = ConstEntr "1" pokemonFuego pokemonAgua
entrenador2 = ConstEntr "2" pokemonAgua pokemonAgua2 
--
superaA :: Pokemon -> Pokemon -> Bool
superaA poke1 poke2 = tipoSuperaATipo (tipoDe poke1) (tipoDe poke2)

tipoDe :: Pokemon -> TipoDePokemon
tipoDe  (ConstPokemon t _ ) = t

tipoSuperaATipo :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSuperaATipo   Agua      Fuego  = True
tipoSuperaATipo   Fuego     Planta = True
tipoSuperaATipo   Planta    Agua   = True 
tipoSuperaATipo   _         _      = False

--
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tp (ConstEntr _ pok1 pok2 ) = cantidadDe tp (tipoDe pok1)  + cantidadDe tp  (tipoDe pok2)

cantidadDe:: TipoDePokemon -> TipoDePokemon -> Int
cantidadDe      Agua    Agua    = 1
cantidadDe      Fuego   Fuego   = 1
cantidadDe      Planta  Planta  = 1
cantidadDe      _       _       = 0 
   
--
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (entre1, entre2) = pokemonsDelEntr entre1 ++ pokemonsDelEntr entre2
pokemonsDelEntr :: Entrenador -> [Pokemon]
pokemonsDelEntr (ConstEntr _ pok1 pok2) = pok1:pok2:[]


----FUNCIONES POLIFORMICAS----
---5---
--5.1.
lomismo :: a -> a
lomismo a = a 
--5.1.B--
siempreSiete :: a -> Int
siempreSiete a = 7
--5.1.C--
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)


-------LISTAS--------
---6---
lista1 = [1,2,3]  
--6.2--
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False
--6.3--
--PRECONDICION: Debe haber al menos un elemento en la lista.
elPrimero :: [a] -> a
elPrimero (x : _) = x 
elPrimero  _       = error "La lista esta vacia"
--6.4---
--PRECONDICION: Debe haber al menos un elemento en la lista.
sinElPrimero :: [a] -> [a]
sinElPrimero (_:x) = (x)
sinelPrimero _     = error "La lista esta vacia"
--6.5-- 
splitHead :: [a] -> (a, [a])
splitHead  x = (elPrimero x, sinElPrimero x)
