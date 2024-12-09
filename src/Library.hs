module Library where
import PdePreludat

data Cliente = UnCliente {
    saldo :: Number,
    esVIP :: Bool,
    nombre :: String
}deriving(Show, Eq)

data Producto = UnProducto {
    tipo :: String,
    precio :: Number
}deriving(Show, Eq)

cambiarSaldo :: Cliente -> Number -> Cliente
cambiarSaldo cliente delta = cliente {saldo = saldo cliente + delta}

-- 1) Definir funcion nuevoClienteVIP

nuevoClienteVIP :: String -> Cliente
nuevoClienteVIP unNombre = UnCliente {
    saldo = 0,
    esVIP = True,
    nombre = unNombre
}

nuevoClienteVIP' :: String -> Cliente
nuevoClienteVIP' = UnCliente 0 True     -- utilizando Point Free

-- 2) Definir la compra de producto por parte de un cliente 

comprar :: Producto -> Cliente -> Cliente
comprar producto cliente = (cambiarSaldo cliente . negate . precioNeto) producto
-- 1ero. Quiero el precio del producto                  (Producto -> Number)
-- 2dos. Quiero multiplicarlo por el iva                (Number -> Number)
-- 1ero y 2dos. Serian el precioNeto
-- 3ero. Quiero hacerlo negativo para luego descontarlo (Number -> Number)
-- 4tos. Quiero descontarlo del saldo del cliente       (Cliente -> Number -> Cliente)

-- (*1.21) . precio   ---> lo saque de arriba y lo meti en una funcion aparte
precioNeto :: Producto -> Number 
precioNeto = (*1.21) . precio

-- 3) Definir comprarEnPromocion. 

comprarEnPromocion :: Producto -> Producto -> Number -> Cliente -> Cliente
comprarEnPromocion prod1 prod2 descuento cliente = ((`cambiarSaldo` descuento)  . comprar prod2 . comprar prod1) cliente

--comprarEnPromocion prod1 prod2 descuento cliente = (flip cambiarSaldo descuento  . comprar prod2 . comprar prod1) cliente

-- 1ero. Quiero comprar el primer producto                                          (Producto -> Cliente -> Cliente)
-- 2dos. A ese cliente, quiero comprar el segundo producto                          (Producto -> Cliente -> Cliente)
-- 3ero. Quiero aplicar el descuento al cliente, PERO OJO tengo que hacer un flip!! (Number -> Cliente -> Cliente)
-- Opcional del 3ero. Tambien puede hacerlo con comillas francesas en vez de flip!! (la hice infija)

-- Instanciamos...

ezequiel :: Cliente
ezequiel = UnCliente 1000 True "Ezequiel"

pan :: Producto
pan = UnProducto "Pan" 100

leche :: Producto
leche = UnProducto "Leche" 50