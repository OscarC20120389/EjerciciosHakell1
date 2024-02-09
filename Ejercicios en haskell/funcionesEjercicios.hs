module Funciones where

    type Precio = Float
    type Porcentaje = Float
    type Producto = (String, Precio)

    --Función aplicar descuento
    aplicarDescuento :: Porcentaje -> Precio -> Precio
    aplicarDescuento descuento precio = precio - (precio * descuento/100)


    -- Función aplicar el IVA 
    aplicarIVA :: Porcentaje -> Precio -> Precio
    aplicarIVA iva precio = precio + (precio * iva / 100)

    -- Función que recibe un diccionario con precios y porcentajes de una cesta de la compra,
    -- y una función para aplicar descuentos o IVA, y devuelve el precio final de la cesta
    calcularPrecioFinal :: [(Float, Float)] -> (Float -> Float -> Float) -> Float
    calcularPrecioFinal cesta funcion = sum [funcion precio porcentaje | (precio, porcentaje) <- cesta]

    -- Definición de la función aplicarFuncion
    aplicarFuncion :: (a -> b) -> [a] -> [b]
    aplicarFuncion _ [] = []
    aplicarFuncion n (x:xs) = n x : aplicarFuncion n xs

    -- Definimos una función que duplica un número
    duplicar :: Int -> Int
    duplicar x = x * 2

    -- Función que cuenta la longitud de una palabra
    longitudPalabra :: String -> Int
    longitudPalabra = length

    -- Función que recibe una frase y devuelve una lista de tuplas con las palabras y sus longitudes
    diccionarioLongitudes :: String -> [(String, Int)]
    diccionarioLongitudes frase = 
        let palabras = words frase -- Dividir la frase en palabras
            longitudes = map longitudPalabra palabras -- Obtener la longitud de cada palabra
        in zip palabras longitudes -- Combinar las palabras con sus longitudes usando zip

    -- Función que calcula el módulo de un vector
    moduloVector :: [Double] -> Double
    moduloVector vector = sqrt (sum $ map (^2) vector)

    main :: IO ()
    main = do
        let precioOriginal = 100.0
        let cantidadDeIVA = 16.0
        -- let descuento = 10.0

        -- let precioConDescuento = aplicarDescuento descuento precioOriginal

        -- putStrLn $ "Precio original: " ++ show precioOriginal
        -- putStrLn $ "Descuento aplicado: " ++ show descuento ++ "%"
        -- putStrLn $ "Precio con descuento: " ++ show precioConDescuento

        -- let precioConIVA = aplicarIVA cantidadDeIVA precioOriginal

        -- putStrLn $ "Precio original: " ++ show precioOriginal
        -- putStrLn $ "IVA aplicado: " ++ show cantidadDeIVA ++ "%"
        -- putStrLn $ "Precio con IVA: " ++ show precioConIVA

        -- let cestaDeCompra = [(2.0, 10), (3.0, 5), (5.0, 8)] -- Lista (porcentaje, precio)
    
        -- -- Calcular el precio final aplicando un descuento del 10%
        -- let precioFinalDescuento = calcularPrecioFinal cestaDeCompra (\porcentaje precio -> aplicarDescuento porcentaje precio)
        -- putStrLn $ "Precio final con descuento: " ++ show precioFinalDescuento
        
        -- -- Calcular el precio final aplicando el IVA del 16%
        -- let precioFinalIVA = calcularPrecioFinal cestaDeCompra (\porcentaje precio -> aplicarIVA cantidadDeIVA precio)
        -- putStrLn $ "Precio final con IVA: " ++ show precioFinalIVA

        -- Usamos la función aplicarFuncion para aplicar la función duplicar a una lista de números
        -- let listaOriginal = [1, 2, 3, 4, 5]
        -- let listaDuplicada = aplicarFuncion duplicar listaOriginal
        -- print listaDuplicada -- Imprimirá: [2,4,6,8,10]

        -- let fraseEjemplo = "Hola mundo, esta es una frase de ejemplo"
        -- let resultado = diccionarioLongitudes fraseEjemplo
        -- print resultado

        let vector = [3.0, 4.0] -- Vector (3, 4)
        let modulo = moduloVector vector
        putStrLn $ "El módulo del vector " ++ show vector ++ " es: " ++ show modulo