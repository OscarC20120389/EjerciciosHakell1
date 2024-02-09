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

        let cestaDeCompra = [(2.0, 10), (3.0, 5), (5.0, 8)] -- Lista (porcentaje, precio)
    
        -- Calcular el precio final aplicando un descuento del 10%
        let precioFinalDescuento = calcularPrecioFinal cestaDeCompra (\porcentaje precio -> aplicarDescuento porcentaje precio)
        putStrLn $ "Precio final con descuento: " ++ show precioFinalDescuento
        
        -- Calcular el precio final aplicando el IVA del 16%
        let precioFinalIVA = calcularPrecioFinal cestaDeCompra (\porcentaje precio -> aplicarIVA cantidadDeIVA precio)
        putStrLn $ "Precio final con IVA: " ++ show precioFinalIVA
        
