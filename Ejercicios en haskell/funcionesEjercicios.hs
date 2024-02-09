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

    main :: IO ()
    main = do
        let precioOriginal = 100.0
        -- let descuento = 10.0

        -- let precioConDescuento = aplicarDescuento descuento precioOriginal

        -- putStrLn $ "Precio original: " ++ show precioOriginal
        -- putStrLn $ "Descuento aplicado: " ++ show descuento ++ "%"
        -- putStrLn $ "Precio con descuento: " ++ show precioConDescuento

