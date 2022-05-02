type Nombre = String
type Precio = Double

type Producto = (String,Double)

precioTotal :: Producto -> Double -> Double -> Double -> Double
precioTotal producto cantidad descuento costoDeEnvio =  (aplicarCostoDeEnvio ((aplicarDescuento producto descuento)  * cantidad)) costoDeEnvio

productoDeElite :: Producto -> Bool
productoDeElite producto = productoCodiciado producto && productoDeLujo producto && not(productoCorriente producto)

productoCodiciado :: Producto -> Bool
productoCodiciado  (nombreProducto,_) = length nombreProducto > 10

productoDeLujo :: Producto -> Bool
productoDeLujo (nombreProducto,_) = elem 'x' nombreProducto || elem 'z' nombreProducto

productoCorriente :: Producto -> Bool
productoCorriente (nombreProducto,_) = head nombreProducto == 'a' || head nombreProducto == 'e' || head nombreProducto == 'i' || head nombreProducto == 'o' ||head nombreProducto == 'u' ||head nombreProducto == 'A' || head nombreProducto == 'E' || head nombreProducto == 'I' || head nombreProducto == 'O' ||head nombreProducto == 'U'

aplicarDescuento :: Producto -> Double -> Double
aplicarDescuento (_,precio) descuento = precio * descuento

entregaSencilla :: String -> Bool
entregaSencilla dia = even.length $ dia

descodiciarProducto :: Producto -> Producto
descodiciarProducto (nombreProducto,precio) = (take 10 nombreProducto,precio)

aplicarCostoDeEnvio ::  Double -> Double -> Double
aplicarCostoDeEnvio precio costoDeEnvio = precio + costoDeEnvio

productoXL :: Producto -> Producto
productoXL producto = (fst producto ++ "XL",snd producto)

versionBarata :: Producto -> Producto
versionBarata producto = (reverse.fst.descodiciarProducto $ producto, snd producto)
