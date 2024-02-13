---
title: "Cap 13 Radar"
author: "EDWARD VELASCO"
date: "2024-02-07"
output: 
  html_document:
    keep_md: true
---


CARGAR LIBRERIAS

```r
pck <- c("tidyr", "dplyr", "readxl", "ggplot2", "randomForest", "car", "Metrics", "raster", "rasterVis", "rgdal", "mapview", "RColorBrewer", "ggplot2", "sf", "glcm", "ggpubr", "sf", "corrplot", "caret", "usdm")

sapply(pck, require, character.only=TRUE)
```

```
##        tidyr        dplyr       readxl      ggplot2 randomForest          car 
##         TRUE         TRUE         TRUE         TRUE         TRUE         TRUE 
##      Metrics       raster    rasterVis        rgdal      mapview RColorBrewer 
##         TRUE         TRUE         TRUE         TRUE         TRUE         TRUE 
##      ggplot2           sf         glcm       ggpubr           sf     corrplot 
##         TRUE         TRUE         TRUE         TRUE         TRUE         TRUE 
##        caret         usdm 
##         TRUE         TRUE
```

# Alos 2 



Primero cargaremos las variables SAR ALOS 2, las cuales se descargaron previamente con el código de Google earth engine (https://code.earthengine.google.com/?scriptPath=users%2Feavelascop1%2FCAP_13_RADAR%3AALOS_2%20Download). 

El mosaico global PALSAR/PALSAR-2 de 25 m es una imagen SAR global perfecta creada mediante el mosaico de franjas de imágenes SAR de PALSAR/PALSAR-2. para el dataset de GEE Las imágenes SAR se ortorectificaron  y se corrigió la pendiente utilizando el modelo de superficie digital ALOS World 3D - 30 m (AW3D30).

Adicional mente en el codigo proporcionado para la descarga del mosaico anual de ALOS 2 se aplico un filtro de de "SPECKLE" para reducir el moteado característico de las imágenes SAR

```r
getwd()
```

```
## [1] "D:/cap_13_Radar"
```

```r
alos2 <- stack(list.files(path="D:/cap_13_Radar/ALOS_15_SPK",full.names=TRUE))
plot(alos2)
names(alos2)
```

```
## [1] "HH" "HV"
```

```r
plot(alos2)
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

como los valores de las Imagenes ALOS 2 vienen por efecto con valores de dNúmeros digitales (DN) de 16 bits. Los valores de DN se pueden convertir a valores gamma cero en unidades de decibelios (dB) usando la siguiente ecuación:

 γ₀ = 10log₁₀(DN²) - 83.0 dB
 


```r
alos2_Gamma_dB <- 10 * log10 (alos2^2) - 83.0 
plot(alos2_Gamma_dB )
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
A continuación, convierta los valores de dB a retrodispersión de potencia aplicando la siguiente ecuación:

Gamma_pw = 10^(0,1*Gamma_dB)


```r
alos2_Gamma_pw <-  10^(0.1*alos2_Gamma_dB)

names(alos2_Gamma_pw)
```

```
## [1] "HH" "HV"
```

```r
plot(alos2_Gamma_pw)
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Sentinel-1

La misión Sentinel-1 proporciona datos de un instrumento de radar de apertura sintética (SAR) de banda C de doble polarización a 5,405 GHz (banda C). Esta colección incluye las escenas de rango de terreno detectado (GRD) S1, procesadas con Sentinel-1 Toolbox para generar un producto calibrado y ortocorregido. La colección se actualiza diariamente. Los nuevos activos se incorporan dentro de los dos días posteriores a su disponibilidad.

Esta colección contiene todas las escenas de GRD. Cada escena tiene 3 resoluciones (10, 25 o 40 metros), 4 combinaciones de bandas (correspondientes a la polarización de la escena) y 3 modos de instrumento. El uso de la colección en un contexto de mosaico probablemente requerirá filtrar hasta un conjunto homogéneo de bandas y parámetros. Cada escena contiene 1 o 2 de 4 bandas de polarización posibles, dependiendo de la configuración de polarización del instrumento. Las combinaciones posibles son monobanda VV o HH, y doble banda VV+VH y HH+HV:

VV: copolarización única, transmisión vertical/recepción vertical
HH: copolarización única, transmisión horizontal/recepción horizontal
VV + VH: polarización cruzada de doble banda, transmisión vertical/recepción horizontal
HH + HV: polarización cruzada de doble banda, transmisión horizontal/recepción vertical
Cada escena también incluye una banda de 'ángulo' adicional que contiene el ángulo de incidencia aproximado desde el elipsoide en grados en cada punto. Esta banda se genera interpolando la propiedad 'incidenceAngle' del campo cuadriculado 'geolocationGridPoint' proporcionado con cada activo.

Cada escena fue preprocesada con Sentinel-1 Toolbox siguiendo los siguientes pasos:

Eliminación de ruido térmico
Calibración radiométrica
Corrección del terreno utilizando SRTM 30 o ASTER DEM para áreas superiores a 60 grados de latitud, donde SRTM no está disponible. Los valores finales corregidos por el terreno se convierten a decibeles mediante escala logarítmica (10*log10(x)).


Ahora procederemos a cargar el dataset SAR sentinel 1 que fueron descargados utilizando el script para google earth engine (https://code.earthengine.google.com/?scriptPath=users%2Feavelascop1%2FCAP_13_RADAR%3ASENTINEL1), como en el script de Alos2 tambien fue aplicado un filtro de Speckle.


```r
S1_2015 <- stack(list.files(path="D:/cap_13_Radar/S1_2015_SPK",full.names=TRUE))

names(S1_2015) <- c("vhAscDesc_2015", "vhAsc_2015","vhDesc_2015" ,"vvAscDesc_2015", "vvAsc_2015","vvDesc_2015")


#resampleamos al tamaño de pixel de las imagenes ALOS2
S1_2015 <- resample(S1_2015, alos2_Gamma_pw)

plot(S1_2015)
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Los datos del dataset de sentinel 1 de GEE vienen en valores de DB, por lo que para comvertirlos en valores de Gamma PW debemos utilizar la formula usada anteriormente con los datos de Alos "

```r
S1_2015_Gamma_pw <-  10^(0.1*S1_2015)

plot(S1_2015_Gamma_pw)
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
names_S1_2015 <- names(S1_2015_Gamma_pw)
```

## Landsat 8

carga indices Landsat

Los datos SAR funcionan mejor cuando se combinan con otro tipo de sensores ópticos como lidar o imágenes ópticas satelitales, en nuestro caso de estudio utilizaremos datos opticos ya que las colecciones son libres y tienen cobertura mundial, por lo que a continuación haremos la carga de los indices satelitales landsat 8 calculados previamente en el codigo de google earth engine: https://code.earthengine.google.com/?scriptPath=users%2Feavelascop1%2FCAP_13_RADAR%3AINDEX_L8



```r
LANDSAT <- stack(list.files(path="D:/cap_13_Radar/LANDSAT_2015",full.names=TRUE))

LANDSAT <- resample(LANDSAT, alos2_Gamma_pw)

plot(LANDSAT)
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Cargamos los datos de las parcelas de campo, las cuales se encuentra en un shp denominado inventario_biomasawgs84.shp


```r
getwd()
```

```
## [1] "D:/cap_13_Radar"
```

```r
parcelas <- st_read("D:/cap_13_Radar/inventario_biomasa_wgs84/inventario_biomasa_wgs84.shp")
```

```
## Reading layer `inventario_biomasa_wgs84' from data source 
##   `D:\cap_13_Radar\inventario_biomasa_wgs84\inventario_biomasa_wgs84.shp' 
##   using driver `ESRI Shapefile'
```

```
## Warning in CPL_read_ogr(dsn, layer, query, as.character(options), quiet, : GDAL
## Error 1: PROJ: proj_identify: C:\Program
## Files\PostgreSQL\14\share\contrib\postgis-3.1\proj\proj.db lacks
## DATABASE.LAYOUT.VERSION.MAJOR / DATABASE.LAYOUT.VERSION.MINOR metadata. It
## comes from another PROJ installation.
```

```
## Simple feature collection with 2107 features and 11 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: -2.865975 ymin: 37.14839 xmax: -2.193143 ymax: 37.35033
## Geodetic CRS:  WGS 84
```

```r
plot(parcelas[3])
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
names(parcelas)
```

```
##  [1] "FID"      "OBJECTI"  "N_Cintf"  "Dnm_cm"   "Npies_h"  "AB_m2_h" 
##  [7] "Dmc_cm"   "Ho_m"     "Bttl_Kg"  "MgC.pr_"  "Ctt_MC."  "geometry"
```
Revisamos y limpiamos los datos de las parcelas 


```r
# Estudio previo de los datos
# Se carga la librería 'car' para usar algunas funciones útiles.


# Se imprime un resumen estadístico de la variable 'Ctt_MC.' en el dataframe 'parcelas'.
summary(parcelas$Ctt_MC.)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   11.18   29.09   37.76   57.55  331.41
```

```r
# Se crea un diagrama de caja (boxplot) para visualizar la distribución de la variable 'Ctt_MC.' en 'parcelas'.
Boxplot(parcelas$Ctt_MC.)
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```
##  [1]  779  838  847  996 1752 1073 1278 1617 1031 1213
```

```r
# Se identifican los valores atípicos (outliers) de la variable 'Ctt_MC.' utilizando boxplot.
outliners <- boxplot(parcelas$Ctt_MC.)$out
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
outliners
```

```
##  [1] 331.4067 250.3730 196.3495 130.4630 127.2519 127.9674 131.3214 128.0279
##  [9] 149.0483 132.7146 138.2332 132.4152 135.2561 128.2277 129.4878 134.6589
## [17] 141.0414
```

```r
# Se filtran los datos para mantener solo aquellos cuyo valor en 'Ctt_MC.' sea menor o igual a 127.2519.
parcelas <- subset.data.frame(parcelas, parcelas$Ctt_MC. <= 120 )

# Se imprime un resumen estadístico de 'Ctt_MC.' después de filtrar los valores.
summary(parcelas$Ctt_MC.)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   11.05   28.55   36.42   55.87  119.38
```

```r
# Se crea un nuevo boxplot para visualizar la distribución actualizada de 'Ctt_MC.'.
Boxplot(parcelas$Ctt_MC.)
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

```r
# Se imprime un resumen estadístico adicional de 'Ctt_MC.' después del filtrado.
summary(parcelas$Ctt_MC.)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   11.05   28.55   36.42   55.87  119.38
```

```r
# Se crea un histograma para visualizar la distribución de 'Ctt_MC.' después del filtrado.
hist(parcelas$Ctt_MC.)
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-9-4.png)<!-- -->

```r
# Se identifican los índices de las observaciones cuyo valor en 'Ctt_MC.' es igual a cero.
which(parcelas$Ctt_MC. == 0)
```

```
## [1] 696 786 792 820 828
```

```r
# Se eliminan las observaciones donde 'Ctt_MC.' es igual a cero del dataframe 'parcelas'.
parcelas <- parcelas[-which(parcelas$Ctt_MC. == 0),]

# Se crea otro histograma para visualizar la distribución de 'Ctt_MC.' después de eliminar los valores iguales a cero.
hist(parcelas$Ctt_MC.)
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-9-5.png)<!-- -->

#Dem Alos world 3d

Ahora vamos a cargar el DEM derivado del la colección ALOS World 3D - 30m (AW3D30), la cual es un conjunto de datos de modelo de superficie digital (DSM) global con una resolución horizontal de aproximadamente 30 metros (malla de 1 arco segundo).

El script de descarga de GEE se encuentra en:  https://code.earthengine.google.com/?scriptPath=users%2Feavelascop1%2FCAP_13_RADAR%3AALOS_DEM


```r
# Cargar el DEM
DEM <- raster("D:/cap_13_Radar/DEM/DEM_ALOS.tif")
# Mostrar el mapa de aspecto
plot(DEM , main = "MAPA DE ELEVACIONES DEM")
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
# Calcular el aspect
ASPECT <- terrain(DEM, opt = "aspect")

# Mostrar el mapa de aspecto
plot(ASPECT, main = "Mapa de Aspecto")
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

```r
# Calcular la pendiente
SLOPE <- terrain(DEM, opt = "slope")

# Mostrar el mapa de pendiente
plot(SLOPE, main = "Mapa de Pendiente")
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-10-3.png)<!-- -->

```r
TOPO <- stack(DEM, SLOPE, ASPECT)


TOPO <- resample(TOPO, alos2_Gamma_pw)
plot(TOPO)
```

![](ejemplo_RADAR_2_files/figure-html/unnamed-chunk-10-4.png)<!-- -->

```r
names(TOPO) <-  c("DEM", "SLOPE", "ASPECT")
```

Cargamos los polígonos de la delimitacion de los pinares en filabres por especie

```r
pinus <- rgdal::readOGR("D:/cap_13_Radar/inventario_biomasa_wgs84/pinos_filabres_wgs_84.shp")
```

```
## Warning: OGR support is provided by the sf and terra packages among others
```

```
## Warning: OGR support is provided by the sf and terra packages among others
```

```
## Warning: OGR support is provided by the sf and terra packages among others
```

```
## Warning: OGR support is provided by the sf and terra packages among others
```

```
## Warning: OGR support is provided by the sf and terra packages among others
```

```
## Warning: OGR support is provided by the sf and terra packages among others
```

```
## Warning: OGR support is provided by the sf and terra packages among others
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "D:\cap_13_Radar\inventario_biomasa_wgs84\pinos_filabres_wgs_84.shp", layer: "pinos_filabres_wgs_84"
## with 1661 features
## It has 4 fields
```
