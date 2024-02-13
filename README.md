---
title: "Cap 13 Radar"
author: "EDWARD VELASCO"
date: "2024-02-07"
output: 
  html_document:
    keep_md: true
---
```{r setup, include=FALSE}
# Establecer el directorio de trabajo
knitr::opts_knit$set(root.dir = "D:/cap_13_Radar")

#opciones para cargar y guardar la memoria de trabajo .Rdata
#load("EJEMPLO_RADAR.RData")
#save.image("EJEMPLO_RADAR.RData")
```

CARGAR LIBRERIAS
```{r message=FALSE, warning=FALSE}
pck <- c("tidyr", "dplyr", "readxl", "ggplot2", "randomForest", "car", "Metrics", "raster", "rasterVis", "rgdal", "mapview", "RColorBrewer", "ggplot2", "sf", "glcm", "ggpubr", "sf", "corrplot", "caret", "usdm")

sapply(pck, require, character.only=TRUE)

```

##Alos 2


Primero cargaremos las variables SAR ALOS 2, las cuales se descargaron previamente con el código de Google earth engine (https://code.earthengine.google.com/?scriptPath=users%2Feavelascop1%2FCAP_13_RADAR%3AALOS_2%20Download). 

El mosaico global PALSAR/PALSAR-2 de 25 m es una imagen SAR global perfecta creada mediante el mosaico de franjas de imágenes SAR de PALSAR/PALSAR-2. para el dataset de GEE Las imágenes SAR se ortorectificaron  y se corrigió la pendiente utilizando el modelo de superficie digital ALOS World 3D - 30 m (AW3D30).

Adicional mente en el codigo proporcionado para la descarga del mosaico anual de ALOS 2 se aplico un filtro de de "SPECKLE" para reducir el moteado característico de las imágenes SAR
```{r}
getwd()
alos2 <- stack(list.files(path="D:/cap_13_Radar/ALOS_15_SPK",full.names=TRUE))
plot(alos2)
names(alos2)

plot(alos2)

```

como los valores de las Imagenes ALOS 2 vienen por efecto con valores de dNúmeros digitales (DN) de 16 bits. Los valores de DN se pueden convertir a valores gamma cero en unidades de decibelios (dB) usando la siguiente ecuación:

 γ₀ = 10log₁₀(DN²) - 83.0 dB
 

```{r}
alos2_Gamma_dB <- 10 * log10 (alos2^2) - 83.0 
plot(alos2_Gamma_dB )
                
```
A continuación, convierta los valores de dB a retrodispersión de potencia aplicando la siguiente ecuación:

Gamma_pw = 10^(0,1*Gamma_dB)

```{r}

alos2_Gamma_pw <-  10^(0.1*alos2_Gamma_dB)

names(alos2_Gamma_pw)
plot(alos2_Gamma_pw)

```

##Sentinel-1

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

```{r}

S1_2015 <- stack(list.files(path="D:/cap_13_Radar/S1_2015_SPK",full.names=TRUE))

names(S1_2015) <- c("vhAscDesc_2015", "vhAsc_2015","vhDesc_2015" ,"vvAscDesc_2015", "vvAsc_2015","vvDesc_2015")


#resampleamos al tamaño de pixel de las imagenes ALOS2
S1_2015 <- resample(S1_2015, alos2_Gamma_pw)

plot(S1_2015)


```

Los datos del dataset de sentinel 1 de GEE vienen en valores de DB, por lo que para comvertirlos en valores de Gamma PW debemos utilizar la formula usada anteriormente con los datos de Alos "
```{r}
S1_2015_Gamma_pw <-  10^(0.1*S1_2015)

plot(S1_2015_Gamma_pw)
names_S1_2015 <- names(S1_2015_Gamma_pw)

```

##Landsat 8

carga indices Landsat

Los datos SAR funcionan mejor cuando se combinan con otro tipo de sensores ópticos como lidar o imágenes ópticas satelitales, en nuestro caso de estudio utilizaremos datos opticos ya que las colecciones son libres y tienen cobertura mundial, por lo que a continuación haremos la carga de los indices satelitales landsat 8 calculados previamente en el codigo de google earth engine: https://code.earthengine.google.com/?scriptPath=users%2Feavelascop1%2FCAP_13_RADAR%3AINDEX_L8


```{r}
LANDSAT <- stack(list.files(path="D:/cap_13_Radar/LANDSAT_2015",full.names=TRUE))

LANDSAT <- resample(LANDSAT, alos2_Gamma_pw)

plot(LANDSAT)
```

Cargamos los datos de las parcelas de campo, las cuales se encuentra en un shp denominado inventario_biomasawgs84.shp

```{r}

getwd()
parcelas <- st_read("D:/cap_13_Radar/inventario_biomasa_wgs84/inventario_biomasa_wgs84.shp")
plot(parcelas[3])
names(parcelas)

```
Revisamos y limpiamos los datos de las parcelas 

```{r}
# Estudio previo de los datos
# Se carga la librería 'car' para usar algunas funciones útiles.


# Se imprime un resumen estadístico de la variable 'Ctt_MC.' en el dataframe 'parcelas'.
summary(parcelas$Ctt_MC.)

# Se crea un diagrama de caja (boxplot) para visualizar la distribución de la variable 'Ctt_MC.' en 'parcelas'.
Boxplot(parcelas$Ctt_MC.)

# Se identifican los valores atípicos (outliers) de la variable 'Ctt_MC.' utilizando boxplot.
outliners <- boxplot(parcelas$Ctt_MC.)$out
outliners

# Se filtran los datos para mantener solo aquellos cuyo valor en 'Ctt_MC.' sea menor o igual a 127.2519.
parcelas <- subset.data.frame(parcelas, parcelas$Ctt_MC. <= 120 )

# Se imprime un resumen estadístico de 'Ctt_MC.' después de filtrar los valores.
summary(parcelas$Ctt_MC.)

# Se crea un nuevo boxplot para visualizar la distribución actualizada de 'Ctt_MC.'.
Boxplot(parcelas$Ctt_MC.)

# Se imprime un resumen estadístico adicional de 'Ctt_MC.' después del filtrado.
summary(parcelas$Ctt_MC.)

# Se crea un histograma para visualizar la distribución de 'Ctt_MC.' después del filtrado.
hist(parcelas$Ctt_MC.)

# Se identifican los índices de las observaciones cuyo valor en 'Ctt_MC.' es igual a cero.
which(parcelas$Ctt_MC. == 0)

# Se eliminan las observaciones donde 'Ctt_MC.' es igual a cero del dataframe 'parcelas'.
parcelas <- parcelas[-which(parcelas$Ctt_MC. == 0),]

# Se crea otro histograma para visualizar la distribución de 'Ctt_MC.' después de eliminar los valores iguales a cero.
hist(parcelas$Ctt_MC.)

```

#Dem Alos world 3d

Ahora vamos a cargar el DEM derivado del la colección ALOS World 3D - 30m (AW3D30), la cual es un conjunto de datos de modelo de superficie digital (DSM) global con una resolución horizontal de aproximadamente 30 metros (malla de 1 arco segundo).

El script de descarga de GEE se encuentra en:  https://code.earthengine.google.com/?scriptPath=users%2Feavelascop1%2FCAP_13_RADAR%3AALOS_DEM

```{r}
# Cargar el DEM
DEM <- raster("D:/cap_13_Radar/DEM/DEM_ALOS.tif")
# Mostrar el mapa de aspecto
plot(DEM , main = "MAPA DE ELEVACIONES DEM")

# Calcular el aspect
ASPECT <- terrain(DEM, opt = "aspect")

# Mostrar el mapa de aspecto
plot(ASPECT, main = "Mapa de Aspecto")

# Calcular la pendiente
SLOPE <- terrain(DEM, opt = "slope")

# Mostrar el mapa de pendiente
plot(SLOPE, main = "Mapa de Pendiente")


TOPO <- stack(DEM, SLOPE, ASPECT)


TOPO <- resample(TOPO, alos2_Gamma_pw)
plot(TOPO)

names(TOPO) <-  c("DEM", "SLOPE", "ASPECT")
```

Cargamos los polígonos de la delimitacion de los pinares en filabres por especie
```{r}
pinus <- rgdal::readOGR("D:/cap_13_Radar/inventario_biomasa_wgs84/pinos_filabres_wgs_84.shp")
crs(pinus) <- "+proj=longlat +datum=WGS84"

mapview(pinus)  
# Cargar la librería sf


# Filtrar por especie "PH"
pinus_PH <- pinus[pinus$ESPECIE == "PH", ]
pinus_PP <- pinus[pinus$ESPECIE == "PT", ]


```


Extraemos los puntos de coincidencia entre las parcelas y los rasters preparados anteriormente
```{r}

#stack final de los set ya preparados

rasters1 <- stack(alos2_Gamma_pw, S1_2015_Gamma_pw,LANDSAT, TOPO)

plot(rasters1)

#hacemos el extract con un buffer de 50 metros para minimizar los posibles errores de proyección de coordenadas 

rasters1_extract <- raster::extract(rasters1,parcelas, buffer=50, fun=mean)

#extraemos los puntos
MALLA_SAR_50<-cbind(parcelas,rasters1_extract)

write.csv(MALLA_SAR_50, "MALLA_SAR_50.csv")


plot(MALLA_SAR_50, max.plot = 50)
names(MALLA_SAR_50)
ds <- as.data.frame(MALLA_SAR_50)
ds <- ds[-48] 

#filtramos el dataset a nuestras especies de interés

DS_PH <- filter(ds, N_Cintf == "Pinus halepensis") 
DS_PP <- filter(ds, N_Cintf == "Pinus pinaster") 

```


```{r}


### Calcular VIF basandonos en las funciones vifcor y vifstep
#############################################################
var.df <- as.data.frame(rasters1)
 
v.1 <-vifcor(var.df , th=0.8) # Busca un par de variables que tengan la maxima correlacion lineal (mayor que th) y excluye una de ellas que tenga mayor VIF. El procedimiento se repite hasta que no quede ninguna variable con alto coeficiente de correlacion (mayor que el umbral) con otras variables
v.1

v.2 <- vifstep(var.df, th=7) # Calcula el VIF para todas las variables, excluye una con el VIF mas alto (mayor que el umbral), repite el procedimiento hasta que no quede ninguna variable con VIF mayor que th.
v.2

re1 <- exclude(rasters1,v.2)
re1
names(re1)
plot(re1)

```
Ahora dividiremos el set de datos en 80% de datos para entrenamiento y 20% para testeo para los dos datasets
de Pinus pinaster y Pinus halepensis

```{r}
# Dividir los datos en un conjunto de entrenamiento (80%) y un conjunto de prueba 20%) para PH
set.seed(123)
train_index <- createDataPartition(y =DS_PH$Ctt_MC., p = 0.8, list = FALSE)
train_PH <-DS_PH[train_index, ]
test_PH <-DS_PH[-train_index, ]

summary(train_PH)
summary(test_PH)

# 

# Dividir los datos en un conjunto de entrenamiento (70%) y un conjunto de prueba (30%)
set.seed(123)
train_index <- createDataPartition(y =DS_PP$Ctt_MC., p = 0.8, list = FALSE)
train_PP <-DS_PP[train_index, ]
test_PP <-DS_PP[-train_index, ]

summary(train_PP)
summary(test_PP)


fitControl <- trainControl(method = "repeatedcv", number=10, repeats=5,
                              #classProbs = TRUE,
                              returnResamp="all",
                              returnData = TRUE,
                              savePredictions=TRUE)

```




MODELO Pinus Pinaster BIOMASA
```{r}

model_PP_AGB <- train (Ctt_MC. ~ 
HH + HV +  vhDesc_2015 + vvAscDesc_2015+ vvAsc_2015    
+ vvDesc_2015 + SR_B2 + DVI +  LAI + NDBI          
+ DEM +  SLOPE +  ASPECT     
,data=train_PP, method="rf",
                trControl=fitControl,
                 prox=TRUE,
                 fitBest = TRUE,
                 )


model_PP_AGB

```
hacemos una evaluación con el set de testeo
```{r}
Predict_model_PP_AGB <- predict(model_PP_AGB, newdata=test_PP)

Observed_ctt <- test_PP$Ctt_MC.

tmp_ctt <- data.frame(Predict_model_PP_AGB, Observed_ctt)

ev<- caret::postResample(test_PP$Ctt_MC., Predict_model_PP_AGB)

ev
```

por ultimo graficamos el modelo observado vs el modelo predicho
```{r}
m1_PN <- tmp_ctt %>% dplyr::select( Predict_model_PP_AGB, Observed_ctt) %>%
  pivot_longer(cols = -Observed_ctt) %>%
  ggplot(aes(x = Observed_ctt, y = value)) + geom_point()+
  stat_smooth(aes(), method="lm", formula=y ~ x) +theme_bw()+
  ylab("Model Pinud pinaster (Mg·ha−1)")+ xlab("observed (Mg·ha−1)")+
  ggtitle("MODEL RF PP VS OBSERVED",subtitle = "R2=0.49 - RSME= 21.43 - %RSME= 16.90%")+
  geom_abline(intercept=0, slope=1, lwd=1, linetype=2, color="red")

 
m1_PN 

```
utilizamos la funcion predict para crear el Raster del modelo generado para Pp. y guardamos en el disco.
```{r}
MgC_PP<- predict(rasters1, model_PP_AGB)

plot(MgC_PP)

#cortamos al area de los poligonos correspondientes a PP
MgC_PP_mask <- mask(MgC_PP, pinus_PP)

#Guardamos los resultados
writeRaster(MgC_PP_mask,filename = paste ("D:/cap_13_Radar/RESULTADOS/","MgC_PP_2015"), bylayer=TRUE, format="GTiff", overwrite=TRUE, sep="")
```

MODELO Pinus Halepensis BIOMASA
```{r}

model_PH_AGB <- train (Ctt_MC. ~ 
HH + HV +  vhDesc_2015 + vvAscDesc_2015+ vvAsc_2015    
+ vvDesc_2015 + SR_B2 + DVI +  LAI + NDBI          
+ DEM +  SLOPE +  ASPECT    
,data=train_PH, method="rf",
                trControl=fitControl,
                 prox=TRUE,
                 fitBest = TRUE,
                 )


model_PH_AGB

```

```{r}
Predict_model_PH_AGB <- predict(model_PH_AGB, newdata=test_PH)

Observed_ctt <- test_PH$Ctt_MC.

tmp_ctt <- data.frame(Predict_model_PH_AGB, Observed_ctt)

ev<- caret::postResample(test_PH$Ctt_MC. ,Predict_model_PH_AGB)

ev

m1_PN <- tmp_ctt %>% dplyr::select( Predict_model_PH_AGB, Observed_ctt) %>%
  pivot_longer(cols = -Observed_ctt) %>%
  ggplot(aes(x = Observed_ctt, y = value)) + geom_point()+
  stat_smooth(aes(), method="lm", formula=y ~ x) +theme_bw()+
  ylab("Model RF Wt (Mg·ha−1)")+ xlab("observed (Mg·ha−1)")+
  ggtitle("MODEL RF Wt VS OBSERVED",subtitle = "R2=0.49 - RSME= 21.43 - %RSME= 16.90%")+
  geom_abline(intercept=0, slope=1, lwd=1, linetype=2, color="red")

 
m1_PN 

```
utilizamos la funcion predict para crear el Raster del modelo generado para Ph. y guardamos en el disco.
```{r}
MgC_PH<- predict(rasters1, model_PH_AGB)

plot(MgC_PH)

#cortamos al area de los poligonos correspondientes a PP
MgC_PH_mask <- mask(MgC_PH, pinus_PH)

#Guardamos resultados en el disco
writeRaster(MgC_PH_mask,filename = paste ("D:/cap_13_Radar/RESULTADOS/","MgC_PH_2015"), bylayer=TRUE, format="GTiff", overwrite=TRUE, sep="")

```
