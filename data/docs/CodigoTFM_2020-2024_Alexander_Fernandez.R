

############################################
# LIBRERÍAS
############################################
library(readxl)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(sf)

############################################
# 1. LECTURA DE DATOS
############################################
data1 <- read_excel(
  "C:/Users/afernandeze/Documents/Consolidado_Diarios_2020_2024.xlsx"
)

############################################
# 2. DEFINICIÓN DE COLUMNAS
############################################
col_texto <- "summary"

palabras_robo <- c(
  "Asalto","Asaltos","Asaltante","Asaltantes","Asaltar","Asaltaron","Asalta",
  "Delincuencia","Delincuente","Delincuentes","Delinquir",
  "Robos","Robo","Robaba","Robaban","Robaron","Rateros","Ratero","Roba","Robar",
  "Ladrones","Ladrón","Raquetero","Raqueteros","Latrocinio","Despojo","Rapiña",
  "Saqueo","Atracos","Atraco","Expropiación","Extorsión","Hurto",
  "Sustracción","Estafas","Estafadores","Estafa","Estafan",
  "Hampones","Hampón","Sustrae","Sustraer","Maleante","Maleantes"
)

palabras_distritos <- c(
  "Ancón","Ate","Barranco","Breña","Carabayllo","Chaclacayo","Chorrillos","Cieneguilla","Comas","El Agustino",
  "Independencia","Jesús María","La Molina","La Victoria","Cercado de Lima","Lince",
  "Los Olivos","Chosica","Lurín","Magdalena del Mar",
  "Miraflores","Pachacámac","Pucusana","Pueblo Libre","Puente Piedra",
  "Punta Hermosa","Punta Negra","Rímac","San Bartolo","San Borja",
  "San Isidro","San Juan de Lurigancho","San Juan de Miraflores","San Luis",
  "San Martín de Porres","San Miguel","Santa Anita",
  "Santa María del Mar","Santa Rosa","Santiago de Surco","Surquillo",
  "Villa el Salvador","Villa Maria del Triunfo","Callao","Ventanilla",
  "Carmen de la Legua Reynoso","Bellavista","La Perla","Mi Perú","La Punta"
)

############################################
# 3. VALIDACIÓN DE COLUMNAS EXISTENTES
############################################
palabras_robo <- intersect(palabras_robo, names(data1))
palabras_distritos <- intersect(palabras_distritos, names(data1))

if (length(palabras_robo) == 0 | length(palabras_distritos) == 0) {
  stop("No se encontraron columnas válidas de robos o distritos en el dataset")
}

############################################
# 4. CONVERSIÓN SEGURA A BINARIO 
############################################
convertir_binario <- function(x) {
  x <- toupper(trimws(as.character(x)))
  as.numeric(x == "SI")
}

data1[palabras_robo] <- lapply(
  data1[palabras_robo],
  convertir_binario
)

data1[palabras_distritos] <- lapply(
  data1[palabras_distritos],
  convertir_binario
)

############################################
# 5. FRECUENCIAS
############################################
frecuencia_robo <- colSums(data1[palabras_robo], na.rm = TRUE)
frecuencia_distritos <- colSums(data1[palabras_distritos], na.rm = TRUE)

tabla_frecuencia_robo <- data.frame(
  Delito = names(frecuencia_robo),
  Frecuencia = as.numeric(frecuencia_robo)
)

tabla_frecuencia_distritos <- data.frame(
  Distrito = names(frecuencia_distritos),
  Frecuencia = as.numeric(frecuencia_distritos)
)

print(tabla_frecuencia_robo)
print(tabla_frecuencia_distritos)

############################################
# 6. MATRIZ DE CONTINGENCIA
############################################

# 0) Paquetes
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)

# 1) Cargar el Excel base de datos
archivo <- "Consolidado_Diarios_2020_2021_Pandemia.xlsx"

# Si el excel tiene varias hojas, aquí lee la primera.
# (Si sabes el nombre, puedes usar sheet="NOMBRE_HOJA")
data1 <- read_excel(archivo, sheet = 1)
data1 <- as.data.frame(data1)

# 2) Las listas 
palabras_robo <- c(
  "Asalto","Asaltos","Asaltante","Asaltantes","Asaltar","Asaltaron","Asalta",
  "Delincuencia","Delincuente","Delincuentes","Delinquir","Robos","Robo","Robaba","Robaban",
  "Robaron","Rateros","Ratero","Roba","Roba","Ladrones","Ladrón","Raquetero","Raqueteros",
  "Latrocinio","Despojo","Rapiña","Saqueo","Atracos","Atraco","Expropiación","Extorsión",
  "Hurto","Sustracción","Estafas","Estafadores","Estafa","Estafan","Hampones","Hampón",
  "Sustrae","Sustraer","Maleante","Maleantes"
)

palabras_distritos <- c(
  "Ancón","Ate","Barranco","Breña","Carabayllo","Chaclacayo","Chorrillos","Cieneguilla","Comas","El Agustino",
  "Independencia","Jesús María","La Molina","La Victoria","Cercado de Lima","Lince",
  "Los Olivos","Chosica","Lurín","Magdalena del Mar",
  "Miraflores","Pachacámac","Pucusana","Pueblo Libre","Puente Piedra",
  "Punta Hermosa","Punta Negra","Rímac","San Bartolo","San Borja",
  "San Isidro","San Juan de Lurigancho","San Juan de Miraflores","San Luis",
  "San Martín de Porres","San Miguel","Santa Anita",
  "Santa María del Mar","Santa Rosa","Santiago de Surco","Surquillo",
  "Villa el Salvador","Villa Maria del Triunfo","Callao","Ventanilla",
  "Carmen de la Legua Reynoso","Bellavista","La Perla","Mi Perú","La Punta"
)

# 3) Quitar duplicados SOLO para que no te “duplique” filas/columnas en la matriz
#    (en tu lista de robo tienes "Roba" repetido)
palabras_robo <- palabras_robo[!duplicated(palabras_robo)]
palabras_distritos <- palabras_distritos[!duplicated(palabras_distritos)]

# 4) Normalizar nombres para empatar columnas aunque cambien tildes/mayúsculas
norm_name <- function(x){
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")  # quita tildes
  x <- tolower(x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# 5) Mapear tus listas a los nombres reales del Excel (data1)
nms <- names(data1)
nms_norm <- norm_name(nms)

idx_robo <- match(norm_name(palabras_robo), nms_norm)
idx_dist <- match(norm_name(palabras_distritos), nms_norm)

# 6) Asegurar que TODAS existan como columnas (si falta alguna, se crea en 0)
ensure_col <- function(df, colname){
  if (!colname %in% names(df)) df[[colname]] <- 0L
  df
}

# nombres de columnas a usar (si existe columna real, usa ese nombre; si no, crea con tu nombre)
cols_robo <- character(length(palabras_robo))
for (i in seq_along(palabras_robo)) {
  if (!is.na(idx_robo[i])) cols_robo[i] <- nms[idx_robo[i]] else {
    cols_robo[i] <- palabras_robo[i]
    data1 <- ensure_col(data1, cols_robo[i])
  }
}

cols_dist <- character(length(palabras_distritos))
for (i in seq_along(palabras_distritos)) {
  if (!is.na(idx_dist[i])) cols_dist[i] <- nms[idx_dist[i]] else {
    cols_dist[i] <- palabras_distritos[i]
    data1 <- ensure_col(data1, cols_dist[i])
  }
}

# 7) Convertir SI/NO (y variantes) a 0/1 de forma robusta
to01 <- function(x){
  x <- trimws(toupper(as.character(x)))
  as.integer(x %in% c("SI","SÍ","1","TRUE","T"))
}

data1[unique(cols_robo)] <- lapply(data1[unique(cols_robo)], to01)
data1[unique(cols_dist)] <- lapply(data1[unique(cols_dist)], to01)

# 8) Matriz de contingencia (conteos de co-ocurrencia por fila)
Xr <- as.matrix(data1[cols_robo])
Xd <- as.matrix(data1[cols_dist])

matriz_contingencia <- t(Xr) %*% Xd
matriz_contingencia <- as.matrix(matriz_contingencia)

# Mostrar con nombres de listas
rownames(matriz_contingencia) <- palabras_robo
colnames(matriz_contingencia) <- palabras_distritos

View(matriz_contingencia)

# 9) Reportes útiles (para validar contra “conteo rápido”)
faltan_robo <- palabras_robo[is.na(idx_robo)]
faltan_dist <- palabras_distritos[is.na(idx_dist)]

total_SI_palabras <- sum(Xr, na.rm = TRUE)                 # "conteo rápido" de SI en palabras
total_SI_distritos <- sum(Xd, na.rm = TRUE)
suma_matriz <- sum(matriz_contingencia, na.rm = TRUE)      # co-ocurrencias robos x distritos

list(
  filas_total = nrow(data1),
  total_SI_en_palabras = total_SI_palabras,
  total_SI_en_distritos = total_SI_distritos,
  suma_total_matriz_coocurrencias = suma_matriz,
  palabras_no_encontradas_en_excel = faltan_robo,
  distritos_no_encontrados_en_excel = faltan_dist
)

View(matriz_contingencia)

############################################
# 7. ANÁLISIS DE CORRESPONDENCIAS
############################################
res_ca <- CA(matriz_contingencia, graph = FALSE)

fviz_ca_biplot(
  res_ca,
  repel = TRUE,
  title = "Análisis de Correspondencias: Robos vs Distritos"
)

############################################
# 8. DENDROGRAMA
############################################
hc <- hclust(dist(matriz_contingencia), method = "ward.D2")
plot(hc, main = "Dendrograma de Robos y Distritos")

############################################
# 8. DENDROGRAMA DE CLUSTERS DE DISTRITOS
############################################

############################################
# 8. DENDROGRAMA DE CLUSTERS DE DISTRITOS
############################################

# Verifica que exista la matriz
stopifnot(exists("matriz_contingencia"))

# Crear objeto (filas = distritos)
X_dist <- t(matriz_contingencia)

# Distancias y clustering
d_dist <- dist(X_dist, method = "euclidean")
hc_dist <- hclust(d_dist, method = "ward.D2")

# Plot
plot(
  hc_dist,
  main = "Dendrograma de Clusters de Distritos (Perfil de Robos)",
  xlab = "Distritos",
  sub = "",
  cex = 0.6
)

# (Opcional) número de clusters
k <- 4
rect.hclust(hc_dist, k = k, border = 2:(k+1))

# (Opcional) tabla de clusters por distrito
clusters_distritos <- cutree(hc_dist, k = k)
print(clusters_distritos)



############################################
# 9. NUBES DE PALABRAS
############################################
wordcloud(
  words = names(frecuencia_robo),
  freq = frecuencia_robo,
  min.freq = 1,
  colors = brewer.pal(8, "Dark2")
)

wordcloud(
  words = names(frecuencia_distritos),
  freq = frecuencia_distritos,
  min.freq = 1,
  colors = brewer.pal(8, "Set3")
)

############################################
# 10. MAPA
############################################

# Instalar paquetes (solo la primera vez)
# install.packages(c("geodata", "terra", "sf", "dplyr", "ggplot2", "viridis", "scales"))

# Cargar librerías
library(geodata)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)   # para escala viridis
library(scales)    # para scales::comma

# Descargar límites administrativos
peru <- gadm(country = "PER", level = 3, path = tempdir())

# Convertir a sf
peru_sf <- st_as_sf(peru)

# Filtrar Lima Metropolitana
lima_met <- peru_sf[
  peru_sf$NAME_1 == "Lima Province" & peru_sf$NAME_2 == "Lima",
]

# Datos simulados
set.seed(123)

pob_pandemia <- data.frame(
  distrito = lima_met$NAME_3,
  poblacion_pandemia = c(1,19,12,4,4,2,13,3,18,14,27,8,13,72,27,6,22,1,5,1,18,2,1,1,7,3,0,13,0,12,12,21,3,10,8,17,5,0,15,0,1,13,1)
)

# Unir datos
lima_pob <- lima_met %>%
  left_join(
    pob_pandemia,
    by = c("NAME_3" = "distrito")
  )

# Mapa con escala log10 SIN que aparezca (log10) en la leyenda
ggplot(lima_pob) +
  geom_sf(
    aes(fill = poblacion_pandemia),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log10",
    name = "Población total",
    labels = comma
  ) +
  labs(
    title = "Lima Metropolitana",
    subtitle = "Población distrital",
    caption = "Datos simulados | Mapa GADM"
  ) +
  theme_minimal()
