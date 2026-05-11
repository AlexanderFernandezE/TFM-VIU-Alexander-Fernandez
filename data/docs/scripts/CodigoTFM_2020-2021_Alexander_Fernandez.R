

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
  "C:/Users/afernandeze/Documents/Consolidado_Noticias_2020_2021.xlsx"
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

############################################
# FRECUENCIAS DE PALABRAS Y DISTRITOS
# ASOCIADOS A DELITOS
############################################

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

############################################
# 5.1. LEER BASE
############################################

data1 <- read_excel("Consolidado_Noticias_2020_2021.xlsx")

############################################
# 5.2. PALABRAS ASOCIADAS A ROBO
############################################

palabras_robo <- c(
  "Asalto","Asaltos","Asaltante","Asaltantes","Asaltar",
  "Asaltaron","Asalta","Delincuencia","Delincuente",
  "Delincuentes","Delinquir","Robos","Robo","Robaba",
  "Robaban","Robaron","Rateros","Ratero","Roba",
  "Robar","Ladrones","Ladrón","Raquetero",
  "Raqueteros","Latrocinio","Despojo","Rapiña",
  "Saqueo","Atracos","Atraco","Expropiación",
  "Extorsión","Hurto","Sustracción","Estafas",
  "Estafadores","Estafa","Estafan","Hampones",
  "Hampón","Sustrae","Sustraer","Maleante",
  "Maleantes"
)

############################################
# 5.3. DISTRITOS (COLUMNAS AW A CT)
############################################

palabras_distritos <- names(data1)[49:98]

############################################
# 5.4. ASEGURAR SOLO COLUMNAS EXISTENTES
############################################

palabras_robo <- intersect(
  palabras_robo,
  names(data1)
)

palabras_distritos <- intersect(
  palabras_distritos,
  names(data1)
)

############################################
# 5.5. CONVERTIR SI/NO A 1/0
############################################

convertir_binario <- function(x){
  
  ifelse(
    toupper(trimws(as.character(x))) == "SI",
    1,
    0
  )
  
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
# 5.6. FRECUENCIA DE PALABRAS
############################################

frecuencia_robo <- colSums(
  data1[palabras_robo],
  na.rm = TRUE
)

tabla_frecuencia_robo <- data.frame(
  Delito = names(frecuencia_robo),
  Frecuencia = as.numeric(frecuencia_robo)
)

print(tabla_frecuencia_robo)

############################################
# 5.7. FILTRAR REGISTROS QUE TENGAN
# AL MENOS UNA PALABRA ASOCIADA A ROBO
############################################

filas_delito <- rowSums(
  data1[palabras_robo],
  na.rm = TRUE
) > 0

############################################
# 5.8. FRECUENCIA DE DISTRITOS
# ASOCIADOS A ROBO
############################################

frecuencia_distritos <- colSums(
  data1[filas_delito, palabras_distritos],
  na.rm = TRUE
)

tabla_frecuencia_distritos <- data.frame(
  Distrito = names(frecuencia_distritos),
  Frecuencia = as.numeric(frecuencia_distritos)
)

print(tabla_frecuencia_distritos)

############################################

############################################
# 5.9. EXTRAER FECHA Y AÑO - FORMATO ROBUSTO
############################################

data1 <- data1 %>%
  mutate(
    date_chr = as.character(date),
    
    fecha_1 = dmy(
      str_trim(str_extract(date_chr, "^[0-9]{2}/[0-9]{2}/[0-9]{4}"))
    ),
    
    fecha_2 = ymd(
      str_trim(str_extract(date_chr, "^[0-9]{4}-[0-9]{2}-[0-9]{2}"))
    ),
    
    fecha = coalesce(fecha_1, fecha_2),
    
    anio = year(fecha)
  )

############################################
# 5.10. CONTAR SOLO NOTICIAS ASOCIADAS A ROBO
############################################

frecuencia_anual <- data1 %>%
  filter(filas_delito) %>%
  filter(!is.na(anio)) %>%
  count(anio, name = "total_noticias")

print(frecuencia_anual)

############################################
# 5.11. GRÁFICO DE LÍNEAS
############################################

ggplot(
  frecuencia_anual,
  aes(
    x = anio,
    y = total_noticias,
    group = 1
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  scale_x_continuous(
    breaks = frecuencia_anual$anio
  ) +
  labs(
    title = "Número anual de noticias asociadas a robo",
    x = "Año",
    y = "Frecuencia de noticias"
  ) +
  theme_minimal()

############################################
# 6. MATRIZ DE CONTINGENCIA
############################################

# 0) Paquetes
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)

# 1) Cargar el Excel base de datos
archivo <- "Consolidado_Noticias_2020_2021.xlsx"

data1 <- read_excel(archivo, sheet = 1)
data1 <- as.data.frame(data1)

# 2) Las listas corregidas
palabras_robo <- c(
  "Asalto","Asaltos","Asaltante","Asaltantes","Asaltar","Asaltaron","Asalta",
  "Delincuencia","Delincuente","Delincuentes","Delinquir",
  "Robos","Robo","Robaba","Robaban","Robaron","Rateros","Ratero","Roba","Robar",
  "Ladrones","Ladrón","Raquetero","Raqueteros",
  "Latrocinio","Despojo","Rapiña","Saqueo","Atracos","Atraco","Expropiación","Extorsión",
  "Hurto","Sustracción","Estafas","Estafadores","Estafa","Estafan",
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

# 3) Quitar duplicados si existieran
palabras_robo <- palabras_robo[!duplicated(palabras_robo)]
palabras_distritos <- palabras_distritos[!duplicated(palabras_distritos)]

# Verificación de cantidad esperada
cat("Cantidad de palabras asociadas a robo:", length(palabras_robo), "\n")
cat("Cantidad de distritos:", length(palabras_distritos), "\n")

# 4) Normalizar nombres para empatar columnas aunque cambien tildes/mayúsculas
norm_name <- function(x){
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# 5) Mapear tus listas a los nombres reales del Excel
nms <- names(data1)
nms_norm <- norm_name(nms)

idx_robo <- match(norm_name(palabras_robo), nms_norm)
idx_dist <- match(norm_name(palabras_distritos), nms_norm)

# 6) Asegurar que TODAS existan como columnas
ensure_col <- function(df, colname){
  if (!colname %in% names(df)) df[[colname]] <- 0L
  df
}

cols_robo <- character(length(palabras_robo))
for (i in seq_along(palabras_robo)) {
  if (!is.na(idx_robo[i])) {
    cols_robo[i] <- nms[idx_robo[i]]
  } else {
    cols_robo[i] <- palabras_robo[i]
    data1 <- ensure_col(data1, cols_robo[i])
  }
}

cols_dist <- character(length(palabras_distritos))
for (i in seq_along(palabras_distritos)) {
  if (!is.na(idx_dist[i])) {
    cols_dist[i] <- nms[idx_dist[i]]
  } else {
    cols_dist[i] <- palabras_distritos[i]
    data1 <- ensure_col(data1, cols_dist[i])
  }
}

# 7) Convertir SI/NO a 0/1
to01 <- function(x){
  x <- trimws(toupper(as.character(x)))
  as.integer(x %in% c("SI","SÍ","1","TRUE","T"))
}

data1[unique(cols_robo)] <- lapply(data1[unique(cols_robo)], to01)
data1[unique(cols_dist)] <- lapply(data1[unique(cols_dist)], to01)

# 8) Matriz de contingencia
Xr <- as.matrix(data1[cols_robo])
Xd <- as.matrix(data1[cols_dist])

matriz_contingencia <- t(Xr) %*% Xd
matriz_contingencia <- as.matrix(matriz_contingencia)

rownames(matriz_contingencia) <- palabras_robo
colnames(matriz_contingencia) <- palabras_distritos

View(matriz_contingencia)

# 9) Reportes útiles
faltan_robo <- palabras_robo[is.na(idx_robo)]
faltan_dist <- palabras_distritos[is.na(idx_dist)]

total_SI_palabras <- sum(Xr, na.rm = TRUE)
total_SI_distritos <- sum(Xd, na.rm = TRUE)
suma_matriz <- sum(matriz_contingencia, na.rm = TRUE)

reporte_validacion <- list(
  filas_total = nrow(data1),
  cantidad_palabras_robo = length(palabras_robo),
  cantidad_distritos = length(palabras_distritos),
  total_SI_en_palabras = total_SI_palabras,
  total_SI_en_distritos = total_SI_distritos,
  suma_total_matriz_coocurrencias = suma_matriz,
  palabras_no_encontradas_en_excel = faltan_robo,
  distritos_no_encontrados_en_excel = faltan_dist
)

print(reporte_validacion)

View(matriz_contingencia)

############################################
# 7. ANÁLISIS DE CORRESPONDENCIAS
############################################

library(FactoMineR)
library(factoextra)
library(ggplot2)

# Convertir a matriz
matriz_contingencia <- as.matrix(matriz_contingencia)

# Análisis de correspondencias
res_ca <- CA(matriz_contingencia, graph = FALSE)

# Gráfico mejorado
grafico_ca <- fviz_ca_biplot(
  res_ca,
  repel = TRUE,
  col.row = "darkgreen",
  col.col = "orange",
  labelsize = 4,
  pointsize = 2,
  title = "Análisis de Correspondencias: Robos vs Distritos"
) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  coord_cartesian(
    xlim = c(-1.5, 7.5),
    ylim = c(-4.5, 4.5)
  )

# Mostrar gráfico
print(grafico_ca)


############################################

############################################
# 8. DENDROGRAMA DE CLUSTERS DE DISTRITOS
############################################

# Instalar paquete 
# install.packages("proxy")

library(proxy)

# Verifica que exista la matriz
stopifnot(exists("matriz_contingencia"))

# Asegurar que la matriz sea numérica
matriz_contingencia <- as.matrix(matriz_contingencia)
mode(matriz_contingencia) <- "numeric"

# Crear matriz para agrupar distritos
# Filas = distritos / Columnas = palabras asociadas a robos
X_dist <- t(matriz_contingencia)

# Convertir a matriz binaria: 1 = aparece el término, 0 = no aparece
X_dist_binaria <- ifelse(X_dist > 0, 1, 0)

# Calcular distancia de Gower
d_dist <- proxy::dist(X_dist_binaria, method = "gower")

# Clustering jerárquico con enlace promedio
hc_dist <- hclust(d_dist, method = "average")

# Graficar dendrograma
plot(
  hc_dist,
  main = "Dendrograma de Clusters de Distritos (Distancia de Gower)",
  xlab = "Distritos",
  ylab = "Distancia de Gower",
  sub = "",
  cex = 0.6
)

# Número de clusters
k <- 6

# Dibujar rectángulos de clusters
rect.hclust(hc_dist, k = k, border = 2:(k + 1))

# Tabla de clusters por distrito
clusters_distritos <- cutree(hc_dist, k = k)

tabla_clusters_distritos <- data.frame(
  Distrito = names(clusters_distritos),
  Cluster = as.numeric(clusters_distritos)
)

print(tabla_clusters_distritos)

############################################
# 9. NUBES DE PALABRAS
############################################


library(readxl)
library(dplyr)
library(writexl)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Colores oscuros
colores_oscuros <- c(
  "darkgreen", "darkorange", "darkred", "darkblue",
  "brown4", "purple4", "gray20", "black"
)

############################################
# 9.1. NUBE DE PALABRAS DEL CONTENIDO TEXTUAL
############################################

library(readxl)
library(dplyr)
library(writexl)
library(tm)
library(wordcloud)
library(RColorBrewer)

datos <- read_excel("Consolidado_Noticias_2020_2021.xlsx")

palabras <- c(
  "Asalto","Asaltos","Asaltante","Asaltantes","Asaltar","Asaltaron","Asalta",
  "Delincuencia","Delincuente","Delincuentes","Delinquir",
  "Robos","Robo","Robaba","Robaban","Robaron","Rateros","Ratero","Roba","Robar",
  "Ladrones","Ladrón","Raquetero","Raqueteros","Latrocinio","Despojo","Rapiña","Saqueo",
  "Atracos","Atraco","Expropiación","Extorsión","Hurto","Sustracción",
  "Estafas","Estafadores","Estafa","Estafan",
  "Hampones","Hampón","Sustrae","Sustraer","Maleante","Maleantes"
)

palabras <- intersect(palabras, names(datos))

datos_filtrados <- datos %>%
  filter(
    if_any(
      all_of(palabras),
      ~ toupper(trimws(as.character(.))) %in% c("SI", "SÍ")
    )
  ) %>%
  select(Diario, title, summary, date, all_of(palabras))

write_xlsx(datos_filtrados, "datos_filtrados_robo.xlsx")

# Unir título y resumen
texto <- paste(datos_filtrados$title, datos_filtrados$summary, sep = " ")

# Limpieza conservando tildes y ñ
texto <- tolower(texto)
texto <- gsub("[^a-záéíóúñü\\s]", " ", texto)
texto <- gsub("\\s+", " ", texto)
texto <- trimws(texto)

# Tokenizar
tokens <- unlist(strsplit(texto, "\\s+"))
tokens <- tokens[tokens != ""]
tokens <- tokens[nchar(tokens) >= 4]

# Stopwords
stop_es <- stopwords("spanish")

stop_extra <- c(
  "tras", "luego", "según", "segun", "años",
  "tres", "cuatro", "caso", "casos", "personas",
  "hombre", "mujer", "joven", "local", "centro",
  "video", "perú", "peru"
)

tokens <- tokens[
  !(tokens %in% stop_es) &
    !(tokens %in% stop_extra)
]

# Frecuencias
frecuencia <- sort(table(tokens), decreasing = TRUE)

# Filtrar para evitar saturación y warnings
frecuencia_pos <- frecuencia[frecuencia >= 8]
frecuencia_pos <- head(frecuencia_pos, 80)

# Colores oscuros
colores_oscuros <- c(
  "darkgreen", "darkorange", "darkred", "darkblue",
  "brown4", "purple4", "gray20", "black"
)

# Mostrar en Plots
par(mar = c(0, 0, 0, 0))

set.seed(123)

suppressWarnings(
  wordcloud(
    words = names(frecuencia_pos),
    freq = as.numeric(frecuencia_pos),
    min.freq = 8,
    max.words = 80,
    random.order = FALSE,
    random.color = FALSE,
    rot.per = 0.03,
    scale = c(2.8, 0.55),
    colors = colores_oscuros,
    use.r.layout = FALSE
  )
)


############################################
# 9.2. NUBE DE PALABRAS ASOCIADAS A ROBO
############################################

library(ggplot2)
library(dplyr)

palabras_robo_nube <- c(
  "Asalto","Asaltos","Asaltante","Asaltantes","Asaltar","Asaltaron","Asalta",
  "Delincuencia","Delincuente","Delincuentes","Delinquir",
  "Robos","Robo","Robaba","Robaban","Robaron","Rateros","Ratero","Roba","Robar",
  "Ladrones","Ladrón","Raquetero","Raqueteros","Latrocinio","Despojo","Rapiña","Saqueo",
  "Atracos","Atraco","Expropiación","Extorsión","Hurto","Sustracción",
  "Estafas","Estafadores","Estafa","Estafan",
  "Hampones","Hampón","Sustrae","Sustraer","Maleante","Maleantes"
)

palabras_robo_nube <- intersect(palabras_robo_nube, names(frecuencia_robo))

frecuencia_robo_pos <- frecuencia_robo[palabras_robo_nube]
frecuencia_robo_pos <- frecuencia_robo_pos[frecuencia_robo_pos > 0]

df_robo <- data.frame(
  Palabra = names(frecuencia_robo_pos),
  Frecuencia = as.numeric(frecuencia_robo_pos),
  stringsAsFactors = FALSE
)

df_robo <- df_robo %>%
  arrange(desc(Frecuencia))

n <- nrow(df_robo)

set.seed(123)

# Distribución tipo nube: anillos concéntricos para evitar filas
angulos <- seq(0, 2 * pi, length.out = n + 1)[-1]
radios <- sqrt(seq(0.05, 1, length.out = n))

df_robo$x <- radios * cos(angulos)
df_robo$y <- radios * sin(angulos)

# Colocar las palabras más frecuentes cerca del centro
pos_centro <- data.frame(
  x = c(0, -0.45, 0.45, 0, -0.70, 0.70, -0.35, 0.35),
  y = c(0, 0.28, 0.28, -0.35, -0.20, -0.20, 0.62, 0.62)
)

m <- min(n, nrow(pos_centro))
df_robo$x[1:m] <- pos_centro$x[1:m]
df_robo$y[1:m] <- pos_centro$y[1:m]

colores_oscuros <- c(
  "darkgreen", "darkorange", "darkred", "darkblue",
  "brown4", "purple4", "gray20", "black"
)

ggplot(df_robo, aes(x = x, y = y)) +
  geom_text(
    aes(
      label = Palabra,
      size = Frecuencia,
      color = Palabra
    ),
    fontface = "bold",
    check_overlap = FALSE
  ) +
  scale_size(range = c(3.2, 11)) +
  scale_color_manual(
    values = rep(colores_oscuros, length.out = n)
  ) +
  coord_fixed(
    ratio = 0.75,
    xlim = c(-1.25, 1.25),
    ylim = c(-1.15, 1.15),
    clip = "off"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(title = "")

############################################
# 9.3. NUBE DE PALABRAS DE DISTRITOS
############################################

library(ggplot2)
library(dplyr)

frecuencia_distritos_pos <- frecuencia_distritos[frecuencia_distritos > 0]

df_distritos <- data.frame(
  Distrito = names(frecuencia_distritos_pos),
  Frecuencia = as.numeric(frecuencia_distritos_pos),
  stringsAsFactors = FALSE
)

# Abreviar nombres largos SOLO para visualización
df_distritos$Distrito <- gsub("San Juan de Lurigancho", "SJL", df_distritos$Distrito)
df_distritos$Distrito <- gsub("San Juan de Miraflores", "SJM", df_distritos$Distrito)
df_distritos$Distrito <- gsub("San Martín de Porres", "SMP", df_distritos$Distrito)
df_distritos$Distrito <- gsub("Villa Maria del Triunfo", "VMT", df_distritos$Distrito)
df_distritos$Distrito <- gsub("Villa el Salvador", "VES", df_distritos$Distrito)
df_distritos$Distrito <- gsub("Carmen de la Legua Reynoso", "Carmen Legua", df_distritos$Distrito)
df_distritos$Distrito <- gsub("Santa María del Mar", "Sta. María", df_distritos$Distrito)
df_distritos$Distrito <- gsub("Magdalena del Mar", "Magdalena", df_distritos$Distrito)
df_distritos$Distrito <- gsub("Santiago de Surco", "Surco", df_distritos$Distrito)
df_distritos$Distrito <- gsub("Puente Piedra", "Pte. Piedra", df_distritos$Distrito)
df_distritos$Distrito <- gsub("Punta Hermosa", "P. Hermosa", df_distritos$Distrito)
df_distritos$Distrito <- gsub("Punta Negra", "P. Negra", df_distritos$Distrito)

# Ordenar de mayor a menor frecuencia
df_distritos <- df_distritos %>%
  arrange(desc(Frecuencia))

n <- nrow(df_distritos)

# Posiciones compactas con distribución menos lineal
set.seed(123)

columnas <- 6
filas <- ceiling(n / columnas)

df_distritos$x <- rep(1:columnas, length.out = n)
df_distritos$y <- -rep(1:filas, each = columnas)[1:n]

# Desplazamientos para dar apariencia más orgánica
df_distritos$x <- df_distritos$x +
  rep(c(0, 0.25, -0.20, 0.15, -0.15, 0.20), length.out = n)

df_distritos$y <- df_distritos$y +
  rep(c(0, 0.12, -0.08, 0.10, -0.10, 0.05), length.out = n)

# Colores oscuros
colores_oscuros <- c(
  "darkgreen", "darkorange", "darkred", "darkblue",
  "brown4", "purple4", "gray20", "black"
)

ggplot(df_distritos, aes(x = x, y = y)) +
  geom_text(
    aes(
      label = Distrito,
      size = Frecuencia,
      color = Distrito
    ),
    fontface = "bold",
    check_overlap = FALSE
  ) +
  scale_size(range = c(3, 9)) +
  scale_color_manual(
    values = rep(colores_oscuros, length.out = n)
  ) +
  coord_fixed(ratio = 0.45, clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(
    title = ""
  )

############################################
# 10. MAPA
# Solo 43 distritos de Lima Metropolitana
############################################

# install.packages(c("geodata", "terra", "sf", "dplyr", "ggplot2", "viridis", "scales"))

library(geodata)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

# Descargar límites administrativos
peru <- gadm(
  country = "PER",
  level = 3,
  path = tempdir()
)

# Convertir a sf
peru_sf <- st_as_sf(peru)

# Filtrar solo Lima Metropolitana
lima_met <- peru_sf[
  peru_sf$NAME_1 == "Lima Province" &
    peru_sf$NAME_2 == "Lima",
]

# Datos de frecuencia para los 43 distritos de Lima Metropolitana
frecuencia_mapa <- data.frame(
  distrito = lima_met$NAME_3,
  frecuencia_robo = c(0,0,1,0,1,0,2,0,4,2,1,0,0,2,2,0,3,0,0,0,0,2,0,0,2,1,0,0,0,0,1,1,0,2,3,0,0,0,0,0,0,2,0)
)

# Unir frecuencias con el mapa
lima_robo <- lima_met %>%
  left_join(
    frecuencia_mapa,
    by = c("NAME_3" = "distrito")
  )

# Reemplazar valores faltantes por 0
lima_robo$frecuencia_robo[is.na(lima_robo$frecuencia_robo)] <- 0

# Mapa de frecuencias de noticias asociadas a robo
ggplot(lima_robo) +
  geom_sf(
    aes(fill = frecuencia_robo),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Frecuencia de noticias",
    labels = comma
  ) +
  labs(
    title = "Distribución distrital de noticias asociadas a robo",
    subtitle = "Lima Metropolitana",
    caption = "Fuente: Elaboración propia con base en noticias web y límites GADM"
  ) +
  theme_minimal()