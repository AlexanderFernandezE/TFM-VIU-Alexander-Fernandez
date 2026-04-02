
# Cargar las librerías necesarias
library(rvest)
library(dplyr)
library(writexl)

# Definir la URL de la página de noticias
url_noticias <- "https://diariocorreo.pe/archivo/todas/2024-01-26/"

# Leer el contenido HTML de la página
html <- read_html(url_noticias)

# Seleccionar los nodos que contienen las noticias
noticias <- html %>% html_nodes(".story-item")

# Verificar el número de noticias encontradas
cat("Número de noticias encontradas:", length(noticias), "\n")

# Inicializar una lista vacía para almacenar los resultados
resultados <- list()

# Recorrer cada noticia y extraer la información
for (i in seq_along(noticias)) {
  title <- noticias[i] %>% html_node(".story-item__title") %>% html_text(trim = TRUE)
  summary <- noticias[i] %>% html_node(".story-item__subtitle") %>% html_text(trim = TRUE)
  date <- noticias[i] %>% html_node(".story-item__date") %>% html_text(trim = TRUE)
  
  # Mostrar los datos extraídos para la noticia actual
  cat("Noticia", i, ":\n")
  cat("Título:", title, "\n")
  cat("Resumen:", summary, "\n")
  cat("Fecha:", date, "\n\n")
  
  resultados[[i]] <- data.frame(
    title = title,
    summary = summary,
    date = date,
    stringsAsFactors = FALSE
  )
}

# Convertir la lista de data frames a un único data frame
resultados_df <- bind_rows(resultados)

# Verificar el data frame antes de guardarlo
print(resultados_df)

# Guardar el data frame en un archivo Excel
write_xlsx(resultados_df, "resultados_noticias.xlsx")
