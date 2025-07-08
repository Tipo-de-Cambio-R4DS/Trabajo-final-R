library(rvest)
library(readxl)    
library(tidyverse)


# Carpeta donde se guardarán los archivos descargados
carpeta_destino <- "datos_tc_bcv"

if (!dir.exists(carpeta_destino)) {
  dir.create(carpeta_destino)
}

# URL base para construir enlaces completos
url_base <- "https://www.bcv.org.ve"

# Vector para almacenar todos los enlaces extraídos
lista_enlaces <- c()

# Iterar por las primeras 5 páginas (0 a 4)
for (i in 0:4) {
  url_pagina <- paste0(url_base, "/estadisticas/tipo-cambio-de-referencia-smc?page=", i)
  
  # Leer el contenido HTML de la página
  html_pagina <- read_html(url_pagina)
  
  # Extraer los enlaces relativos de los archivos Excel o páginas de interés
  enlaces_relativos <- html_pagina %>%
    html_elements('.views-field-field-diario a') %>%
    html_attr('href')
  
  # Acumular enlaces
  lista_enlaces <- c(lista_enlaces, enlaces_relativos)
}

# Eliminar enlaces duplicados
lista_enlaces <- unique(lista_enlaces)

# Para evitar descargar demasiados archivos, tomar solo los primeros 22 (o ajusta según necesites)
lista_enlaces <- lista_enlaces[1:min(22, length(lista_enlaces))]

# Descargar cada archivo Excel a la carpeta destino
for (enlace in lista_enlaces) {
  
  # Construir enlace completo si es relativo
  if (!startsWith(enlace, "http")) {
    enlace_completo <- paste0(url_base, enlace)
  } else {
    enlace_completo <- enlace
  }
  
  # Nombre del archivo para guardar (extraído del enlace)
  nombre_archivo <- basename(enlace_completo)
  
  # Ruta completa donde se guardará el archivo
  ruta_guardado <- file.path(carpeta_destino, nombre_archivo)
  
  # Verificar si el archivo ya existe para no descargarlo de nuevo
  if (file.exists(ruta_guardado)) {
    message("El archivo ", nombre_archivo, " ya existe. Se omite descarga.")
    next
  }
  message("Descargando archivo: ", nombre_archivo)
  download.file(enlace_completo, destfile = ruta_guardado, mode = "wb")
}

# 2. Lectura y procesamiento de datos
lista_dfs <- list()
archivos_excel <- list.files(path = carpeta_destino, pattern = "\\.(xls|xlsx)$", full.names = TRUE)

for (archivo_path in archivos_excel) {
  tryCatch({ 
    hojas <- excel_sheets(archivo_path)
    if (length(hojas) == 0) next
    
    for (nombre_hoja in hojas) {
      tryCatch({
        fecha_raw <- read_excel(archivo_path, sheet = nombre_hoja, range = "B5", col_names = FALSE)
        # str_extract funciona porque tidyverse carga stringr
        fecha_valor <- as.Date(str_extract(as.character(fecha_raw[[1,1]]), "\\d{2}/\\d{2}/\\d{4}"), format = "%d/%m/%Y")
        
        df_temp <- read_excel(archivo_path, sheet = nombre_hoja, skip = 8) %>%
          setNames(make.names(names(.), unique = TRUE))
        
        if (nrow(df_temp) == 0 || ncol(df_temp) < 2) next
        
        df_listo_hoja <- df_temp %>%
          rename(Moneda = names(.)[1], Compra_Bs = names(.)[5], Venta_Bs = names(.)[6]) %>%
          select(Moneda, Compra_Bs, Venta_Bs) %>%
          mutate(Fecha = fecha_valor)
        
        lista_dfs[[length(lista_dfs) + 1]] <- df_listo_hoja
      }, error = function(e) message("  Error hoja '", nombre_hoja, "' en '", basename(archivo_path), "'"))
    }
  }, error = function(e) message("ERROR ARCHIVO '", basename(archivo_path), "'"))
}

# 3. Combinar y limpiar
if (length(lista_dfs) > 0) {
  monedas_deseadas <- c('USD', 'EUR', 'BRL', 'CNY', 'COP')
  fecha_reconversion <- as.Date("2021-10-01")
  df_listo <- bind_rows(lista_dfs) %>%
    filter(Moneda %in% monedas_deseadas) %>%
    select(Fecha, Moneda, Compra_Bs, Venta_Bs) %>%
    mutate(
      Compra_Bs = ifelse(Fecha < fecha_reconversion, Compra_Bs / 1000000, Compra_Bs),
      Venta_Bs = ifelse(Fecha < fecha_reconversion, Venta_Bs / 1000000, Venta_Bs)
    ) %>%
    filter(!is.na(Compra_Bs) & !is.na(Venta_Bs)) %>%
    arrange(Fecha, Moneda)
  
  glimpse(df_listo)
} else {
  df_listo <- data.frame()
}
View(df_listo)

# tuvimos un problema con unos valores en la fecha 17/12/24, por lo que lo
# corregimos de la siguiente manera
# Vector con las monedas que queremos corregir
monedas_con_error <- c("BRL", "COP")

# Fechas adyacentes
fecha_error <- as.Date("2024-12-17")
fechas_vecinas <- as.Date(c("2024-12-16", "2024-12-18"))

# Iterar sobre las monedas problemáticas
for (moneda_actual in monedas_con_error) {
  
  # Calcular promedio solo con las fechas vecinas
  promedio_vecino <- df_listo %>%
    filter(Moneda == moneda_actual, Fecha %in% fechas_vecinas) %>%
    summarise(
      Compra_promedio = mean(Compra_Bs, na.rm = TRUE),
      Venta_promedio  = mean(Venta_Bs, na.rm = TRUE)
    )
  
  # Reemplazar los valores en la fecha del error
  df_listo <- df_listo %>%
    mutate(
      Compra_Bs = ifelse(Fecha == fecha_error & Moneda == moneda_actual,
                         promedio_vecino$Compra_promedio, Compra_Bs),
      Venta_Bs = ifelse(Fecha == fecha_error & Moneda == moneda_actual,
                        promedio_vecino$Venta_promedio, Venta_Bs)
    )
}

df_cop <- df_listo %>%
  filter(Moneda == "COP")

df_usd <- df_listo %>%
  filter(Moneda == "USD")

df_brl <- df_listo %>%
  filter(Moneda == "BRL")

df_eur <- df_listo %>%
  filter(Moneda == "EUR")

df_cny <- df_listo %>%
  filter(Moneda == "CNY")

save(df_cop, df_cny, df_brl, df_eur, df_usd, file = 'datos_monedas.RData')
