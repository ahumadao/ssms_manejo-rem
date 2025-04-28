# 1. Importar librerías ----
pacman::p_load(rio, here, tidyverse, janitor)

# 2. Función para corregir rutas de Windows ----
corrector <- function(path) {
  str_replace_all(path, "\\\\", "/")
}

# 3. Función procesador de cada archivo REM ----
procesador <- function(ruta_archivo, sheet = "B", format = "xlsx") {
  import(ruta_archivo, sheet = sheet, format = format) %>%
    clean_names() %>%
    select(1:3) %>%
    slice(-2) %>%
    set_names(c("codigo", "glosa", "total")) %>%
    filter(!str_detect(glosa, regex("total", ignore_case = TRUE))) %>%
    mutate(
      seccion = case_when(
        is.na(codigo) & is.na(total) ~ str_to_upper(glosa),
        TRUE                         ~ NA_character_
      ),
      seccion = if_else(
        !is.na(seccion) & lead(!is.na(seccion)),
        str_c(seccion, "-"),
        seccion
      )
    ) %>%
    fill(seccion, .direction = "down") %>%
    group_by(codigo, glosa, seccion) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      nombre_archivo         = basename(ruta_archivo),
      codigo_establecimiento = str_extract(nombre_archivo, "^[0-9]+")
    )
}

# 4. Función para importar todos o sólo algunos REMs ----
import_rem <- function(
    codes    = NULL,  # vector de códigos DEIS (ej. c("13100","15000")) o NULL para todos
    base_dir = "Z:/HOSPITAL/REM_2024_HOSP/Acumulados/Serie_Bs",
    sheet    = "B",
    format   = "xlsx"
) {
  # Listar todos los .xlsm en subcarpetas Bs_01_a_02 y Bs_03_a_12
  archivos <- list.files(
    path       = corrector(base_dir),
    pattern    = "\\.xlsm$",
    recursive  = TRUE,
    full.names = TRUE
  ) %>% corrector()
  
  # Filtrar por códigos si se solicitan
  if (!is.null(codes)) {
    codes  <- as.character(codes)
    patron <- str_c("^(", str_c(codes, collapse = "|"), ")")
    archivos <- archivos[str_detect(basename(archivos), patron)]
    if (length(archivos) == 0) {
      stop("No se encontraron archivos para los códigos: ", str_c(codes, collapse = ", "))
    }
  }
  
  # Aplicar procesador y unir todo
  map_dfr(archivos, ~ procesador(.x, sheet = sheet, format = format))
}

# 
# # 4.1 Ejemplos de uso ----
# # Cargar TODOS los REM de enero-febrero y marzo-diciembre:
# data_rem_all <- import_rem()
# 
# # Cargar sólo los establecimientos 13100 y 15000:
# data_rem_sel <- import_rem(codes = c(13100, 15000))







