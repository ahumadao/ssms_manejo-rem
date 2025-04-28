#1. Importo librerías y archivo de limpieza ----
pacman::p_load(rio, here, tidyverse, janitor)
source(here('1-limpieza.R'))

#2. Importo el/los establecimientos que necesito. ----
data_rem <- import_rem(codes = 13100) |> 
  group_by(codigo_establecimiento, seccion, codigo, glosa) |>
  summarise(total = sum(total, na.rm = TRUE))

#3. Filtro 

### BUSCAR IMAGENOLOGIA
# buscar <- data_rem |>
#   filter(str_detect(seccion, regex("examenes radiolo|ultrasono|tomografia|resonancia", ignore_case = TRUE)) | 
#            str_detect(glosa, regex("angiograf", ignore_case = TRUE)) | 
#            str_detect(glosa, regex("cintigra|pet-ct|petct|-ct", ignore_case = TRUE)),
#            !is.na(codigo)) |>
#   group_by(seccion,glosa) |>
#   summarise(total = sum(total))

### BUSCAR ENDOSCOPIAS GASTRO
buscar <- data_rem |>
  filter(str_detect(seccion, regex("mientos gastro", ignore_case = TRUE)),
           !is.na(codigo)) |>
  group_by(seccion,glosa) |>
  summarise(total = sum(total))


#4. Análisis

#5. Exportar
export(buscar, here('output','gastro_hblt_2024.xlsx'))

