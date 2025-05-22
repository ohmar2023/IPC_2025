
rm(list = ls())

source("rutinas/99_librerias_funciones/librerias.R")

# -----------------------------------------------------------------------------
# Lectura base de datos
# -----------------------------------------------------------------------------

# bdd_precios_col <- read_excel("insumos/01_unidades_educativas/01_marco_ue/bdd_final_precios_IE_F.XLSX") %>% 
#   clean_names()

bdd_precios_col <- read_excel("insumos/01_unidades_educativas/01_marco_ue/marco_muestral.xlsx") %>% 
  clean_names()

# -----------------------------------------------------------------------------
# Para quedarnos con las instituciones que dispongan información en al menos uno
# de los niveles que ofertan
# -----------------------------------------------------------------------------

aux_1 <- bdd_precios_col %>% select(contains("matricula")) %>% names()
aux_2 <- bdd_precios_col %>% select(contains("pension")) %>% names()

bdd_precios_col <- bdd_precios_col %>% 
  rowwise() %>% 
  mutate(costo_medio_matr = mean(c(.data[[aux_1[[1]]]], 
                             .data[[aux_1[[2]]]],
                             .data[[aux_1[[3]]]],
                             .data[[aux_1[[4]]]]), na.rm=TRUE),
         
         costo_medio_pen = mean(c(.data[[aux_2[[1]]]],
                            .data[[aux_2[[2]]]],
                            .data[[aux_2[[3]]]], 
                            .data[[aux_2[[4]]]]), na.rm=TRUE)) %>%
  ungroup() %>% 
  filter(costo_medio_matr != 0 | costo_medio_pen != "NaN") %>% 
  rowwise() %>% 
  mutate(n_nivel_matr = sum(c(!is.na(.data[[ aux_1[1] ]]) , 
                              !is.na(.data[[ aux_1[2] ]]) ,
                              !is.na(.data[[ aux_1[3] ]]) ,
                              !is.na(.data[[ aux_1[4] ]]) )) ,
         n_nivel_pen  = sum(c(!is.na(.data[[ aux_2[1] ]]) , 
                              !is.na(.data[[ aux_2[2] ]]) ,
                              !is.na(.data[[ aux_2[3] ]]) ,
                              !is.na(.data[[ aux_2[4] ]]) )),
         control = ifelse(n_nivel_matr == n_nivel_pen, TRUE, FALSE)) %>% 
  ungroup() %>% 
  filter(n_nivel_matr >= 3 & control == TRUE) %>% 
  ungroup() %>% 
  filter(sostenimiento == "Particular")

# -----------------------------------------------------------------------------
# Exportando base de datos
# -----------------------------------------------------------------------------

ruta <- "productos/01_unidades_educativas/01_marco/"
export(bdd_precios_col, paste0(ruta, "marco_colegios_ipc.rds"))




