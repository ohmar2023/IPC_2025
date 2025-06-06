
rm(list = ls())

source("rutinas/99_librerias_funciones/librerias.R")
source("rutinas/01_unidades_educativas/02_tamanio/99_funciones_colegios.R")

#-------------------------------------------------------------------------------
# Lectuta marco
#-------------------------------------------------------------------------------

inc_for <- read_excel("insumos/01_unidades_educativas/01_marco_ue/inclusion_forzosa.xlsx") %>% clean_names()

ruta <- "productos/01_unidades_educativas/01_marco/marco_colegios_ipc.rds"
marco_colegios_ipc_sin_gye_uio <- read_rds(ruta) %>% 
  filter(!(amie %in% inc_for$amie))
  
#-------------------------------------------------------------------------------
# 
#-------------------------------------------------------------------------------

tamanio_matr_1 <- tamanio_media(base = marco_colegios_ipc_sin_gye_uio, 
                                nc = 0.90,
                                er = 0.1,
                                dominio = "Nacional",
                                variable = "costo_medio_matr")

tamanio_matr_dom <- tamanio_media(base = marco_colegios_ipc_sin_gye_uio, 
                                nc = 0.90,
                                er = 0.1,
                                dominio = "canton",
                                variable = "costo_medio_matr")
  
sin_gye_uio_0.90_0.01 <- tamanio_matr_dom %>% 
  mutate(p = N/sum(N), 
         tam_nacional = ceiling(p * tamanio_matr_1$tam),
         tam_nacional = ifelse(tam_nacional < 5, tam_nacional + 1 , tam_nacional),
         dif_2 = N - tam_nacional) %>% 
  adorn_totals(c("row")) 

#-------------------------------------------------------------------------------
# Exportar
#-------------------------------------------------------------------------------

export(sin_gye_uio_0.90_0.01, "productos/01_unidades_educativas/02_tamanio_media/tamanio_UE_final.xlsx")


# dir <- paste0("productos/01_unidades_educativas/02_tamanio_media")
# dir.create(dir, showWarnings = F) 
# dir.exists(dir)
# 
# wb <- createWorkbook("Tamanio media")
# addWorksheet(wb, "tamanio_matr_1")
# addWorksheet(wb, "tamanio_matr_dom")
# addWorksheet(wb, "tamanio_pen_1")
# addWorksheet(wb, "tamanio_pen_dom")
# 
# writeData(wb, sheet = "tamanio_matr_1", tamanio_matr_1)
# writeData(wb, sheet = "tamanio_matr_dom", tamanio_matr_dom)
# writeData(wb, sheet = "tamanio_pen_1", tamanio_pen_1)
# writeData(wb, sheet = "tamanio_pen_dom", tamanio_pen_dom)
# 
# saveWorkbook(wb, paste0(dir,"/tamanio_media_90_01_sin_gye_uio.xlsx"), overwrite = T)

