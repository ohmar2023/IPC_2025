
rm(list = ls())

source("rutinas/99_librerias/librerias.R")
source("rutinas/01_unidades_educativas/02_tamanio/99_funciones_colegios.R")

#-------------------------------------------------------------------------------
# Lectuta marco
#-------------------------------------------------------------------------------

ruta <- "productos/01_unidades_educativas/01_marco/marco_colegios_ipc.rds"
#marco_colegios_ipc <- read_rds(ruta)

marco_colegios_ipc_3_niveles <- read_rds(ruta)
  
#-------------------------------------------------------------------------------
# Vamos a buscar las instituciones que dispongan de almenos 3 niveles
#-------------------------------------------------------------------------------

# aux_1 <- marco_colegios_ipc %>% select(contains("matricula")) %>% names()
# aux_2 <- marco_colegios_ipc %>% select(contains("pension")) %>% names()
# 
# marco_colegios_ipc_3_niveles <- marco_colegios_ipc %>% 
#   rowwise() %>% 
#   mutate(n_nivel_matr = sum(c(!is.na(.data[[ aux_1[1] ]]) , 
#                               !is.na(.data[[ aux_1[2] ]]) ,
#                               !is.na(.data[[ aux_1[3] ]]) ,
#                               !is.na(.data[[ aux_1[4] ]]) )) ,
#          n_nivel_pen  = sum(c(!is.na(.data[[ aux_2[1] ]]) , 
#                               !is.na(.data[[ aux_2[2] ]]) ,
#                               !is.na(.data[[ aux_2[3] ]]) ,
#                               !is.na(.data[[ aux_2[4] ]]) )),
#          control = ifelse(n_nivel_matr == n_nivel_pen, TRUE, FALSE)
#   ) %>% 
#   ungroup() %>% 
#   filter(n_nivel_matr >= 3 & control == TRUE)


#-------------------------------------------------------------------------------
# 
#-------------------------------------------------------------------------------

# NACIONAL - MATRICULA ---------------------------------------------------------

tamanio_matr_1 <- tamanio_media(base = marco_colegios_ipc_3_niveles, 
                                nc = 0.90,
                                er = 0.1,
                                dominio = "Nacional",
                                variable = "costo_medio_matr")


tamanio_matr_dom <- tamanio_media(base = marco_colegios_ipc_3_niveles, 
                                nc = 0.90,
                                er = 0.1,
                                dominio = "canton",
                                variable = "costo_medio_matr")


niveles_3_0.90_0.01 <- tamanio_matr_dom %>% 
  mutate(p = N/sum(N), 
         tam_nacional = ceiling(p * tamanio_matr_1$tam),
         dif_2 = N - tam_nacional) %>% 
  adorn_totals(c("row")) 

#-------------------------------------------------------------------------------
# Exportar
#-------------------------------------------------------------------------------

export(niveles_3_0.90_0.01, "niveles_3_0.90_0.01.xlsx")













# tamanio_matr_1 <- marco_colegios_ipc_3_niveles %>% 
#   #filter(sostenimiento != "Fiscomisional") %>% 
#   mutate(dominio = canton,
#          var_disenio = costo_medio_matr) %>% 
#   group_by(dominio) %>% 
#   summarise(N = n(),
#             desv = sd(var_disenio,na.rm = T),
#             sum_var_disenio = sum(var_disenio,na.rm = T),
#             y = mean(var_disenio)) %>% 
#   mutate(numerador = (desv)^2,
#          denominador = ((er*y/z)^2) + (desv^2/N),
#          tam = ceiling(numerador/denominador),
#          dif = N - tam) %>% 
#   select(dominio,N,tam,dif) %>% 
#   adorn_totals(c("row"))








