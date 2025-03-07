rm(list = ls())

source("rutinas/99_librerias/librerias.R")

#-------------------------------------------------------------------------------
# Lectuta marco
#-------------------------------------------------------------------------------

ruta <- "productos/01_marco/marco_colegios_ipc.rds"
marco_colegios_ipc <- read_rds(ruta)

#-------------------------------------------------------------------------------
# PARAMETROS
#-------------------------------------------------------------------------------

nc = 0.90
z = qnorm(nc+(1-nc)/2)
er = 0.1

# CALCULO -----------------------------------------------------------------

tamanio_matr <- marco_colegios_ipc %>% 
  mutate(dominio = 1,
         var_disenio = costo_medio_matr) %>% 
  group_by(dominio) %>% 
  summarise(N = n(),
            desv = sd(var_disenio,na.rm = T),
            sum_var_disenio = sum(var_disenio,na.rm = T),
            y = mean(var_disenio)) %>% 
  mutate(numerador = (desv)^2,
         denominador = ((er*y/z)^2) + (desv^2/N),
         tam = ceiling(numerador/denominador),
         dif = N - tam) %>% 
  adorn_totals(c("row"))




