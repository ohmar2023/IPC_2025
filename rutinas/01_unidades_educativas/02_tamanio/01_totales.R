
rm(list = ls())

source("rutinas/99_librerias/librerias.R")

#-------------------------------------------------------------------------------
# Lectuta marco
#-------------------------------------------------------------------------------

ruta <- "productos/01_marco/marco_colegios_ipc.rds"
marco_colegios_ipc <- read_rds(ruta)

#-------------------------------------------------------------------------------
# Variable de diseño
#-------------------------------------------------------------------------------

marco_colegios_ipc <- marco_colegios_ipc %>% 
  mutate(var_di_matr = costo_medio_matr * total_estudiantes,
         var_di_pen = costo_medio_pen * total_estudiantes)

#-------------------------------------------------------------------------------
# PARAMETROS
#-------------------------------------------------------------------------------

nc = 0.90
z = qnorm(nc+(1-nc)/2)
er = 0.2

# CALCULO -----------------------------------------------------------------

tamanio_matr <- marco_colegios_ipc %>% 
  mutate(dominio = 1,
         var_disenio = var_di_matr) %>% 
  group_by(dominio) %>% 
  summarise(N=n(),
            desv = sd(var_disenio,na.rm = T),
            sum_var_disenio = sum(var_disenio,na.rm = T)) %>% 
  mutate(numerador = (N*desv)^2,
         denominador = ((N-1)/N)*((er*sum_var_disenio/z)^2)+N*(desv^2),
         tam = ceiling(numerador/denominador),
         dif = N - tam) %>% 
  adorn_totals(c("row"))


tamanio_pen <- marco_colegios_ipc %>% 
  mutate(dominio = canton,
         var_disenio = var_di_pen) %>% 
  group_by(dominio) %>% 
  summarise(N=n(),
            desv = sd(var_disenio,na.rm = T),
            sum_var_disenio = sum(var_disenio,na.rm = T)) %>% 
  mutate(numerador = (N*desv)^2,
         denominador = ((N-1)/N)*((er*sum_var_disenio/z)^2)+N*(desv^2),
         tam = ceiling(numerador/denominador),
         dif = N - tam) %>% 
  adorn_totals(c("row"))

tamanio_pen %>% select(dominio,N, tam) %>% View("pension")
tamanio_matr %>% select(dominio,N, tam) %>% View("matricula")






