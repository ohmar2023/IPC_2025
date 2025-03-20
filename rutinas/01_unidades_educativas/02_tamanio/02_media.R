rm(list = ls())

source("rutinas/99_librerias/librerias.R")

#-------------------------------------------------------------------------------
# Lectuta marco
#-------------------------------------------------------------------------------

ruta <- "productos/01_unidades_educativas/01_marco/marco_colegios_ipc.rds"
marco_colegios_ipc <- read_rds(ruta)

#-------------------------------------------------------------------------------
# PARAMETROS
#-------------------------------------------------------------------------------

nc = 0.90
z = qnorm(nc+(1-nc)/2)
er = 0.1

# NACIONAL - MATRICULA ---------------------------------------------------------

tamanio_matr_1 <- marco_colegios_ipc %>% 
  #filter(sostenimiento != "Fiscomisional") %>% 
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
  select(dominio,N,tam,dif) %>% 
  adorn_totals(c("row"))

# DOMINIOS - MATRICULA ---------------------------------------------------------

tamanio_matr_dom <- marco_colegios_ipc %>% 
  #filter(sostenimiento != "Fiscomisional") %>% 
  mutate(dominio = canton,
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
  select(dominio,N,tam,dif) %>% 
  adorn_totals(c("row"))

# NACIONAL - PENSION  ---------------------------------------------------------

tamanio_pen_1 <- marco_colegios_ipc %>% 
  #filter(sostenimiento != "Fiscomisional") %>% 
  mutate(dominio = 1,
         var_disenio = costo_medio_pen) %>% 
  group_by(dominio) %>% 
  summarise(N = n(),
            desv = sd(var_disenio,na.rm = T),
            sum_var_disenio = sum(var_disenio,na.rm = T),
            y = mean(var_disenio)) %>% 
  mutate(numerador = (desv)^2,
         denominador = ((er*y/z)^2) + (desv^2/N),
         tam = ceiling(numerador/denominador),
         dif = N - tam) %>% 
  select(dominio,N,tam,dif) %>% 
  adorn_totals(c("row"))

# Dominio - PENSION  ---------------------------------------------------------

tamanio_pen_dom <- marco_colegios_ipc %>% 
  #filter(sostenimiento != "Fiscomisional") %>% 
  mutate(dominio = canton,
         var_disenio = costo_medio_pen) %>% 
  group_by(dominio) %>% 
  summarise(N = n(),
            desv = sd(var_disenio,na.rm = T),
            sum_var_disenio = sum(var_disenio,na.rm = T),
            y = mean(var_disenio)) %>% 
  mutate(numerador = (desv)^2,
         denominador = ((er*y/z)^2) + (desv^2/N),
         tam = ceiling(numerador/denominador),
         dif = N - tam) %>% 
  select(dominio,N,tam,dif) %>% 
  adorn_totals(c("row"))

#-------------------------------------------------------------------------------
# Exportar
#-------------------------------------------------------------------------------


dir <- paste0("productos/01_unidades_educativas/02_tamanio_media")
dir.create(dir, showWarnings = F) 
dir.exists(dir)

wb <- createWorkbook("Tamanio media")
addWorksheet(wb, "tamanio_matr_1")
addWorksheet(wb, "tamanio_matr_dom")
addWorksheet(wb, "tamanio_pen_1")
addWorksheet(wb, "tamanio_pen_dom")

writeData(wb, sheet = "tamanio_matr_1", tamanio_matr_1)
writeData(wb, sheet = "tamanio_matr_dom", tamanio_matr_dom)
writeData(wb, sheet = "tamanio_pen_1", tamanio_pen_1)
writeData(wb, sheet = "tamanio_pen_dom", tamanio_pen_dom)

saveWorkbook(wb, paste0(dir,"/tamanio_matricula_90_01.xlsx"), overwrite = T)





