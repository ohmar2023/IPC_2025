
rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# -----------------------------------------------------------------------------
# Lectura base de datos
# -----------------------------------------------------------------------------

bdd_precios_col <- read_excel("insumos/01_marco/bdd_final_precios_IE_F.XLSX") %>% 
  clean_names()

#bdd_precios_col[is.na(bdd_precios_col)] = 0

#-------------------------------------------------------------------------------
# Variable de diseño
#-------------------------------------------------------------------------------


aux_1 <- bdd_precios_col %>% select(contains("matricula")) %>% names()
aux_2 <- bdd_precios_col %>% select(contains("pension")) %>% names()

bdd_precios_col <- bdd_precios_col %>% 
  rowwise() %>% 
  mutate(costo_matr = mean(c(.data[[aux_1[[1]]]], 
                             .data[[aux_1[[2]]]],
                             .data[[aux_1[[3]]]],
                             .data[[aux_1[[4]]]]), na.rm=TRUE),
         
         costo_pen = mean(c(.data[[aux_2[[1]]]],
                            .data[[aux_2[[2]]]],
                            .data[[aux_2[[3]]]], 
                            .data[[aux_2[[4]]]]), na.rm=TRUE)) %>%
  ungroup() %>% 
  filter(costo_matr != 0 | costo_matr != "NaN") %>% 
  mutate(var_di_matr = costo_matr * total_estudiantes,
         var_di_pen = costo_pen * total_estudiantes)


bdd_precios_col %>% dim()
bdd_precios_col <-  bdd_precios_col %>% filter(sostenimiento != "Fiscomisional")

#-------------------------------------------------------------------------------
# PARAMETROS
#-------------------------------------------------------------------------------

nc = 0.90
z = qnorm(nc+(1-nc)/2)
er = 0.2

# CALCULO -----------------------------------------------------------------

tamanio_matr <- bdd_precios_col %>% 
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


tamanio_pen <- bdd_precios_col %>% 
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









