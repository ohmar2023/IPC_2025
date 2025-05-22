
rm(list = ls())

source("rutinas/99_librerias_funciones/librerias.R")
source("rutinas/99_librerias_funciones/99_mult_6.R")

#-------------------------------------------------------------------------------
# Lectura estimaciones
#-------------------------------------------------------------------------------

est <- readRDS("productos/02_viv_alquileres/02_estimaciones/estimaciones.rds")

# -----------------------------------------------------------------------------
# Lectura marco que viene del censo
# -----------------------------------------------------------------------------

ruta <- "productos/02_viv_alquileres/01_marco/marco_ipc_alquileres_2025.rds"
marco_ipc_alquileres_2025 <- import(ruta)

#-------------------------------------------------------------------------------
# Tamaño para los dominios sin considerar Galápagos
#-------------------------------------------------------------------------------

nc = 0.95
z = qnorm(nc + (1-nc)/2)
er = 0.065

tamanio_1 <- est %>% 
  filter(dominio != "20" & dominio != "080150_1" & dominio != "080150_2") %>% 
  group_by(dominio, nombre_dom) %>% 
  summarise(N = N_alq_censo,
            n = unique(n),
            desv = sd_alq,
            y = med_alq) %>% 
  mutate(numerador = (desv)^2,
         denominador = ((er*y/z)^2) + (desv^2/N),
         tam = ceiling(numerador/denominador), 
         tam = mult_6(tam, N)) %>% 
  select(dominio, nombre_dom, tam)

#-------------------------------------------------------------------------------
# Tamaño solo para Galápagos
#-------------------------------------------------------------------------------

nc = 0.90
z = qnorm(nc + (1-nc)/2)
er = 0.10

tamanio_2 <- est %>% 
  filter(dominio == "20") %>% 
  group_by(dominio, nombre_dom) %>% 
  summarise(N = N_alq_censo,
            n = unique(n),
            desv = sd_alq,
            y = med_alq) %>% 
  mutate(numerador = (desv)^2,
         denominador = ((er*y/z)^2) + (desv^2/N),
         tam = ceiling(numerador/denominador), 
         tam = mult_6(tam, N)) %>% 
  select(dominio, nombre_dom, tam)


# Distribución del tamaño de Galpápagos para las tres islas PPT.

tamanio_galapagos <- marco_ipc_alquileres_2025 %>% 
  filter(dominio == "20") %>% 
  group_by(dominio, I02, V01) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(ppt = n/sum(n), 
         tam_ppt = ceiling(tamanio_2$tam * ppt),
         tam_ppt = mult_6(tam_ppt, n)) 

tamanio_2 <- tamanio_2 %>% mutate(tam = sum(tamanio_galapagos$tam_ppt))

#-------------------------------------------------------------------------------
# Tamaño solo para ESMERALDAS
#-------------------------------------------------------------------------------

nc = 0.95
z = qnorm(nc + (1-nc)/2)
er = 0.075

tamanio_3 <- est %>% 
  filter(dominio == "080150_1" | dominio == "080150_2") %>% 
  group_by(dominio, nombre_dom) %>% 
  summarise(N = N_alq_censo,
            n = unique(n),
            desv = sd_alq,
            y = med_alq) %>% 
  mutate(numerador = (desv)^2,
         denominador = ((er*y/z)^2) + (desv^2/N),
         tam = ceiling(numerador/denominador), 
         tam = mult_6(tam, N)) %>% 
  select(dominio, nombre_dom, tam)


#-------------------------------------------------------------------------------
# Tamanio Fianl
#-------------------------------------------------------------------------------

tamanio <- rbind(tamanio_1, tamanio_2, tamanio_3)

#-------------------------------------------------------------------------------
# Exportando
#-------------------------------------------------------------------------------

ruta <- "productos/02_viv_alquileres/03_tamanio/"
export(tamanio, paste0(ruta, "tamanio.rds"))
export(tamanio_galapagos, paste0(ruta, "tamanio_galapagos.rds"))


