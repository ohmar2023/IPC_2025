
rm(list = ls())

source("rutinas/99_librerias_funciones/librerias.R")
source("rutinas/99_librerias_funciones/99_mult_6.R")

#-------------------------------------------------------------------------------
# Lectura estimaciones
#-------------------------------------------------------------------------------

#est <- readRDS("productos/02_viv_alquileres/02_pedido_secundario/02_estimaciones/estimaciones.rds")
est <- readRDS("productos/02_viv_alquileres/02_pedido_secundario/02_estimaciones/estimaciones.rds")

# -----------------------------------------------------------------------------
# Lectura marco que viene del censo
# -----------------------------------------------------------------------------

ruta <- "productos/02_viv_alquileres/02_pedido_secundario/01_marco/marco_ipc_alquileres_2025.rds"
marco_ipc_alquileres_2025 <- import(ruta)

#-------------------------------------------------------------------------------
# Tamaño para los dominios sin considerar Galápagos ni Esmeraldas
#-------------------------------------------------------------------------------

nc = 0.95
z = qnorm(nc + (1-nc)/2)
er = 0.05

tamanio_1 <- est %>% 
  filter(dominio != "20" & dominio != "080150") %>% 
  group_by(dominio, nombre_dom) %>% 
  summarise(N = N_alq_censo,
            n = unique(n),
            desv = sd_alq,
            y = med_alq) %>% 
  mutate(numerador = (desv)^2,
         denominador = ((er*y/z)^2) + (desv^2/N),
         tam = ceiling(numerador/denominador)) %>% 
         #tam = mult_6(tam, N)) %>% 
  select(dominio, nombre_dom, tam)

#-------------------------------------------------------------------------------
# Tamaño solo para ESMERALDAS
#-------------------------------------------------------------------------------

nc = 0.95
z = qnorm(nc + (1-nc)/2)
er = 0.065

tamanio_2 <- est %>% 
  filter(dominio == "080150") %>% 
  group_by(dominio, nombre_dom) %>% 
  summarise(N = N_alq_censo,
            n = unique(n),
            desv = sd_alq,
            y = med_alq) %>% 
  mutate(numerador = (desv)^2,
         denominador = ((er*y/z)^2) + (desv^2/N),
         tam = ceiling(numerador/denominador)) %>% 
         #tam = mult_6(tam, N)) %>% 
  select(dominio, nombre_dom, tam)

#-------------------------------------------------------------------------------
# Tamanio Fianl
#-------------------------------------------------------------------------------

tamanio <- rbind(tamanio_1, tamanio_2) 

#-------------------------------------------------------------------------------
# Distribución tamaño para CASA (01) y DEPARTAMENTO (02)
#-------------------------------------------------------------------------------

tamanio <- marco_ipc_alquileres_2025 %>% 
  group_by(id_dom, V01) %>% 
  summarise(n_casas_dep = n()) %>%
  group_by(id_dom) %>% 
  mutate(prop = n_casas_dep / sum(n_casas_dep) ) %>%
  ungroup() %>% 
  left_join(select(tamanio, tam, id_dom = dominio, nombre_dom)) %>% 
  mutate(tam_distr = ceiling(tam * prop), 
         tam_distr = mult_6(tam_distr, n_casas_dep)) %>% 
  filter(id_dom != "20")

#tamanio_01 <- readRDS("productos/02_viv_alquileres/01_pedido_inicial/03_tamanio/tamanio.rds")

if(tamanio %>% group_by(id_dom) %>% 
   summarise(s_p = sum(prop)) %>% 
   select(s_p) %>% unique() == 1){
  print("Proporciones correctas")
}else{
  message("Error en proporciones")
}

#-------------------------------------------------------------------------------
# Exportando
#-------------------------------------------------------------------------------

ruta <- "productos/02_viv_alquileres/02_pedido_secundario/03_tamanio/"
export(tamanio %>% select(id_dom, nombre_dom, V01, tam_distr), 
       paste0(ruta, "tam_ped_002_final.rds"))





