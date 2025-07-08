
rm(list = ls())

source("rutinas/99_librerias_funciones/librerias.R")

# ------------------------------------------------------------------------------
# CARGAMOS LOS TAMAÑOS ---------------------------------------------------------
# ------------------------------------------------------------------------------

tamanio <- readRDS("productos/02_viv_alquileres/02_pedido_secundario/03_tamanio/tam_ped_002_final.rds")

# -----------------------------------------------------------------------------
# Lectura marco que viene del censo
# -----------------------------------------------------------------------------

marco_ipc_alquileres_2025 <- import("productos/02_viv_alquileres/02_pedido_secundario/01_marco/marco_ipc_alquileres_2025.rds") 
aux1 <- dim(marco_ipc_alquileres_2025)[1]

# -----------------------------------------------------------------------------
# Agregando tamaño muestral al marco
# -----------------------------------------------------------------------------

marco_ipc_alquileres_2025 <- marco_ipc_alquileres_2025 %>% 
  left_join(select(tamanio, id_dom, V01, tam_distr), by = c("id_dom", "V01"))

if(aux1 == dim(marco_ipc_alquileres_2025)[1]){
  print("left join correcto")
}else{
  print("left jpin fallido")
}

# -----------------------------------------------------------------------------
# Seleccion
# -----------------------------------------------------------------------------

set.seed(as.numeric(20250704)) 

muestra_alq_1 <- marco_ipc_alquileres_2025 %>% 
  filter(id_dom != "20") %>% 
  group_by(id_dom) %>% 
  sample_n(unique(tam_distr)) 


muestra_alq <- muestra_alq_1

if((muestra_alq %>% group_by(dominio, tam) %>% summarise(selec = n()) %>% 
  mutate(control = ifelse(tam == selec, 1, 0)) %>% 
  filter(control == 0) %>% 
  dim())[1] != 0){message("Error en la selección")}else{" *** Selección correcta ***"}

#-------------------------------------------------------------------------------
# Exportando
#-------------------------------------------------------------------------------

ruta <- "productos/02_viv_alquileres/04_seleccion/"
export(muestra_alq %>% select(-tam, -id_dom), paste0(ruta, "muestra_alq.rds"))



