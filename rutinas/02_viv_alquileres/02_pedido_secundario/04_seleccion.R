
rm(list = ls())

source("rutinas/99_librerias_funciones/librerias.R")

# ------------------------------------------------------------------------------
# CARGAMOS LOS TAMAÑOS ---------------------------------------------------------
# ------------------------------------------------------------------------------

tamanio <- readRDS("productos/02_viv_alquileres/03_tamanio/tamanio.rds")

# -----------------------------------------------------------------------------
# Lectura marco que viene del censo
# -----------------------------------------------------------------------------

marco_ipc_alquileres_2025 <- import("productos/02_viv_alquileres/01_marco/marco_ipc_alquileres_2025.rds") 

# -----------------------------------------------------------------------------
# Agregando tamaño muestral al marco
# -----------------------------------------------------------------------------

marco_ipc_alquileres_2025 <- marco_ipc_alquileres_2025 %>% 
  left_join(select(tamanio, dominio, tam), by = "dominio")


# -----------------------------------------------------------------------------
# Seleccion
# -----------------------------------------------------------------------------

set.seed(as.numeric(20250414)) 

muestra_alq_1 <- marco_ipc_alquileres_2025 %>% 
  filter(dominio != "20") %>% 
  group_by(dominio) %>% 
  sample_n(unique(tam)) 

muestra_alq_2 <- marco_ipc_alquileres_2025 %>% 
  filter(dominio == "20") %>% 
  left_join(select(tamanio_galapagos, dominio, I02, V01, tam_ppt), 
            by = c("dominio", "I02", "V01" )) %>% 
  group_by(dominio, I02, V01) %>% 
  sample_n(unique(tam_ppt)) %>% 
  select(-tam_ppt)

muestra_alq <- rbind(muestra_alq_1, muestra_alq_2)

if((muestra_alq %>% group_by(dominio, tam) %>% summarise(selec = n()) %>% 
  mutate(control = ifelse(tam == selec, 1, 0)) %>% 
  filter(control == 0) %>% 
  dim())[1] != 0){message("Error en la selección")}else{" *** Selección correcta ***"}

#-------------------------------------------------------------------------------
# Exportando
#-------------------------------------------------------------------------------

ruta <- "productos/02_viv_alquileres/04_seleccion/"
export(muestra_alq %>% select(-tam, -id_dom), paste0(ruta, "muestra_alq.rds"))



