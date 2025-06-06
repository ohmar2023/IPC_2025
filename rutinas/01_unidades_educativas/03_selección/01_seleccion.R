
rm(list = ls())

source("rutinas/99_librerias_funciones/librerias.R")

# ------------------------------------------------------------------------------
# Cargamos tamanio -------------------------------------------------------------
# ------------------------------------------------------------------------------

tamanio <- import("productos/01_unidades_educativas/02_tamanio_media/tamanio_UE_final.xlsx")

#-------------------------------------------------------------------------------
# UE inclusión forzosa
#-------------------------------------------------------------------------------

inc_for <- read_excel("insumos/01_unidades_educativas/01_marco_ue/inclusion_forzosa.xlsx") %>% clean_names()

#-------------------------------------------------------------------------------
# Marco
#-------------------------------------------------------------------------------

ruta <- "productos/01_unidades_educativas/01_marco/marco_colegios_ipc.rds"
marco <- read_rds(ruta) %>% 
  mutate( inclusion_forzosa = ifelse(amie %in% inc_for$amie, 1, 0), 
          dominio = canton) %>% 
  left_join(select(tamanio, dominio, tam_nacional), by = "dominio")

# ------------------------------------------------------------------------------
# Hacemos la selección y juntamos las EU de inclusión forsoza
# ------------------------------------------------------------------------------


set.seed(20250602) #Periodo 2

seleccion <-  marco %>%  
  filter(inclusion_forzosa == 0) %>% 
  group_by(dominio) %>% 
  sample_n(unique(tam_nacional))

seleccion <- rbind(seleccion, marco %>% filter(inclusion_forzosa == 1))

seleccion %>% group_by(dominio) %>% summarise(n()) %>% 
  adorn_totals() %>% View()

# ------------------------------------------------------------------------------
# Validación selección de la muestra
# ------------------------------------------------------------------------------

seleccion %>% group_by(dominio) %>% summarise(n()) %>% 
  adorn_totals() %>% View()

dim(seleccion)

# ------------------------------------------------------------------------------
# Exportar
# ------------------------------------------------------------------------------

export(seleccion,"productos/01_unidades_educativas/03_seleccion_upm/muestra_UE.xlsx")

  


