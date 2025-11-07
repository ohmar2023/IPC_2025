
rm(list = ls())

source("rutinas/99_librerias_funciones/librerias.R")
source("rutinas/99_librerias_funciones/99_mult_6.R")

#-------------------------------------------------------------------------------

ruta <- "productos/02_viv_alquileres/02_pedido_secundario/01_marco/marco_ipc_alquileres_2025.rds"
marco_ipc_alquileres_2025 <- import(ruta)

#-------------------------------------------------------------------------------
# Tabla 1
tabla_1 <- marco_ipc_alquileres_2025 %>% 
  group_by(nombre_dom) %>% 
  summarise(n = n()) %>% 
  adorn_totals()
#-------------------------------------------------------------------------------
# Tabla 2
ruta <- "productos/02_viv_alquileres/02_pedido_secundario/03_tamanio/tam_ped_002_final.rds"
tamanio <- import(ruta)

tabla_2 <- tamanio %>% 
  mutate(tipo = ifelse(substr(dominio, 8, 8) == 1 , "Casa", "Departamento")) %>% 
  select(-dominio) %>% 
  pivot_wider(names_from = tipo, 
              values_from = tam_distr) %>% 
  adorn_totals(c("row","col")) 

# -----------------------------------------------------------------------------
# Exportando
# -----------------------------------------------------------------------------

dir <- paste("documentos/04_elaborados/02_documentos_metodologicos/02_Arriendos")
#dir.create(dir, showWarnings = F) 
dir.exists(dir)

wb <- createWorkbook("Tablas")
addWorksheet(wb, "tabla 1")
addWorksheet(wb, "tabla 2")
writeData(wb, sheet = "tabla 1", tabla_1)
writeData(wb, sheet = "tabla 2", tabla_2)

saveWorkbook(wb, paste0(dir,"/tablas_informes.xlsx"), overwrite = T)

if(sum(dir(dir) == paste0(fecha, ".xlsx"))==1){
  print(paste0("--- Se exportó correctamente el documento ",paste0(fecha, ".xlsx"), " ---" ))
}else{
  message("El código falló")
}


