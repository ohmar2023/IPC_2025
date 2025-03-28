
rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# -----------------------------------------------------------------------------
# Lectura marco que viene del censo
# -----------------------------------------------------------------------------

ruta <- "productos/02_viv_alquileres/01_marco/marco_ipc_alquileres_2025.rds"
marco_ipc_alquileres_2025 <- import(ruta)

# -----------------------------------------------------------------------------
# Lectura base ENEMDU

    #vi141: Es la variable que recoge el monto que se paga por arriendo.
    #vi14: Tenencia de la vivienda. 1: Arrendada. 2: Anticresis
    #vi02: 1: Casa o villa. 2: Departamento
# -----------------------------------------------------------------------------

base_enemdu <- readRDS("insumos/02_viv_alquileres/01_estimaciones/Fexp24_ENEMDU_hog_2_3.rds") %>% 
  clean_names() %>% 
  remove_all_labels() %>% 
  select(-n) %>% 
  mutate(vi141 = as.numeric(vi141), 
         vi141 =ifelse(vi141 == 999999 | vi141 == 99999, NA, vi141),
         vi141 = ifelse(vi14 %in% c(1,2), vi141, NA),
         area = substr(estrato,3,3),
         id_dom = substr(id_upm,1,6), 
         id_dom = ifelse(substr(id_upm,1,2) == "20", "20", id_dom)) %>% 
  filter(vi02 %in% c("1","2") & area == 1) #Consideramos solo departamentos y casas

# -----------------------------------------------------------------------------
# Ciudades
# -----------------------------------------------------------------------------

base_enemdu <- base_enemdu %>% 
  filter(id_dom %in% unique(marco_ipc_alquileres_2025$id_dom) & area == 1) %>% 
  mutate(id_upm_aux = paste0(id_upm,mes),
         estrato_mes = paste0(estrato, mes),
         id_dom_2 = paste0(id_dom,"_",vi02),
         id_dom_2 = ifelse(substr(id_upm,1,2) == "20", "20", id_dom_2)
         )

# -----------------------------------------------------------------------------
# Diseño
# -----------------------------------------------------------------------------

dis_enemdu <- base_enemdu %>% as_survey_design(ids = id_upm_aux,
                                        strata = estrato_mes,
                                        weights = fexp_acum_aju,
                                        nest = T)

options(survey.lonely.psu = "certainty")

#dominio = "id_dom"
dominio = "id_dom_2"
  
est_media <- dis_enemdu %>% 
  group_by(dominio = .data[[dominio]]) %>% 
  summarise(med_alq = survey_mean(vi141, vartype=c("se","ci","cv","var"), na.rm = T, deff = T),
            n = sum(!is.na(vi141)),
            N = sum(fexp_acum_aju))

est_sd <- dis_enemdu %>% 
  group_by(dominio = .data[[dominio]]) %>% 
  summarise(sd_alq = survey_sd(vi141, na.rm = T))

hog_censo <- marco_ipc_alquileres_2025 %>% 
  group_by(dominio = .data[[dominio]]) %>% 
  summarise(N_alq_censo = n())

est <- est_media %>% 
  left_join(est_sd, by = "dominio") %>% 
  left_join(hog_censo, by = "dominio")

#-------------------------------------------------------------------------------
# PARAMETROS
#-------------------------------------------------------------------------------

nc = 0.95
z = qnorm(nc+(1-nc)/2)
er = 0.065

# CALCULO -----------------------------------------------------------------

tamanio <- est %>% 
  mutate(dominio = dominio) %>% 
  group_by(dominio) %>% 
  summarise(N = N_alq_censo,
            n = unique(n),
            desv = sd_alq,
            y = med_alq) %>% 
  mutate(numerador = (desv)^2,
         denominador = ((er*y/z)^2) + (desv^2/N),
         tam = ceiling(numerador/denominador)) %>% 
  select(dominio, tam) %>% 
  adorn_totals(c("row"))


# Analizar el escenario dominio considerando depar y casa (1 y 2 variable censo viv - V01)
# Pendiente galapagos, me deben enviar los niveles de desagregación

export(tamanio,"alquileres_1_2_95_065.xlsx")
export(est,"est_1_2_dom_2.xlsx")




