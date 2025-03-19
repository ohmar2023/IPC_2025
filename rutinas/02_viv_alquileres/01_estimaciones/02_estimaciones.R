
rm(list = ls())

source("rutinas/99_librerias/librerias.R")

# -----------------------------------------------------------------------------
# Lectura base de datos
# -----------------------------------------------------------------------------

base <- readRDS("insumos/02_viv_alquileres/01_estimaciones/Fexp24_ENEMDU_hog_2_1.rds") %>% 
  clean_names() %>% 
  remove_all_labels() %>% 
  select(-n) %>% 
  mutate(vi141 = as.numeric(vi141), 
         vi141 =ifelse(vi141 == 999999 | vi141 == 99999, NA, vi141),
         vi141 = ifelse(vi14 %in% c(1,2), vi141, NA),
         area = substr(estrato,3,3),
         dominio = substr(id_upm,1,6))

# -----------------------------------------------------------------------------
# Ciudades
# -----------------------------------------------------------------------------

v_cantones <- c("170150","090150","010150","070150","180150",
                     "080150", "230150", "130850", "110150")

base <- base %>% 
  filter(dominio %in% v_cantones, area == 1) %>% 
  mutate(id_upm_aux = paste0(id_upm,mes),
         estrato_mes = paste0(estrato, mes))

# -----------------------------------------------------------------------------
# Diseño
# -----------------------------------------------------------------------------

dis_enemdu <- base %>% as_survey_design(ids = id_upm_aux,
                                        strata = estrato_mes,
                                        weights = fexp_acum_aju,
                                        nest = T)

options(survey.lonely.psu = "certainty")

est_media <- dis_enemdu %>% 
  group_by(dominio) %>% 
  summarise(med_alq = survey_mean(vi141, vartype=c("se","ci","cv","var"), na.rm = T, deff = T),
            n = sum(!is.na(vi141)),
            N = sum(fexp_acum_aju))

est_sd <- dis_enemdu %>% group_by(dominio) %>% 
  summarise(sd_alq = survey_sd(vi141, na.rm = T))

est <- est_media %>% 
  left_join(est_sd, by = "dominio") %>% 
  left_join(select(hog_censo, N_2, dominio = "id_dom"), by = "dominio")

#-------------------------------------------------------------------------------
# PARAMETROS
#-------------------------------------------------------------------------------

nc = 0.95
z = qnorm(nc+(1-nc)/2)
er = 0.05

# CALCULO -----------------------------------------------------------------

tamanio <- est %>% 
  mutate(dominio = dominio) %>% 
  group_by(dominio) %>% 
  summarise(N = N_2,
            n = unique(n),
            desv = sd_alq,
            y = med_alq) %>% 
  mutate(numerador = (desv)^2,
         denominador = ((er*y/z)^2) + (desv^2/N),
         tam = ceiling(numerador/denominador),
         dif = n - tam) %>% 
  adorn_totals(c("row"))

tamanio %>% view("N_1")


