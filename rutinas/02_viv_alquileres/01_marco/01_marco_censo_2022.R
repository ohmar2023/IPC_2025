
rm(list = ls())

source("rutinas/99_librerias/librerias.R")
source("rutinas/99_librerias/unzip.R")

# -----------------------------------------------------------------------------
# LECTURA BASE CENSO VIVIENDAS ------------------------------------------------
# -----------------------------------------------------------------------------

viv_2022 <- read_zip("INSUMOS/03_censo", "bases.zip", "viv_2022.csv") %>% 
  mutate(
    I01 = str_pad(I01,width = 2,side = "left", pad = "0"),#provincia
    I02 = str_pad(I02,width = 2,side = "left", pad = "0"),#canton
    I03 = str_pad(I03,width = 2,side = "left", pad = "0"),#parroquia
    
    I04 = str_pad(I04,width = 3,side = "left", pad = "0"),#zona
    I05 = str_pad(I05,width = 3,side = "left", pad = "0"),#sector
    
    I06 = str_pad(I06,width = 3,side = "left", pad = "0"),#manzana (tiene 3)
    I07 = str_pad(I07,width = 3,side = "left", pad = "0"),#localidad (tiene 2)
    
    I08 = str_pad(I08,width = 3,side = "left", pad = "0"),#numero de edificio
    I10 = str_pad(I10,width = 3,side = "left", pad = "0") #numero de vivienda
  ) 
  
viv_2022 <- viv_2022 %>% 
  mutate(
    man_loc = ifelse(I04 == "999",I07,I06),
    id_dom = paste0(I01,I02,I03),
    id_viv = paste0(id_dom,I04,I05,
                    man_loc,I08,I10))

# -----------------------------------------------------------------------------
# LECTURA BASE CENSO HOGARES --------------------------------------------------
# -----------------------------------------------------------------------------

hog_2022 <- read_zip("INSUMOS/03_censo", "bases.zip", "hog_2022.csv") %>%   
  mutate(
    I01 = str_pad(I01,width = 2,side = "left", pad = "0"),#provincia
    I02 = str_pad(I02,width = 2,side = "left", pad = "0"),#canton
    I03 = str_pad(I03,width = 2,side = "left", pad = "0"),#parroquia
    
    I04 = str_pad(I04,width = 3,side = "left", pad = "0"),#zona
    I05 = str_pad(I05,width = 3,side = "left", pad = "0"),#sector
    
    I06 = str_pad(I06,width = 3,side = "left", pad = "0"),#manzana (tiene 3)
    I07 = str_pad(I07,width = 3,side = "left", pad = "0"),#localidad (tiene 2)
    
    I08 = str_pad(I08,width = 3,side = "left", pad = "0"),#numero de edficio
    I10 = str_pad(I10,width = 3,side = "left", pad = "0") #numero de vivienda
  )

hog_2022 <- hog_2022 %>% 
  mutate(
    man_loc = ifelse(I04 == "999",I07,I06),
    id_dom = paste0(I01,I02,I03),
    id_viv = paste0(id_dom,I04,I05,
                    man_loc,I08,I10)) %>% 
  filter(H09 == 4) %>% # 4. Arrendada/anticresis
  group_by(id_viv) %>% 
  #mutate(cuartos_cocinar = ifelse(1 %in% unique(H02),TRUE,FALSE)) %>%
  ungroup()

hog_2022 <- hog_2022[!duplicated(hog_2022$id_viv),]
dim(hog_2022)
n_distinct(hog_2022$id_viv)

#-------------------------------------------------------------------------------
# JUNTANDO LA BASE DE HOGARES Y VIVIENDAS --------------------------------------
#-------------------------------------------------------------------------------

viv_hog <- viv_2022 %>% filter(id_viv %in% hog_2022$id_viv)
viv_hog <- viv_hog %>% 
  left_join(select(hog_2022, H09, id_viv),by = "id_viv")

dim(viv_hog)
n_distinct(viv_hog$id_viv)

#-------------------------------------------------------------------------------
# Filtrando base acorde a las necesidades del pedido:

  #Nos quedamos con v01 (Tipo de vivienda) = 1. Casa/villa y 2. Departamento en casa o edificio
  #Nos quedamos con AUR	(Área urbana o rural) = 1
  #Consideramos solo los dominios especificados en v_ciudades_auto
  #Para el caso de Galápagos (I01 == "20") se cosnsidera su dom a nivel de provincia
  #Se crea id_dom_2 para hacer el calculo de tamaño cosniderando casa y depart.
  #No es correcto usar id_dom para el caso de Galápagos. Considerar solo a nivel de prov. 
#-------------------------------------------------------------------------------

v_ciudades_auto <- c("170150","090150","010150","070150","180150",
                     "080150", "230150", "130850", "110150")

t_1 <- viv_hog %>% 
  filter((V01 %in% c(1,2) & AUR == 1)) %>% 
  filter(id_dom %in% v_ciudades_auto | I01 == "20") %>% 
  mutate(id_dom_2 = paste0(id_dom,"_",V01 )) %>% 
  mutate(id_dom = ifelse(I01 == "20", "20", id_dom),
         id_dom_2 = ifelse(I01 == "20", "20", id_dom_2))

t_1 <- t_1 %>% select(id_dom,
                      id_dom_2,
                      id_viv,
                      I01,I02,
                      I03,I04,
                      I05,I06,
                      I08,I10,man_loc,
                      V01,
                      V15)

# -----------------------------------------------------------------------------
# Agregando los nombres de los dominios al marco
# -----------------------------------------------------------------------------

t_1 <- t_1 %>% mutate(nombre_dom = case_when(id_dom == "170150" ~ "Quito",
                                             id_dom == "090150" ~ "Guayaquil",
                                             id_dom == "010150" ~ "Cuenca",
                                             id_dom == "070150" ~ "Machala",
                                             id_dom == "180150" ~ "Ambato",
                                             id_dom == "080150" ~ "Esmeraldas",
                                             id_dom == "230150" ~ "Sto. Domingo", 
                                             id_dom == "130850" ~ "Manta", 
                                             id_dom == "110150" ~ "Loja",
                                             id_dom == "20" ~ "Galápagos",
                                             TRUE ~ "Error"))

#-------------------------------------------------------------------------------
# Exportando -------------------------------------------------------------------
#-------------------------------------------------------------------------------

ruta <- "productos/02_viv_alquileres/01_marco/"

export(t_1, paste0(ruta, "/marco_ipc_alquileres_2025.rds"))










