
rm(list = ls())

source("rutinas/99_librerias/librerias.R")
source("rutinas/99_librerias/unzip.R")

# -----------------------------------------------------------------------------
# LECTURA BASE CENSO VIVIENDAS -------------------------------------------------
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
    man_loc = ifelse(I04=="999",I07,I06),
    id_dom = paste0(I01,I02,I03),
    id_viv = paste0(id_dom,I04,I05,
                    man_loc,I08,I10))

# LECTURA BASE CENSO HOGARES ---------------------------------------------------

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
    man_loc = ifelse(I04=="999",I07,I06),
    id_dom = paste0(I01,I02,I03),
    id_viv = paste0(id_dom,I04,I05,
                    man_loc,I08,I10)) %>% 
  filter(H09 == 4) %>% 
  group_by(id_viv) %>% 
  mutate(cuartos_cocinar = ifelse(1 %in% unique(H02),TRUE,FALSE)) %>%
  ungroup()

#hog_2022 %>% group_by(id_viv) %>% summarise(n()) %>% View()

# hog_2022 %>% group_by(id_viv) %>% 
#   mutate(nueva = sum(H01)) %>%
#   ungroup() %>% View()

hog_2022 <- hog_2022[!duplicated(hog_2022$id_viv),]
dim(hog_2022)
n_distinct(hog_2022$id_viv)

#-------------------------------------------------------------------------------
# JUNTANDO LA BASE DE HOGARES Y VIVIENDAS --------------------------------------
#-------------------------------------------------------------------------------

viv_hog <- viv_2022 %>% filter(id_viv %in% hog_2022$id_viv)
viv_hog <- viv_hog %>% 
  left_join(select(hog_2022,cuartos_cocinar,id_viv),by = "id_viv")

dim(viv_hog)
n_distinct(viv_hog$id_viv)

### CIUDADES A COSNIDERAR ###
v_ciudades_auto <- c("170150","090150","010150","070150","180150",
                     "080150", "230150", "130850", "110150")

t_1 <- viv_hog %>% 
  filter(V01 %in% c(1,2,3)) %>% 
  filter(id_dom %in% v_ciudades_auto) 

t_1 <- t_1 %>% select(id_dom,id_viv,
                      I01,I02,
                      I03,I04,
                      I05,I06,
                      I08,I10,man_loc,
                      V01,
                      V15,
                      cuartos_cocinar)

export(t_1,"lista_viv_arriendo.xlsx")

tabla <- t_1 %>% group_by(id_dom,V01) %>% summarise(n = n())

pivot_wider(tabla,
            names_from = "V01",
            values_from = n) %>% 
  adorn_totals() %>% 
  View()


hog_2022 %>% filter(id_viv %in% t_1$id_viv) %>% 
  group_by(id_viv) %>% 
  summarise(n = n()) %>% 
  filter(n != 1 ) %>% 
  View()


names(t_1)
dim(t_1)








