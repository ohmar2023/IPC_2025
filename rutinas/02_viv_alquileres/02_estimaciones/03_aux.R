

source("rutinas/99_librerias/librerias.R")
source("rutinas/99_librerias/unzip.R")

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


v_ciudades_auto <- c("170150","090150","010150","070150","180150",
                     "080150", "230150", "130850", "110150")


# ------------------------------------------------------------------------------
# LECTURA BASE CENSO HOGARES ---------------------------------------------------
# ------------------------------------------------------------------------------

hog_tot_censo = hog_2022 %>% 
  filter(id_dom %in% v_ciudades_auto) %>% 
  group_by(id_dom) %>% 
  summarise(N_2 = n()) 


hog_alq_censo = hog_2022 %>% 
  filter(id_dom %in% v_ciudades_auto) %>% 
  group_by(id_dom) %>% 
  summarise(N_alq_censo = n()) 

export(hog_tot_censo %>% left_join(hog_alq_censo) %>% 
  rename(N_tot_censo = N_2), "hog_censo.xlsx")
