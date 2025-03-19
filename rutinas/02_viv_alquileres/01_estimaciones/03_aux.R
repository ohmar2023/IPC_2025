
v_ciudades_auto <- c("170150","090150","010150","070150","180150",
                     "080150", "230150", "130850", "110150")

hog_censo = hog_2022 %>% 
  filter(id_dom %in% v_ciudades_auto) %>% 
  group_by(id_dom) %>% 
  summarise(N_2 = n()) 

base %>% 
  filter(dominio %in% v_ciudades_auto) %>% 
  group_by(dominio) %>% 
  summarise(N = sum(fexp_acum_aju)) %>% 
  adorn_totals() %>% 
  View()
