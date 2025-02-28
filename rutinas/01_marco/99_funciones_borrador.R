
# -----------------------------------------------------------------------------
# Variable que identifica instituciones con cuantos niveles disponga
# -----------------------------------------------------------------------------

n_niveles <- apply(bdd_precios_col %>% select(19:26), 1, FUN =  function(x)  sum(!is.na(x)))
bdd_precios_col <- cbind(bdd_precios_col, n_niveles)

# -----------------------------------------------------------------------------
#Identificamos si los costos en los diferentes niveles son iguales o no
# -----------------------------------------------------------------------------

f_filas_1 <- function(x,y){ifelse(x == y, x , FALSE)}

aux <- bdd_precios_col %>% select(contains("matricula")) 
reduce(aux, f_filas_1)

cbind(apply(aux, 1, n_distinct),reduce(aux, f_filas_1)) %>% View()

reduce(list(c(1,1,1,1), c(2,2,1,1)), f_filas_1)

bdd_precios_col %>% 
  rowwise() %>% 
  mutate(c = n_distinct(.data[[names(aux)[1]]], 
                        .data[[names(aux)[2]]],
                        .data[[names(aux)[3]]],
                        .data[[names(aux)[4]]])) %>% 
  ungroup() %>% 
  View()