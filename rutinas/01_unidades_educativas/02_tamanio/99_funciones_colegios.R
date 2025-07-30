
# ------------------------------------------------------------------------------
# FUNCION PARA REDONDEAR A MULTIPLOS DE 6
# ------------------------------------------------------------------------------

# mult_6 <- function(n, N){
#   n1 = ifelse(n %% 6 == 1, n - 1, 
#               ifelse(n %% 6 == 0, n, n + (6 - n %% 6)))
#   
#   n1 = ifelse(n1 <= N,n1, n)
#   return(n1)
# }

# ------------------------------------------------------------------------------
# FUNCION CALCULO TAMAÑO MUESTRAL
# ------------------------------------------------------------------------------

tamanio_media <- function(base, 
                          nc = 0.95,
                          z = qnorm(nc+(1-nc)/2),
                          er = 0.1,
                          dominio,
                          variable){
  if(!dominio %in% names(base))
  {
    tamanio <- base %>% 
      mutate(dominio = dominio,
             var_disenio = .data[[variable]]) %>% 
      group_by(dominio) %>% 
      summarise(N = n(),
                desv = sd(var_disenio,na.rm = T),
                sum_var_disenio = sum(var_disenio,na.rm = T),
                y = mean(var_disenio)) %>% 
      mutate(numerador = (desv)^2,
             denominador = ((er*y/z)^2) + (desv^2/N),
             tam = ceiling(numerador/denominador),
             #tam = ifelse(tam < 5, tam + 1 , tam),
              dif = N - tam) %>% 
       select(dominio,N,tam,dif) 
  }else{
    tamanio <- base %>% 
      mutate(dominio = .data[[dominio]],
             var_disenio = .data[[variable]]) %>% 
      group_by(dominio) %>% 
      summarise(N = n(),
                desv = sd(var_disenio,na.rm = T),
                sum_var_disenio = sum(var_disenio,na.rm = T),
                y = mean(var_disenio)) %>% 
      mutate(numerador = (desv)^2,
             denominador = ((er*y/z)^2) + (desv^2/N),
             tam = ceiling(numerador/denominador),
             #tam = ifelse(tam < 5, tam + 1 , tam),
             dif = N - tam) %>% 
      select(dominio,N,tam,dif) 
  }
}



