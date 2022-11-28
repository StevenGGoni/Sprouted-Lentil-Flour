
# libraries ---------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(coda)
library(MCMCglmm)
library(desirability)


# citation ----------------------------------------------------------------

# citation("openxlsx")


# Funciones ---------------------------------------------------------------

source("bayesian_R2_MCMCglmm.R")
source("posterior_ICC.R")

# read data ---------------------------------------------------------------


datos <- openxlsx::read.xlsx("data/SP_DdE.xlsx")

skimr::skim(datos)

# modelo ------------------------------------------------------------------


# Textura -----------------------------------------------------------------

mod_T_0 <- MCMCglmm::MCMCglmm(Textura ~ 1,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

mod_T_1 <- MCMCglmm::MCMCglmm(Textura ~ A*B*C*D*E,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

# summary

summary(mod_T_1)

# ICC

ICC <- posterior_ICC(modelo = mod_T_1)

ICC$resultados_ICC %>% 
  purrr::modify_at(1:5, scales::percent, accuracy = 0.001)

# Cálculo del R^2

R2_m1 <- bayesian_R2_GLMM(mod_T_1, mod_T_0)

R2_m1$resultadosR2m
R2_m1$resultadosR2c

## Se respeta el principio de jerarquía de los DdE

mod_T_2 <- MCMCglmm::MCMCglmm(Textura ~ A + B + C + D + E + # principales
                                # dobles
                                A:B + A:C + B:C + A:D + B:D + C:D + A:E + 
                                B:E + C:E + D:E + 
                                # triples
                                C:D:E + B:D:E + A:D:E + B:C:E + + A:C:E + 
                                A:B:E + A:C:D + A:B:D + A:B:C  +
                                # cuadruples
                                A:B:C:E + A:B:D:E + A:C:D:E,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

# summary

summary(mod_T_2)

# ICC

ICC <- posterior_ICC(modelo = mod_T_2)

ICC$resultados_ICC %>% 
  purrr::modify_at(1:5, scales::percent, accuracy = 0.001)

# Cálculo del R^2

R2_m2 <- bayesian_R2_GLMM(mod_T_2, mod_T_0)

R2_m2$resultadosR2m
R2_m2$resultadosR2c

# Criterios de información 

MuMIn::DIC(mod_T_0, mod_T_1, mod_T_2)

# Moda y mediana posterior

moda_post_t <- MCMCglmm::posterior.mode(mod_T_2$Sol)

median_post_t <- data.frame(mod_T_2$Sol) %>% 
  purrr::map_dbl(median)

# Resultados finales

res_text <- summary(mod_T_2)

list(mod_T_2$VCV[,1], mod_T_2$VCV[,2]) %>% 
  purrr::map_dbl(median) %>% 
  round(2)

res_text$Gcovariances %>% 
  rbind(res_text$Rcovariances) %>% round(2)

coef_text <- res_text$solutions %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "Term") %>% 
  dplyr::mutate(post.mode = moda_post_t,
                post.median = median_post_t) %>% 
  dplyr::relocate(post.median, post.mode, .after = post.mean)


coef_text

# Criterios de convergencia

## Traza

plot(mod_T_2)

## Autocorrelación

mod_T_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::autocorr.plot()

## Raftery-Lewis

mod_T_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::raftery.diag()

## Heildelberger y Welch

mod_T_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::heidel.diag()

## Geweke

mod_T_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::geweke.plot()

# Convergencia R^2

## Traza

data.frame(R2m = R2_m2$posteriorR2m, 
           R2c = R2_m2$posteriorR2c) %>% 
  as.mcmc() %>% 
  plot()

## Autocorrelación

data.frame(R2m = R2_m2$posteriorR2m, 
           R2c = R2_m2$posteriorR2c) %>% 
  as.mcmc() %>% 
  coda::as.mcmc.list() %>% 
  coda::autocorr.plot()


# Rendimiento -------------------------------------------------------------

mod_R_0 <- MCMCglmm::MCMCglmm(Rendimiento ~ 1,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

mod_R_1 <- MCMCglmm::MCMCglmm(Rendimiento ~ A*B*C*D*E,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

# summary

summary(mod_R_1)

# ICC

ICC <- posterior_ICC(modelo = mod_R_1)

ICC$resultados_ICC %>% 
  purrr::modify_at(1:5, scales::percent, accuracy = 0.001)


# Cálculo del R^2

R2_m1 <- bayesian_R2_GLMM(mod_R_1, mod_R_0)

R2_m1$resultadosR2m
R2_m1$resultadosR2c

## Se respeta el principio de jerarquía de los DdE

mod_R_2 <- MCMCglmm::MCMCglmm(Rendimiento ~ A + B + C + D + E + # principales
                                # dobles
                                A:B + A:C + B:C + A:D + A:E + B:E + C:E + 
                                # triples
                                B:C:E + A:C:E + A:B:E + A:B:C  +
                                # cuadruples
                                A:B:C:E,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

# summary

summary(mod_R_2) 

# ICC

ICC <- posterior_ICC(modelo = mod_R_2)

ICC$resultados_ICC %>% 
  purrr::modify_at(1:5, scales::percent, accuracy = 0.001)

# Cálculo del R^2

R2_m2 <- bayesian_R2_GLMM(mod_R_2, mod_R_0)

R2_m2$resultadosR2m
R2_m2$resultadosR2c

# Criterios de información 

MuMIn::DIC(mod_R_0, mod_R_1, mod_R_2)

# Moda y mediana posterior

moda_post_R <- MCMCglmm::posterior.mode(mod_R_2$Sol)

median_post_R <- data.frame(mod_R_2$Sol) %>% 
  purrr::map_dbl(median)

# Resultados finales

res_rend <- summary(mod_R_2)

list(mod_R_2$VCV[,1], mod_R_2$VCV[,2]) %>% 
  purrr::map_dbl(median) %>% round(3)

res_rend$Gcovariances %>% 
  rbind(res_rend$Rcovariances)

coef_rend <- res_rend$solutions %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "Term") %>% 
  dplyr::mutate(post.mode = moda_post_R,
                post.median = median_post_R) %>% 
  dplyr::relocate(post.median, post.mode, .after = post.mean)


coef_rend

# Criterios de convergencia

## Traza

plot(mod_R_2)

## Autocorrelación

mod_R_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::autocorr.plot()

## Raftery-Lewis

mod_R_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::raftery.diag()

## Heildelberger y Welch

mod_R_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::heidel.diag()

## Geweke

mod_R_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::geweke.plot()

# Convergencia R^2

## Traza

data.frame(R2m = R2_m2$posteriorR2m, 
           R2c = R2_m2$posteriorR2c) %>% 
  as.mcmc() %>% 
  plot()

## Autocorrelación

data.frame(R2m = R2_m2$posteriorR2m, 
           R2c = R2_m2$posteriorR2c) %>% 
  as.mcmc() %>% 
  coda::as.mcmc.list() %>% 
  coda::autocorr.plot()


# Color -------------------------------------------------------------------

mod_C_0 <- MCMCglmm::MCMCglmm(Color ~ 1,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

mod_C_1 <- MCMCglmm::MCMCglmm(Color ~ A*B*C*D*E,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

# summary

summary(mod_C_1)

# ICC

ICC <- posterior_ICC(modelo = mod_C_1)

ICC$resultados_ICC %>% 
  purrr::modify_at(1:5, scales::percent, accuracy = 0.001)


# Cálculo del R^2

R2_m1 <- bayesian_R2_GLMM(mod_C_1, mod_C_0)

R2_m1$resultadosR2m
R2_m1$resultadosR2c

## Se respeta el principio de jerarquía de los DdE

mod_C_2 <- MCMCglmm::MCMCglmm(Color ~ A + B + C + D + E + # principales
                                # dobles
                                A:B + A:C + B:C + A:D + B:D + 
                                C:D + A:E + B:E + D:E + 
                                # triples
                                B:D:E + A:D:E + A:B:E + B:C:D + A:C:D + A:B:D +
                                # cuadruples
                                A:B:D:E,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

# summary

summary(mod_C_2) 

# ICC

ICC <- posterior_ICC(modelo = mod_C_2)

ICC$resultados_ICC %>% 
  purrr::modify_at(1:5, scales::percent, accuracy = 0.001)

# Cálculo del R^2

R2_m2 <- bayesian_R2_GLMM(mod_C_2, mod_C_0)

R2_m2$resultadosR2m
R2_m2$resultadosR2c

# Criterios de información 

MuMIn::DIC(mod_C_0, mod_C_1, mod_C_2)

# Moda y mediana posterior

moda_post_C <- MCMCglmm::posterior.mode(mod_C_2$Sol)

median_post_C <- data.frame(mod_C_2$Sol) %>% 
  purrr::map_dbl(median)

# Resultados finales

res_col <- summary(mod_C_2)

list(mod_C_2$VCV[,1], mod_C_2$VCV[,2]) %>% 
  purrr::map_dbl(median) %>% 
  round(2)

res_col$Gcovariances %>% 
  rbind(res_col$Rcovariances)

coef_col <- res_col$solutions %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "Term") %>% 
  dplyr::mutate(post.mode = moda_post_C,
                post.median = median_post_C) %>% 
  dplyr::relocate(post.median, post.mode, .after = post.mean)


coef_col

# Criterios de convergencia

## Traza

plot(mod_C_2)

## Autocorrelación

mod_C_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::autocorr.plot()

## Raftery-Lewis

mod_C_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::raftery.diag()

## Heildelberger y Welch

mod_C_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::heidel.diag()

## Geweke

mod_C_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::geweke.plot()

# Convergencia R^2

## Traza

data.frame(R2m = R2_m2$posteriorR2m, 
           R2c = R2_m2$posteriorR2c) %>% 
  as.mcmc() %>% 
  plot()

## Autocorrelación

data.frame(R2m = R2_m2$posteriorR2m, 
           R2c = R2_m2$posteriorR2c) %>% 
  as.mcmc() %>% 
  coda::as.mcmc.list() %>% 
  coda::autocorr.plot()


# Peso --------------------------------------------------------------------

mod_P_0 <- MCMCglmm::MCMCglmm(Peso ~ 1,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

mod_P_1 <- MCMCglmm::MCMCglmm(Peso ~ A*B*C*D*E,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

# summary

summary(mod_P_1)

# ICC

ICC <- posterior_ICC(modelo = mod_P_1)

ICC$resultados_ICC %>% 
  purrr::modify_at(1:5, scales::percent, accuracy = 0.001)


# Cálculo del R^2

R2_m1 <- bayesian_R2_GLMM(mod_P_1, mod_P_0)

R2_m1$resultadosR2m
R2_m1$resultadosR2c

## Se respeta el principio de jerarquía de los DdE

mod_P_2 <- MCMCglmm::MCMCglmm(Peso ~ A + B + C + D + E + # principales
                                # dobles
                                A:B + A:C + B:C + A:D + B:D + 
                                C:D + A:E + B:E + D:E + C:E +
                                # triples
                                C:D:E + A:D:E + A:C:E + B:C:D + A:C:D +
                                A:B:D + A:B:C + A:B:E +
                                # cuadruples
                                A:C:D:E + A:B:C:D,
                              random = ~PC,
                              data = datos, 
                              thin = 1,
                              verbose = FALSE)

# summary

summary(mod_P_2) 

# ICC

ICC <- posterior_ICC(modelo = mod_P_2)

ICC$resultados_ICC %>% 
  purrr::modify_at(1:5, scales::percent, accuracy = 0.001)

ICC$posterior_ICC

# Cálculo del R^2

R2_m2 <- bayesian_R2_GLMM(mod_P_2, mod_P_0)

R2_m2$resultadosR2m
R2_m2$resultadosR2c

# Criterios de información 

MuMIn::DIC(mod_P_0, mod_P_1, mod_P_2)

# Moda y mediana posterior

moda_post_P <- MCMCglmm::posterior.mode(mod_P_2$Sol)

median_post_P <- data.frame(mod_P_2$Sol) %>% 
  purrr::map_dbl(median)

# Resultados finales

res_peso <- summary(mod_P_2)

list(mod_P_2$VCV[,1], mod_P_2$VCV[,2]) %>% 
  purrr::map_dbl(median) %>% round(2)

res_peso$Gcovariances %>% 
  rbind(res_peso$Rcovariances)

coef_peso <- res_peso$solutions %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "Term") %>% 
  dplyr::mutate(post.mode = moda_post_P,
                post.median = median_post_P) %>% 
  dplyr::relocate(post.median, post.mode, .after = post.mean)


coef_peso

# Criterios de convergencia

## Traza

plot(mod_P_2)

## Autocorrelación

mod_P_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::autocorr.plot()

## Raftery-Lewis

mod_P_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::raftery.diag()

## Heildelberger y Welch

mod_P_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::heidel.diag()

## Geweke

mod_P_2$Sol %>% 
  coda::as.mcmc.list() %>% 
  coda::geweke.plot()

# Convergencia R^2

## Traza

data.frame(R2m = R2_m2$posteriorR2m, 
           R2c = R2_m2$posteriorR2c) %>% 
  as.mcmc() %>% 
  plot()

## Autocorrelación

data.frame(R2m = R2_m2$posteriorR2m, 
           R2c = R2_m2$posteriorR2c) %>% 
  as.mcmc() %>% 
  coda::as.mcmc.list() %>% 
  coda::autocorr.plot()



# Unión de modelos --------------------------------------------------------


coef_col[, c(1:3, 5:6)] %>% 
  dplyr::full_join(coef_rend[, c(1:3, 5:6)], by = "Term") %>% 
  dplyr::full_join(coef_peso[, c(1:3, 5:6)], by = "Term") %>% 
  dplyr::full_join(coef_text[, c(1:3, 5:6)], by = "Term") %>% 
  dplyr::mutate(across(where(is.numeric), round, 2))


# Deseabilidad compuesta --------------------------------------------------

## Funciones a optimizar

peso <- function(x){
  
  183.8268 + 1.1708*x[1] + -0.3748*x[2] + 0.7908*x[3] + -1.3535*x[4] + 
    -0.4093*x[5] + 4.0037*x[1]*x[2] + -0.5198*x[1]*x[3] + 0.06*x[2]*x[3] + 
    -0.2501*x[1]*x[4] + 1.338*x[2]*x[4] + -1.2282*x[3]*x[4] + 
    -0.3924*x[1]*x[5] + -3.0855*x[2]*x[5] + -0.9768*x[4]*x[5] + 
    -0.704*x[3]*x[5] + 1.548*x[3]*x[4]*x[5] + 0.4003*x[1]*x[4]*x[5] +
    -1.1693*x[1]*x[3]*x[5] + 0.0925*x[2]*x[3]*x[4] + -2.3412*x[1]*x[3]*x[4] + 
    -0.318*x[1]*x[2]*x[4] + -0.3669*x[1]*x[2]*x[3] + 1.239*x[1]*x[2]*x[5] + 
    2.302*x[1]*x[3]*x[4]*x[5] + 1.8425*x[1]*x[2]*x[3]*x[4]
  
}

rendimiento <- function(x){
  
  0.8422 + 0.0235*x[1] + 0.0104*x[2] + 0.0017*x[3] + 0.0277*x[4] + 
    0.0227*x[5] + -0.0018*x[1]*x[2] + -3e-04*x[1]*x[3] + -0.0094*x[2]*x[3] + 
    -0.0265*x[1]*x[4] + 0.0077*x[1]*x[5] + 0.0049*x[2]*x[5] + 
    -0.0014*x[3]*x[5] + 0.0072*x[2]*x[3]*x[5] + 8e-04*x[1]*x[3]*x[5] + 
    -0.006*x[1]*x[2]*x[5] + -0.0117*x[1]*x[2]*x[3] + 0.0148*x[1]*x[2]*x[3]*x[5]
  
}

color <- function(x){
  
  17.1073 + -1.7144*x[1] + 2.1826*x[2] + 0.1288*x[3] + 0.394*x[4] + 
    0.3558*x[5] + 0.1484*x[1]*x[2] + 0.0341*x[1]*x[3] + -0.3212*x[2]*x[3] + 
    0.6569*x[1]*x[4] + 0.3114*x[2]*x[4] + 0.238*x[3]*x[4] + 0.1125*x[1]*x[5] + 
    0.1773*x[2]*x[5] + 0.4334*x[4]*x[5] + 0.2779*x[2]*x[4]*x[5] + 
    0.591*x[1]*x[4]*x[5] + 0.1888*x[1]*x[2]*x[5] + -0.4624*x[2]*x[3]*x[4] + 
    0.3348*x[1]*x[3]*x[4] + -0.1345*x[1]*x[2]*x[4] + 
    0.6177*x[1]*x[2]*x[4]*x[5]
  
}

textura <- function(x){
  
  15.7111 + -0.8224*x[1] + -2.0011*x[2] + 0.7837*x[3] + 2.3896*x[4] + 
    0.0056*x[5] + 0.5784*x[1]*x[2] + 0.2217*x[1]*x[3] + 0.0891*x[2]*x[3] + 
    -1.0607*x[1]*x[4] + -1.0986*x[2]*x[4] + 0.4772*x[3]*x[4] + 
    -0.0412*x[1]*x[5] + 0.4185*x[2]*x[5] + 0.9018*x[3]*x[5] + 
    0.2309*x[4]*x[5] + 0.7917*x[3]*x[4]*x[5] + -0.6457*x[2]*x[4]*x[5] + 
    -0.8555*x[1]*x[4]*x[5] + -0.2651*x[2]*x[3]*x[5] + -1.4684*x[1]*x[3]*x[5] + 
    0.2495*x[1]*x[2]*x[5] + -0.4108*x[1]*x[3]*x[4] + 0.2324*x[1]*x[2]*x[4] + 
    -0.5399*x[1]*x[2]*x[3] + 1.3337*x[1]*x[2]*x[3]*x[5] + 
    0.8077*x[1]*x[2]*x[4]*x[5] + -0.6823*x[1]*x[3]*x[4]*x[5]  
  
}

## Definción de los parámetros de optimización

pesoD <- desirability::dMax(low = 160.73, high = 202.47, tol = 0)
rendimientoD <- desirability::dMax(low = 0.6857, high = 0.9402, tol = 0)
colorD <- desirability::dMin(low = 10, high = 23, tol = 0)
texturaD <- desirability::dMax(low = 9.42, high = 34.5, tol = 0)

overallD <- desirability::dOverall(pesoD, rendimientoD, colorD, texturaD)


optimizador <- function(x, dObj){
  
  p <- peso(x)
  r <- rendimiento(x)
  c <- color(x)
  t <- textura(x)
  
  out <- predict(dObj, 
                 data.frame(p = p,
                            r = r,
                            c = c, 
                            t = t))
  
  if(any(abs(x) > 1)) out <- 0
  
  out
  
}

region <- tidyr::expand_grid(A = c(-1, 1), 
                             B = c(-1, 1),
                             C = c(-1, 1),
                             D = c(-1, 1),
                             E = c(-1, 1))

for(i in 1:dim(region)[1]) {
  
  tmp <- optim(par = as.vector(region[i,]),
               fn = optimizador,
               dObj = overallD,
               control = list(fnscale = -1))
  if(i == 1)
  {
    best <- tmp
  } else {
    if(tmp$value > best$value) best <- tmp
  }
}


pred <- data.frame(peso(best$par), rendimiento(best$par), 
          color(best$par), textura(best$par))

# Deseabilidad individual y compuesta

(pred *c(1, 100, 1, 1)) %>% 
  t() %>%
  rbind(NA) %>% 
  cbind(predict(overallD, pred, all = T) %>% 
          t()) %>% 
  as.data.frame() %>% 
  tibble::remove_rownames() %>% 
  setNames(c("Respuesta esperada", "Deseabilidad")) %>% 
  dplyr::mutate(Variable = c("Peso", "Rendimiento",
                             "Color", "Textura",
                             "Deseabilidad compuesta"),
                Unidades = c("g", "%", "ΔE", "cm", "NA"),
                across(where(is.numeric), round, 4)) %>% 
  dplyr::relocate(Variable, .before = 1) %>% 
  dplyr::relocate(Unidades, .before = Deseabilidad)
  

# Mejor combinación 

best$par
