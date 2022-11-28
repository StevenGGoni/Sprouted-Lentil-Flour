
#' Pseudo R Cuadrado para modelos generalizados bayesianos de efectos mixtos
#' de la familia gaussiana (family = "gaussian")
#'
#' @param mod objeto del modelo de interés de MCMCglmm 
#' @param mod_nulo objeto del modelo nulo de MCMCglmm 
#' @param prob número escalar entre 0-1 que da la probalidad para el intervalo
#' de credibilidad de coda::HPDinterval(), por defecto en 0.95
#'
#' @return objeto lista con las distribuciones posteriores de R2m y R2c
#' y dos data.frame con las medias y modas posteriores y los intervalos de
#' credibildiad a la probabilidad prob
#' @export 
#'
#' @examples
bayesian_R2_GLMM <- function(mod, mod_nulo, prob = 0.95){
  
  
  # Definición de parámetros ----
  
  mm0 <- mod_nulo
  mmF <- mod
  p <- prob
  
  
  # Preparación para cálculo de pseudo R^2 ----
  
  mmF_list <- split(mmF$Sol, seq(nrow(mmF$Sol)))
  
  vmVarF <- purrr::map(mmF_list, ~.x %*% t(mmF$X)) %>% 
    purrr::map_dbl(~var(as.vector(.x)))
  
  # Calculo de R2m ----
  
  R2m <- vmVarF/(vmVarF + rowSums(mmF$VCV)) %>% 
    coda::as.mcmc()
  
  mean_R2m <- mean(R2m)
  
  median_R2m <- median(R2m)
  
  mode_R2m <- MCMCglmm::posterior.mode(R2m)
  
  int_cred_R2m <- coda::HPDinterval(R2m, prob = p)
  
  
  # Cálculo de R2c ----
  
  R2c <- (vmVarF + rowSums(mmF$VCV) - mmF$VCV[, -1])/ # mmF$VCV[, -1] = units
    (vmVarF + rowSums(mmF$VCV)) %>% 
    coda::as.mcmc()
  
  mean_R2c <- mean(R2c)
  
  median_R2c <- median(R2c)
  
  mode_R2c <- MCMCglmm::posterior.mode(R2c)
  
  int_cred_R2c <- coda::HPDinterval(R2c, prob = p)
  
  # Return ----
  
  list(posteriorR2m = R2m, # Marginal
       resultadosR2m = data.frame(mean_R2m, mode_R2m, median_R2m,
                                  int_cred_R2m, prob_cred = p), 
       posteriorR2c = R2c, # Condicional
       resultadosR2c = data.frame(mean_R2c, mode_R2c, median_R2c,
                                  int_cred_R2c, prob_cred = p))
  
}