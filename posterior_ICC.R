#' Posterior ICC
#'
#' @param modelo objeto del modelo de interés de MCMCglmm
#' @param prob número escalar entre 0-1 que da la probalidad para el intervalo
#' de credibilidad de coda::HPDinterval(), por defecto en 0.95
#'
#' @return objeto lista con las distribucion posterior de ICC
#' y una data.frame con la media y moda posterior y los intervalos de
#' credibildiad a la probabilidad prob
#' @export
#'
#' @examples
posterior_ICC <- function(modelo, prob = 0.95){
  
  mod <- modelo
  p <- prob
  
  
  ICC <- (rowSums(mod$VCV) - mod$VCV[, -1])/rowSums(mod$VCV)
  
  mean_ICC <- mean(ICC)
  median_ICC <- median(ICC)
  mode_ICC <- MCMCglmm::posterior.mode(ICC)
  int_cred_ICC <- coda::HPDinterval(ICC, prob = p)
  
  list(posterior_ICC = ICC, 
       resultados_ICC = data.frame(mean_ICC, median_ICC, 
                                   mode_ICC, int_cred_ICC))
  
}