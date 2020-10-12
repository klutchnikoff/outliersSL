#' Title
#'
#' @param n todo
#' @param M todo
#' @param ratio_card_max todo
#' @param epsilon todo
#' @param trunc_eps todo
#'
#' @return
#' @export
#'
simulate_blocs <- function(n,
                             M = 2,
                             ratio_card_max = 2,
                             epsilon = 0,
                             trunc_eps = TRUE){

  #########################
  # tirage unif dans disque
  #########################

  rdisk <- function(n, center, rmax){
    R <- sqrt(runif(n, 0, rmax**2))
    Th <- runif(n, 0, 2*pi)
    pp <- center$x+center$y*1i + R*exp(1i * Th)
    data.frame(x = Re(pp), y = Im(pp))
  }

  #################
  # initialisations
  #################

  gamma <- rep(1, M) + runif(M, 0, ratio_card_max - 1)
  gamma <- gamma / sum(gamma)
  gammabar <- min(gamma) - max(gamma)/2
  eps_max <- gammabar/(2*gammabar+1)
  if (trunc_eps & epsilon>eps_max){
    epsilon <- eps_max
  }

  ##############################
  # tirage des rayons
  # et des cetres en respectant
  # les contraintes de l'article
  ##############################

  radius <-  runif(M, min = sqrt(1/(16*M*pi)), max = sqrt(1/(4*M*pi)))
  test <- TRUE
  while(test){
    centers <- data.frame(x = runif(M, min = 0, max = 1),
                          y = runif(M, min = 0, max = 1))
    # ici on ajoute un petit espacement
    espacement <- 0.5
    test1 <- (2+espacement)*max(radius) < min(dist(centers))
    test2 <- (0 < centers$x - radius) & (centers$x + radius < 1)
    test3 <- (0 < centers$y - radius) & (centers$y + radius < 1)
    test <- !(all(test1) & all(test2) & all(test3))
  }

  ###################
  # Tirage des points
  ###################

  N <- rmultinom(1, n, c((1-epsilon)*gamma, epsilon))
  points <- data.frame()
  for(m in 1:M){
    pp <- rdisk(N[m], centers[m,], radius[m])
    pp$cluster <- m
    points <- rbind(points, pp)
  }

  #####################
  # Tirage des outliers
  #####################

  if(N[M+1]>0){
    for(n in 1:N[M+1]){
      test <- TRUE
      while(test){
        outlier_x <- runif(1, 0, 1)
        outlier_y <- runif(1, 0, 1)
        test <- any((centers$x-outlier_x)**2 + (centers$y-outlier_y)**2 < radius**2)
      }
      pp <- data.frame(x = outlier_x,
                       y = outlier_y,
                       cluster = 0)
      points <- rbind(pp, points)
    }
  }

  ########
  # Return
  ########

  points$cluster <- as.factor(points$cluster)
  points
}
