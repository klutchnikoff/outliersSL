#' Title
#'
#' @param cl_true Vector of labels
#' @param cl_estim Vector of labels
#'
#' @return
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter select
#'
check_clustering <- function(cl_true, cl_estim){

  create_list_clusters <- function(cl){
    cl <- cl %>% as.vector() %>% sort() %>% as.factor()
    CL <- list()
    K <- length(levels(cl))
    df <- data.frame(id = 1:length(cl), cl = cl)
    for (k in 1:K){
      level <- levels(df$cl)[k]
      if (level != "0") {
        CL[[k]] <- df %>%
          filter(cl == level) %>%
          select(id) %>%
          unlist() %>%
          as.numeric()
      }
    }
    CL
  }

  #######################
  # Création des clusters
  #######################

  CL_true <- create_list_clusters(cl_true)
  CL_estim <- create_list_clusters(cl_estim)

  ###############
  # Vérifications
  ###############

  cpt <- 0
  taille <- 0
  for(k in 1:length(CL_true)){
    if(!is.null(CL_true[[k]])){
      for(l in 1:length(CL_estim)){
        if(!is.null(CL_estim[[l]])){
          if(all(CL_true[[k]] %in% CL_estim[[l]])){ cpt <- cpt + 1; break}
        }
      }
    }
  }

  ###################################
  # Nombre de clusters (non outliers)
  ###################################

  taille <- 0
  for(k in 1:length(CL_true)){
    if(!is.null(CL_true[[k]])){taille <- taille + 1}
  }
  (cpt == taille)
}
