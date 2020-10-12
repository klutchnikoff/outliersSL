#' Robust version of hclust
#'
#' @param df a data.frame
#' @param M number
#' @param Mmax Number
#' @param M_init Number
#' @param method Method to pass to fastcluster::hclust.vector
#'
#' @return
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom fastcluster hclust hclust.vector
#' @importFrom stats cutree
robust_hclust <- function(df,
                          M = 2,
                          Mmax = NULL,
                          method="single",
                          M_init = min(floor(nrow(df)/2), 200)){
  # ==============
  # initialization
  # ==============

  METHODS <- c("single", "complete", "average", "mcquitty",
               "ward.D", "centroid", "median", "ward.D2")
  METHODS_VECT <- c("single", "ward", "centroid", "median")
  if (method == "ward") {
    message("The \"ward\" method has been renamed to \"ward.D\"; note new \"ward.D2\"")
    method <- "ward.D"
  }
  methodidx <- pmatch(method, METHODS)
  if (is.na(methodidx))
    stop(paste("Invalid clustering method '", method, "' for vector data.",
               sep = ""))
  if (methodidx == -1)
    stop("Ambiguous clustering method.")
  method <-  METHODS[methodidx]
  if(is.null(Mmax)){
    Mmin <- M
    Mmax <- M
  } else {
    Mmin <- min(M, Mmax)
  }

  # =========
  # fastclust
  # =========

  if (method %in% METHODS_VECT){
    hc <- fastcluster::hclust.vector(df, method=method)
  } else {
    hc <- fastcluster::hclust(dist(df), method=method)
  }

  ct <- cutree(hc, k = Mmin:M_init)

  # =========
  # robustify
  # =========

  for(M in Mmin:Mmax){
    cardinal <- apply(ct[, (M-Mmin+1):(M_init-Mmin+1)],
                      MARGIN = 2,
                      FUN = function(x){sort(table(x), decreasing = TRUE)[M]})
    cardinal <- rev(cardinal)
    nbcut_M = M_init - max(which(cardinal == max(cardinal))) + 1
    clusters <- ct[, nbcut_M - 1]
    if(nbcut_M > M){
      outliers <- names(sort(table(clusters), decreasing = TRUE)[(M+1):nbcut_M])
      clusters[clusters %in% outliers] <- 0
    }
    clusters <- as.factor(clusters)
    if (0 %in% levels(clusters)){
      levels(clusters) <- 0:(length(levels(clusters))-1)
    } else {
      levels(clusters) <- 1:length(levels(clusters))
    }

    df[[paste0("M", M)]] <- as.factor(clusters)
  }

  # ======
  # return
  # ======

  df
}

