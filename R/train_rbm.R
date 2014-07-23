#' Extracts hidden features from multiple predictors (x) using RBM and returns the final RBM model.
#'
#' @param x Multi-column predictors (numeric - matrix).
#' @param n_features Number of hidden features to be extracted. (Default: 100)
#' @param n_batchsize Size of mini batches for RBM training. (Default: 100)
#' @param n_epochs Number of iterative steps required for each RBM training. (Default: 100)
#' @param rmv_nearZeroVar Should the variables of near zero variance to be removed? (Default: TRUE)
#'
#' @return rbm The main RBM object with the weights of hidden features and related stats.
#' @return nzv The columns that have near zero variance.
#' @return pp The caret object that contains the pre-processing stats.
#'
#' @examples
#' ## Train a single layer of RBM with 100 hidden features
#' model_rbm <- train_rbm(x, n_features = 100)
#'
#' ## Transform the original predictors (x) into new predictors (x_new)
#' x_new <- transform_x(model_rbm, x)
#'
#'@export
#'@import deepnet
#'@import caret

train_rbm <- function(x,
                      n_features = 100,
                      n_batchsize = 100,
                      n_epochs = 100,
                      rmv_nearZeroVar = TRUE) {

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Initalise
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tt <- start_timer()
  cat("\n")
  cat("=====================================================================\n")
  cat("[deepr]: Training a Restricted Boltzmann Machine\n")
  cat("=====================================================================\n")

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Pre-process x_train and x_test
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## Make sure x is a matrix
  if (!is.matrix(x)) x <- as.matrix(x)

  ## Remove variables with near zero variance
  if (rmv_nearZeroVar) {
    cat("[deepr]: Removing variables with near zero variance ...\n")
    col_nzv <- nearZeroVar(x)
    if (length(col_nzv) >= 1) x <- x[, -col_nzv]
  } else {
    col_nzv <- NULL
  }

  ## Normalise to between 0 and 1
  cat("[deepr]: Normalising x to values between 0 and 1 ...\n")
  x_pp <- preProcess(x, method = c('range'))
  x_norm <- predict(x_pp, x)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Train one or multiple RBMs
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## Display
  cat("[deepr]: Training a RBM Layer of",
      n_features, "Hidden Features ...\n")

  ## Trim x_pp only if needed for the correct batchsize
  if ((dim(x_norm)[1] %% n_batchsize) != 0) {
    n_trim <- round(dim(x_norm)[1] / n_batchsize) * n_batchsize
    row_samp <- sample(1:dim(x_norm)[1], n_trim)
    x_trim <- x_norm[row_samp,]
  } else {
    x_trim <- x_norm
  }

  ## Training RBM with deepnet::rbm.train
  ## Note: using x_trim instead of x0
  ## Leave most settings as default
  model_rbm <- rbm.train(x_trim,
                         hidden = n_features,
                         batchsize = n_batchsize,
                         numepochs = n_epochs)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("[deepr]: Returning the RBM Model ...\n")
  cat("[deepr]: All Done! Total Duration:", round(stop_timer(tt)), "sec.\n")
  cat("=====================================================================\n\n")

  ## Create a list
  output <- list(rbm = model_rbm,
                 nzv = col_nzv,
                 pp = x_pp)
  return(output)

}
