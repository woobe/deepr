#' Extracts hidden features from multiple predictors (x) using RBM and returns the new predictors (x_new) & final RBM (model).
#'
#' @param x Multi-column predictors (numeric - matrix).
#' @param n_features Number of hidden features to be extracted. It can be single layer (e.g. 100) or multiple layers (e.g. c(400, 400, 20)). (Default: 100)
#' @param n_batchsize Size of mini batches for RBM training. (Default: 100)
#' @param n_epochs Number of iterative steps required for each RBM training. (Default: 100)
#'
#' @examples
#' ## Train a single layer of RBM with 100 hidden features
#' model_rbm <- train_rbm(x, n_features = 100)
#' x_new <- model_rbm$x_new
#'
#' ## Train three stacked layers of RBM with 400, 400 and 100 hidden features (note: takes a long time)
#' model_rbm <- train_rbm(x, n_features = c(400, 400, 100))

train_rbm <- function(x,
                      n_features = 100,
                      n_batchsize = 100,
                      n_epochs = 100) {

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Initalise
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tt <- start_timer()
  cat("\n")
  cat("=====================================================================\n")
  cat("[deepr]: Training Single/Multiple Layer(s) of RBM\n")
  cat("=====================================================================\n")

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Pre-process x_train and x_test
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("\n[deepr]: Pre-processing predictors (x) ...\n")

  ## Remove variables with near zero variance
  cat("[deepr]: Removing variables with near zero variance ...\n")
  col_nzv <- nearZeroVar(x)
  x_remain <- x[, -col_nzv]

  ## Normalise to between 0 and 1
  cat("[deepr]: Normalising x to values between 0 and 1 ...\n")
  x_pp <- preProcess(x_remain, method = c('range'))
  x_norm <- predict(x_pp, x_remain)
  rm(x_pp, x_remain)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Train one or multiple RBMs
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## Initial x0
  x0 <- x_norm

  ## Main Loop for Iterative RBM Training
  for (n_layer in 1:length(n_features)) {

    ## Display
    cat("[deepr]: Training RBM (Restricted Boltzmann Machine) Layer", n_layer,
        "of", n_features[n_layer], "Hidden Features ...\n")

    ## If there is more than one RBM layer, use the previous x1 as x0
    if (n_layer > 1) x0 <- x1

    ## Trim x_pp only if needed for the correct batchsize
    if ((dim(x0)[1] %% n_batchsize) != 0) {
      n_trim <- round(dim(x0)[1] / n_batchsize) * n_batchsize
      row_samp <- sample(1:dim(x0)[1], n_trim)
      x_trim <- x0[row_samp,]
    } else {
      x_trim <- x0
    }

    ## Training RBM with deepnet::rbm.train
    ## Note: using x_trim instead of x0
    ## Leave most settings as default
    rbm <- rbm.train(x_trim,
                     hidden = n_features[n_layer],
                     batchsize = n_batchsize,
                     numepochs = n_epochs)

    ## Converting hidden weights into normal predictors x
    ## Note: using x0 instead of x_trim
    x1 <- rbm.up(rbm, x0)

  }


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("[deepr]: Returning new predictors (x_new) and final RBM (model) ...\n")
  outputs <- list(x_new = x1, model = rbm)
  cat("[deepr]: All Done! Total Duration:", round(stop_timer(tt)), "seconds.\n")
  return(outputs)


}
