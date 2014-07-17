#' Transforms original predictors (x) into new predictors (x_new) using a trained RBM model object.
#'
#' @param model_rbm A trained RBM object from train_rbm(...).
#' @param x Original predictors.
#'
#' @examples
#' ## Extracts hidden features and create new predictors.
#' model_rbm <- train_rbm(x, n_features = 10)
#' x_new <- transform_x(model_rbm, x)

transform_x <- function(model_rbm, x) {

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Initalise
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tt <- start_timer()
  cat("\n")
  cat("=====================================================================\n")
  cat("[deepr]: Transforming Predictors Using a Trained RBM\n")
  cat("=====================================================================\n")

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Pre-process x_train and x_test
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("[deepr]: Pre-processing predictors (x) based on the RBM object ...\n")

  ## Make sure x is a matrix
  if (!is.matrix(x)) x <- as.matrix(x)

  ## Remove variables with near zero variance if needed
  if (length(model_rbm$nzv) > 0) {
    x <- x[, -model_rbm$nzv]
  }

  ## Normalise to between 0 and 1
  x_norm <- as.matrix(predict(model_rbm$pp, x))

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Converting x_norm into x_new
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("[deepr]: Converting original predictors (x) into new ones (x_new) ...\n")
  x_new <- deepnet::rbm.up(model_rbm$rbm, x_norm)

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("[deepr]: Returning new predictors (x_new) ...\n")
  cat("[deepr]: All Done! Total Duration:", round(stop_timer(tt)), "sec.\n")
  cat("=====================================================================\n\n")
  return(x_new)

}
