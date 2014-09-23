#' Activate multiple CPU cores for parallel computing.
#'
#' @param n_core Number of CPU cores to be activated for parallel computing.
#' @param verbose Display messages (TRUE or FALSE)
#'
#' @examples
#' activate_core(4)
#'
#' @export
#' @import foreach
#' @import parallel
#' @import doParallel

activate_core <- function(n_core = 4, verbose = TRUE) {

  if (verbose) tt <- start_timer()

  if (verbose) cat("[deepr]: Activaing parallel processing ... ")

  cl <- makeCluster(n_core)
  registerDoParallel(cl)

  if (verbose) cat(n_core, "cores have been successfully activated in", stop_timer(tt), "seconds.\n")

}
