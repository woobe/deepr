#' Stop the timer and return the time difference in seconds.
#'
#' @examples
#' tt <- start_timer()
#' stop_timer(tt)
#'
#' @export

stop_timer <- function(time_start) {
  time_diff <- proc.time() - time_start
  time_diff <- time_diff[1] + time_diff[2] + time_diff[3]
  return(round(as.numeric(time_diff), digits = 2))
}
