#' Upgrade H2O Package and JVM to the latest or a specific version
#'
#' @param model_rbm A trained RBM object from train_rbm(...).
#'
#' @examples
#' ## Upgrade it to the latest version
#' upgrade_h2o()
#'
#' ## Upgrade it to a specific version
#' upgrade_h2o(h2o_ver = '1442')
#'
#' @export
#' @import RCurl stringr


upgrade_h2o <- function(h2o_ver = NULL) {

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Determine the latest version of H2O (if h2o_ver is not specified)
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (is.null(h2o_ver)) {
    if(url.exists("http://s3.amazonaws.com/h2o-release/h2o/master/latest")) {
      h <- basicTextGatherer()
      curlPerform(url = "http://s3.amazonaws.com/h2o-release/h2o/master/latest", writefunction = h$update)
      h2o_ver <- h$value()
      h2o_ver = gsub("\n","", h2o_ver)
    }
  }
  cat("[deepr]: Trying to install H2O version", h2o_ver, "...\n")

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check whether the installed version is the same as the latest/specified
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if ("h2o" %in% rownames(installed.packages())) {

    ## Check version number
    cat("[deepr]: Checking current H2O version ...\n")
    h2o_ver_installed <- as.character(packageVersion("h2o"))
    h2o_ver_installed <- substr(h2o_ver_installed,
                                nchar(h2o_ver_installed)-4+1,
                                nchar(h2o_ver_installed))
    chk_h2o_ver <- h2o_ver == h2o_ver_installed

  } else {

    ## If there is no previously installed H2O ...
    chk_h2o_ver <- FALSE
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## If the versions do not match / there is no previously installed H2O
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (chk_h2o_ver) {

    ## Display message if the system has the current version
    cat("[deepr]: You have H2O version", h2o_ver, "already.\n")

  } else {

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Set the CRAN mirror
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    local({r <- getOption("repos"); r["CRAN"] <- "http://cran.us.r-project.org"; options(repos = r)})

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Remove any previously installed H2O packages for R
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    cat("[deepr]: Removing previously installed H2O package ...\n")
    if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
    if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }


    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Install H2O R Package
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    cat("[deepr]: Installing H2O version", h2o_ver, "...\n")
    txt_repo <- (c(paste0(paste0("http://s3.amazonaws.com/h2o-release/h2o/master/",
                                 h2o_ver),"/R"),
                   getOption("repos")))
    suppressMessages(
      install.packages("h2o", repos = txt_repo, quiet = FALSE)
    )

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Initialise a H2O Cluster and force it to download a matching h2o.jar
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ## Load H2O
    cat("[deepr]: Loading H2O version", h2o_ver, "...\n")
    suppressMessages(library(h2o))

    ## Initialise a H2O Cluster and force it to download
    cat("[deepr]: Initialising a local cluster ...\n")
    localH2O <- h2o.init(forceDL = TRUE)

    ## Shut down the cluster
    cat("[deepr]: Shutting down the local cluster ...\n")
    h2o.shutdown(localH2O, prompt = FALSE)

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Finish
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    cat("[deepr]: All done! Your H2O package has been upgraded.\n\n")

  }

}
