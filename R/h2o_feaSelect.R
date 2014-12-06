#' Feature Selection using H2O Random Forest
#'
#' @param x_train predictors
#' @param y_train labels
#'
#' @examples
#' ## Select Best Features
#' h2o_feaSelect(x_train, y_train)
#'
#' @export
#' @import dplyr tidyr caret ggplot2

h2o_feaSelect <- function(x_train, y_train,
                          type = "classification",
                          n_fold = 5,
                          n_repeat = 3,
                          n_tree = 100,
                          n_size = c(10:15),
                          n_seed = 1234,
                          n_threads = -1,
                          plot_graph = TRUE,
                          balance = TRUE) {

  ## Combine x_train and y_train
  df_train <- data.frame(x_train, y = y_train)

  ## Load H2O R package (import method doesn't work for now as it is not on CRAN)
  suppressMessages(library(h2o))

  ## Initiate H2O Cluster
  localH2O <- h2o.init(nthreads = n_threads)
  train_hex <- as.h2o(localH2O, df_train)

  ## ===========================================================================
  ## Calculate Variable Importance using H2O Random Forest
  ## ===========================================================================

  ## Empty Shells
  df_z <- data.frame(matrix(NA, nrow = ncol(x_train), ncol = n_repeat + 1))
  colnames(df_z) <- c("var", paste0("repeat_", 1:n_repeat))
  df_z[, 1] <- colnames(x_train)

  ## Loop (at least 3 times)
  for (nn_repeat in 1:max(c(3, n_repeat))) {

    ## Train Model
    if (type == "classification") {
      model <- h2o.randomForest(x = 1:ncol(x_train),
                                y = ncol(x_train) + 1,
                                data = train_hex,
                                classification = TRUE,
                                ntree = n_tree,
                                importance = TRUE,
                                balance.classes = balance)
    } else {
      model <- h2o.randomForest(x = 1:ncol(x_train),
                                y = ncol(x_train) + 1,
                                data = train_hex,
                                classification = FALSE,
                                ntree = n_tree,
                                importance = TRUE)
    }

    ## Store Z-Scores
    df_z[, nn_repeat + 1] <- as.numeric(model@model$varimp[3, ])

  }

  ## Median
  df_z$median <- apply(df_z[, -1], 1, median)

  ## Sort
  order_z <- order(df_z$median, decreasing = TRUE)
  df_z <- df_z[order_z, ]

  ## Display
  cat("The 5 Most Important Variables:\n")
  print(head(df_z[, c(1, ncol(df_z))]))

  ## ===========================================================================
  ## Iterative Steps
  ## ===========================================================================

  ## Set Seed
  set.seed(n_seed)

  ## Empty Shell
  df_result_all <- c()

  ## Loop
  for (nn_repeat in 1:n_repeat) {

    ## Random Split
    rand_fold <- createFolds(y_train, k = n_fold)

    ## Empty Shells
    df_result <- data.frame(matrix(NA, nrow = n_fold, ncol = length(n_size) + 1))
    colnames(df_result) <- c("fold", n_size)
    df_result$fold <- 1:n_fold

    ## Iterative
    for (nn_size in 1:length(n_size)) {

      ## Extract Variables
      var_temp <- as.character(df_z[1:n_size[nn_size], 1])

      ## CV Loop
      for (nn_fold in 1:n_fold) {

        ## Display
        cat("\n[deepr]: CV Runs ... Repeat: ", nn_repeat, "/", n_repeat, " ... ",
            "Size: ", nn_size, "/", length(n_size), " ... ",
            "Fold: ", nn_fold, "/", n_fold, " ... ",
            "No. of Variables: ", n_size[nn_size], "\n",
            sep = "")

        ## Extract rows
        row_train <- as.integer(unlist(rand_fold[-nn_fold]))
        row_valid <- as.integer(unlist(rand_fold[nn_fold]))

        ## H2O Random Forest
        if (type == "classification") {
          model <- h2o.randomForest(x = var_temp,
                                    y = ncol(x_train) + 1,
                                    data = train_hex[row_train,],
                                    classification = TRUE,
                                    ntree = n_tree,
                                    importance = FALSE,
                                    balance.classes = balance)
        } else {
          model <- h2o.randomForest(x = var_temp,
                                    y = ncol(x_train) + 1,
                                    data = train_hex[row_train,],
                                    classification = FALSE,
                                    ntree = n_tree,
                                    importance = FALSE)
        }

        ## OOB Performance
        yy_valid <- as.data.frame(h2o.predict(model, train_hex[row_valid,]))
        if (type == "classification") {
          score_temp <- confusionMatrix(yy_valid[,1], y_train[row_valid])$overall[1]
          cat("OOB Accuracy:", round(score_temp, 4), "\n")
        } else {
          ## add regression metric later
        }

        ## Update df_result
        df_result[nn_fold, nn_size + 1] <- as.numeric(score_temp)

      }

    }

    ## Store
    df_result_all <- rbind(df_result_all, df_result)

  }

  ## ===========================================================================
  ## Summarise Results
  ## ===========================================================================

  ## Reshape
  df_ggplot <- df_result_all %>% gather(fold)
  df_ggplot <- df_ggplot[, -1]

  ## Rename
  if (type == "classification") {
    colnames(df_ggplot) <- c("n_var", "accuracy")
  } else {
    colnames(df_ggplot) <- c("n_var", "RMSE")
  }

  ## ggplot2
  p <- ggplot(df_ggplot, aes(n_var, accuracy)) + geom_boxplot()
  if (plot_graph) print(p)

  ## Stats
  df_summary <- data.frame(n_var = n_size,
                           avg_score = apply(df_result_all[, -1], 2, mean),
                           med_score = apply(df_result_all[, -1], 2, median),
                           sd_score = apply(df_result_all[, -1], 2, sd))
  df_summary$avg_med <- (df_summary$avg_score + df_summary$med_score) / 2
  df_summary$avg_med_sd <- df_summary$avg_med / df_summary$sd_score

  ## Determine best set - Rule 1: Best Median Score
  best_loc <- which(df_summary$med_score == max(df_summary$med_score))

  ## Rule 2: Best of Avg + Median Score
  if (length(best_loc) > 1) {
    best_loc <- which(df_summary$avg_med == max(df_summary$avg_med))
  }

  ## Rule 3: Best of Avg + Median Score and lowest SD
  if (length(best_loc) > 1) {
    best_loc <- which(df_summary$avg_med == max(df_summary$avg_med_sd))
  }

  ## Identify best set
  best_n_var <- df_summary[best_loc, 1]
  best_var <- df_z[1:best_n_var, 1]

  ## ===========================================================================
  ## Outputs
  ## ===========================================================================

  output <- list(best_var = best_var,
                 result = df_result_all,
                 summary = df_summary,
                 zscores = df_z,
                 ggplot = p)
  return(output)

}
