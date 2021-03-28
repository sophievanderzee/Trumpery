# Copyright (c) 2020, Sophie Van Der Zee, Ronald Poppe, Alice Havrileck,
# and Aurelien Baillon
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


#' Library with helper functions, part of the code that accompanies:
#' <CITATION>


#' manova_et_al: Run MANOVA and print the summary statistics
#'
#' Applies MANOVA and produces a full table with LIWC name and abbreviation,
#' means for correct and incorrect, F stats, p-value, the Bayes factor and
#' Cohen's d including the confidence interval. When verbosity is on, it also
#' stores the MANOVA table as htm file
#'
#' @param file_name    htm file name where table is saved
#' @param dataset      full dataset of tweets
#' @param manova_model the manova model
#' @param liwc_names   LIWC variable names with description
#' @param verbose      controls the printing and saving of results (default: TRUE)
#'
#' @return             lists of variables that are sig at 5% and at the 1% and
#'                     the variable with highest (absolute) Cohen's d

manova_et_al <- function(file_name, dataset, manova_model, liwc_names, verbose = TRUE) {
  # run manova and print the summary statistics
  manova_result <- manova(manova_model)
  manova_result_sum <- summary.aov(manova_result)
  if (verbose) {
    print(summary(manova_result))
    print(etasq(manova_result))
  }
  
  # initialize additional statistics variables
  p_value_list <- c()
  F_value_list <- c()
  var_name_list <- c()
  mean_correct_list <- c()
  mean_incorrect_list <- c()
  bf_list <- c()
  cohen_d_list <- c()
  ci_list <- c()
  high_d = 0
  high_d_var = ""
  
  
  # run pairwise comparisons and compute other statistics such as Cohen's d and the Bayes factor of t-tests
  for (i in 1:length(manova_result_sum)) {
    # p-values
    pvalue <- unlist(manova_result_sum[i])[9]
    p_value_list <- c(p_value_list, pvalue)
    
    # variable name
    var_name <- substr(names(pvalue), start = 11, stop = nchar(names(pvalue)) - 8) 
    var_name_list <- c(var_name_list, var_name)

    # F-values
    F_value_list <- c(F_value_list, format(round(unlist(manova_result_sum[i])[7], digits = 2), nsmall = 2))
    
    # mean values for correct and incorrect tweets
    mean_correct_list <- c(mean_correct_list, format(round(mean(dataset[dataset$Veracity == 0, var_name]), digits = 2), nsmall = 2))
    mean_incorrect_list <- c(mean_incorrect_list, format(round(mean(dataset[dataset$Veracity == 1, var_name]), digits = 2), nsmall = 2))
    
    # run a Bayes t-test, replace by >1000 if very high
    bf = 0
    if(mean(dataset[, var_name])>0)
    { bf <- round(extractBF(ttestBF(dataset[dataset$Veracity == 0, var_name], dataset[dataset$Veracity == 1, var_name]))$bf, 2)
      if (verbose) {
        if(bf > 1000){
          bf <- c(">1,000")
        }
      }
    }
    bf_list <- c(bf_list, format(bf, nsmall = 3))
    
    # Cohen's d and its confidence interval
    cohen_d <- cohen.d(dataset[dataset$Veracity == 0, var_name], dataset[dataset$Veracity == 1, var_name])
    cohen_d_list <- c(cohen_d_list, format(round(cohen_d$estimate, 2), nsmall = 2))
    ci <- format(round(cohen_d$conf.int, 2), 2)
    ci_list <- c(ci_list, paste("[", ci[1], ",", ci[2], "]", sep = ""))
    
    # check whether the variable has the highest absolute Cohen's d
    if (!is.na(cohen_d$estimate)) {
      if(abs(cohen_d$estimate) > high_d) {
        high_d <- abs(cohen_d$estimate)
        high_d_var <- var_name
      }
    }
  }
  
  # adjust p-values using the FDR correction
  p_value_list <- format(round(p.adjust(p_value_list, method = "fdr"), 3), nsmall = 3)
  
  # determine level of significance and store variables significant at the 5% and 1%
  starlevel_list<-c()
  sig05_var <- c()
  sig01_var <- c()
  for(i in 1:length(manova_result_sum)) {
    starlevel <- ""
    if (!is.na(p_value_list[i])) {
      # variables significant at 5%
      if (p_value_list[i] < 0.05) {
        starlevel <- "*"
        # add to list of variables significant at 5%
        sig05_var <- paste(sig05_var, var_name_list[i], sep = "+")
      }
      # variables significant at 1%
      if (p_value_list[i] < 0.01) {
        starlevel <- "**"
        # add to list of variables significant at 1%
        sig01_var <- paste(sig01_var, var_name_list[i], sep = "+")
      }
      # variables significant at 0.1%
      if (p_value_list[i] < 0.001) {
        starlevel <- "***"
      }
      starlevel_list <- c(starlevel_list,starlevel)
    }
  }
  
  # display table with variable description
  full_table <- as.data.frame(cbind(var_name_list, mean_correct_list, mean_incorrect_list, F_value_list, p_value_list, starlevel_list, bf_list,
    cohen_d_list, ci_list))
  names(full_table) <- c("LIWC_name", "Mean_if_correct", "Mean_if_incorrect", "F_stat.", "p-value", "Sig.", "Bayes Factor", "Cohen's d", "CI")
  full_table <- merge(liwc_names, full_table, by = "LIWC_name")
  
  # save table to htm file
  if(verbose) {
    print(xtable(full_table), type = "html", file_name)
  }
  
  # return lists of variables significant at the 5% and 1%, and the variable with highest absolute Cohen's d
  return(c(sig05_var, sig01_var, high_d_var))
}



#' model_selection_table: Run variable selection models
#'
#' Runs variable selection models forward, backward, using only the variables
#' significant at 5% and 1% and LASSO regression. When verbosity is on, also
#' stores the model selection table in html format
#'
#' @param file_name    htm file name where table is saved
#' @param dataset      full dataset of tweets
#' @param models       models with variables significant at 5% and 1% and the
#'                     variable with highest absolute Cohen's d
#' @param verbose      controls the printing and saving of results (default: TRUE)
#'
#' @return             logit models (only forward model when verbose == FALSE)

model_selection_table <- function(file_name, dataset, models, verbose = TRUE) {
  # create models with only significant variables and variable with highest cohen's d 
  if (length(models) == 3) {
    model_with_sig05_var <- as.formula(paste("Veracity~", substring(models[1], 2)))
    model_with_sig01_var <- as.formula(paste("Veracity~", substring(models[2], 2)))
    model_baseline <- as.formula(paste("Veracity~", models[3]))
  } else{
    # in some simulations, there may be no significant variables
    if (length(models) == 2) {
      model_with_sig05_var <- as.formula(paste("Veracity~", substring(models[1], 2)))
      model_with_sig01_var <- as.formula(paste("Veracity~", models[2]))
      model_baseline <- as.formula(paste("Veracity~", models[2]))
    } else {
      model_with_sig05_var <- as.formula(paste("Veracity~", models[1]))
      model_with_sig01_var <- as.formula(paste("Veracity~", models[1]))
      model_baseline <- as.formula(paste("Veracity~", models[1]))
    }
  }

  # estimate and compare five logit models
  # 1. a model that select variables forwards, starting from the variable with the highest cohen's d
  logit_forward <- step(glm(model_baseline, data = dataset, family = "binomial"), scope = model_with_sig05_var, direction = "forward")

  # run only other models when verbosity is off
  if(verbose) {
    # 2. a model that select variables backwards, starting from the variables significant at 5%
    logit_backward <- step(glm(model_with_sig05_var, data = dataset, family = "binomial"), direction = "backward")
    
    # 3. a model with the variables that are significant at 5%
    logit_sig05 <- glm(model_with_sig05_var, data = dataset, family = "binomial")
    
    # 4. a model with the variables that are significant at 1%
    logit_sig01 <- glm(model_with_sig01_var, data = dataset, family = "binomial")
    
    # 5. a LASSO regression, starting from the variables significant at 5%
    MSEs <- NULL
    # model selection runs 1,000 times to account for differences in initalization
    for (i in 1:100) {
      lasso <- cv.glmnet(model.matrix(model_with_sig05_var, dataset), dataset$Veracity, family = "binomial", type.measure = "mse")
      MSEs <- cbind(MSEs, lasso$cvm)
    }
    rownames(MSEs) <- lasso$lambda
    
    # select the value of lambda that gives minimum mean cross-validated error
    lambda.min <- as.numeric(names(which.min(rowMeans(MSEs))))
    
    # store the selected variables 
    lasso_coeffs <- coef(lasso, s = lambda.min)
    lasso_coeffs_name <- lasso_coeffs@Dimnames[[1]][lasso_coeffs@i + 1]
    lasso_selection <- c()
    for(i in 2:length(lasso_coeffs_name)) {
      lasso_selection <- paste(lasso_selection, lasso_coeffs_name[i], sep = "+")
    }
    
    # we re-estimate the model that the LASSO regression selected
    model_lasso <- as.formula(paste("Veracity~", substring(lasso_selection,2)))
    logit_lasso <- glm(model_lasso, data = dataset, family = "binomial")

    # create a table with the models, number of variables, log-likelihood, AIC and AUC
    model_numbcoef <- c(length(logit_forward$coefficients)-1, length(logit_backward$coefficients) - 1, length(logit_sig05$coefficients) - 1, 
      length(logit_sig01$coefficients) - 1, length(logit_lasso$coefficients) - 1)
    model_loglik <- c(logLik(logit_forward), logLik(logit_backward), logLik(logit_sig05), logLik(logit_sig01), logLik(logit_lasso))
    model_aic <- c(logit_forward$aic, logit_backward$aic, logit_sig05$aic, logit_sig01$aic, logit_lasso$aic)
    model_auc <- c(roc(dataset$Veracity, predict.glm(logit_forward, dataset, type="response"), levels = c(0, 1), direction = "<")$auc,
      roc(dataset$Veracity, predict.glm(logit_backward, dataset, type = "response"), levels = c(0, 1), direction = "<")$auc,
      roc(dataset$Veracity, predict.glm(logit_sig05, dataset, type = "response"), levels = c(0, 1), direction = "<")$auc,
      roc(dataset$Veracity, predict.glm(logit_sig01, dataset, type = "response"), levels = c(0, 1), direction = "<")$auc,
      roc(dataset$Veracity, predict.glm(logit_lasso, dataset, type = "response"), levels = c(0, 1), direction = "<")$auc)
    table_model_selection <- rbind(c("Model", "Forward", "Backward", "Sig. 5%", "Sig. 1%", "LASSO"),
      c("Number of variables", model_numbcoef),
      c("Log-likelihood", format(round(model_loglik, digits = 2), nsmall = 2)),
      c("AIC", format(round(model_aic, digits = 2), nsmall = 2)),
      c("AUC", format(round(model_auc, digits = 3), nsmall = 3)))
    tablefile<-file(file_name)
      writeLines(stargazer(table_model_selection, align = TRUE, type = "html", style = "ajs"), tablefile)
    close(tablefile)

    # return five logit models
    return(list(logit_forward,logit_backward,logit_sig05,logit_sig01,logit_lasso))
  } else {
    # return only forward logit model
    return(logit_forward)
  }
}



#' margins_table: Creates a table with marginal effects
#'
#' Runs a logistic regression, creates a table with marginal
#' effects for a logit model and saves the table in html format
#'
#' @param file_name    htm file name where table is saved
#' @param dataset      full dataset of tweets
#' @param sel_model    selected logit model

margins_table <- function(file_name, dataset, sel_model) {
  # create table with 
  sel_model <- Reduce(paste, deparse(sel_model$formula))
  sel_model_margins <- logitmfx(sel_model, dataset)

  # save table as html file
  tablefile <- file(file_name)
    writeLines(stargazer(sel_model_margins$mfxest, align = TRUE, type = "html", style = "ajs"), tablefile)
  close(tablefile)
}



#' classify: calculates true/false pos/neg
#'
#' Classifies a prediction given a cut-off value and
#' returns true/false positives/negatives
#'
#' @param pred      predicted value
#' @param verif     actual label
#' @param cutoff    cut-off score for classification
#'
#' @return          true/false positives/negatives

classify <- function(pred, verif, cutoff)
{
  # make classification
  predic <- (sign(pred - cutoff) + 1) / 2
  trueNeg <- ifelse(predic == 0 & verif == 0, 1, 0)
  truePos <- ifelse(predic == 1 & verif == 1, 1, 0)
  falseNeg <- ifelse(predic == 0 & verif == 1, 1, 0)
  falsePos <- ifelse(predic == 1 & verif == 0, 1, 0)
  colSums(cbind(trueNeg, truePos, falseNeg, falsePos))
}



#' all_pred_stat: compute performance measures
#'
#' Calculates hit rate, false alarm rate and accuracy
#'
#' @param pred      predicted value
#' @param verif     actual label
#' @param cutoff    cut-off score for classification
#'
#' @return          accuracy, hit rate, false alarm rate, precision, and F1

all_pred_stat <- function(pred,verif,cutoff)
{
  # classify prediction
  c_res <- classify(pred, verif, cutoff)
  
  # calculate accuracy, hit_rate
  acc <- (c_res[1] + c_res[2]) / (c_res[1] + c_res[2] + c_res[3] + c_res[4])
  hit_rate <- (c_res[2]) / (c_res[2] + c_res[3])
  false_alarm_rate <- (c_res[4]) / (c_res[4] + c_res[1])
  precision <- (c_res[2]) / (c_res[2] + c_res[4])
  f_1 <- 2 * (hit_rate * precision) / (precision + hit_rate)
  
  cbind(cutoff, acc, hit_rate, false_alarm_rate, precision, f_1)
}



#' accuracy: calculate accuracy and produce ROC curve
#'
#' Calculates hit rate, false alarm rate and accuracy
#'
#' @param file_name     png file name where plot is saved
#' @param traindataset  train dataset
#' @param testdataset   test dataset
#' @param sel_model     logit model
#' @param verbose       controls the printing and saving of results (default: TRUE)
#'
#' @return             accuracy and AUC for train and test set

accuracy <- function(traindataset, testdataset, sel_model, verbose = FALSE, file_name = NULL, picture_name = NULL)
{
  # use model to predict on train dataset and on test dataset
  train_predictions <- predict.glm(sel_model, traindataset, type = "response")
  test_predictions <- predict.glm(sel_model, testdataset, type = "response")
  
  # compute accuracy and hit rates for 50% and prior
  acc_train <- all_pred_stat(train_predictions, traindataset$Veracity, mean(traindataset$Veracity))
  acc_test <- all_pred_stat(test_predictions, testdataset$Veracity, mean(traindataset$Veracity))
  acc_train5 <- all_pred_stat(train_predictions, traindataset$Veracity, 0.5)
  acc_test5 <- all_pred_stat(test_predictions, testdataset$Veracity, 0.5)
  auc_train <- roc(traindataset$Veracity, train_predictions, levels = c(0, 1), direction = "<")$auc
  auc_test <- roc(testdataset$Veracity, test_predictions, levels = c(0, 1), direction = "<")$auc
  
  if (verbose) {
    # compute CI for Table 3
    acc_ci_train5 <- ci.coords(roc(traindataset$Veracity, train_predictions, levels = c(0, 1), direction = "<"), x = 1 - acc_train5[4],
	  input = "specificity", ret = ("accuracy"))
    acc_ci_train <- ci.coords(roc(traindataset$Veracity, train_predictions, levels = c(0, 1), direction = "<"), x = 1 - acc_train[4],
	  input = "specificity", ret = ("accuracy"))
    acc_ci_test5 <- ci.coords(roc(testdataset$Veracity, test_predictions, levels = c(0, 1), direction = "<"), x = 1 - acc_test5[4],
	  input = "specificity", ret = ("accuracy"))
    acc_ci_test <- ci.coords(roc(testdataset$Veracity, test_predictions, levels = c(0, 1), direction = "<"), x = 1 - acc_test[4],
	  input = "specificity", ret = ("accuracy"))
    auc_ci_train <- ci.auc(traindataset$Veracity, train_predictions, levels = c(0, 1), direction = "<")
    auc_ci_test <- ci.auc(testdataset$Veracity, test_predictions, levels = c(0, 1), direction = "<")  
    
	# make Table 3
    perf_row1 <- c(1, round(acc_train5[1:2], 4), paste("[", round(acc_ci_train5[1], 4), ",", round(acc_ci_train5[3], 4), "]", sep = ""),
	  round(acc_train5[3:6], 4), round(auc_train[1], 3), paste("[", round(auc_ci_train[1], 3), ",", round(auc_ci_train[3], 3), "]", sep = ""))
    perf_row2 <- c(1, round(acc_train[1:2], 4), paste("[", round(acc_ci_train[1], 4), ",", round(acc_ci_train[3], 4), "]", sep = ""),
	  round(acc_train[3:6], 4), round(auc_train[1], 3), paste("[", round(auc_ci_train[1], 3), ",", round(auc_ci_train[3], 3), "]", sep = ""))
    perf_row3 <- c(2, round(acc_test5[1:2], 4), paste("[", round(acc_ci_test5[1], 4), ",", round(acc_ci_test5[3], 4), "]", sep = ""),
	  round(acc_test5[3:6], 4), round(auc_test[1], 3), paste("[", round(auc_ci_test[1], 3), ",", round(auc_ci_test[3], 3), "]", sep = ""))
    perf_row4 <- c(2, round(acc_test[1:2], 4), paste("[", round(acc_ci_test[1], 4), ",", round(acc_ci_test[3], 4), "]", sep = ""),
	  round(acc_test[3:6], 4), round(auc_test[1], 3), paste("[", round(auc_ci_test[1], 3), ",", round(auc_ci_test[3], 3), "]", sep = ""))
    perf_table <- data.frame(rbind(perf_row1, perf_row2, perf_row3, perf_row4))
    colnames(perf_table) <- c("Dataset", "Cut-off", "Accuracy", "Acc. CI", "Hit rate", "False alarm rate", "Precision", "F1", "AUC", "AUC CI")
    write.xlsx(perf_table, file = file_name, sheetName = "performance_table", append = FALSE)
    
    # ROC curve for Figure 2
    pdf(picture_name)
    plot.roc(traindataset$Veracity, train_predictions, levels = c(0, 1), direction = "<", col = c("black"), xlab = "False Alarm Rate",
	  ylab = "Hit Rate", cex.lab = 10/9,  axes = FALSE)
    plot.roc(testdataset$Veracity, test_predictions, levels = c(0, 1), direction = "<", col = c("darkgrey"), add = TRUE)
    axis(1, las=1, at = c(0,0.2,0.4,0.6,0.8,1), labels = c(1,0.8,0.6,0.4,0.2,0))
    axis(2, las=2, pos = 1)
    
    # when giving the coordinates of points, we need to indicate the sensitivity for the x-axis instead of the false alarm rate
    points(1 - acc_train[4], acc_train[3], pch = 24, bg = c("white"), cex = 10/9)
    points(1 - acc_test[4], acc_test[3], pch = 24, bg = c("white"), cex = 10/9)
    points(1 - mean(traindataset$Veracity), mean(traindataset$Veracity), pch = 19, cex = 10/9)
    
    # add labels
    text(0.85, 0.84, labels = paste("(", round(100 * acc_train[4], digits = 0), "%,", round(100 * acc_train[3], digits = 0), "%)", sep = ""))
    text(0.85, 0.80, labels = paste("Acc.1 = ", round(100 * acc_train[2], digits = 0), "%", sep = ""))
    text(0.65, 0.68, labels = paste("(", round(100 * acc_test[4], digits = 0), "%,", round(100 * acc_test[3], digits = 0), "%)", sep = ""))
    text(0.65, 0.64, labels = paste("Acc.2 = ", round(100 * acc_test[2], digits = 0), "%", sep = ""))
    text(0.6, 0.28, labels = paste("(", round(100 * (mean(traindataset$Veracity)), digits = 0), "%,", round(100 * mean(traindataset$Veracity), digits = 0), "%)", sep = ""))
    text(0.6, 0.24, labels = paste("Acc.1 = ", round(100 * ((1 - mean(traindataset$Veracity)) * (1 - mean(traindataset$Veracity)) + mean(traindataset$Veracity) *
      mean(traindataset$Veracity)), digits = 0), "%", sep = ""))
    text(0.6, 0.20, labels = paste("Acc.2 = ", round(100 * ((1 - mean(testdataset$Veracity)) * (1 - mean(traindataset$Veracity)) + mean(testdataset$Veracity) *
      mean(traindataset$Veracity)), digits = 0), "%", sep = ""))
    dev.off()
  } else {
    # return accuracy rates and AUC for train and test sets
    return(cbind(acc_train, auc_train, acc_test, auc_test))
  }
}