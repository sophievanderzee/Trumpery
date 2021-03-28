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


#' Main and supplementary analyses, part of the code that accompanies:
#' <CITATION>


# required libraries, uncomment to install
#install.packages("rJava")
#install.packages("xlsx")
#install.packages("heplots")
#install.packages("BayesFactor")
#install.packages("xtable")
#install.packages("glmnet")
#install.packages("pROC")
#install.packages("stargazer")
#install.packages("effsize")
#install.packages("mfx")
library(xlsx)
library(heplots)
library(BayesFactor)
library(xtable)
library(glmnet)
library(pROC)
library(stargazer)
library(effsize)
library(mfx)

# clean the workspace
rm(list = ls())

# set file paths
setwd(".")
file_name_liwc_variables = "trumpery_data_liwc_variables.xlsx" 
file_name_coded_tweets = "trumpery_data_coded_tweets.xlsx"
file_name_deception_models = "trumpery_data_deception_models.xlsx"

# set name for current study
study_name = "results_trumpery"

# load additional functions. The file should be in the working directory.
source("trumpery_function_library.R")

# import dataset and select non-empty lines
complete_dataset <- read.xlsx(file_name_coded_tweets, 1)
complete_dataset <- subset(complete_dataset,Dataset > 0)
var_names <- as.data.frame(read.xlsx(file_name_liwc_variables, 1))

# define train set and test set
trainset = subset(complete_dataset, Dataset == 1)
testset = subset(complete_dataset, Dataset == 2)


# -----------------------------------------------------------------------------
# Study 1: run MANOVA and display a table with pairwise comparisons 
# -----------------------------------------------------------------------------

# returns lists of variables that are sig at 5% and 1% and the variable with highest (absolute) Cohen's d
manova_model <- with(trainset, cbind(WC,Analytic,Clout,Authentic,Tone,WPS,Sixltr,Dic,function.,pronoun,ppron,i,we,you,shehe,they,ipron,article,prep,auxverb,adverb,conj,negate,verb,adj,compare,interrog,number,quant,affect,posemo,negemo,anx,anger,sad,social,family,friend,female,male,cogproc,insight,cause,discrep,tentat,certain,differ,percept,see,hear,feel,bio,body,health,sexual,ingest,drives,affiliation,achieve,power,reward,risk,focuspast,focuspresent,focusfuture,relativ,motion,space,time,work,leisure,home,money,relig,death,informal,swear,netspeak,assent,nonflu,filler,AllPunc,Period,Comma,Colon,QMark,Exclam,Dash,Quote,Apostro,Parenth,OtherP,Hashtag,At,inhib,incl,excl,humans,Senses,Optim,Self,Other,Metaph)~Veracity)
# produces paper Table 1, supplementary material Table S1
basic_models <- manova_et_al(paste(study_name, "main_manova_table.htm", sep = "_"), trainset, manova_model, var_names)

# returns the 5 estimated logit models: forward, backward, all 5%-sig var, all 1%-sig var, LASSO and 
# produces supplementary material Table S2
estimated_models <- model_selection_table(paste(study_name, "main_selection_table.htm", sep = "_"), trainset, basic_models)

## inspect the table and manually select the model (in paper: forward (model 1))
selected_model <- estimated_models[[1]]

# build a table with marginal effects for the selected model
# produces paper Table 2
margins_table(paste(study_name, "main_margins_table.htm", sep = "_"), trainset, selected_model)


# -----------------------------------------------------------------------------
# Study 2: compute accuracy on train and test sets and draw ROC curves 
# -----------------------------------------------------------------------------

# compute accuracy and draw ROC curves
# produces Figure 2 and Table 3
accuracy(trainset, testset, selected_model, verbose = TRUE, file_name = paste(study_name, "main_performance_table.xlsx", sep = "_"),
  picture_name = paste(study_name, "main_roc_curve.pdf", sep = "_"))

# -----------------------------------------------------------------------------
# Study 3: compare against deception models previously reported in literature
# -----------------------------------------------------------------------------

# read list of deception models
models_to_compare <- read.xlsx(file_name_deception_models, 1)
models_to_compare <- rbind(data.frame(name = "Personalized", TypeModel = "", model = Reduce(paste, deparse(selected_model$formula[[3]]))),
  models_to_compare)
models_to_compare <- cbind(models_to_compare, LogLik = 0, AIC = 0, AUC_train = 0, acc_train = 0, hit_lie_train = 0, hit_truth_train = 0,
  AUC_test = 0, acc_test = 0, hit_lie_test = 0, hit_truth_test = 0)
nm <- nrow(models_to_compare)

# estimate all models
for(i in 1:nm) {
  curr_model <- glm(as.formula(paste("Veracity~", models_to_compare$model[i])), data = trainset, family = "binomial")
  models_to_compare[i,4] <- format(round(logLik(curr_model), 2), nsmall = 2)
  models_to_compare[i,5] <- format(round(curr_model$aic, 2), nsmall = 2)
  models_to_compare[i,6] <- format(round(roc(trainset$Veracity, predict.glm(curr_model, trainset, type = "response"), levels = c(1, 0),
    direction = ">")$auc, 3), nsmall = 3)
  models_to_compare[i,7:9] <- round(100 * all_pred_stat(predict.glm(curr_model, trainset, type = "response"), trainset$Veracity,
    mean(trainset$Veracity))[2:4], 0)
  models_to_compare[i,10] <- format(round(roc(testset$Veracity, predict.glm(curr_model, testset, type = "response"), levels = c(1, 0),
    direction = ">")$auc, 3), nsmall = 3)
  models_to_compare[i,11:13] <- round(100 * all_pred_stat(predict.glm(curr_model, testset, type = "response"), testset$Veracity,
    mean(trainset$Veracity))[2:4], 0)
}

# write results for all deception models to file
# produces Table 4
write.xlsx(models_to_compare, file = paste(study_name, "main_model_comparison.xlsx", sep = "_"), sheetName = "model_comparison", append = FALSE)



# -----------------------------------------------------------------------------
# Robustness check 1: test a model without word quantity (WC) variable
# -----------------------------------------------------------------------------

# create train and test sets, build model on train set and evaluate on test set
trainset = subset(complete_dataset, Dataset == 1)
testset = subset(complete_dataset, Dataset == 2)
manova_model <- with(trainset, cbind(Analytic,Clout,Authentic,Tone,WPS,Sixltr,Dic,function.,pronoun,ppron,i,we,you,shehe,they,ipron,article,prep,
  auxverb,adverb,conj,negate,verb,adj,compare,interrog,number,quant,affect,posemo,negemo,anx,anger,sad,social,family,friend,female,male,cogproc,
  insight,cause,discrep,tentat,certain,differ,percept,see,hear,feel,bio,body,health,sexual,ingest,drives,affiliation,achieve,power,reward,risk,
  focuspast,focuspresent,focusfuture,relativ,motion,space,time,work,leisure,home,money,relig,death,informal,swear,netspeak,assent,nonflu,filler,
  AllPunc,Period,Comma,Colon,QMark,Exclam,Dash,Quote,Apostro,Parenth,OtherP,Hashtag,At,inhib,incl,excl,humans,Senses,Optim,Self,Other,Metaph)
  ~Veracity)
basic_models <- manova_et_al(paste(study_name, "supmat_manova_table_without_wc.htm", sep = "_"), trainset, manova_model, var_names)

# produce supplementary material Table S3
estimated_models <- model_selection_table(paste(study_name, "supmat_selection_table_without_wc.htm", sep = "_"), trainset, basic_models)

## inspect the table and manually select the model (in paper: forward (model 1))
selected_model <- estimated_models[[1]]

# produce supplementary material Table S4
margins_table(paste(study_name, "supmat_margins_table_without_wc.htm", sep = "_"), trainset, selected_model)
perf_score <- accuracy(trainset, testset, selected_model)
print(paste("Test set accuracy = ", round(perf_score[9], 4), sep=""))
print(paste("Test set AUC = ", round(perf_score[14], 3), sep=""))
# -----------------------------------------------------------------------------
# Robustness check 2: test a model without topical variables money and religion
# -----------------------------------------------------------------------------

# exclude topical variables: work,leisure,home,money,relig,death,Metaph,achieve,bio,body,health,sexual,ingest
trainset = subset(complete_dataset, Dataset == 1)
testset = subset(complete_dataset, Dataset == 2)
manova_model <- with(trainset, cbind(WC,Analytic,Clout,Authentic,Tone,WPS,Sixltr,Dic,function.,pronoun,ppron,i,we,you,shehe,they,ipron,article,
  prep,auxverb,adverb,conj,negate,verb,adj,compare,interrog,number,quant,affect,posemo,negemo,anx,anger,sad,social,family,friend,female,male,cogproc,
  insight,cause,discrep,tentat,certain,differ,percept,see,hear,feel,drives,affiliation,power,reward,risk,focuspast,focuspresent,focusfuture,relativ,
  motion,space,time,informal,swear,netspeak,assent,nonflu,filler,AllPunc,Period,Comma,Colon,QMark,Exclam,Dash,Quote,Apostro,Parenth,OtherP,Hashtag,
  At,inhib,incl,excl,humans,Senses,Optim,Self,Other)~Veracity)
basic_models <- manova_et_al(paste(study_name, "supmat_manova_table_without_topics.htm", sep = "_"), trainset, manova_model, var_names)

# produce supplementary material Table S5
estimated_models <- model_selection_table(paste(study_name, "supmat_selection_table_without_topics.htm", sep = "_"), trainset, basic_models)

## inspect the table and manually select the model (in paper: forward (model 1))
selected_model <- estimated_models[[1]]

# produce supplementary material Table S6
margins_table(paste(study_name, "supmat_margins_table_without_topics.htm", sep = "_"), trainset, selected_model)
perf_score <- accuracy(trainset, testset, selected_model)
print(paste("Test set accuracy = ", round(perf_score[9], 4), sep=""))
print(paste("Test set AUC = ", round(perf_score[14], 3), sep=""))

# -----------------------------------------------------------------------------
# Robustness check 3: test topical models for money and work
# -----------------------------------------------------------------------------

# derive a model for tweets involving work-related words and another model for tweets not related to work
# start from the previously obtained basic_models because there are too few data points to run full MANOVA
trainset_nowork = subset(complete_dataset, Dataset == 1 & work == 0)
trainset_work = subset(complete_dataset, Dataset == 1 & work > 0)
testset_nowork = subset(complete_dataset, Dataset == 2 & work == 0)
testset_work = subset(complete_dataset, Dataset == 2 & work > 0)
selected_model_nowork <- model_selection_table(paste(study_name, "supmat_selection_table_nowork.htm", sep = "_"), trainset_nowork, basic_models,
  FALSE)
selected_model_work <- model_selection_table(paste(study_name, "supmat_selection_table_work.htm", sep = "_"), trainset_work, basic_models, FALSE)
perf_score_nowork <- accuracy(trainset_nowork, testset_nowork, selected_model_nowork)
perf_score_work <- accuracy(trainset_work, testset_work, selected_model_work)

# derive a model for tweets involving money-related words and another model for tweets not related to money
# start from the previously obtained basic_models because there are too few data points to run full MANOVA
trainset_nomoney = subset(complete_dataset, Dataset == 1 & money == 0)
trainset_money = subset(complete_dataset, Dataset == 1 & money > 0)
testset_nomoney = subset(complete_dataset, Dataset == 2 & money == 0)
testset_money = subset(complete_dataset, Dataset == 2 & money > 0)
selected_model_nomoney <- model_selection_table(paste(study_name, "supmat_selection_table_nomoney.htm", sep = "_"), trainset_nomoney, basic_models,
  FALSE)
selected_model_money <- model_selection_table(paste(study_name, "supmat_selection_table_money.htm", sep = "_"), trainset_money, basic_models, FALSE)
perf_score_nomoney <- accuracy(trainset_nomoney, testset_nomoney, selected_model_nomoney)
perf_score_money <- accuracy(trainset_money, testset_money, selected_model_money)
# combine results to produce supplementary material Table S7
S7_row1 <- c("Train set (Dataset 1)", round(perf_score_money[7], 3), round(perf_score_nomoney[7], 3), round(perf_score_work[7], 3),
  round(perf_score_nowork[7], 3))
S7_row2 <- c("Test set (Dataset 2)", round(perf_score_money[14], 3), round(perf_score_nomoney[14], 3), round(perf_score_work[14], 3),
  round(perf_score_nowork[14], 3))
S7_table <- data.frame(rbind(S7_row1, S7_row2))
colnames(S7_table) <- c("Dataset", "Money", "No Money", "Work", "No Work")
write.xlsx(S7_table, file = paste(study_name, "supmat_topic_perf.xlsx", sep = "_"), sheetName = "performance_table", append = FALSE)

# -----------------------------------------------------------------------------
# Robustness check 4: test sensitivity to train/test splits
# -----------------------------------------------------------------------------

# simulate 1,000 random splits using a forward logit model
predictions <- c()
for (i in 1:1000) {
  this_sample <- sample(nrow(complete_dataset), nrow(trainset))
  trainset = complete_dataset[this_sample,]
  testset = complete_dataset[-this_sample,]
  manova_model <- with(trainset, cbind(WC,Analytic,Clout,Authentic,Tone,WPS,Sixltr,Dic,function.,pronoun,ppron,i,we,you,shehe,they,ipron,article,
    prep,auxverb,adverb,conj,negate,verb,adj,compare,interrog,number,quant,affect,posemo,negemo,anx,anger,sad,social,family,friend,female,male,
	cogproc,insight,cause,discrep,tentat,certain,differ,percept,see,hear,feel,bio,body,health,sexual,ingest,drives,affiliation,achieve,power,reward,
	risk,focuspast,focuspresent,focusfuture,relativ,motion,space,time,work,leisure,home,money,relig,death,informal,swear,netspeak,assent,nonflu,
	filler,AllPunc,Period,Comma,Colon,QMark,Exclam,Dash,Quote,Apostro,Parenth,OtherP,Hashtag,At,inhib,incl,excl,humans,Senses,Optim,Self,Other,
	Metaph)~Veracity)
  basic_models <- manova_et_al(paste(study_name, "supmat_manova_table_random_split.htm", sep = "_"), trainset, manova_model, var_names, FALSE)
  selected_model <- model_selection_table(paste(study_name, "supmat_selection_table_random_split.htm", sep = "_"), trainset, basic_models, FALSE)
  predictions <- rbind(predictions, accuracy(trainset, testset, selected_model))
}
colMeans(predictions)

# create histogram with 20 bins
# produce supplementary material Figure S1
png(paste(study_name, "supmat_histogram_random_splits.png", sep = "_"))
hist(predictions[,14], 20, xlab = "AUC", ylab=NULL, main = NULL, cex = 1.5)
dev.off()

#Check special case of reversing trainset and test set
trainset <- subset(complete_dataset, Dataset == 2)
testset <- subset(complete_dataset, Dataset == 1)
manova_model <- with(trainset, cbind(WC,Analytic,Clout,Authentic,Tone,WPS,Sixltr,Dic,function.,pronoun,ppron,i,we,you,shehe,they,ipron,article,prep,
  auxverb,adverb,conj,negate,verb,adj,compare,interrog,number,quant,affect,posemo,negemo,anx,anger,sad,social,family,friend,female,male,cogproc,
  insight,cause,discrep,tentat,certain,differ,percept,see,hear,feel,bio,body,health,sexual,ingest,drives,affiliation,achieve,power,reward,risk,
  focuspast,focuspresent,focusfuture,relativ,motion,space,time,work,leisure,home,money,relig,death,informal,swear,netspeak,assent,nonflu,filler,
  AllPunc,Period,Comma,Colon,QMark,Exclam,Dash,Quote,Apostro,Parenth,OtherP,Hashtag,At,inhib,incl,excl,humans,Senses,Optim,Self,Other,Metaph)
  ~Veracity)
basic_models <- manova_et_al(paste(study_name, "supmat_manova_table_random_split.htm", sep = "_"), trainset, manova_model, var_names, FALSE)
selected_model <- model_selection_table(paste(study_name, "supmat_selection_table_random_split.htm", sep = "_"), trainset, basic_models, FALSE)
perf_score <- accuracy(trainset, testset, selected_model)
print(paste("Test set AUC = ", round(perf_score[14], 3), sep = ""))


# -----------------------------------------------------------------------------
# Robustness check 5: test random veracity labels
# -----------------------------------------------------------------------------

# simulate 1,000 runs with random veracity labels using a forward logit model
predictions <- c()
for (i in 1:1000) {
  trainset <- subset(complete_dataset, Dataset == 1)
  testset <- subset(complete_dataset, Dataset == 2)
  trainset <- transform(trainset, Veracity = sample(trainset$Veracity))
  testset <- transform(testset, Veracity = sample(testset$Veracity))
  manova_model <- with(trainset, cbind(WC,Analytic,Clout,Authentic,Tone,WPS,Sixltr,Dic,function.,pronoun,ppron,i,we,you,shehe,they,ipron,article,
    prep,auxverb,adverb,conj,negate,verb,adj,compare,interrog,number,quant,affect,posemo,negemo,anx,anger,sad,social,family,friend,female,male,
	cogproc,insight,cause,discrep,tentat,certain,differ,percept,see,hear,feel,bio,body,health,sexual,ingest,drives,affiliation,achieve,power,reward,
	risk,focuspast,focuspresent,focusfuture,relativ,motion,space,time,work,leisure,home,money,relig,death,informal,swear,netspeak,assent,nonflu,
	filler,AllPunc,Period,Comma,Colon,QMark,Exclam,Dash,Quote,Apostro,Parenth,OtherP,Hashtag,At,inhib,incl,excl,humans,Senses,Optim,Self,Other,
	Metaph)~Veracity)
  basic_models <- manova_et_al(paste(study_name, "supmat_manova_table_placebo.htm", sep = "_"), trainset, manova_model, var_names, FALSE)
  selected_model <- model_selection_table(paste(study_name, "supmat_selection_placebo.htm", sep = "_"), trainset, basic_models, FALSE)
  predictions <- rbind(predictions, accuracy(trainset, testset, selected_model))
}
colMeans(predictions)

# create histogram with 20 bins
# produce supplementary material Figure S2
png(paste(study_name, "supmat_histogram_placebo.png", sep = "_"))
hist(predictions[,14], 20, xlab = "AUC", ylab=NULL, main = NULL, cex = 1.5)
dev.off()
