#Author: Stephen Coleman
#Purpose: Analyse this nonsense
require(dplyr)
require(data.table)
require(plyr)
require(gdata)
require(ROCR)
require(pROC)
require(randomForest)
require(earth)
require(ggplot2)

setwd("C:/Users/Captain Bluebear/Desktop/TP/Year 4/Semester 2 - Hillary/MA4492 - Project/R")


# Produces roc curve with 95% CI
roc_comp <- function(fit, test)
{
  fitted.results <- predict(fit, type = 'prob', newdata = test)
  predic <- prediction(fitted.results[,2], test[["Measured.Fixation"]])
  perf_comp <- performance(predic, "tpr", "fpr")
  roc_fit <- roc(test[["Measured.Fixation"]], 
                fitted.results[,2], ci = T, plot = F)
  sens.ci.fit <- ci.se(roc_fit, specificities=seq(0, 1, 0.1))
  plot(roc_fit, main="ROC curve", print.auc=TRUE)
  plot(sens.ci.fit, type="shape", col="lightblue")
  
}


prep_train_test <- function(train, test)
{
  output <- list()
  output[[1]] <- train[, c(4,5,6,12,21,22,24,20)]
  output[[2]] <- test[, c(4,5,6,12,21,22,24,20)]
  output[[1]] <- na.omit(output[[1]])
  output[[2]] <- na.omit(output[[2]])
  return(output)
}


data_clean <- function(data, prep_merge = F)
{
  if(prep_merge)
  {
    keepers <- data[data[["Fix.Cat"]] == 1,]
    not_certain_keep <- data[data[["Fix.Cat"]] == 0,]
  }
  else
  {
    keepers <- data[data[["Measured.Fixation"]] == 1,]
    not_certain_keep <- data[data[["Measured.Fixation"]] == 0,]  
  }
  
  rows_to_keep <- sample(seq_len(nrow(not_certain_keep)), size = nrow(keepers))
  also_keepers <- not_certain_keep[rows_to_keep,]
  output <- rbind.fill(keepers, also_keepers)
  if(prep_merge)
  {
    rows_to_drop <- as.integer(row.names(output[output[["Fixations"]] > 25,]))
    output <- output[-rows_to_drop,]  
  }
  return(output)
}

fix_factor <- function(data, prep_merge = F)
{
  if(prep_merge)
  {
    data[["Fix.Cat"]] <- factor(data[["Fix.Cat"]])
  }
  else
  {
    data[["Measured.Fixation"]] <- factor(data[["Measured.Fixation"]], levels = c(0,1))
  }
  return(data)
}



prep_data <- function(data)
{
  face.options <- c(0:12)
  
  test <- fix_factor(test)
  train <- fix_factor(train)  

  test[["Face.Order"]] <- factor(test[["Face.Order"]], levels = face.options)
  train[["Face.Order"]] <- factor(train[["Face.Order"]], levels = face.options)

  data[["Log.Centrality"]][data[["Log.Centrality"]] == (-Inf)] <- -1
  data <- na.omit(data)
  return(data)
}

# Computes accuracy of random forest model
tree_acc <- function(fit_rf, test)
{
  preds <- predict(fit_rf, test)
  cf <- data.frame(Prediction = preds, Actual = test$Measured.Fixation)
  cf$Comparison <- 0
  cf$Comparison[cf$Prediction == cf$Actual] <- 1

  false_positive <- sum(as.numeric(cf[["Prediction"]][cf[["Prediction"]] == 1 
                                    & cf[["Actual"]] == 0]) - 1)/nrow(cf)

  false_negative <- sum(as.numeric(cf[["Prediction"]][cf[["Prediction"]] == 0 
                                    & cf[["Actual"]] == 1]) - 1)/nrow(cf)
  
  
  sum(cf$Comparison)/length(cf$Comparison)

  acc_rand_comp <- sum(cf$Comparison)/length(cf$Comparison)
  
  output <- data.frame("Accuracy" = acc_rand_comp, 
                       "Sensitivity" = (1- false_positive),
                       "Specifity" = (1 - false_negative))
  return(output)
}

# Function that procduces ROC cruve for random forest (does not plot)
rf_roc <- function(fit_rf, test)
{
  rf.pr.comp <- predict(fit_rf,type="prob",newdata=test)[,2]
  rf.pred.comp <- prediction(rf.pr.comp, test[["Measured.Fixation"]])
  rf.perf.comp <- performance(rf.pred.comp,"tpr","fpr")

  roc.rf.comp <- roc(test[["Measured.Fixation"]], rf.pr.comp, ci = TRUE, plot = FALSE)
  return(roc.rf.comp)
}

rf_roc_plot_ci <- function(rf_roc_output, title = "ROC curve")
{
  sens.ci.rf.comp <- ci.se(rf_roc_output, specificities=seq(0, 1, 0.05))
  plot(rf_roc_output, main= title, print.auc=TRUE)
  plot(sens.ci.rf.comp, type="shape", col="lightblue")
}

# fit type is one of rf or glm
auc_calc <- function(fit,test, fit_type = "rf")
{
  if(fit_type == "rf")
  {
    fitted.results <- predict(fit, test, type = "prob")
    fitted.results <- ifelse(fitted.results > 0.5,1,0)
    misClasificError <- mean(fitted.results[,2] != test[["Measured.Fixation"]])
    p <- fitted.results[,2]
  }  
  else
  {
    fitted.results <- predict(fit, test, type = "response")
    fitted.results <- ifelse(fitted.results > 0.5,1,0)
    misClasificError <- mean(fitted.results != test[["Measured.Fixation"]])
    p <- fitted.results
  }
  print(paste("Accuracy ", round(1 - misClasificError, digits = 3)))
  
  pr <- prediction(p, test[["Measured.Fixation"]])
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")

  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  auc
  return(auc)
}

# index_names is the numbers of those to drop
rf_res <- function(train, test, data_name, index_names, 
                   prep_merge = F, dim = 2, no_trees = 500, central = F)
{
  if(prep_merge == F)
  {
   fit <- randomForest(Measured.Fixation ~., data = train[, -index_names],
                      importance = T, ntree = no_trees)
  }

  else
  {
    fit <- randomForest(Fix.Cat ~., data = train[, -index_names],
                      importance = T, ntree = no_trees)
  }

  if(central)
  {
    data_name <- paste0(data_name, "_central")
  }

  dir.create(data_name, showWarnings = FALSE)
  
  
  write.table(names(train)[-index_names], paste0(data_name, "/ColUsed.txt"), sep = "\t",
              col.names = F, row.names = F)
  
  write.csv(importance(fit), paste0(data_name, "/importance.csv"))
  
  
  file_name <- paste0(data_name)
  jpeg(file = paste0(data_name, "/varImpPlot_", no_trees, '.jpeg'))
  varImpPlot(fit, cex = 0.6, 
           main = paste0("Variable importance in random forest"))  
  dev.off()

  accuracy_frame <- tree_acc(fit, test)
  write.table(accuracy_frame, paste0(data_name, "/Accuracy.txt"), sep = "\t",
              row.names = F)  
  
  roc.rf.comp <- rf_roc(fit, test)
  
  jpeg(file = paste0(data_name, "/ROC_curve.jpeg"))
  rf_roc_plot_ci(roc.rf.comp)
  dev.off()

  jpeg(file = paste0(data_name, "/Error_across_trees.jpeg"))
  plot(fit, main = "Error rate over trees")
  dev.off()

}


rf_all <- function()
{
  for(i in 1:4)
  {
    print(i)
    
    if(i == 1)
    {
      file_name <- "data_dim2_krigFALSE"
    }
    else if(i == 2)
    {
      file_name <- "data_dim2_krigTRUE"
    }
    else if(i == 3)
    {
      file_name <- "data_dim3_krigFALSE"
    }
    else
    {
      file_name <- "data_dim3_krigTRUE"
    }

    print(file_name)

    test <- read.csv(paste0("test_", file_name, ".csv"))
    train <- read.csv(paste0("train_", file_name, ".csv"))

    print(summary(test))
    summary(train)

    test <- data_clean(test)
    train <- data_clean(train)

    test <- na.omit(test)
    train <- na.omit(train)
    
    print("Here")

    test <- fix_factor(test)
    train <- fix_factor(train)

    print("Factored")

    not_used <- c(1:4, 8:13)

    
    
    rf_res(train, test, file_name, not_used)
  }
}


rf_all_other_names <- function()
{
  for(i in 1:4)
  {
    if(i == 1)
    {
      file_name <- "data_dim2_krigFALSE"
    }
    else if(i == 2)
    {
      file_name <- "data_dim2_krigTRUE"
    }
    else if(i == 3)
    {
      file_name <- "data_dim3_krigFALSE"
    }
    else
    {
      file_name <- "data_dim3_krigTRUE"
    }
    
    file_name <- "data_dim2_krigFALSE"
    test <- read.csv(paste0("test_", file_name, ".csv"))
    train <- read.csv(paste0("train_", file_name, ".csv"))

    test <- data_clean(test)
    train <- data_clean(train)

    test <- na.omit(test)
    train <- na.omit(train)
    
    test <- fix_factor(test)
    train <- fix_factor(train)

    train[["Log.Centrality"]][train[["Log.Centrality"]] == (-Inf)] <- -1
    test[["Log.Centrality"]][test[["Log.Centrality"]] == (-Inf)] <- -1
    
    not_used <- c(1:4, 8)
    
    print(not_used)
    
    rf_res(train, test, file_name, not_used, central = T)
    
  }
}

rf_all()
gc()
rf_all_other_names()


test_3d_krig <- read.csv("test_data_dim3_krigTRUE_prep_merge.csv")
train_3d_krig <- read.csv("train_data_dim3_krigTRUE_prep_merge.csv")
test <- data_clean(test_3d_krig, prep_merge = T)
train <- data_clean(train_3d_krig, prep_merge = T)
# 
# test_3d_krig <- read.csv("test_data_dim3_krigTRUE.csv")
# train_3d_krig <- read.csv("train_data_dim3_krigTRUE.csv")
# 
test_2d_krig <- read.csv("test_data_dim2_krigTRUE.csv")
train_2d_krig <- read.csv("train_data_dim2_krigTRUE.csv")


test <- data_clean(test_2d_krig)
train <- data_clean(train_2d_krig)


# rows_to_keep <- sample(seq_len(nrow(test)), size = nrow(test)/20)
# test <- test[rows_to_keep,]
# 
# rows_to_keep <- sample(seq_len(nrow(train)), size = nrow(train)/20)
# train <- train[rows_to_keep,]

train <- fix_factor(train)
test <- fix_factor(test)

train <- fix_factor(train, prep_merge = T)
test <- fix_factor(test, prep_merge = T)

train[["Log.Centrality"]][train[["Log.Centrality"]] == (-Inf)] <- -1
test[["Log.Centrality"]][test[["Log.Centrality"]] == (-Inf)] <- -1
# names(train)

not_used <- c(1:4, 8)             #These are both for
maybe_not_used <- c(1:4, 8:13)    # 2D data

test <- na.omit(test)
train <- na.omit(train)



fit_log <- glm(Measured.Fixation ~.,
                 data = train[, -not_used], family = "binomial"(link = 'logit'))

fit_probit <- glm(Measured.Fixation ~.,
                 data = train[, - not_used], family = "binomial"(link = 'probit'))

fit_log_not_used <- glm(Measured.Fixation ~.,
                         data = train[, -maybe_not_used], family = "binomial"(link = 'logit'))

fit_probit_not_used <- glm(Measured.Fixation ~.,
                           data = train[, - maybe_not_used], family = "binomial"(link = 'probit'))
summary(train)

fit_rf <- randomForest(Measured.Fixation ~., data = train[,-maybe_not_used], 
                         importance = T, ntree = 500)

fit_rf_not_used <- randomForest(Measured.Fixation ~., data = train[,-maybe_not_used], 
                       importance = T, ntree = 500)


# acc_rf <- tree_acc(fit_probit_not_used, test)

frame_dat <- read.csv("frame_description_3D.csv")

frame_data <- frame_dat[frame_dat[["Frame.No"]] == 17,]
rm(frame_dat)
fix_dat <- read.csv("data_3D_afterKrig.csv")
fix_data <- fix_dat[fix_dat[["Index"]] == 17,]
x_y <- data.frame("X" = fix_data[["X.grid"]], "Y" = fix_data[["Y.grid"]])
write.table(x_y, "x_y.txt", sep = '\t', row.names = F)


frame_comp <- merge(frame_data, fix_data, by = c("X.grid", "Y.grid"), all.x = T)
nrow(frame_comp)
importance(fit_rf)

pred <- predict(fit_rf, frame_comp, type ="prob")[,2]

frame_comp[["Pred"]] <- pred

p <- ggplot(frame_comp, aes(X.grid, Y.grid), main = "Saliency for frame 17") + geom_raster(aes(fill = Pred))
p

points(x_y[["X"]], x_y[["Y"]])

not_used_cont <-c(1:4, 8:13, 32) 
names(train)
# Only make sense for continuous data
fit_gauss <- glm(Fixations ~.,
                data = train[, -not_used_cont])

fit_pois <- glm(Fixations ~.,
                data = train[, -not_used_cont], family = poisson())

summary(fit_pois)

fit_mars <- earth(Fixations ~.,
                  data = train[, -not_used_cont], pmethod="backward")

plot(fit_mars, which=1)
plot(fit_mars, which=3)
plot(fit_mars, which=2)
plot(fit_mars, which=4)

plot(fit_mars, col.npreds = 0)
# example(plot.earth)
summary(fit_mars)

prediction_mars <- predict(fit_mars, test)
prediction_pois <- predict(fit_pois, test)
mars_comp <- data.frame("Predicted" = round(prediction_mars), "Actual" = test[["Fixations"]])

summary(mars_comp)


pois_comp <- data.frame("Predicted" = round(prediction_pois), "Actual" = test[["Fixations"]])
summary(pois_comp)

mars_comp[["Error"]] <- (mars_comp[["Actual"]]) -mars_comp[["Fixations"]]
mars_comp[["Root.Square.Error"]] <- sqrt(mars_comp[["Error"]]*mars_comp[["Error"]])
rmse_mars <- mean(mars_comp[["Root.Square.Error"]])

pois_comp[["Error"]] <- (pois_comp[["Actual"]]) -pois_comp[["Predicted"]]
pois_comp[["Root.Square.Error"]] <- sqrt(pois_comp[["Error"]]*pois_comp[["Error"]])
rmse_pois <- mean(pois_comp[["Root.Square.Error"]])

mars_comp[["Fix.Cat"]] <- test[["Fix.Cat"]]
mars_comp[["Pred.Fix.Cat"]] <- 0
mars_comp[["Pred.Fix.Cat"]][mars_comp[["Fixations"]] != 0] <- 1

mars_comp[["Cat.Error"]] <- 0
mars_comp[["Cat.Error"]][mars_comp[["Fix.Cat"]] == mars_comp[["Pred.Fix.Cat"]]] <- 1
summary(mars_comp)
sum(mars_comp[["Cat.Error"]])/nrow(mars_comp)

pois_comp[["Fix.Cat"]] <- test[["Fix.Cat"]]
pois_comp[["Pred.Fix.Cat"]] <- 0
pois_comp[["Pred.Fix.Cat"]][pois_comp[["Predicted"]] != 0] <- 1

pois_comp[["Cat.Error"]] <- 0
pois_comp[["Cat.Error"]][pois_comp[["Fix.Cat"]] == pois_comp[["Pred.Fix.Cat"]]] <- 1
summary(pois_comp)

prediction_gauss <- predict(fit_gauss, test)
gauss_comp <- data.frame("Predicted" = round(prediction_gauss), "Actual" = test[["Fixations"]])
summary(gauss_comp)

gauss_comp[["Error"]] <- (gauss_comp[["Actual"]]) -gauss_comp[["Predicted"]]
gauss_comp[["Root.Square.Error"]] <- sqrt(gauss_comp[["Error"]]*gauss_comp[["Error"]])
rmse_gauss <- mean(gauss_comp[["Root.Square.Error"]])


gauss_comp[["Fix.Cat"]] <- test[["Fix.Cat"]]
gauss_comp[["Pred.Fix.Cat"]] <- 0
gauss_comp[["Pred.Fix.Cat"]][gauss_comp[["Predicted"]] != 0] <- 1

gauss_comp[["Cat.Error"]] <- 0
gauss_comp[["Cat.Error"]][gauss_comp[["Fix.Cat"]] == gauss_comp[["Pred.Fix.Cat"]]] <- 1
summary(gauss_comp)

sum(gauss_comp[["Cat.Error"]])/nrow(gauss_comp)


logLik(fit_pois)
logLik(fit_gauss)
aic(fit_pois)
logLik(fit_mars)

sum(pois_comp[["Cat.Error"]])/nrow(pois_comp)
summary(fit_mars,digit=3)

summary(train[train[["Fixations"]] > 2,])

rf_res(train, test, "data_2d", maybe_not_used)
rf_res(train, test, "data_3d_krig", maybe_not_used, dim = 3)


auc_rf <- auc_calc(fit_rf, test)