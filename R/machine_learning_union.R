
#' Machine Learning Based on xgboost, random forest, and mlp neural network
#'
#' @param data A data frame containing the dependent variable, which should be a factor, with the column name of "y".
#' @param prop The proportion of dividing data into training and validation sets.
#' @param var_num Number of independent variables in the data.
#'
#' @return For regression models, return the performance evaluation list and true/predicted value match plot for each model; For classification models, return the performance evaluation list and ROC curve for each model.
#' @export
#'
#' @examples colnames(iris)[1] <- "y"; ML_analysis(data=iris,prop=0.7,var_num=4)
ML_analysis <- function(data,prop,var_num){

  if(!requireNamespace("tidymodels",quietly = TRUE)){
    stop("tidymodels for the this function to work. Please install it",
         call. = FALSE)
  }

  if(!requireNamespace("fastDummies",quietly = TRUE)){
    stop("fastDummies for the this function to work. Please install it",
         call. = FALSE)
  }

  set.seed(1234)
  split <- initial_split(data,
                         prop = prop,
                         strata = y)
  train <- training(split)
  test <- testing(split)
  rbCol=c("#CD2626","#1E90FF","#FFC125")
  metric_res <- list()

  if(class(data$y)=="numeric"){

    par(mar= c(5,5,1,1),mfrow=c(1,3),
        cex.lab=1.2,cex.axis= 1.2)

    xgb_recipe <- train %>% recipe(y~.) %>%
      step_corr(all_numeric_predictors(),threshold = 0.5) %>%
      step_center(all_numeric_predictors(),-all_outcomes()) %>%
      step_scale(all_numeric_predictors(),-all_outcomes()) %>%
      prep()

    xgb_cv_folds <- bake(xgb_recipe,new_data = train) %>%
      rsample::vfold_cv(v=5)
    xgb_test <- bake(xgb_recipe,new_data = test)

    xgb_model <- boost_tree(trees = tune(),
                            min_n = tune(),
                            tree_depth = tune()) %>%
      set_engine("xgboost") %>%
      set_mode("regression")

    xgb_grid <- grid_max_entropy(extract_parameter_set_dials(xgb_model),
                                 size=20)

    xgb_workflow <- workflow() %>%
      add_model(xgb_model) %>%
      add_formula(y~.)

    xgb_tunned <- tune_grid(object = xgb_workflow,
                            resamples = xgb_cv_folds,
                            grid = xgb_grid,
                            metrics=metric_set(rmse, rsq, ccc),
                            control = control_grid(verbose = F))

    xgb_final_model <- select_best(xgb_tunned,
                                   metric = "ccc") %>%
      finalize_model(xgb_model,.)

    xgb_test_prediction <- xgb_final_model %>%
      fit(y~.,data = test)

    xgb_pred = predict(xgb_test_prediction,test) %>%
      bind_cols(select(test,y))

    metric_res[[1]] <- rbind(rmse(xgb_pred,y,.pred),
                             rsq(xgb_pred,y,.pred),
                             ccc(xgb_pred,y,.pred),
                             mae(xgb_pred,y,.pred),
                             mase(xgb_pred,y,.pred))

    plot(1:length(xgb_pred$y), xgb_pred$y,
         ylim=c(min(xgb_pred$.pred), max(xgb_pred$y)),
         pch=20, lwd=2, cex.main=1.2, col="red",
         cex.axis=1.2,cex.lab=1.2,
         xlab = paste0("Number of observations"),
         ylab = "Value",
         main=paste0("xgboost model"," ","R2"," ",
                     round(rsq(xgb_pred,y,.pred)$.estimate,4)))
    lines(1:length(xgb_pred$y),
          xgb_pred$.pred, lwd="1", col="blue")

    rf_recipe <- train %>% recipe(y~.) %>%
      step_corr(all_numeric_predictors(),threshold = 0.5) %>%
      step_center(all_numeric_predictors(),-all_outcomes()) %>%
      step_scale(all_numeric_predictors(),-all_outcomes()) %>%
      prep()

    rf_cv_folds <- bake(rf_recipe,new_data = train) %>%
      rsample::vfold_cv(v=5)
    rf_test <- bake(rf_recipe,new_data = test)

    rf_model <- rand_forest(mtry = tune(),
                            trees = tune()) %>%
      set_engine("ranger",importance="impurity") %>%
      set_mode("regression")

    rf_grid <- expand.grid(mtry=round(0.5*var_num,0):round(0.9*var_num,0),
                           trees=seq(100,500,100))

    rf_workflow <- workflow() %>%
      add_model(rf_model) %>%
      add_formula(y~.)

    rf_tunned <- tune_grid(object = rf_workflow,
                           resamples = rf_cv_folds,
                           grid = rf_grid,
                           metrics=metric_set(rmse, rsq, ccc),
                           control = control_grid(verbose = F))

    rf_final_model <- select_best(rf_tunned,
                                  metric = "ccc") %>%
      finalize_model(rf_model,.)

    rf_test_prediction <- rf_final_model %>%
      fit(y~.,data = test)

    rf_pred = predict(rf_test_prediction,test) %>%
      bind_cols(select(test,y))

    metric_res[[2]] <- rbind(rmse(rf_pred,y,.pred),
                             rsq(rf_pred,y,.pred),
                             ccc(rf_pred,y,.pred),
                             mae(rf_pred,y,.pred),
                             mase(rf_pred,y,.pred))

    plot(1:length(rf_pred$y), rf_pred$y,
         ylim=c(min(rf_pred$.pred), max(rf_pred$y)),
         pch=20, lwd=2, cex.main=1.2, col="red",
         cex.axis=1.2,cex.lab=1.2,
         xlab = paste0("Number of observations"),
         ylab = "Value",
         main=paste0("random forest model"," ","R2"," ",
                     round(rsq(rf_pred,y,.pred)$.estimate,4)))
    lines(1:length(rf_pred$y),
          rf_pred$.pred, lwd="1", col="blue")

    nnet_recipe <- train %>% recipe(y~.) %>%
      step_corr(all_numeric_predictors(),threshold = 0.5) %>%
      step_center(all_numeric_predictors(),-all_outcomes()) %>%
      step_scale(all_numeric_predictors(),-all_outcomes()) %>%
      prep()

    nnet_cv_folds <- bake(nnet_recipe,new_data = train) %>%
      rsample::vfold_cv(v=5)
    nnet_test <- bake(nnet_recipe,new_data = test)

    nnet_model <- mlp(hidden_units = tune(),
                      penalty = tune(),
                      epochs = tune()) %>%
      set_engine("nnet") %>%
      set_mode("regression")

    nnet_grid <- grid_max_entropy(extract_parameter_set_dials(nnet_model),
                                  size=20)

    nnet_workflow <- workflow() %>%
      add_model(nnet_model) %>%
      add_formula(y~.)

    nnet_tunned <- tune_grid(object = nnet_workflow,
                             resamples = nnet_cv_folds,
                             grid = nnet_grid,
                             metrics=metric_set(rmse, rsq, ccc),
                             control = control_grid(verbose = F))

    nnet_final_model <- select_best(nnet_tunned,
                                    metric = "ccc") %>%
      finalize_model(nnet_model,.)

    nnet_test_prediction <- nnet_final_model %>%
      fit(y~.,data = test)

    nnet_pred = predict(nnet_test_prediction,test) %>%
      bind_cols(select(test,y))

    metric_res[[3]] <- rbind(rmse(nnet_pred,y,.pred),
                             rsq(nnet_pred,y,.pred),
                             ccc(nnet_pred,y,.pred),
                             mae(nnet_pred,y,.pred),
                             mase(nnet_pred,y,.pred))

    plot(1:length(nnet_pred$y), nnet_pred$y,
         ylim=c(min(nnet_pred$.pred), max(nnet_pred$y)),
         pch=20, lwd=2, cex.main=1.2, col="red",
         cex.axis=1.2,cex.lab=1.2,
         xlab = paste0("Number of observations"),
         ylab = "Value",
         main=paste0("mlp nnet model"," ","R2"," ",
                     round(rsq(nnet_pred,y,.pred)$.estimate,4)))
    lines(1:length(nnet_pred$y),
          nnet_pred$.pred, lwd="1", col="blue")


  }else if(class(data$y)=="factor"){

    xgb_recipe <- train %>% recipe(y~.) %>%
      step_corr(all_numeric_predictors(),threshold = 0.5) %>%
      step_center(all_numeric_predictors(),-all_outcomes()) %>%
      step_scale(all_numeric_predictors(),-all_outcomes()) %>%
      prep()

    xgb_cv_folds <- bake(xgb_recipe,new_data = train) %>%
      rsample::vfold_cv(v=5)
    xgb_test <- bake(xgb_recipe,new_data = test)

    xgb_model <- boost_tree(trees = tune(),
                            min_n = tune(),
                            tree_depth = tune()) %>%
      set_engine("xgboost") %>%
      set_mode("classification")

    xgb_grid <- grid_max_entropy(extract_parameter_set_dials(xgb_model),
                                 size=20)

    xgb_workflow <- workflow() %>%
      add_model(xgb_model) %>%
      add_formula(y~.)

    xgb_tunned <- tune_grid(object = xgb_workflow,
                            resamples = xgb_cv_folds,
                            grid = xgb_grid,
                            metrics=metric_set(accuracy,bal_accuracy,
                                               sens,spec,
                                               npv,ppv,precision,
                                               recall,f_meas,
                                               roc_auc,pr_auc),
                            control = control_grid(verbose = F))

    xgb_final_model <- select_best(xgb_tunned,
                                   metric = "accuracy") %>%
      finalize_model(xgb_model,.)

    xgb_test_prediction <- xgb_final_model %>%
      fit(y~.,data = test)

    xgb_pred = predict(xgb_test_prediction,test) %>%
      bind_cols(select(test,y)) %>%
      bind_cols(predict(xgb_test_prediction,test,type = "prob"))

    metric_res[[1]] <- rbind(accuracy(xgb_pred,y,.pred_class),
                             bal_accuracy(xgb_pred,y,.pred_class),
                             sens(xgb_pred,y,.pred_class),
                             spec(xgb_pred,y,.pred_class),
                             npv(xgb_pred,y,.pred_class),
                             ppv(xgb_pred,y,.pred_class),
                             precision(xgb_pred,y,.pred_class),
                             recall(xgb_pred,y,.pred_class),
                             f_meas(xgb_pred,y,.pred_class),
                             roc_auc(xgb_pred,y,.pred_0),
                             pr_auc(xgb_pred,y,.pred_0))

    par(mar= c(5,5,1,1),cex.lab=1.2,cex.axis= 1.2)

    y_onehot_xgb <- dummy_cols(test$y)
    colnames(y_onehot_xgb) <- c('drop', 'none', 'one')
    y_onehot_xgb <- subset(y_onehot_xgb, select = -c(drop))

    z_xgb = cbind(xgb_pred, y_onehot_xgb)

    z_xgb$none <- as.factor(z_xgb$none)
    roc_none_xgb <- roc_curve(data = z_xgb, none, .pred_0)
    roc_none_xgb$specificity <- 1 - roc_none_xgb$specificity
    colnames(roc_none_xgb) <- c('threshold', 'tpr', 'fpr')
    auc_none_xgb <- roc_auc(data = z_xgb, none, .pred_0)
    auc_none_xgb <- auc_none_xgb$.estimate
    none_xgb <- paste('xgboost',' ','(AUC=',toString(round(1-auc_none_xgb,2)),')',sep = '')

    plot(roc_none_xgb$fpr,
         roc_none_xgb$tpr,
         type="l", xlim=c(0,1), ylim=c(0,1),col=rbCol[1],
         xlab="False positive rate", ylab="True positive rate",
         lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)+
      title(main = "ROC curve of models")
    abline(0,1)
    text(x=0.85,y=0.7,paste0(none_xgb),col=rbCol[1])


    rf_recipe <- train %>% recipe(y~.) %>%
      step_corr(all_numeric_predictors(),threshold = 0.5) %>%
      step_center(all_numeric_predictors(),-all_outcomes()) %>%
      step_scale(all_numeric_predictors(),-all_outcomes()) %>%
      prep()

    rf_cv_folds <- bake(rf_recipe,new_data = train) %>%
      rsample::vfold_cv(v=5)
    rf_test <- bake(rf_recipe,new_data = test)

    rf_model <- rand_forest(mtry = tune(),
                            trees = tune()) %>%
      set_engine("ranger",importance="impurity") %>%
      set_mode("classification")

    rf_grid <- expand.grid(mtry=round(0.5*var_num,0):round(0.9*var_num,0),
                           trees=seq(100,500,100))

    rf_workflow <- workflow() %>%
      add_model(rf_model) %>%
      add_formula(y~.)

    rf_tunned <- tune_grid(object = rf_workflow,
                           resamples = rf_cv_folds,
                           grid = rf_grid,
                           metrics=metric_set(accuracy,bal_accuracy,
                                              sens,spec,
                                              npv,ppv,precision,
                                              recall,f_meas,
                                              roc_auc,pr_auc),
                           control = control_grid(verbose = F))

    rf_final_model <- select_best(rf_tunned,
                                  metric = "accuracy") %>%
      finalize_model(rf_model,.)

    rf_test_prediction <- rf_final_model %>%
      fit(y~.,data = test)

    rf_pred = predict(rf_test_prediction,test) %>%
      bind_cols(select(test,y)) %>%
      bind_cols(predict(rf_test_prediction,test,type = "prob"))

    metric_res[[2]] <- rbind(accuracy(rf_pred,y,.pred_class),
                             bal_accuracy(rf_pred,y,.pred_class),
                             sens(rf_pred,y,.pred_class),
                             spec(rf_pred,y,.pred_class),
                             npv(rf_pred,y,.pred_class),
                             ppv(rf_pred,y,.pred_class),
                             precision(rf_pred,y,.pred_class),
                             recall(rf_pred,y,.pred_class),
                             f_meas(rf_pred,y,.pred_class),
                             roc_auc(rf_pred,y,.pred_0),
                             pr_auc(rf_pred,y,.pred_0))

    y_onehot_rf <- dummy_cols(test$y)
    colnames(y_onehot_rf) <- c('drop', 'none', 'one')
    y_onehot_rf <- subset(y_onehot_rf, select = -c(drop))

    z_rf = cbind(rf_pred, y_onehot_rf)

    z_rf$none <- as.factor(z_rf$none)
    roc_none_rf <- roc_curve(data = z_rf, none, .pred_0)
    roc_none_rf$specificity <- 1 - roc_none_rf$specificity
    colnames(roc_none_rf) <- c('threshold', 'tpr', 'fpr')
    auc_none_rf <- roc_auc(data = z_rf, none, .pred_0)
    auc_none_rf <- auc_none_rf$.estimate


    lines(roc_none_rf$fpr,roc_none_rf$tpr, type="l",
          xlim=c(0,1), ylim=c(0,1),col=rbCol[2],lwd = 2)
    text(x=0.85,y=0.6,paste0('random forest',' ','(AUC=',toString(round(1-auc_none_rf,2)),')',sep = ''),col=rbCol[2])


    nnet_recipe <- train %>% recipe(y~.) %>%
      step_corr(all_numeric_predictors(),threshold = 0.5) %>%
      step_center(all_numeric_predictors(),-all_outcomes()) %>%
      step_scale(all_numeric_predictors(),-all_outcomes()) %>%
      prep()

    nnet_cv_folds <- bake(nnet_recipe,new_data = train) %>%
      rsample::vfold_cv(v=5)
    nnet_test <- bake(nnet_recipe,new_data = test)

    nnet_model <- mlp(hidden_units = tune(),
                      penalty = tune(),
                      epochs = tune()) %>%
      set_engine("nnet") %>%
      set_mode("classification")

    nnet_grid <- grid_max_entropy(extract_parameter_set_dials(nnet_model),
                                  size=20)

    nnet_workflow <- workflow() %>%
      add_model(nnet_model) %>%
      add_formula(y~.)

    nnet_tunned <- tune_grid(object = nnet_workflow,
                             resamples = nnet_cv_folds,
                             grid = nnet_grid,
                             metrics=metric_set(accuracy,bal_accuracy,
                                                sens,spec,
                                                npv,ppv,precision,
                                                recall,f_meas,
                                                roc_auc,pr_auc),
                             control = control_grid(verbose = F))

    nnet_final_model <- select_best(nnet_tunned,
                                    metric = "accuracy") %>%
      finalize_model(nnet_model,.)

    nnet_test_prediction <- nnet_final_model %>%
      fit(y~.,data = test)

    nnet_pred = predict(nnet_test_prediction,test) %>%
      bind_cols(select(test,y)) %>%
      bind_cols(predict(nnet_test_prediction,test,type = "prob"))

    metric_res[[3]] <- rbind(accuracy(nnet_pred,y,.pred_class),
                             bal_accuracy(nnet_pred,y,.pred_class),
                             sens(nnet_pred,y,.pred_class),
                             spec(nnet_pred,y,.pred_class),
                             npv(nnet_pred,y,.pred_class),
                             ppv(nnet_pred,y,.pred_class),
                             precision(nnet_pred,y,.pred_class),
                             recall(nnet_pred,y,.pred_class),
                             f_meas(nnet_pred,y,.pred_class),
                             roc_auc(nnet_pred,y,.pred_0),
                             pr_auc(nnet_pred,y,.pred_0))

    y_onehot_nnet <- dummy_cols(test$y)
    colnames(y_onehot_nnet) <- c('drop', 'none', 'one')
    y_onehot_nnet <- subset(y_onehot_nnet, select = -c(drop))

    z_nnet = cbind(nnet_pred, y_onehot_nnet)

    z_nnet$none <- as.factor(z_nnet$none)
    roc_none_nnet <- roc_curve(data = z_nnet, none, .pred_0)
    roc_none_nnet$specificity <- 1 - roc_none_nnet$specificity
    colnames(roc_none_nnet) <- c('threshold', 'tpr', 'fpr')
    auc_none_nnet <- roc_auc(data = z_nnet, none, .pred_0)
    auc_none_nnet <- auc_none_nnet$.estimate

    lines(roc_none_nnet$fpr,roc_none_nnet$tpr, type="l",
          xlim=c(0,1), ylim=c(0,1),col=rbCol[3],lwd = 2)
    text(x=0.85,y=0.5,paste0('mlp nnet',' ','(AUC=',toString(round(1-auc_none_nnet,2)),')',sep = ''),col=rbCol[3])


  }else{}
  metric_res

}




