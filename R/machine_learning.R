#' Machine Learning Based on Random Forest for Selecting Variables
#'
#' @param data A data frame containing the dependent variable, which should be a factor, with the column name of "y".
#' @param cv_k The K-fold cross validation method divides the proportion of training and validation sets, such as 5 representing 4 training sets and 1 validation set.
#'
#' @return A plot of contribution of random forest variables to the model; A data frame of accuracy, F1 score (including training and validation sets) after each cross validation.
#' @export
#'
#' @examples colnames(iris)[5] <- "y"; rf_analysis(data=iris,cv_k=4)
#'
rf_analysis <- function(data,cv_k){

  if(!requireNamespace("randomForest",quietly = TRUE)){
    stop("randomForest for the this function to work. Please install it",
         call. = FALSE)
  }

  if(!requireNamespace("pROC",quietly = TRUE)){
    stop("pROC for the this function to work. Please install it",
         call. = FALSE)
  }

  if(!requireNamespace("MLmetrics",quietly = TRUE)){
    stop("MLmetrics for the this function to work. Please install it",
         call. = FALSE)
  }

  add_cv_cohort <- function(dat,cv_k){
    if(nrow(dat)%%cv_k==0){
      dat$cv_cohort <- sample(rep(1:cv_k,each=nrow(dat)%/%cv_k))
    }else{
      dat$cv_cohort <- sample(c(rep(1:(nrow(dat)%%cv_k),each=nrow(dat)%/%cv_k+1),
                                rep((nrow(dat)%%cv_k+1):cv_k,each=nrow(dat)%/%cv_k)))
    }
    return(dat)
  }

  if(is.numeric(data$y)==TRUE){

  }else{

    machinedata_cv <- add_cv_cohort(data,cv_k)
    rf.grid <- expand.grid(nt = seq(100, 500, by = 100),
                           mrty = c(1, 2, 3, 4, 5))
    rf.grid$acc <- rf.grid$f1 <- NA

    rf.f1 <- rf.acc <- c()

    set.seed(1234)
    for (k in 1:nrow(rf.grid)) {
      for (i in 1:cv_k) {
        indexs <- which(machinedata_cv$cv_cohort==i)
        train <- machinedata_cv[-indexs,]
        test <- machinedata_cv[indexs,]

        train <- train[,-ncol(train)]
        test <- test[,-ncol(test)]

        rf.model <- randomForest(y~., data = train,
                                 n.trees = rf.grid$nt[k],
                                 mtry = rf.grid$mrty[k])
        rf.pred <- predict(rf.model, test)

        rf.f1[i] <- F1_Score(test$y, rf.pred)
        rf.acc[i] <- sum(rf.pred == test$y)/nrow(test)
      }
      rf.grid$f1[k] <- mean(rf.f1)
      rf.grid$acc[k] <- mean(rf.acc)
      print(paste("finished with:", k))
    }

    ntree <- rf.grid$nt[which.max(rf.grid$acc)]
    mtry <- rf.grid$mrty[which.max(rf.grid$acc)]

    risk_factor <- list()
    train_f1 <- train_acc <- c()
    test_f1 <- test_acc <- c()

    par(mfcol=c(2,cv_k),mar=c(1,1,1,1))
    p <- for (i in 1:cv_k) {
      indexs <- which(machinedata_cv$cv_cohort==i)
      train <- machinedata_cv[-indexs,]
      test <- machinedata_cv[indexs,]

      train <- train[,-ncol(train)]
      test <- test[,-ncol(test)]

      model <- randomForest(y~.,
                            data = train,
                            n.trees = ntree,
                            mtry = mtry)
      varImpPlot(model,main=paste0("cross validation"," ",i),
                 lwd=2,cex.main=1.2,
                 cex.axis=1.2,cex.lab=1.2)
      risk_factor[[i]] <- as.data.frame(model$importance)

      pred1 <- predict(model,train)
      pred1
      plot(roc(as.numeric(pred1),
               as.numeric(train$y)),
           col="red",lwd=2,cex.main=1.2,
           cex.axis=1.2,cex.lab=1.2)
      lr.auc <- auc(roc(as.numeric(pred1),
                        as.numeric(train$y)))
      text(0.3, 0.4, labels=paste('AUC=', round(lr.auc, 3), sep=''), cex=1.5,col="red")

      pred2 <- predict(model,test)
      plot(roc(as.numeric(pred2),
               as.numeric(test$y)),
           col="blue",lwd=2,cex.main=1.2,
           cex.axis=1.2,cex.lab=1.2,add=TRUE)
      lr.auc <- auc(roc(as.numeric(pred2),
                        as.numeric(test$y)))
      text(0.3, 0.2, labels=paste('AUC=', round(lr.auc, 3), sep=''), cex=1.5,col="blue")

      train_f1[i] <- F1_Score(train$y, pred1)
      train_acc[i] <- sum(pred1 == train$y)/nrow(train)

      test_f1[i] <- F1_Score(test$y, pred2)
      test_acc[i] <- sum(pred2 == test$y)/nrow(test)

    }

  }
  risk_factor
  results <- cbind(train_f1,train_acc,test_f1,test_acc)
  colnames(results) <- c("train_f1","train_acc","test_f1","test_acc")
  rownames(results) <- paste0(rep("cross validation"),"_",1:cv_k)
  results
}


#' Machine Learning Based on regressive analysis for evaluate model
#'
#' @param data A data frame containing the dependent variable, which should be a numeric on the last column, with the column name of "y".
#' @param cv_k The K-fold cross validation method divides the proportion of training and validation sets, such as 5 representing 4 training sets and 1 validation set.
#'
#' @return A plot of actual and predicted values of dependent variable after each cross validation; A data frame of mse, mae, rmse, R2 (including training and validation sets) after each cross validation.
#' @export
#'
#' @examples data <- MASS::Boston; colnames(data)[ncol(data)] <- "y"; glmnet_analysis(data=data,cv_k=5)
glmnet_analysis <- function(data,cv_k){

  if(!requireNamespace("glmnet",quietly = TRUE)){
    stop("glmnet for the this function to work. Please install it",
         call. = FALSE)
  }

  if(!requireNamespace("caret",quietly = TRUE)){
    stop("caret for the this function to work. Please install it",
         call. = FALSE)
  }

  add_cv_cohort <- function(dat,cv_k){
    if(nrow(dat)%%cv_k==0){
      dat$cv_cohort <- sample(rep(1:cv_k,each=nrow(dat)%/%cv_k))
    }else{
      dat$cv_cohort <- sample(c(rep(1:(nrow(dat)%%cv_k),each=nrow(dat)%/%cv_k+1),
                                rep((nrow(dat)%%cv_k+1):cv_k,each=nrow(dat)%/%cv_k)))
    }
    return(dat)
  }

  machinedata_cv <- add_cv_cohort(dat = data,cv = cv_k)

  train_mse <- train_mae <- train_rmse <- train_r2 <- c()
  test_mse <- test_mae <- test_rmse <- test_r2 <- c()

  par(mfcol=c(2,cv_k))
  for (i in 1:cv_k) {
    indexs <- which(machinedata_cv$cv_cohort==i)
    train <- machinedata_cv[-indexs,]
    test <- machinedata_cv[indexs,]

    train <- train[,-ncol(train)]
    test <- test[,-ncol(test)]

    x1=as.matrix(train[,-ncol(train)])
    y1=as.matrix(train[,ncol(train)])

    x2=as.matrix(test[,-ncol(test)])
    y2=as.matrix(test[,ncol(test)])

    lasso.model <- glmnet(x1,y1,
                          nlambda = 100,
                          family = "gaussian",
                          alpha = 1)
    lambda <- lasso.model$lambda[which.min(lasso.model$lambda)]

    model <- glmnet(x1,y1,
                    lambda = lambda,
                    family = "gaussian",
                    alpha = 1)

    pred1 <- predict(model,x1)

    train_mse[i] <- round(mean((y1-pred1)^2),4)
    train_mae[i] <- round(MAE(y1,pred1),4)
    train_rmse[i] <- round(RMSE(y1,pred1),4)
    train_r2[i] <- round(R2(y1,pred1,form="traditional"),4)

    cross_Validation_i = 1:length(y1)
    plot(cross_Validation_i, y1, ylim=c(min(pred1), max(y1)),
         pch=20, lwd=2, cex.main=1.2, col="red",
         cex.axis=1.2,cex.lab=1.2,
         xlab = paste0("Number of training set"," ",i),ylab = "value",
         main=paste0("Cross Validation","_",i))
    lines(cross_Validation_i, pred1, lwd="1", col="blue")


    pred2 <- predict(model,x2)

    test_mse[i] <- mean((y2-pred2)^2)
    test_mae[i] <- MAE(y2,pred2)
    test_rmse[i] <- RMSE(y2,pred2)
    test_r2[i] <- R2(y2,pred2,form="traditional")

    cross_Validation_i = 1:length(y2)
    plot(cross_Validation_i, y2, ylim=c(min(pred2), max(y2)),
         pch=20, lwd=2, cex.main=1.2, col="red",
         cex.axis=1.2,cex.lab=1.2,
         xlab = paste0("Number of Testing set"," ",i),ylab = "value",
         main=paste0("Cross Validation","_",i))
    lines(cross_Validation_i, pred2, lwd="1", col="blue")

  }
  results <- cbind(train_mse,train_mae,train_rmse,train_r2,
                   test_mse,test_mae,test_rmse,test_r2)
  results

}


