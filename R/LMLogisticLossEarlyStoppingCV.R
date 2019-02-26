LMLogisticLossEarlyStoppingCV <- function(
  X.mat,
  y.vec,
  fold.vec,
  max.iterations
){
  step_size <- 0.1
  for(fold.i in fold.vec)
  {
    validation_indices <- which(fold.vec %in% c(fold.i))
    validation_set <- X.mat[validation_indices,]
    train_set <- X.mat[-validation_indices,]
    train_labels <- y.vec[-validation_indices]
    validation_labels <- y.vec[validation_indices] 
    n_rows_validation_set <- nrow(validation_set)
    n_rows_train_set <- nrow(train_set)
    
    W <- LMSquareLossIterations(train_set,train_labels, max.iterations, step_size)
    for(prediction.set.name in c("train", "validation")){
      if(identical(prediction.set.name, "train")){
        to.be.predicted <- train_set
      }
      else{
        to.be.predicted <- validation_set
      }
      pred <- cbind(1,to.be.predicted) %*% W 
      if(identical(prediction.set.name, "train")){
        loss.mat <-ifelse(pred.mat>0.5, 1, 0) != train_labels
        train.loss.mat[,fold.i] <- colMeans(loss.mat)
      }
      else{
        loss.mat <-ifelse(pred.mat>0.5, 1, 0) != validation_labels
        validation.loss.mat[,fold.i] <- colMeans(loss.mat)
      }
    }
  }
    mean.validation.loss.vec <- colMeans(validation.loss.mat)
    selected.steps <- which.min(mean.validation.loss.vec)
    w.head <- LMSquareLossIterations(X.mat,y.vec, selected.steps, step_size)
    weight_vec <- w.head[selected.steps,]
}