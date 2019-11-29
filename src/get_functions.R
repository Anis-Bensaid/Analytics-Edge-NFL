generate_formula <- function(dataframe, i) {
  col_names <- colnames(dataframe)
  
  x_colnames <- c()
  predict_cols <- c()
  
  for (name in colnames(dataframe)) {
    if (grepl(paste("Pred_", toString(i), sep=""), name)) {
      predict_cols <- append(predict_cols, name)
    } else if (!grepl("Yard_", name) && !grepl("Pred_", name)) {
      x_colnames <- append(x_colnames, name)
    }
  }
  
  formula_string <- paste(paste("factor(Yard_", toString(i), sep=""), ") ~", paste(x_colnames, collapse=" + "))
  if (!is_empty(predict_cols)) {
    formula_string <- paste(formula_string, "+", paste(predict_cols, collapse=" + "))
  }
  return(as.formula(formula_string))
}


train_models = function(data, method, metric="Accuracy", tuneGrid=NULL, trControl=NULL, tuneLength=3, thresh_hold=0){
  Y_COL_NAME = "Yard_"
  nb.yards = sum(grepl(paste(Y_COL_NAME,"\\d",sep=""), colnames(data)))
  nrows.data = nrow(data)
  models <- vector(mode="list", length=nb.yards)
  for (i in 1:nb.yards) {
    target_col = paste(Y_COL_NAME, toString(i), sep="")
    if (sum(as.numeric(data[,target_col])) > thresh_hold){
      form <- generate_formula(data, i)
      mod <- train(form = form, 
                   data = data, 
                   #subset = subset,
                   metric = metric, 
                   method = method, 
                   tuneGrid = tuneGrid, 
                   trControl= trControl,
                   tuneLength = tuneLength
      )
      models[[i]] = mod
      print(paste("Model", toString(i), "trained."))
    } else {
      print(paste("Yardage", toString(i), "is equal to 0 in all the training set."))
    }
  }
  models
}


get_predictions = function(models, data, model_name="Model"){
  nrows.data = nrow(data)
  nb.yards = length(models)
  predictions = matrix(NA, nrow=nrows.data)
  colnames.predictions = c()
  for (i in 1:nb.yards) {
    if (is.null(models[[i]])) {
      predictions = cbind(predictions, matrix(0, nrow=nrows.data))
    } else {
      mod = models[[i]]
      predictions = cbind(predictions, predict(mod, newdata=data, type="prob")[,2])
    }
    colnames.predictions = c(colnames.predictions, paste(model_name,"_Pred_",toString(i), sep=""))
  }
  predictions = data.frame(predictions[,2:ncol(predictions)])
  colnames(predictions) <- colnames.predictions
  predictions
}

normalise_predictions = function(predictions){
  predictions/rowSums(predictions) 
}

get_distribution = function(predictions){
  t(apply(predictions, 1, cumsum))
}

get_score = function(distribution, heaviside){
  mean(rowMeans((distribution - heaviside)^2))
}

