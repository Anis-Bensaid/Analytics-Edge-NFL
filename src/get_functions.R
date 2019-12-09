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
                   tuneLength = tuneLength,
                   na.action = na.exclude
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

# Plot distribution and heaviside together for a given row i in the data
plot_d_h <- function(distrib, heavy, i, yards=-99:99) {
  ggplot() +
    geom_line(aes(y=distrib[i,], x=yards, color="Distribution")) +
    geom_line(aes(y=heaviside[i,], x=yards, color="Heaviside")) +
    labs(title="CDF and Heaviside Function", x="Yards", y="Probability", color="Function") +
    theme(plot.title = element_text(hjust=0.5),
          axis.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=14), 
          title=element_text(size=15))
}

# Plot distribution data alone
# This can be the average distribution over all rows, or just for a single row i
 plot_distrib <- function(distrib, i=1, yards=-99:99, mean=FALSE){
   if (mean==FALSE){
     ggplot() +
       geom_line(aes(y=distrib[i,], x=yards)) +
       labs(title=paste("CDF for Row", toString(i), sep=" "), x="Yards", y="CDF") +
       theme(plot.title = element_text(hjust=0.5),
             axis.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=14), 
             title=element_text(size=15))
   } else {
     ggplot()+
       geom_line(aes(y=colMeans(distrib), x=yards)) +
       labs(title="Average CDF over all Rows", x="Yards", y="CDF") +
       theme(plot.title = element_text(hjust=0.5),
             axis.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=14), 
             title=element_text(size=15))
   }
 }
 
 # Plot heavyside alone for a single row i
 plot_heavy <- function(heavy, i, yards=-99:99) {
   ggplot()+
     geom_line(aes(y=heavy[i,], x=yards)) +
     labs(title=paste("Heaviside for Row", toString(i), sep=" "), x="Yards", y="Heaviside", color="Function") +
     theme(plot.title = element_text(hjust=0.5),
           axis.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=14), 
           title=element_text(size=15))
 }



