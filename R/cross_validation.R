
#' 
#'@import 
#'@export
#'@example
#'utils::data(anorexia, package = "MASS")
#'anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt), family = gaussian, data = anorexia)
#'kfold_validation(anorex.1, 10)
kfold_validation <- function(object,...) UseMethod("kfold_validation")

kfold_validation.glm<-function(object, k){
  nr=eval(call("dim",  object$call$data), envir=parent.frame())[1]
  group_index = split(1:nr, cut(1:nr, k))
  valid_prediction=NULL
  for (i in seq(1, length(group_index))){
    train_index=unlist(group_index[-i])
    #print(train_index)
    #print(class(train_index))
    test_index=unlist(group_index[i])    
    data_train = eval(call("[", object$call$data), envir = parent.frame())[train_index, ]
    #print(dim(data_train))
    data_test = eval(call("[", object$call$data), envir = parent.frame())[test_index, ]
    train_model = eval(call("glm",object$call$formula, data=data_train, family=anorex.1$call$family))
    valid_prediction = c(valid_prediction, predict(train_model, data_test, type="response"))     
  }
  return(valid_prediction)
}
