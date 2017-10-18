##############################
#
# testscript to play with git#
#
###############################


# this is some text

#this is a next line of text  


### this is an examplefunction ###

part.predict <- function(modObj, X) {
  
  #predict y for each predictor, holding all others at their mean
  #IN: X: dataframe with predictors
  
  mn.or.mod <- function(col) {
    if (!is.factor(col)) {    
      return(rep(mean(col),length(col))) #mean for continuous variables
    } else  return(rep( factor(modal(col),levels=levels(col)),length(col)))  #modal for factors
  }  
  
  part.predict.colwise <- function(xCol,mod) {
    #set all columns to their mean / modal values
    i =1
    newDat=X
    for (i in 1:ncol(newDat)) {
      newDat[,i] <- (mn.or.mod(newDat[,i])) }
    nam <- names(newDat)
    #create sequence in xCol for prediction
    newDat[,xCol]  <- seq.pred(X[,xCol])
    names(newDat) <- nam 
    #predict y for xCol holding all other values constant
    if (class(mod)[1] == "gbm") {
      y_hat  <- data.frame( predict(mod,newdata=newDat,type="response",n.trees=mod$gbm.call$best.trees) )
    } else {
      y_hat  <- data.frame( predict(mod,newdata=newDat,type="response") )  
    }
    return(y_hat)
  }
  partY <- data.frame( lapply(seq(1:ncol(X)),part.predict.colwise,modObj) )
  colnames(partY) <- names(X)
  return(partY)
}
