# '@export 
md_reform<-function(object, ...)UseMethod("md_reform")

# '@export
md_reform.glm<-function(object){
  
  terms = attr(object$term,"term.labels")
  sterms=terms(!grepl(":", terms))
  xterms=terms(grepl(":", terms))
  xterms=unlist(strsplit(xterms, ":"))
  allterms=unique(c(sterms, xterms))
  valterms=list()
  
  for ( vars in allterms){
    pdtr_val = eval(call("model.frame", as.formula(paste("~", vars, sep="")), object$call$data), envir=parent.frame())
    pdtr_val = pdtr_val[[1]] 
    if(is.factor(pdtr_val)){
      pdtr_val=levels(pdtr_val)
    }else{
      pdtr_val=range(pdtr_val)
    }
    valterms=c(valterms, pdtr_val)        
  }
  id_pred=seq(1, length(allterms))
  
  add_excel=NULL
  for (vars in sterms){
    pdtr_val = eval(call("model.frame", as.formula(paste("~", vars, sep="")), object$call$data), envir=parent.frame())
    pdtr_val = pdtr_val[[1]] 
    if(is.factor(pdtr_val)){
      for (levelv in levels(pdtr_val)){
        excel_val=paste("=if(VLOOKUP(", vars, ",Table, 2)==", levelv, "1, 0)", sep="")
        add_excel=c(add_excel, excel_val)
      }
      }else{
        excel_val="=VLOOKUP(", vars, ",Table, 2)"
      }      
  }
  for(vars in xterms){
    var1=unlist(strsplit(xterms, ":"))[1]
    var2=unlist(strsplit(xterms, ":"))[1]
    pdtr_val1 = eval(call("model.frame", as.formula(paste("~", var1, sep="")), object$call$data), envir=parent.frame())
    pdtr_val1 = pdtr_val1[[1]]     
    pdtr_val2 = eval(call("model.frame", as.formula(paste("~", var2, sep="")), object$call$data), envir=parent.frame())
    pdtr_val2 = pdtr_val2[[1]] 
    if(is.factor(pdtr_val1)&is.factor(pdtr_val2)){
      for (levelv in levels(pdtr_val1:pdtr_val2)){
        excel_val=paste("=if(VLOOKUP(", vars, ",Table, 2)==", levelv, "1, 0)", sep="")
        add_excel=c(add_excel, excel_val)
      }
    }else if (is.numeric(pdtr_val1)&is.numeric(pdtr_val2)){
      excel_val="=VLOOKUP(", var1, ",Table, 2)"
      add_excel=c(add_excel, excel_val)
    }
    
  }
    
  }
  md=list(predictors=allterms, id_pred=id_pred, v_predictors=valterms, object$coefficients)
  class(md) = "glm_reform"
  return(md)
}