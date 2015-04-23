#' @export 
md_reform<-function(object, ...)UseMethod("md_reform")

#' @export
md_reform.glm<-function(object){
  terms = attr(object$terms,"term.labels")
  sterms=terms[!grepl(":", terms)]
  xterms=terms[grepl(":", terms)]
  xterms_split=unlist(strsplit(xterms, ":"))
  allterms=unique(c(sterms, xterms_split))
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
  
  print(sterms)
  md_mat_val=NULL
  for (vars in sterms){
    pdtr_val = eval(call("model.frame", as.formula(paste("~", vars, sep="")), object$call$data), envir=parent.frame())
    pdtr_val = pdtr_val[[1]] 
    if(is.factor(pdtr_val)){
      for (levelv in levels(pdtr_val)){
        i_md_mat_val=create_vlookup_text(vars, "factor", levelv)
        md_mat_val=c(md_mat_val, i_md_mat_val)
      }
      }else{
        
        i_md_mat_val=create_vlookup_text(vars, "numeric")
        md_mat_val=c(md_mat_val, i_md_mat_val)
      }      
  }
  print(xterms)
  for(vars in xterms){
    var1=unlist(strsplit(vars, ":"))[1]
    var2=unlist(strsplit(vars, ":"))[2]
    pdtr_val1 = eval(call("model.frame", as.formula(paste("~", var1, sep="")), object$call$data), envir=parent.frame())
    pdtr_val1 = pdtr_val1[[1]]   
    pdtr_val2 = eval(call("model.frame", as.formula(paste("~", var2, sep="")), object$call$data), envir=parent.frame())
    pdtr_val2 = pdtr_val2[[1]] 
    if(is.factor(pdtr_val1)&is.factor(pdtr_val2)){
      for (levelv in levels(pdtr_val1:pdtr_val2)){
        i_md_mat_val=create_vlookup_text(vars, "factor", levelv)
        md_mat_val=c(md_mat_val, i_md_mat_val)
      }
    }else if (is.numeric(pdtr_val1)&is.numeric(pdtr_val2)){
      i_md_mat_val=paste(create_vlookup_text(var1, "numeric"), create_vlookup_text(var2, "numeric", ss=""), sep="*")
      md_mat_val=c(md_mat_val, i_md_mat_val)
    }else{
      if(!is.null(levels(pdtr_val1))){
        for (levelv in levels(pdtr_val1)){
          i_md_mat_val=paste(create_vlookup_text(var1, "factor", levelv),create_vlookup_text(var2, "numeric", ss=""), sep="*")        
          md_mat_val=c(md_mat_val, i_md_mat_val)
        }
      }else{
        for (levelv in levels(pdtr_val2)){
          i_md_mat_val=paste(create_vlookup_text(var2, "factor", levelv),create_vlookup_text(var1, "numeric", ss=""), sep="*")
          md_mat_val=c(md_mat_val, i_md_mat_val)
        }
      }
    }
  }
  md=list(predictors=allterms, id_pred=id_pred, v_predictors=valterms, Coeff=object$coefficients, model_matrix_value=md_mat_val)
  class(md) = "glm_reform"
  return(md)    
}

#' @export
create_vlookup_text<-function(variable, mode, value='TBD', ss='='){
  if(mode!="factor"&mode!="numeric")
  {
    stop("mode has to be factor or numeric")
  }
  if(mode=="factor"){
    return(paste(ss, 'if(VLOOKUP("', variable, '", Table,2)=', '"', value, '"',  ', 1, 0)', sep=""))
  }
  if(mode=="numeric")
  {
    return(paste(ss, 'VLOOKUP("', variable, '",Table, 2)', sep=""))
  }  
}
