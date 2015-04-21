

#' @import openxlsx
#' @export
create_excel <- function(object,...) UseMethod("create_excel")

#'@export
create_excel.glm<-function(object, filename="glmmodel.xlsx"){
  wb <- createWorkbook()  
  addWorksheet(wb, "Sheet1")
  sr=1
  sc=1
  varlist=attr(object$terms, "term.labels")
  print(varlist)
  for (variables in varlist){
    vv=eval(call("model.frame", as.formula(paste("~", variables, sep="")), object$call$data), envir=parent.frame())
    if(is.factor(vv[[variables]])){
      vv=unique(vv[[variables]])
      vv=paste(variables, vv[order(vv)], sep="")
    }else{
      vv=range(vv[[variables]])
      vv=vv[order(vv)]
    }    
    writeData(wb, sheet = "Sheet1", variables, startRow=sr, startCol=sc)
    writeData(wb, sheet = "Sheet1", vv, startRow=sr+1, startCol=sc)
    sc=sc+1    
  }
  addWorksheet(wb, "Coeff")
  writeData(wb, sheet = "Coeff", data.frame(variables=names(object$coefficients), Coeff=object$coefficients), startRow=1, startCol=1)
  
  ExcelCoeffTable=paste("Coeff!", "A1:B", length(object$coefficients)+1, sep="")
  addWorksheet(wb, "Estimation")
  sr = 1
  sc = 1
  for (variables in varlist){
    
    if(is.factor(eval(call("model.frame", as.formula(paste("~", variables, sep="")), object$call$data), envir=parent.frame())[[1]])){
      writeData(wb, sheet="Estimation", variables, startRow=sr, startCol=sc)
      writeData(wb, sheet="Estimation", paste("=VLOOKUP(B",sr, ",", ExcelCoeffTable, ",2, FALSE)", sep=""), startRow=sr, startCol=sc+2)      
    }else{
      writeData(wb, sheet="Estimation", variables, startRow=sr, startCol=sc)
      writeData(wb, sheet="Estimation", paste("=", "B", sr, "*VLOOKUP(A",sr, ",", ExcelCoeffTable, ",2, FALSE)", sep=""), startRow=sr, startCol=sc+2)  
    } 
    sr=sr+1  
    
  }
  
  saveWorkbook(wb, file = filename, overwrite = TRUE)
  
}