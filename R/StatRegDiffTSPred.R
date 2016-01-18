#' Method to predict according to the stational difference time series model
#'
#' This method implements the predicted value and their standard deviation
#' according to the regular difference time series model 
#' \eqn{(1-B)^s y_{t}=a_{t}}{(1-B)^s y<sub>t</sub>=a<sub>t</sub>}
#'
#' @param x object upon which the prediction will be made
#'
#' @param StatDiff stational difference s of the time series; by default it is 12L
#'
#' @param forward integer indicating the number of periods ahead when the 
#' prediction will be made; by default it is 2L
#' 
#' @param VarNames character vector with the variable names for which the 
#' prediction will be made; by default it is NULL
#' 
#' @param keyVar character vector with the variable names used as key variables
#' in data.table operations; by default it is NULL
#'
#' @return It returns a list with components Pred and STD, containing the point
#' prediction and the estimated standard deviations, respectively. Depending
#' on the class of the input parameter x, it returns:  
#' 
#' \itemize{
#'  \item For input class vector, it returns numeric vectors
#'  \item For input class matrix, it returns matrices
#'  \item For input class data.table, it returns data.tables
#'  \item For input class StBusQ, it returns data.tables
#'  \item For input class GenStBusQ, it returns data.tables
#' }
#'
#' @examples
#' 
#' # Predicting one and two months ahead in time
#' data(Example1.TS)
#' StatRegDiffTSPred(Example1.TS, forward = 1L)
#' StatRegDiffTSPred(Example1.TS, forward = 2L)
#' 
#' # Predicting upon a times series with many NA values
#' data(Example2.TS)
#' StatRegDiffTSPred(Example2.TS, forward = 1L)
#' 
#' # On a matrix
#' Mat <- rbind(Example1.TS, Example2.TS)
#' StatRegDiffTSPred(Mat, forward = 1L)
#' 
#' @export
setGeneric("StatRegDiffTSPred", function(x,  StatDiff = 12L, forward = 2L, 
                                      VarNames = NULL, keyVar = NULL){
  standardGeneric("StatRegDiffTSPred")})

#' @rdname StatRegDiffTSPred
#' 
#' @export
setMethod(
  f = "StatRegDiffTSPred",
  signature = c("vector"),
  function(x,  StatDiff = 12L, forward, VarNames = NULL, keyVar = NULL){
    
    x <- as.numeric(x)
    
    if (length(x) == 0 | all(is.na(x))) return(list(Pred = as.numeric(NA), STD = as.numeric(NA)))
    
    index.1 <- length(x)
    index.s <- length(x) + 1L - StatDiff
    ahead <- 0
    NA.flag <- F
    if (index.s <= 1) NA.flag <- T
    while (NA.flag == F && (is.na(x[index.1]) | is.na(x[index.s]) | is.na(x[index.s - 1L])) ){
      index.1 <- index.1 - 1L
      index.s <- index.s - StatDiff
      ahead <- ahead + 1L
      if (index.s <= 1) NA.flag <- T
    }
    
    output <- list(Pred = ifelse(!NA.flag, x[index.1] + x[index.s] - x[index.s - 1L], NA))
    #x <- c(x, output$Pred)
    
    if (!all(is.na(x)) && !all(x[!is.na(x)] == 0)) {
      for (i in seq(along = x)){
        if (is.na(x[i])) next
        if (x[i] == 0) {
          x[i] <- NA
        } else break
      }
    }
    
    d.x <- diff(x, lag = 1L)
    dsd.x <- diff(d.x, lag = StatDiff)
    if (length(dsd.x) == 0) {
      output[['STD']] <- NA
    } else {
      std <- sqrt(sum(dsd.x * dsd.x, na.rm = T)/length(dsd.x[!is.na(dsd.x)]))
      output[['STD']] <- ifelse(!NA.flag, std, NA)
    }
    
    ahead <- ahead + forward
    if (forward >= 2L){
      output <- StatRegDiffTSPred(x, StatDiff = StatDiff, forward = forward - 1L)
      output[['STD']] <- ahead * output[['STD']]
    } 
    return(output)
  }
)
#' @rdname StatRegDiffTSPred
#' 
#' @export
setMethod(
  f = "StatRegDiffTSPred",
  signature = c("matrix"),
  function(x,  StatDiff = 12L, forward, VarNames = NULL, keyVar = NULL){
    
    output <- apply(x, 1, StatRegDiffTSPred, StatDiff = StatDiff, forward = forward)
    output <- Reduce(rbind, output)
    out <- list(Pred = Reduce(rbind, output[, 1]), STD = Reduce(rbind, output[, 2]))
    out <- lapply(out, function(mat){
      dimnames(mat)[[1]] <- dimnames(x)[[1]]
      return(mat)
    }
    )
    return(out)       
  }
)
#' @rdname StatRegDiffTSPred
#' 
#' @export
setMethod(
  f = "StatRegDiffTSPred",
  signature = c("list"),
  function(x,  StatDiff = 12L, forward, VarNames = NULL, keyVar = NULL){
    
    ElemClass <- unique(unlist(lapply(x, class)))
    if (length(ElemClass) != 1 | !ElemClass %in% c('data.table', 'StBusQ', 'GenStBusQ')) stop('[StatRegDiffTSPred list] x must be a list of data.tables, StBusQs or GenStBusQs.')
    
    if (length(VarNames) == 0) stop('[StatRegDiffTSPred list] Debe especificar VarNames.')
    
    if (ElemClass == 'data.table'){
      
      ValidUnits <- x[[length(x)]][, keyVar, with = F]
      
      Data.list <- lapply(x, setkeyv, keyVar)
      Data.list <- lapply(Data.list, function(x){x[ValidUnits, c(keyVar, VarNames), with = F]})
      Data.list <- rbindlist(Data.list)
      setkeyv(Data.list, keyVar)
      
      output.DT <- Data.list[, lapply(.SD, StatRegDiffTSPred, StatDiff = StatDiff, 
                                      forward = forward), .SDcols = VarNames, by = keyVar]
      
      output <- list(Pred = output.DT[seq(1, dim(output.DT)[[1]], by = 2)],
                     STD = output.DT[seq(2, dim(output.DT)[[1]], by = 2)])
    }
    
    if (ElemClass %in% c('StBusQ', 'GenStBusQ')){
      
      ValidUnits <- x[[length(x)]]@Data[, keyVar, with = F]
      ColNames <- names(x[[length(x)]]@Data)
      setkeyv(ValidUnits, keyVar)
      ValidUnits <- ValidUnits[!duplicated(ValidUnits)]
      Data.list <- lapply(x, function(Q){
        
        setkeyv(Q@Data, keyVar)
        out <- Q@Data[ValidUnits]
        setkeyv(out, 'IDDD')
        out <- out[VarNames]
        setcolorder(out, ColNames)
        return(out)
        
      })
      Data.list <- rbindlist(Data.list)
      setkeyv(Data.list, c(keyVar, 'IDDD'))
      Data.list[, Valor := ifelse(Valor == '', NA_real_, as.numeric(Valor))]
      output.DT <- Data.list[, lapply(.SD, StatRegDiffTSPred, StatDiff = StatDiff, 
                                      forward = forward), .SDcols = 'Valor', by = c(keyVar, 'IDDD')]
      output.Var <- split(output.DT, output.DT[['IDDD']])
      output <- list()
      output[['Pred']] <- output.Var[[1]][seq(1, dim(output.Var[[1]])[[1]], by = 2), keyVar, with = F]
      output[['STD']] <- output.Var[[1]][seq(2, dim(output.Var[[1]])[[1]], by = 2), keyVar, with = F]    
      for(i in seq(along = VarNames)){
        
        output[['Pred']][, VarNames[i] := output.Var[[i]][seq(1, dim(output.Var[[i]])[[1]], by = 2), 'Valor', with = F], with = F]
        output[['STD']][, VarNames[i] := output.Var[[i]][seq(2, dim(output.Var[[i]])[[1]], by = 2), 'Valor', with = F], with = F]
      }
    }
    return(output)
  }
)