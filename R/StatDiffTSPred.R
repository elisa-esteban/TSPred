#' @title Method to predict according to the stational difference time series
#' model.
#'
#' @description This method implements the predicted value and their standard
#' deviation according to the stational difference time series model
#' \eqn{(1-B)^s (1-B)y_{t}=a_{t}}{(1-B)^s (1-B)y<sub>t</sub>=a<sub>t</sub>}.
#'
#' @param x object upon which the prediction will be made.
#'
#' @param StatDiff stational differences of the time series; by default it is
#' 12L.
#'
#' @param forward integer indicating the number of periods ahead when the
#' prediction will be made; by default it is 2L.
#'
#' @param VarNames character vector with the variable names for which the
#' prediction will be made; by default it is NULL
#'
#' @return It returns a list with components Pred and STD, containing the point
#' prediction and the estimated standard deviations, respectively. Depending
#' on the class of the input parameter x, it returns:
#'
#' \itemize{
#'  \item For input class vector, it returns numeric vectors.
#'  \item For input class matrix, it returns matrices.
#'  \item For input class StQList, it returns list whose components are
#'   data.tables.
#' }
#'
#' @examples
#'
#' # Predicting one and two months ahead in time
#' data(Example1.TS)
#' StatDiffTSPred(Example1.TS, forward = 1L)
#' StatDiffTSPred(Example1.TS, forward = 2L)
#'
#' # Predicting upon a times series with many NA values
#' data(Example2.TS)
#' StatDiffTSPred(Example2.TS, forward = 1L)
#'
#' # On a matrix
#' Mat <- rbind(Example1.TS, Example2.TS)
#' StatDiffTSPred(Mat, forward = 1L)
#'
#' @export
setGeneric("StatDiffTSPred", function(x,  StatDiff = 12L, forward = 2L,
                                      VarNames = NULL){
  standardGeneric("StatDiffTSPred")})

#' @rdname StatDiffTSPred
#'
#' @export
setMethod(
  f = "StatDiffTSPred",
  signature = c("vector"),
  function(x,  StatDiff = 12L, forward = 2L, VarNames = NULL){

    x <- as.numeric(x)

    if (length(x) <= (StatDiff - 1L) | all(is.na(x))){

         return(list(Pred = as.numeric(NA), STD = as.numeric(NA)))
    }

    index <- length(x) + 1L - StatDiff
    ahead <- 0
    while (is.na(x[index])){
      index <- index - StatDiff
      ahead <- ahead + 1L
      if (index <= 0) return(list(Pred = as.numeric(NA), STD = as.numeric(NA)))
    }

    output <- list(Pred = x[index])

    if (!all(is.na(x)) && !all(x[!is.na(x)] == 0)) {
      for (i in seq(along = x)){
        if (is.na(x[i])) next
        if (x[i] == 0) {
          x[i] <- NA
        } else break
      }
    }
    ds.x <-diff(x, lag = StatDiff)
    std <- sqrt(mean(ds.x * ds.x, na.rm = T))
    output[['STD']] <- std

    ahead <- ahead + forward
    if (forward >= 2L){
      output <- StatDiffTSPred(x, StatDiff = StatDiff, forward = forward - 1L)
      names(output[['Pred']]) <- NULL
      output[['STD']] <- ahead * output[['STD']]

    }
    return(output)
  }
)
#' @rdname StatDiffTSPred
#'
#' @export
setMethod(
  f = "StatDiffTSPred",
  signature = c("matrix"),
  function(x,  StatDiff = 12L, forward, VarNames = NULL){

    output <- apply(x, 1, StatDiffTSPred, StatDiff = StatDiff, forward = forward)
    output <- Reduce(rbind, output)
    out <- list(Pred = Reduce(rbind, output[, 1]),
                STD = Reduce(rbind, output[, 2]))
    out <- lapply(out, function(mat){
      dimnames(mat)[[1]] <- dimnames(x)[[1]]
      return(mat)
    }
    )
    return(out)
  }
)
#' @rdname StatDiffTSPred
#'
#' @import StQList
#'
#' @export
setMethod(
  f = "StatDiffTSPred",
  signature = c("StQList"),
  function(x,  StatDiff = 12L, forward, VarNames = NULL){

    if (length(VarNames) == 0) stop('[StatDiffTSPred StQList] Debe especificar VarNames.')

      Data.list <- getData(x, VarNames)

      keyVar <- vector('list', length(VarNames))
      keyVar <- lapply(keyVar, function(x) {
          setdiff(names(Data.list[[length(Data.list)]]), c('IDDD', 'Value'))})

      for (i in 1:length(keyVar)){

          key <- keyVar[[i]]
          keyQual <- key
          nQual <- length(key)
          for (j in 1:nQual){
              if (all(Data.list[[length(Data.list)]][IDDD == VarNames[i]][, key[j], with = F] == '')){
                  keyQual <- setdiff(keyQual, key[j])
              }
          }
          keyVar[[i]] <- keyQual
      }

      ValidUnits <- Data.list[[length(Data.list)]][, unlist(keyVar), with = F]
      setkeyv(ValidUnits, unlist(keyVar))
      ValidUnits <- ValidUnits[!duplicated(ValidUnits)]
      Data.list <- lapply(Data.list, function(Data){

          Data <- Data[, c(unlist(keyVar), 'IDDD', 'Value'), with = F]
          setkeyv(Data, unlist(keyVar))
          out <- Data[ValidUnits]
          setkeyv(out, 'IDDD')
          out <- out[VarNames]
          return(out)

      })
      Data.list <- rbindlist(Data.list)
      setkeyv(Data.list, c(unlist(keyVar), 'IDDD'))
      Data.list[, Value := ifelse(Value == '', NA_real_, as.numeric(Value))]

      output.DT <- vector('list', length(VarNames))
      output <- vector('list', length(VarNames))
      for (i in 1:length(VarNames)){

          output.DT[[i]] <- Data.list[, lapply(.SD, StatDiffTSPred,
                                               StatDiff = StatDiff,
                                               VarNames = VarNames[i],
                                               forward = forward),
                                      .SDcols = 'Value',
                                      by = c(keyVar[[i]], 'IDDD')][IDDD == VarNames[i]]

          output[[i]] <- list()
          output[[i]][['Pred']] <- output.DT[[i]][seq(1, dim(output.DT[[i]])[[1]], by = 2),
                                                  keyVar[[i]], with = F]
          output[[i]][['STD']] <- output.DT[[i]][seq(2, dim(output.DT[[i]])[[1]], by = 2),
                                                 keyVar[[i]], with = F]
          output[[i]][['Pred']][, VarNames[i] := output.DT[[i]][seq(1, dim(output.DT[[i]])[[1]], by = 2),
                                                                'Value', with = F], with = F]
          output[[i]][['STD']][, VarNames[i] := output.DT[[i]][seq(2, dim(output.DT[[i]])[[1]], by = 2),
                                                               'Value', with = F], with = F]
      }
      names(output) <- VarNames

    return(output)
  }
)
