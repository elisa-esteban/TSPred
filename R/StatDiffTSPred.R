#' @title Method to predict according to the stational difference time series model
#'
#' @description This method implements the predicted value and their standard deviation according to
#' the stational difference time series model
#' \eqn{(1-B)^s (1-B)y_{t}=a_{t}}{(1-B)^s (1-B)y<sub>t</sub>=a<sub>t</sub>}.
#'
#' @param x object upon which the prediction will be made.
#'
#' @param StatDiff stational differences of the time series; by default it is 12L.
#'
#' @param forward integer indicating the number of periods ahead when the prediction will be made;
#' by default it is 2L.
#'
#' @param VarNames character vector with the variable names for which the prediction will be made;
#' by default it is NULL
#'
#' @return It returns a list with components Pred and STD, containing the point prediction and the
#' estimated standard deviations, respectively. Depending on the class of the input parameter x, it
#' returns:
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
#' \dontrun{
#' # With an object of class StQList
#' data(StQListExample)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
#' StatDiffTSPred(StQListExample, VarNames = VarNames)
#' }
#'
#' @import data.table StQ
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
  signature = c("StQList"),
  function(x,  StatDiff = 12L, forward = 2L, VarNames = NULL){

      if (length(VarNames) == 0) stop('[StatDiffTSPred StQList] Debe especificar VarNames.')

      if (length(VarNames) == 1){

          DT <- getValues(x, VarNames)
          IDQuals <- setdiff(names(DT), c(VarNames, 'Period'))
          DT[, orderPeriod := orderRepoTime(Period), by = IDQuals]
          setkeyv(DT, c(IDQuals, 'orderPeriod'))
          output <- DT[, StatDiffTSPred(get(VarNames), StatDiff = StatDiff, forward = forward), by = IDQuals]
          setnames(output, c('Pred', 'STD'), paste0(c('Pred', 'STD'), VarNames))
          return(output)

      } else {

          DT.list <- lapply(VarNames, function(Var){

              LocalOutput <- getValues(x, Var)
              setnames(LocalOutput, Var, 'Value')
              LocalOutput[, Variable := Var]
              return(LocalOutput)
          })

          DT <- rbindlist(DT.list)
          IDQuals <- setdiff(names(DT), c('Variable', 'Period', 'Value'))
          DT[, orderPeriod := orderRepoTime(Period), by = IDQuals]
          setkeyv(DT, c(IDQuals, 'Variable', 'orderPeriod'))
          output <- DT[, StatDiffTSPred(Value, StatDiff = StatDiff, forward = forward), by = c(IDQuals, 'Variable')]
          Form <- paste0(IDQuals, ' ~ Variable')
          output.Pred <- dcast(output, as.formula(Form), value.var = 'Pred')
          setnames(output.Pred, VarNames, paste0('Pred', VarNames))
          output.STD <- dcast(output, as.formula(Form), value.var = 'STD')
          setnames(output.STD, VarNames, paste0('STD', VarNames))
          output <- merge(output.Pred, output.STD, by = IDQuals, all = TRUE)
          return(output)
      }
  }
)
