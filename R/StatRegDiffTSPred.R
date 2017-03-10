#' @title Method to predict according to the stational difference time series model
#'
#' @description This method implements the predicted value and their standard deviation according to
#' the regular difference time series model
#' \eqn{(1-B)^s y_{t}=a_{t}}{(1-B)^s y<sub>t</sub>=a<sub>t</sub>}.
#'
#' @param x object upon which the prediction will be made.
#'
#' @param StatDiff stational differences of the time series; by default it is 12L.
#'
#' @param forward integer indicating the number of periods ahead when the prediction will be made;
#' by default it is 2L.
#'
#' @param VarNames character vector with the variable names for which the prediction will be made;
#' by default it is NULL.
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
#' \dontrun{
#' # With an object of class StQList
#' data(StQListExample)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
#' StatRegDiffTSPred(StQListExample, StatDiff = 9L, VarNames = VarNames)
#' }
#'
#' @export
setGeneric("StatRegDiffTSPred", function(x,  StatDiff = 12L, forward = 2L,
                                      VarNames = NULL){
  standardGeneric("StatRegDiffTSPred")})

#' @rdname StatRegDiffTSPred
#'
#' @export
setMethod(
  f = "StatRegDiffTSPred",
  signature = c("vector"),
  function(x,  StatDiff = 12L, forward = 2L, VarNames = NULL){

    x <- as.numeric(x)

    if (length(x) == 0 | all(is.na(x))){
        return(list(Pred = as.numeric(NA), STD = as.numeric(NA)))
    }

    index.1 <- length(x)
    index.s <- length(x) + 1L - StatDiff
    ahead <- 0
    NA.flag <- F
    if (index.s <= 1) NA.flag <- T
    while ( NA.flag == F &&
           (is.na(x[index.1]) | is.na(x[index.s]) | is.na(x[index.s - 1L])) ){
      index.1 <- index.1 - 1L
      index.s <- index.s - StatDiff
      ahead <- ahead + 1L
      if (index.s <= 1) NA.flag <- T
    }

    output <- list(Pred = ifelse(!NA.flag,
                                 x[index.1] + x[index.s] - x[index.s - 1L], NA))

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
#' @import data.table StQ
#'
#' @export
setMethod(
  f = "StatRegDiffTSPred",
  signature = c("StQList"),
  function(x,  StatDiff = 12L, forward = 2L, VarNames = NULL){

      if (length(VarNames) == 0) stop('[StatRegDiffTSPred StQList] Debe especificar VarNames.')

      if (length(VarNames) == 1){

          DT <- getValues(x, VarNames)
          IDQuals <- setdiff(names(DT), c(VarNames, 'Period'))
          DT[, orderPeriod := orderRepoTime(Period), by = IDQuals]
          setkeyv(DT, c(IDQuals, 'orderPeriod'))
          output <- DT[, StatRegDiffTSPred(get(VarNames), StatDiff = StatDiff, forward = forward), by = IDQuals]
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
          output <- DT[, StatRegDiffTSPred(Value, StatDiff = StatDiff, forward = forward), by = c(IDQuals, 'Variable')]
          Form <- paste0(IDQuals, ' ~ Variable')
          output.Pred <- dcast(output, as.formula(Form), value.var = 'Pred')
          setnames(output.Pred, VarNames, paste0('Pred', VarNames))
          output.STD <- dcast(output, as.formula(Form), value.var = 'STD')
          setnames(output.STD, VarNames, paste0('STD', VarNames))
          output <- merge(output.Pred, output.STD, by = IDQuals, all = TRUE)
          return(output)
      }

      QualsVal <- strsplit(VarNames, '_')
      QualsVal <- lapply(QualsVal, function(Values){Values[2:length(Values)]})

      OrigVarNames <- VarNames
      VarNames <- ExtractNames(VarNames)
      Data.list <- getData(x, VarNames)
      IDQuals <- unlist(lapply(Data.list, getIDQual))

      DD <- getDD(Data.list[[length(Data.list)]])
      Data.list <- lapply(Data.list, getData)

      slotsNames <- names(getSlots('DD'))
      slotsNames <- slotsNames[slotsNames != 'VarNameCorresp']
      slotsDD <- lapply(slotsNames, function(x){slot(DD,x)})
      DD <- DatadtToDT(Reduce('+', slotsDD))

      keyVar <- vector('list', length(VarNames))

      for (i in 1:length(VarNames)){

          key <- DD[Variable == VarNames[i]]
          Quals <- setdiff(names(key), c('Variable', 'Sort', 'Class', 'Length', 'ValueRegExp'))
          key <- transpose(key[, Quals, with = FALSE])[['V1']]
          key <- key[key != '']
          keyVar[[i]] <- setdiff(key, IDQuals)

      }

      Data.list <- lapply(seq(along = Data.list), function(index){

          Data <- DatadtToDT(Data.list[[index]])
          Data_Var <- lapply(seq(along = VarNames), function(var){

              Quals <- QualsVal[[var]]
              keys <- keyVar[[var]]

              if (length(Quals) == length(keys)){

                  for (i in seq(along = keys)){

                      col <- keys[i]
                      Data <- Data[, aux := Data[[col]] == Quals[i]]
                      Data <- Data[aux == TRUE]
                      Data[, aux := NULL]
                  }

              } else {

                  for (i in 1:(length(keys) - 1)){

                      col <- keys[i]
                      Data <- Data[, aux := Data[[col]] == Quals[i]]
                      Data <- Data[aux == TRUE]
                      Data[, aux := NULL]
                  }

                  Data <- Data[, aux := Data[[keys[length(keys)]]] == '']
                  Data <- Data[aux == TRUE]
                  Data[, aux := NULL]
              }

              return(Data)
          })

          out <- rbindlist(Data_Var, fill = TRUE)
          return(out)
      })

      UnitQuals <- names(getUnits(x[[length(x)]]))
      keyVarTot <- unique(c(UnitQuals, unlist(keyVar)))
      ValidUnits <- Data.list[[length(Data.list)]][, keyVarTot, with = F]
      setkeyv(ValidUnits, keyVarTot)
      ValidUnits <- ValidUnits[!duplicated(ValidUnits, by = key(ValidUnits))]
      Data.list <- lapply(Data.list, function(Data){

          Data <- Data[, c(keyVarTot, 'IDDD', 'Value'), with = F]
          setkeyv(Data, keyVarTot)
          out <- Data[ValidUnits]
          setkeyv(out, 'IDDD')
          out <- out[VarNames]
          return(out)

      })
      Data.list <- rbindlist(Data.list)
      setkeyv(Data.list, c(unlist(keyVar), 'IDDD'))
      Data.list[, Value := ifelse(Value == '', NA_real_, Value)]

      output.DT <- Data.list[, lapply(.SD, StatRegDiffTSPred,
                                      StatDiff = StatDiff,
                                      forward = forward),
                             .SDcols = 'Value',
                             by = setdiff(names(Data.list), 'Value')]

      output.Pred <- output.DT[seq(1, dim(output.DT)[[1]], by = 2), c(UnitQuals, 'IDDD', 'Value'), with = F]
      formulaPred <- as.formula(paste0(paste0(UnitQuals, collapse = ' + '), ' ~ IDDD'))
      output.Pred <- dcast.data.table(output.Pred, formulaPred, value.var = 'Value')
      setnames(output.Pred, VarNames, OrigVarNames)

      output.STD <- output.DT[seq(2, dim(output.DT)[[1]], by = 2), c(UnitQuals, 'IDDD', 'Value'), with = F]
      formulaSTD <- as.formula(paste0(paste0(UnitQuals, collapse = ' + '), ' ~ IDDD'))
      output.STD <- dcast.data.table(output.STD, formulaSTD, value.var = 'Value')
      setnames(output.STD, VarNames, OrigVarNames)

      output <- list(Pred = output.Pred, STD = output.STD)

    return(output)
  }
)
