#' @title Method to predict according to the regular difference time series
#' model.
#'
#' @description This method implements the predicted value and their standard
#' deviation according to the regular difference time series model
#' \eqn{(1-B)y_{t}=a_{t}}{(1-B)y<sub>t</sub>=a<sub>t</sub>}.
#'
#' @param x object upon which the prediction will be made.
#'
#' @param VarNames character vector with the variable names for which the
#' prediction will be made; by default it is NULL.
#'
#' @param forward integer indicating the number of periods ahead when the
#' prediction will be made; by default it is 2L.
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
#' RegDiffTSPred(Example1.TS, forward = 1L)
#' RegDiffTSPred(Example1.TS, forward = 2L)
#'
#' # Predicting upon a times series with many NA values
#' data(Example2.TS)
#' RegDiffTSPred(Example2.TS, forward = 1L)
#'
#' # On a matrix
#' Mat <- rbind(Example1.TS, Example2.TS)
#' RegDiffTSPred(Mat, forward = 1L)
#'
#' @export
setGeneric("RegDiffTSPred", function(x, VarNames, forward = 2L){
    standardGeneric("RegDiffTSPred")})

#' @rdname RegDiffTSPred
#'
#' @export
setMethod(
    f = "RegDiffTSPred",
    signature = c("vector"),
    function(x, VarNames, forward = 2L){

        x <- as.numeric(x)

        # zero-length or NA vectors returns NA
        if (length(x) == 0 | all(is.na(x))) return(list(Pred = NA_real_,
                                                        STD = NA_real_))

        # search for the first nonNA value
        index <- length(x)
        ahead <- 0
        while (is.na(x[index])){
            index <- index - 1L
            ahead <- ahead + 1L
        }

        aux <- x[index]
        names(aux) <- NULL
        output <- list(Pred = aux)

        if (!all(is.na(x)) && !all(x[!is.na(x)] == 0)) {
            for (i in seq(along = x)){

              if (is.na(x[i])) next
              if (x[i] == 0) {
                x[i] <- NA
              } else break
            }
        }

        d.x <-diff(x, lag = 1L)
        std <- sqrt(mean(d.x * d.x, na.rm = T))
        output[['STD']] <- std

        ahead <- ahead + forward
        if (forward >= 2L){
            output <- RegDiffTSPred(x, forward = forward - 1L)
            output[['STD']] <- ahead * output[['STD']]
        }

        return(output)
    }
)
#' @rdname RegDiffTSPred
#'
#' @export
setMethod(
    f = "RegDiffTSPred",
    signature = c("matrix"),
    function(x, VarNames, forward = 2L){

        output <- apply(x, 1, RegDiffTSPred, forward = forward)
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
#' @rdname RegDiffTSPred
#'
#' @import StQ
#'
#' @export
setMethod(
    f = "RegDiffTSPred",
    signature = c("StQList"),
    function(x, VarNames, forward = 2L){

        if (length(VarNames) == 0) stop('[RegDiffTSPred StQList] Debe especificar VarNames.')

        Data.list <- getData(x, VarNames)
        Data.list <- lapply(Data.list, getData)

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

        keyVarTot <- unique(unlist(keyVar))
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
        setkeyv(Data.list, c(keyVarTot, 'IDDD'))
        Data.list[, Value := ifelse(Value == '', NA_real_, Value)]

        output.DT <- vector('list', length(VarNames))
        output <- vector('list', length(VarNames))
        for (i in 1:length(VarNames)){

            output.DT[[i]] <- Data.list[, lapply(.SD, RegDiffTSPred,
                                                 VarNames = VarNames[i],
                                                 forward = forward),
                                   .SDcols = 'Value',
                                   by = c(keyVar[[i]], 'IDDD')][IDDD == VarNames[i]]
            output[[i]] <- list()
            output[[i]][['Pred']] <- output.DT[[i]][seq(1, dim(output.DT[[i]])[[1]], by = 2),
                                                    keyVar[[i]], with = F]
            output[[i]][['STD']] <- output.DT[[i]][seq(2, dim(output.DT[[i]])[[1]], by = 2),
                                                   keyVar[[i]], with = F]
            output[[i]][['Pred']][, (VarNames[i]) := output.DT[[i]][seq(1, dim(output.DT[[i]])[[1]], by = 2), 'Value', with = F]]
            output[[i]][['STD']][, (VarNames[i]) := output.DT[[i]][seq(2, dim(output.DT[[i]])[[1]], by = 2), 'Value', with = F]]
        }
        names(output) <- VarNames

        return(output)

   }
)
