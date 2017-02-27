<<<<<<< HEAD
#' @title Method to predict according to ARIMA model
#'
#' @description This method implements the predicted value and their standard deviation according to
#' ARIMA model.
#'
#' @param x object upon which the prediction will be made.
#'
#' @param VarNames character vector with the variable names for which the prediction will be made;
#' by default it is NULL.
#'
#' @param forward integer indicating the number of periods ahead when the prediction will be made;
#' by default it is 2L.
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
#' AutoArimaTSPred(Example1.TS, forward = 1L)
#' AutoArimaTSPred(Example1.TS, forward = 2L)
#'
#' # Predicting upon a times series with many NA values
#' data(Example2.TS)
#' AutoArimaTSPred(Example2.TS, forward = 1L)
#'
#' # On a matrix
#' Mat <- rbind(Example1.TS, Example2.TS)
#' AutoArimaTSPred(Mat, forward = 1L)
#'
#' \dontrun{
#' # With an object of class StQList
#' data(StQList_Example)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
#' AutoArimaTSPred(StQList_Example, VarNames)
#' }
#'
#' @export
setGeneric("AutoArimaTSPred", function(x, VarNames, forward = 2L){
    standardGeneric("AutoArimaTSPred")})

#' @rdname AutoArimaTSPred
#'
#' @import forecast
#'
#' @export
setMethod(
    f = "AutoArimaTSPred",
    signature = c("vector"),
    function(x, VarNames, forward = 2L){

        x <- as.numeric(x)

        # zero-length or NA vectors returns NA
        if (length(x) == 0 | all(is.na(x))) return(list(Pred = NA_real_,
                                                        STD = NA_real_))

        fit <- auto.arima(x)
        out <- forecast(fit, h = forward)

        std <- sqrt(out$model$sigma2)
        output <- list(Pred = out$mean[forward], STD = std)

        return(output)
    }
)
#' @rdname AutoArimaTSPred
#'
#' @import forecast
#'
#' @export
setMethod(
    f = "AutoArimaTSPred",
    signature = c("matrix"),
    function(x, VarNames, forward = 2L){

        out <- apply(x, 1, AutoArimaTSPred, forward = forward)
        out <- Reduce(rbind, out)
        output <- list(Pred = Reduce(rbind, out[, 1]),
                    STD = Reduce(rbind, out[, 2]))
        output <- lapply(output, function(mat){
            dimnames(mat)[[1]] <- dimnames(x)[[1]]
            return(mat)
        }
        )
        return(output)

    }
)
#' @rdname AutoArimaTSPred
#'
#' @import data.table StQ forecast
#'
#' @export
setMethod(
    f = "AutoArimaTSPred",
    signature = c("StQList"),
    function(x, VarNames, forward = 2L){

        if (length(VarNames) == 0) stop('[AutoArimaTSPred StQList] Debe especificar VarNames.')

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
        DD <- Reduce('+', slotsDD)

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
        setkeyv(Data.list, c(keyVarTot, 'IDDD'))
        Data.list[, Value := ifelse(Value == '', NA_real_, Value)]

        output.DT <- Data.list[, lapply(.SD, AutoArimaTSPred,
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
||||||| merged common ancestors
=======
#' @title Method to predict according to ARIMA model
#'
#' @description This method implements the predicted value and their standard deviation according to
#' ARIMA model.
#'
#' @param x object upon which the prediction will be made.
#'
#' @param VarNames character vector with the variable names for which the prediction will be made;
#' by default it is NULL.
#'
#' @param forward integer indicating the number of periods ahead when the prediction will be made;
#' by default it is 2L.
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
#' AutoArimaTSPred(Example1.TS, forward = 1L)
#' AutoArimaTSPred(Example1.TS, forward = 2L)
#'
#' # Predicting upon a times series with many NA values
#' data(Example2.TS)
#' AutoArimaTSPred(Example2.TS, forward = 1L)
#'
#' # On a matrix
#' Mat <- rbind(Example1.TS, Example2.TS)
#' AutoArimaTSPred(Mat, forward = 1L)
#'
#' \dontrun{
#' # With an object of class StQList
#' data(StQList_Example)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
#' AutoArimaTSPred(StQList_Example, VarNames)
#' }
#'
#' @export
setGeneric("AutoArimaTSPred", function(x, VarNames, forward = 2L){
    standardGeneric("AutoArimaTSPred")})

#' @rdname AutoArimaTSPred
#'
#' @import forecast
#'
#' @export
setMethod(
    f = "AutoArimaTSPred",
    signature = c("vector"),
    function(x, VarNames, forward = 2L){

        x <- as.numeric(x)

        # zero-length or NA vectors returns NA
        if (length(x) == 0 | all(is.na(x))) return(list(Pred = NA_real_,
                                                        STD = NA_real_))

        fit <- auto.arima(x)
        out <- forecast(fit, h = forward)

        std <- sqrt(out$model$sigma2)
        output <- list(Pred = out$mean[forward], STD = std)

        return(output)
    }
)
#' @rdname AutoArimaTSPred
#'
#' @import forecast
#'
#' @export
setMethod(
    f = "AutoArimaTSPred",
    signature = c("matrix"),
    function(x, VarNames, forward = 2L){

        out <- apply(x, 1, AutoArimaTSPred, forward = forward)
        out <- Reduce(rbind, out)
        output <- list(Pred = Reduce(rbind, out[, 1]),
                       STD = Reduce(rbind, out[, 2]))
        output <- lapply(output, function(mat){
            dimnames(mat)[[1]] <- dimnames(x)[[1]]
            return(mat)
        }
        )
        return(output)

    }
)
#' @rdname AutoArimaTSPred
#'
#' @import data.table StQ forecast
#'
#' @export
setMethod(
    f = "AutoArimaTSPred",
    signature = c("StQList"),
    function(x, VarNames, forward = 2L){

        if (length(VarNames) == 0) stop('[AutoArimaTSPred StQList] Debe especificar VarNames.')

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
        setkeyv(Data.list, c(keyVarTot, 'IDDD'))
        Data.list[, Value := ifelse(Value == '', NA_real_, Value)]

        output.DT <- Data.list[, lapply(.SD, AutoArimaTSPred,
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
>>>>>>> 7f7f362ee324e3eb90fbdd7e7255db813864b96c
