#' @title Method to predict according to an ARIMA model
#'
#' @description This method implements the predicted value and their standard deviation according to
<<<<<<< HEAD:R/AutoArimaTSPred.R
#' an ARIMA model.
||||||| merged common ancestors
#' ARIMA model.
=======
#' an ARIMA model as computed by the base package function arima().
>>>>>>> dca5fe21834e7ebe8b5c67fd5aae84b0980a48d6:R/ArimaBaseTSPred.R
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
#' ArimaBaseTSPred(Example1.TS, forward = 1L)
#' ArimaBaseTSPred(Example1.TS, forward = 2L)
#'
#' # Predicting upon a times series with many NA values
#' data(Example2.TS)
#' ArimaBaseTSPred(Example2.TS, forward = 1L)
#'
<<<<<<< HEAD:R/AutoArimaTSPred.R
||||||| merged common ancestors
#' # On a matrix
#' Mat <- rbind(Example1.TS, Example2.TS)
#' AutoArimaTSPred(Mat, forward = 1L)
#'
=======
#' # On a matrix
#' Mat <- rbind(Example1.TS, Example2.TS)
#' ArimaBaseTSPred(Mat, forward = 1L)
#'
>>>>>>> dca5fe21834e7ebe8b5c67fd5aae84b0980a48d6:R/ArimaBaseTSPred.R
#' \dontrun{
#' # With an object of class StQList
#' data(StQListExample)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
<<<<<<< HEAD:R/AutoArimaTSPred.R
#' AutoArimaTSPred(StQListExample, VarNames)
||||||| merged common ancestors
#' AutoArimaTSPred(StQList_Example, VarNames)
=======
#' ArimaBaseTSPred(StQList_Example, VarNames)
>>>>>>> dca5fe21834e7ebe8b5c67fd5aae84b0980a48d6:R/ArimaBaseTSPred.R
#' }
#'
<<<<<<< HEAD:R/AutoArimaTSPred.R
#' @import forecast data.table StQ RepoTime
#'
||||||| merged common ancestors
=======
#' @import data.table StQ
#'
>>>>>>> dca5fe21834e7ebe8b5c67fd5aae84b0980a48d6:R/ArimaBaseTSPred.R
#' @export
<<<<<<< HEAD:R/AutoArimaTSPred.R
setGeneric("AutoArimaTSPred", function(x, VarNames, forward = 2L, ...){
    standardGeneric("AutoArimaTSPred")})
||||||| merged common ancestors
setGeneric("AutoArimaTSPred", function(x, VarNames, forward = 2L){
    standardGeneric("AutoArimaTSPred")})
=======
setGeneric("ArimaBaseTSPred", function(x, VarNames, forward = 2L, ...){
    standardGeneric("ArimaBaseTSPred")})
>>>>>>> dca5fe21834e7ebe8b5c67fd5aae84b0980a48d6:R/ArimaBaseTSPred.R

#' @rdname ArimaBaseTSPred
#'
#'
#' @export
setMethod(
    f = "ArimaBaseTSPred",
    signature = c("vector"),
    function(x, VarNames, forward = 2L, ...){

        x <- as.numeric(x)

        # zero-length or NA vectors returns NA
        if (length(x) == 0 | all(is.na(x))) return(list(Pred = NA_real_,
                                                        STD = NA_real_))

        fit <- arima(x, ...)
        out <- predict(fit, n.ahead = forward)

<<<<<<< HEAD:R/AutoArimaTSPred.R
        std <- sqrt(out$model$sigma2)
        output <- list(Pred = out$mean[forward], STD = std)
||||||| merged common ancestors
        std <- sqrt(out$model$sigma2)
        output <- list(Pred = out$mean[forward], STD = std)

=======
        std <- sqrt(out$se)
        std <- std[length(std)]
        Pred <- out$pred
        Pred <- Pred[length(Pred)]
        output <- list(Pred = Pred, STD = std)

>>>>>>> dca5fe21834e7ebe8b5c67fd5aae84b0980a48d6:R/ArimaBaseTSPred.R
        return(output)
    }
)
<<<<<<< HEAD:R/AutoArimaTSPred.R
||||||| merged common ancestors
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
=======
#' @rdname ArimaBaseTSPred
#'
#' @export
setMethod(
    f = "ArimaBaseTSPred",
    signature = c("matrix"),
    function(x, VarNames, forward = 2L, ...){

        out <- apply(x, 1, AutoArimaTSPred, forward = forward, ...)
        out <- Reduce(rbind, out)
        output <- list(Pred = Reduce(rbind, out[, 1]),
                       STD = Reduce(rbind, out[, 2]))
        output <- lapply(output, function(mat){
            dimnames(mat)[[1]] <- dimnames(x)[[1]]
            return(mat)
        }
        )
        return(output)
>>>>>>> dca5fe21834e7ebe8b5c67fd5aae84b0980a48d6:R/ArimaBaseTSPred.R

<<<<<<< HEAD:R/AutoArimaTSPred.R
#' @rdname ArimaBaseTSPred
||||||| merged common ancestors
    }
)
#' @rdname AutoArimaTSPred
#'
#' @import data.table StQ forecast
=======
    }
)
#' @rdname ArimaBaseTSPred
>>>>>>> dca5fe21834e7ebe8b5c67fd5aae84b0980a48d6:R/ArimaBaseTSPred.R
#'
#' @export
setMethod(
    f = "ArimaBaseTSPred",
    signature = c("StQList"),
    function(x, VarNames, forward = 2L, ...){

        if (length(VarNames) == 0) stop('[TSPred:: ARIMABaseTSPred]  VarNames must be specified.')

        if (length(VarNames) == 1){

            DT <- getValues(x, VarNames)
            IDQuals <- setdiff(names(DT), c(VarNames, 'Period'))
            DT[, orderPeriod := orderRepoTime(Period), by = IDQuals]
            setkeyv(DT, c(IDQuals, 'orderPeriod'))
            output <- DT[, AutoArimaTSPred(get(VarNames), forward = forward), by = IDQuals]
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
            output <- DT[, AutoArimaTSPred(Value, forward = forward), by = c(IDQuals, 'Variable')]
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
