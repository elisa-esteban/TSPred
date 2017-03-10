#' @title Method to predict according to an ARIMA model
#'
#' @description This method implements the predicted value and their standard deviation according to
#' an ARIMA model.
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
#' \dontrun{
#' # With an object of class StQList
#' data(StQListExample)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
#' AutoArimaTSPred(StQListExample, VarNames)
#' }
#'
#' @import forecast data.table StQ RepoTime
#'
#' @export
setGeneric("AutoArimaTSPred", function(x, VarNames, forward = 2L, ...){
    standardGeneric("AutoArimaTSPred")})

#' @rdname AutoArimaTSPred
#'
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
#'
#' @rdname ArimaBaseTSPred
#'
#' @export
setMethod(
    f = "AutoArimaTSPred",
    signature = c("StQList"),
    function(x, VarNames, forward = 2L){

        if (length(VarNames) == 0) stop('[AutoArimaTSPred StQList] Debe especificar VarNames.')

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
