#' @title Method to predict according to the regular difference time series model
#'
#' @description This method implements the predicted value and their standard deviation according to
#' the regular difference time series model \eqn{(1-B)y_{t}=a_{t}}{(1-B)y<sub>t</sub>=a<sub>t</sub>}.
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
#' RegDiffTSPred(Example1.TS, forward = 1L)
#' RegDiffTSPred(Example1.TS, forward = 2L)
#'
#' # Predicting upon a times series with many NA values
#' data(Example2.TS)
#' RegDiffTSPred(Example2.TS, forward = 1L)
#'
#' \dontrun{
#' # With an object of class StQList
#' data(StQListExample)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
#' RegDiffTSPred(StQListExample, VarNames)
#' }
#'
#' @import data.table StQ RepoTime
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
        if (length(x) == 0 | all(is.na(x))) return(list(Pred = NA_real_, STD = NA_real_))

        # search for the first nonNA value
        index <- length(x)
        ahead <- 0
        while (is.na(x[index])) {
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
                x[i] <- NA_real_
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

        output <- data.table(Pred = output[['Pred']], STD = output[['STD']])

        return(output)
    }
)
#'
#' @rdname RegDiffTSPred
#'
#' @export
setMethod(
    f = "RegDiffTSPred",
    signature = c("StQList"),
    function(x, VarNames, forward = 2L){

        if (length(VarNames) == 0) stop('[RegDiffTSPred StQList] The input parameter VarNames must be specified.\n')

        if (length(VarNames) == 1){

            DT <- getValues(x, VarNames)
            IDQuals <- setdiff(names(DT), c(VarNames, 'Period'))
            DT[, orderPeriod := orderRepoTime(Period), by = IDQuals]
            setkeyv(DT, c(IDQuals, 'orderPeriod'))
            output <- DT[, RegDiffTSPred(get(VarNames), forward = forward), by = IDQuals]
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
            output <- DT[, RegDiffTSPred(Value, forward = forward), by = c(IDQuals, 'Variable')]
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
