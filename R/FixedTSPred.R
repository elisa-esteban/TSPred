#' @title Method to predict a prefixed value for time series.
#'
#' @description This method implements the predicted value and their standard deviations as a
#' prefixed value.
#'
#' @param x object upon which the prediction will be made.
#'
#' @param VarNames character vector with the variable names for which the prediction will be made;
#' by default it is NULL.
#'
#' @param value numeric vector with the value for the prediction and the standard deviation.
#'
#' @return It returns a \code{data.table} with components Pred and STD, containing the point
#' prediction and the estimated standard deviations, respectively, for each variable.
#'
#' @examples
#'
#' # Predicting one and two months ahead in time
#' data(Example1.TS)
#' FixedTSPred(Example1.TS, value = 0L)
#'
#' # Predicting upon a times series with many NA values
#' data(Example2.TS)
#' FixedTSPred(Example2.TS, value = 0L)
#'
#' \dontrun{
#' # With an object of class StQList
#' data(StQListExample)
#' VarNames <- c('Personal_07.')
#' FixedTSPred(StQListExample, VarNames, value = 0L)
#' }
#'
#' @import forecast imputeTS data.table StQ RepoTime parallel
#'
#' @export
setGeneric("FixedTSPred", function(x, VarNames, value = 0L){
    standardGeneric("FixedTSPred")})
#'
#' @rdname FixedTSPred
#'
#' @export
setMethod(
    f = "FixedTSPred",
    signature = c("vector"),
    function(x, VarNames, value = 0L){

        x <- as.numeric(x)
        output <- data.table(Pred = value, STD = value)
        return(output)

    }
)
#'
#' @rdname FixedTSPred
#'
#' @export
setMethod(
    f = "FixedTSPred",
    signature = c("StQList"),
    function(x, VarNames, value = 0L){

        if (length(VarNames) == 0) stop('[TSPred:: FixedTSPred StQList] The input parameter VarNames must be specified.\n')


        DD <- getDD(x[[length(getData(x))]])
        IDQuals <- getMicroData(DD)[Sort == 'IDQual'][['Variable']]

        ouput <- unique(getUnits(x)[, (IDQuals), with = FALSE])
        Vars <- unlist(lapply(VarNames, function(x){out <- paste0(c('Pred', 'STD'),x)}))

        output <- ouput[ , (Vars) := value]
        return(output)
    }
)
