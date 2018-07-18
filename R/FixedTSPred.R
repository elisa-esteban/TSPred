#' @title Method to predict a prefixed value for time series.
#'
#' @description This method implements the predicted value and their standard
#' deviations as a prefixed value.
#'
#' @param x object upon which the prediction will be made.
#'
#' @param VarNames character vector with the variable names for which the
#' prediction will be made; by default it is NULL.
#'
#' @param value numeric vector with the value for the prediction and the
#' sta
#'
#' @return It returns a list with components Pred and STD, containing the point
#' prediction and the standard deviations, respectively, with the prefixed value.
#' Depending on the class of the input parameter x, it returns:
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
#' FixedTSPred(Example1.TS, value = 0L)
#'
#' # Predicting upon a times series with many NA values
#' data(Example2.TS)
#' FixedTSPred(Example2.TS, value = 0L)
#'
#' \dontrun{
#' # With an object of class StQList
#' data(StQListExample)
#' VarNames <- c('ActivEcono_35._6._2.1.4._0', 'GeoLoc_35._6._2.1._1.2.5.')
#' FixedTSPred(StQListExample, VarNames)
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
