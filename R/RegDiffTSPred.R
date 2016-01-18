#' @title Method to predict according to the regular difference time series model
#'
#' @description This method implements the predicted value and their standard deviation
#' according to the regular difference time series model
#' \eqn{(1-B)y_{t}=a_{t}}{(1-B)y<sub>t</sub>=a<sub>t</sub>}
#'
#' @param x object upon which the prediction will be made
#'
#' @param forward integer indicating the number of periods ahead when the
#' prediction will be made; by default it is 2L
#'
#' @param VarNames character vector with the variable names for which the
#' prediction will be made; by default it is NULL
#'
#' @param keyVar character vector with the variable names used as key variables
#' in data.table operations; by default it is NULL
#'
#' @return It returns a list with components Pred and STD, containing the point
#' prediction and the estimated standard deviations, respectively. Depending
#' on the class of the input parameter x, it returns:
#'
#' \itemize{
#'  \item For input class vector, it returns numeric vectors
#'  \item For input class matrix, it returns matrices
#'  \item For input class data.table, it returns data.tables
#'  \item For input class StBusQ, it returns data.tables
#'  \item For input class GenStBusQ, it returns data.tables
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
        out <- list(Pred = Reduce(rbind, output[, 1]), STD = Reduce(rbind, output[, 2]))
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
#' @import data.table
#'
#' @export
setMethod(
    f = "RegDiffTSPred",
    signature = c("list"),
    function(x, VarNames, forward = 2L){

        ElemClass <- unique(unlist(lapply(x, function(DT){class(DT)[1]})))
        if (length(ElemClass) != 1 | ElemClass != 'data.table') stop('[RegDiffTSPred list] x debe ser una lista de data.table.')

        if (missing(VarNames)) stop('[RegDiffTSPred list] Debe especificar VarNames.')

        keyVar <- setdiff(names(x[[length(x)]]), VarNames)
        if (length(keyVar) == 0) stop('[RegDiffTSPred list] Debe especificar keyVar.')

        ValidUnits <- x[[length(x)]][, keyVar, with = F]
        Data.list <- lapply(x, setkeyv, keyVar)
        Data.list <- lapply(Data.list, function(x){x[ValidUnits, c(keyVar, VarNames), with = F]})

        Data <- rbindlist(Data.list)
        setkeyv(Data, keyVar)

        output.DT <- Data[, lapply(.SD, RegDiffTSPred, forward = forward), .SDcols = VarNames, by = keyVar]

        output <- list(Pred = output.DT[seq(1, dim(output.DT)[[1]], by = 2)],
                       STD = output.DT[seq(2, dim(output.DT)[[1]], by = 2)])
        return(output)
    }
)
#' @rdname RegDiffTSPred
#'
#' @import StBusQ
#'
#' @export
setMethod(
    f = "RegDiffTSPred",
    signature = c("StBusQList"),
    function(x, VarNames, forward = 2L){

        if (missing(VarNames)) stop('[RegDiffTSPred list] Debe especificar VarNames.')

        Data.list <- StBusQ::dcast.StBusQ(x, VarNames)
        Data.list <- lapply(Data.list, function(Per){

                            Reduce(function(x, y){merge(x, y, by = setdiff(names(x), VarNames))}, Per)
            })
        mc <- match.call()
        mc[['x']] <- Data.list
        output <- eval(mc)
        return(output)

   }
)
