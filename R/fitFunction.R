#' @title Fit Function to Data
#' @description
#'   Fit predefined functions to data via nonlinear least squares using \code{\link{nls}}.
#' @template dataset
#' @template predictors
#' @template responses
#' @template groups
#' @template scriptvars
#' @template returnResults
#' @details
#'   One script variables is summarized in \code{scriptvars} list:\cr
#'   \describe{
#'     \item{math.fun}{[\code{character(1)}]\cr
#'       Function selection for fitting data.}
#'   }
#' @return
#'   Logical [\code{TRUE}] invisibly or, if \code{return.results = TRUE}, \code{\link{list}} of 
#'   resulting \code{\link{data.frame}} objects:
#'   \item{coeff}{Estimated coefficients and standard errors for every variable.}
#'   \item{predictions}{
#'     Brushable dataset with predictions and residuals added to original values and groups,
#'     if available.
#'   }
#' @export
#' @importFrom stats coef fitted fitted.values median nls residuals var
#' @importFrom utils head tail
#' @examples
#' # Generate data from logistic function:
#' fun = function(x, a, b, c, d, sigma = 1) {
#'   a+(b-a) / (1+exp(-d*(x-c))) + rnorm(length(x), sd = sigma)
#'   }
#' library(data.table)
#' dt = data.table(  x1 = sample(seq(-10, 10, length.out = 100))
#'                   , group1 = sample(x = c("A", "B"), replace = TRUE, size = 100)
#'                   )
#' dt[group1 == "A", y1 := fun(x1, 1, 10, 1, 0.6, 0.1)]
#' dt[group1 == "B", y1 := fun(x1, 8, 2, -1, 0.3, 0.1)]
#' # Fit the logistic function:
#' res = fitFunction(dt, "x1", "y1", "group1", list(math.fun = "Logistic"), TRUE)
#' # Show estimated coefficients:
#' res$coeff
#' # Plot fitted vs. residuals:
#' plot(res$predictions$Fitted, res$predictions$Residuals)
fitFunction = function(dataset = cs.in.dataset()
                       , preds = cs.in.predictors(), resps = cs.in.responses(), groups = cs.in.groupvars()
                       , scriptvars = cs.in.scriptvars()
                       , return.results = FALSE
                       ) {
  # convert dataset to data.table
  dtDataset = as.data.table(dataset)
  
  # sanity checks
  assertCharacter(preds, any.missing = FALSE, len = 1)
  assertCharacter(resps, any.missing = FALSE, len = 1)
  assertCharacter(groups, any.missing = FALSE, max.len = 1)
  assertDataTable(dtDataset)
  assertSubset(names(dtDataset), choices = c(preds, resps, groups))
  assertDataTable(dtDataset[, preds, with = FALSE], any.missing = FALSE)
  assertList(scriptvars, len = 1)
  assertChoice(scriptvars$math.fun, c("Logistic"))
  assertFlag(return.results)
  
  # add group with single instance, if missing
  if (length(groups) == 0) {
    groups = tail(make.unique(c(preds, resps, "grps")), 1)
    dtDataset[, (groups) := "A"]
  }
  
  # update to valid names
  preds = make.names(preds)
  resps = make.names(resps)
  groups = make.names(groups)
  colnames(dtDataset) = make.names(colnames(dtDataset))
  
  # due to non-sense notes in R CMD check
  Fitted = Message = RMSE = Residuals = pseudoR2 = NULL
  
  # group values
  grp.vals = unique(dtDataset[, groups, with = FALSE])
  
  # set number of parameters and formula
  if (scriptvars$math.fun == "Logistic") {
    par.names = c("a", "b", "c", "d")
    frml = "response ~ a+(b-a) / (1+exp(-d*(predictor-c)))"
    # get starting values for logistic function
    dtStart = data.table(grp.vals, key = groups)
    dtStart = merge(dtStart
                    , dtDataset[, head(setorderv(.SD, preds)), by = groups][, lapply(.SD, mean), .SDcols = resps, by = groups]
                    # , dtDataset[, lapply(.SD, quantile, probs = c(0.99, 0.01)), .SDcols = resps, by = groups]
                    )
    dtStart = merge(dtStart
                    , dtDataset[, tail(setorderv(.SD, preds)), by = groups][, lapply(.SD, mean), .SDcols = resps, by = groups]
                    )
    dtStart = merge(dtStart
                    , dtDataset[, lapply(.SD, median), .SDcols = preds, by = groups]
                    )
    dtStart = cbind(dtStart, 1)
    names(dtStart)[-1] = par.names
  }
  # replace x and y by correct predictor and response name
  frml = gsub("predictor", preds, frml)
  frml = gsub("response", resps, frml)
  
  # create coefficient data.table
  coeff.names = paste0(rep(c("Coeff_", "StdErr_"), each = length(par.names)) , par.names)
  dtCoeff = data.table(grp.vals, key = groups)
  dtCoeff[, c(coeff.names) := numeric()]
  dtCoeff[, c("pseudoR2", "RMSE", "Iterations", "Tolerance") := numeric()]
  dtCoeff[, Message := character()]
  
  # resulting fitted and resid
  dtPredictions = data.table(dtDataset[, c(groups, resps), with = FALSE])
  dtPredictions[, c("Fitted", "Residuals") := numeric()]
  
  # go through groups
  for (grp in grp.vals[[groups]]) {
    nls.res = NULL
    # catch error in nls with tryCatch
    tryCatch(
      nls.res <- nls(  formula = as.formula(frml)
                     , data = dtDataset[get(groups) == grp, c(preds, resps), with = FALSE]
                     , start = as.list(dtStart[grp, par.names, with = FALSE])
                     )
      # , silent = TRUE
      , error = function(e) {
        dtCoeff[grp, Message := e$message]
      } 
    )
    # next group on error
    if (is.null(nls.res))
      next
    # extract estimated coefficients
    dtCoeff[grp, (coeff.names) := as.list(coef(summary(nls.res))[, 1:2])]
    # pseudo R2
    # https://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-least-squares-fit
    # https://de.wikipedia.org/wiki/Pseudo-Bestimmtheitsma%C3%9F
    dtCoeff[grp, pseudoR2 := var(fitted.values(nls.res)) / (var(fitted.values(nls.res)) + var(residuals(nls.res)))]
    # RMSE
    dtCoeff[grp, RMSE := summary(nls.res)$sigma]
    # algorithm messages
    dtCoeff[grp, c("Iterations", "Tolerance", "Message") := nls.res$convInfo[c(2, 3, 5)]]
    # fitted and residulas
    dtPredictions[get(groups) == grp, Fitted := fitted(nls.res)]
    dtPredictions[get(groups) == grp, Residuals := residuals(nls.res)]
  }
  
  # drop grouping variable
  if (nrow(dtCoeff) == 1) {
    dtCoeff[, (groups) := NULL]
    dtPredictions[, (groups) := NULL]
  }
  
  # Export to Cornerstone
  cs.out.dataset(dtCoeff, "Coefficient Table")
  cs.out.dataset(dtPredictions, "Fit Estimate", brush = TRUE)
  # return results
  if (return.results) {
    res = list(coeff = dtCoeff, predictions = dtPredictions)
    return(res)
  } else {
    invisible(TRUE)
  }
}
