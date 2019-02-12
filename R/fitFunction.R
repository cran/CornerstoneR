#' @title Fit Function to Data via nls
#' @description
#'   Fit predefined functions to data via nonlinear least squares using \code{\link{nls}}.
#' @template dataset
#' @template predictors
#' @template responses
#' @template groups
#' @template auxiliaries
#' @template scriptvars
#' @template returnResults
#' @templateVar packagelink \code{\link{nls}}
#' @template threedots
#' @details
#'   The following script variables are summarized in \code{scriptvars} list:\cr
#'   \describe{
#'     \item{math.fun}{[\code{character(1)}]\cr
#'       Function selection for fitting data. It is possible to choose a predefined model, or
#'       compose a model manually by selecting \code{User Defined}.\cr
#'       Default is \code{User Defined}}
#'     \item{preds.frml}{[\code{character(1)}]\cr
#'       Only required if \code{math.fun} is set to \code{User Defined}.
#'       Valid R \code{\link{formula}} for the right hand side (predictors) of the model equation.}
#'     \item{resp.frml}{[\code{character(1)}]\cr
#'       Only required if \code{math.fun} is set to \code{User Defined}.
#'       Valid R \code{\link{formula}} for the left hand side (response) of the model equation.}
#'     \item{start.vals}{[\code{character(1)}]\cr
#'       Only required if \code{math.fun} is set to \code{User Defined}.
#'       Specify starting values for all terms of the right hand side as a comma separated list
#'       with a period as decimal separator.}
#'     \item{weights}{[\code{character(1)}]\cr
#'       Select a weighting variable from the auxiliary variables.}
#'     \item{algo.nls}{[\code{character(1)}]\cr
#'       Specifies the algorithm to use. For details see \code{\link{nls}}.\cr
#'       Default is \code{plinear}.}
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
#' # Set script variables
#' scriptvars = list(math.fun = "Logistic", resp.frml = "", preds.frml = ""
#'                   , start.vals = "", weights = "", algo.nls = "default"
#'                   )
#' # Fit the logistic function:
#' res = fitFunction(dt, "x1", "y1", "group1", "", scriptvars, TRUE)
#' # Show estimated coefficients:
#' res$coeff
#' # Plot fitted vs. residuals:
#' plot(res$predictions$Fitted, res$predictions$Residuals)
fitFunction = function(dataset = cs.in.dataset()
                       , preds = cs.in.predictors(), resps = cs.in.responses(), groups = cs.in.groupvars()
                       , auxs = cs.in.auxiliaries()
                       , scriptvars = cs.in.scriptvars()
                       , return.results = FALSE
                       , ...
                       ) {
  # convert dataset to data.table
  dtDataset = as.data.table(dataset)
  
  # sanity checks
  assertCharacter(preds, any.missing = FALSE) # specialise at formulas
  assertCharacter(resps, any.missing = FALSE, len = 1)
  assertCharacter(groups, any.missing = FALSE, max.len = 1)
  assertCharacter(auxs, any.missing = FALSE)
  assertDataTable(dtDataset)
  assertSubset(names(dtDataset), choices = c(preds, resps, groups, auxs))
  # check protected names in dataset, conflicts with data.table usage are possible
  assertDisjunct(names(dtDataset), c("pred", "preds", "resp", "resps", "group", "groups", "brush", "brushed"))
  assertDataTable(dtDataset[, preds, with = FALSE], any.missing = FALSE)
  assertList(scriptvars, len = 6)
  assertChoice(scriptvars$math.fun, c("User Defined", "Logistic"))
  if (scriptvars$math.fun == "User Defined") {
    assertString(scriptvars$resp.frml)
    assertString(scriptvars$preds.frml)
    assertString(scriptvars$start.vals, min.chars = 3)
  }
  if (scriptvars$weights != "") {
    assertSubset(scriptvars$weights, choices = auxs)
    weighting = dtDataset[[scriptvars$weights]]
  } else {
    weighting = NULL
  }
  assertSubset(scriptvars$algo.nls, choices = c("default", "plinear", "port"))
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
  Fitted = StopMessage = RMSE = Residuals = pseudoR2 = NULL
  
  # group values
  grp.vals = unique(dtDataset[, groups, with = FALSE])
  
  # starting values by group
  dtStart = data.table(grp.vals, key = groups)
  # arrange formula, parameters, start values
  if (scriptvars$math.fun == "User Defined") {
    # at least one predictor
    # coefficients without predictor result in an error because of arguments 'weights' in nls
    assertCharacter(preds, any.missing = FALSE, min.len = 1)
    # define formula: sanity check is done implicitly in nls
    frml = paste(scriptvars$resp.frml, scriptvars$preds.frml, sep = "~")
    # get parameter names and start values to dtStart
    start.vals = unlist(strsplit(scriptvars$start.vals, "[,]"))
    start.vals = strsplit(start.vals, "[=]")
    start.vals = lapply(start.vals, trimws)
    if (any(vapply(start.vals, length, numeric(1)) != 2)   # check combination of name and value
        | any(unlist(lapply(start.vals, grepl, pattern = "^$")))   # check empty strings
        ) {
      stop(paste0("The string of start values '", scriptvars$start.vals, "' is malformed.\n"
                  , "It has to be a comma separated list from a combination of 'name=value'. "
                  , "The decimal separator in 'value' should be a period."))
    }
    par.names = vapply(start.vals, head, character(1), n = 1L)
    start.vals = suppressWarnings(as.numeric(vapply(start.vals, tail, character(1), n = 1L)))
    assertNumeric(start.vals, any.missing = FALSE)
    dtStart[, (par.names) := data.table(t(start.vals))]
  } else if (scriptvars$math.fun == "Logistic") {
    # only one predictor
    assertCharacter(preds, any.missing = FALSE, len = 1)
    # set parameter names and formula
    par.names = c("a", "b", "c", "d")
    frml = paste0(resps, " ~ a+(b-a) / (1+exp(-d*(", preds, "-c)))")
    # get starting values for logistic function
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
  
  # create coefficient data.table
  coeff.names = paste0(rep(c("Coeff_", "StdErr_"), each = length(par.names)) , par.names)
  dtCoeff = data.table(grp.vals, key = groups)
  dtCoeff[, c(coeff.names) := numeric()]
  dtCoeff[, c("pseudoR2", "RMSE") := numeric()]
  dtCoeff[, c("Converged") := logical()]
  dtCoeff[, c("Iterations", "Tolerance", "StopCode") := numeric()]
  dtCoeff[, StopMessage := character()]
  
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
                     , algorithm = scriptvars$algo.nls
                     , weights = weighting
                     , ...
                     )
      , error = function(e) {
        dtCoeff[grp, StopMessage := e$message]
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
    dtCoeff[grp, c("Converged", "Iterations", "Tolerance", "StopCode", "StopMessage") := nls.res$convInfo]
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
