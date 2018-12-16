#' @title Reshape Grouped Data to Wide
#' @description
#'   Reshaping grouped data via \code{\link[data.table:dcast.data.table]{dcast}} to 'wide' format with
#'   rows for each unique combination of group variables. The response are arranged in
#'   separate columns for each datum in predictors. If a combination of groups identifies
#'   multiple rows, the number of rows in a group is returned to CS for the whole dataset
#'   instead of the response variable value.
#' @template dataset
#' @template predictors
#' @template responses
#' @template groups
#' @template scriptvars
#' @template returnResults
#' @details
#'   One script variables is summarized in \code{scriptvars} list:\cr
#'   \describe{
#'     \item{nodrop}{[\code{logical(1)}]\cr
#'       Drop missing combinations (\code{FALSE}) or include all (\code{TRUE}).
#'       Default is \code{FALSE}.}
#'   }
#' @return
#'   Logical [\code{TRUE}] invisibly or, if \code{return.results = TRUE}, \code{\link{list}} of 
#'   resulting \code{\link{data.frame}} object:
#'   \item{reshapeWide}{Dataset with reshaped data.}
#' @export
#' @examples 
#' # Reshape dataset to wide format:
#' reshapeWide(Indometh, "Subject", "time", "conc", list(nodrop = FALSE), return.results = TRUE)
reshapeWide = function(dataset = cs.in.dataset()
                       , preds = cs.in.predictors(), resps = cs.in.responses(), groups = cs.in.groupvars()
                       , scriptvars = cs.in.scriptvars()
                       , return.results = FALSE
                       ) {
  # sanity checks
  assertDataFrame(dataset)
  assertCharacter(preds, any.missing = FALSE, min.len = 1)
  assertCharacter(resps, any.missing = FALSE, min.len = 1)
  assertCharacter(groups, any.missing = FALSE, min.len = 1)
  assertSubset(names(dataset), choices = c(preds, resps, groups))
  assertList(scriptvars, len = 1)
  assertFlag(scriptvars$nodrop)
  assertFlag(return.results)
  
  # convert to data.table
  dtDataset = as.data.table(dataset)
  # update to valid names
  preds = make.names(preds)
  resps = make.names(resps)
  groups = make.names(groups)
  colnames(dtDataset) = make.names(colnames(dtDataset))
  
  # cast data to wide dataset
  # groups ~ preds, value.var = resps
  res = data.table::dcast(  data = dtDataset
                          , formula = as.formula(paste(paste(groups, collapse = "+"), "~", paste(preds, collapse = "+")))
                          , drop = !scriptvars$nodrop
                          , value.var = resps
                          )
  
  # add response name for one response
  if (length(resps) == 1) {
    colnames(res)[-seq_along(groups)] = paste(resps, colnames(res)[-seq_along(groups)], sep = "_")
  }
  
  # export to Cornerstone
  cs.out.dataset(res, "Wide Data")
  
  # return results
  if (return.results) {
    res = list(reshapeWide = res)
    return(res)
  } else {
    invisible(TRUE)
  }
}
