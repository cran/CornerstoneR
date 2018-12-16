#' @title Random Forest
#' @description 
#'   Random Forest via \code{\link[ranger]{ranger}}. Predicts response variables or brushed set of 
#'   rows from predictor variables, using Random Forest classification or regression.
#' @template dataset
#' @template predictors
#' @template responses
#' @template brush
#' @template scriptvars
#' @template returnResults
#' @details
#'   Three script variables are summarized in \code{scriptvars} list:\cr
#'   \describe{
#'     \item{brush.pred}{[\code{logical(1)}]\cr
#'       Use \code{brush} vector as additional predictor.\cr
#'       Default is \code{FALSE}.}
#'     \item{use.rows}{[\code{character(1)}]\cr
#'       Rows to use in model fit. Possible values are \code{all}, \code{unbrushed}, or 
#'       \code{brushed}.\cr
#'       Default is \code{all}.}
#'     \item{num.trees}{[\code{integer(1)}]\cr
#'       Number of trees to fit in \code{\link[ranger]{ranger}}.\cr
#'       Default is \code{500}.}
#'   }
#' @return
#'   Logical [\code{TRUE}] invisibly or, if \code{return.results = TRUE}, \code{\link{list}} of 
#'   resulting \code{\link{data.frame}} objects:
#'   \item{statistics}{General statistics of random forest.}
#'   \item{importances}{
#'     Variable importances of prediction variables in descending order of importance
#'     (most important first)
#'   }
#'   \item{predictions}{
#'     Brushable dataset with predicted values for \code{dataset}. The original input and other
#'     columns can be added to this dataset through the menu \code{Columns -> Add from Parent ...}.
#'   }
#'   \item{confusion}{
#'     For categorical response variables or brush state only. A table with counts of each 
#'     distinct combination of predicted and actual values.
#'   }
#' @export
#' @examples
#' # Fit random forest to iris data:
#' res = randomForest(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), "Species"
#'                    , brush = rep(FALSE, nrow(iris)), return.results = TRUE
#'                    )
#' # Show general statistics:
#' res$statistics
randomForest = function(dataset = cs.in.dataset()
                        , preds = cs.in.predictors(), resps = cs.in.responses(), brush = cs.in.brushed()
                        , scriptvars = cs.in.scriptvars()
                        , return.results = FALSE
                        ) {
  # convert dataset to data.table
  dtDataset = as.data.table(dataset)
  
  # sanity checks
  assertCharacter(preds, any.missing = FALSE, min.len = 1)
  assertCharacter(resps, any.missing = FALSE)
  assertLogical(brush, any.missing = FALSE, len = nrow(dtDataset))
  assertDataTable(dtDataset)
  assertSubset(names(dtDataset), choices = c(preds, resps))
  assertDataTable(dtDataset[, preds, with = FALSE], any.missing = FALSE)
  assertList(scriptvars, len = 3, null.ok = TRUE)
  # FIXME: workaround for CS-2582
  if (testNull(scriptvars)) {
    # if NULL, default values
    scriptvars = list(brush.pred = FALSE, use.rows = "all", num.trees = 500)
  }
  # FIXME: workaround for CS-2639
  if (is.na(scriptvars$brush.pred))
    scriptvars$brush.pred = FALSE
  assertFlag(scriptvars$brush.pred)
  assertChoice(scriptvars$use.rows, c("all", "unbrushed", "brushed"))
  assertCount(scriptvars$num.trees, positive = TRUE)
  assertFlag(return.results)
  
  # update to valid names
  preds = make.names(preds)
  resps = make.names(resps)
  colnames(dtDataset) = make.names(colnames(dtDataset))
  
  # get script variables to single variables
  use.rows = scriptvars$use.rows
  
  # due to non-sense notes in R CMD check
  Importance = Freq = N = runtime = Statistic = Value = Variable = NULL
  
  # use brush as a predictor, results in mandatory response -> additional assert
  # FIXME: unit test
  # FIXME: "brush" as existing name from CS
  if (scriptvars$brush.pred) {
    assertCharacter(resps, min.len = 1)
    dtDataset[, brush := as.factor(brush)]
    preds = c(preds, "brush")
    use.rows = "all"
  }
  
  # on missing response: add brush to data and use it as response
  # use all rows, not brushed or unbrushed
  if (length(resps) == 0) {
    dtDataset[, brush := as.factor(brush)]
    resps = "brush"
    use.rows = "all"
  }
  
  # subsetting data via brush
  if (use.rows == "all") {
    brush[] = TRUE
  } else if (use.rows == "unbrushed") {
    brush = !brush
  }
  
  # init resulting data.tables
  nresps = length(resps)
  ndata = nrow(dtDataset)
  stat.names = c("Response", "Type", "Number of Trees", "Sample Size"
                 , "Number of Independent Variables", "Mtry", "Minimal Node Size"
                 , "Variable Importance Mode"
                 , "OOB Prediction Error [%]", "OOB Prediction Error (MSE)", "OOB R squared"
                 , "Runtime R Script [s]"
                 )
  statistics = data.table(resps = resps
                          , type = character(nresps)
                          , ntrees = integer(nresps)
                          , samplesize = integer(nresps)
                          , npreds = integer(nresps)
                          , mtry = integer(nresps)
                          , minnodesize = integer(nresps)
                          , impmode = character(nresps)
                          , oobpredperc = rep(NaN, nresps)
                          , oobpredmse = rep(NaN, nresps)
                          , oobr2 = rep(NaN, nresps)
                          , runtime = numeric(nresps)
                          )
  importances = data.table(resps = resps)
  for (pred in preds) {
    importances[, (pred) := numeric(nresps)]
  }
  predictions = data.table(logical(ndata))
  colnames(predictions) = paste(c("V", resps), collapse = "")
  for (resp in resps) {
    predictions[, (paste0("Obs.", resp)) := logical(ndata)]
    if (testFactor(dtDataset[[resp]])) {
      predictions[, (resp) := character(ndata)]
      predictions[, (paste0("Pred.", resp)) := character(ndata)]
      predictions[, (paste0("Resid.", resp)) := logical(ndata)]
    } else {
      predictions[, (resp) := numeric(ndata)]
      predictions[, (paste0("Pred.", resp)) := numeric(ndata)]
      predictions[, (paste0("Resid.", resp)) := numeric(ndata)]
    }
  }
  predictions[, (paste(c("V", resps), collapse = "")) := NULL]
  confusions = list()
  
  for (resp in resps) {
    # Time measurement
    time.start = Sys.time()
    
    # create formula: response vs. all other variables
    model = as.formula(paste0(resp, " ~ ", paste(preds, collapse = "+")))
    # fit the random forest on subset with removed NAs
    rf = ranger::ranger(model, na.omit(dtDataset[brush], cols = resp)
                        , num.trees = scriptvars$num.trees
                        , importance = "permutation"
                        , respect.unordered.factors = "order"
                        )
    
    # get model statistics
    # FIXME: add splitrule
    statistics[resps == resp, 2:8 := list(rf$treetype, rf$num.trees, rf$num.samples
                                          , rf$num.independent.variables, rf$mtry
                                          , rf$min.node.size, rf$importance.mode
                                          )]
    if(testFactor(dtDataset[[resp]])) {
      statistics[resps == resp, "oobpredperc" := 100*rf$prediction.error]
    } else {
      statistics[resps == resp, c("oobpredmse", "oobr2") := list(rf$prediction.error, rf$r.squared)]
    }
    # get variable importances
    importances[resps == resp, names(rf$variable.importance) := as.list(rf$variable.importance)]
    # calculate predictions table
    predictions[, (paste0("Obs.", resp)) := !is.na(dtDataset[, resp, with = FALSE]) & brush]
    predictions[, (resp) := dtDataset[, resp, with = FALSE]]
    pred.resp = paste0("Pred.", resp)
    predictions[, (pred.resp) := data.table(predict(rf, dtDataset)$predictions)]
    if (testFactor(dtDataset[[resp]])) {
      predictions[, (paste0("Resid.", resp)) := eval(as.name(resp)) != eval(as.name(pred.resp))]
    } else {
      predictions[, (paste0("Resid.", resp)) := eval(as.name(resp)) - eval(as.name(pred.resp))]
    }
    
    # calculate Confusion table for classification task
    if (testFactor(dtDataset[[resp]])) {
      confusion = cbind(dtDataset[, resp, with = FALSE], predictions[, pred.resp, with = FALSE])
      # use 'table' instead of data.table 'by=' to get all comparisions and zero frequencies
      confusion = data.table(table(confusion))
      confusion[, Freq := N/sum(N)*100]
      confusions[[resp]] = confusion[order(-N)]
    }
    
    # End time measurement
    time.diff = Sys.time() - time.start
    statistics[resps == resp, runtime := time.diff]
  }
  # rename columns
  colnames(statistics) = stat.names
  colnames(importances)[1] = "Response"
  
  # Transpose if only one response
  if (length(resps) == 1) {
    # transpose
    statistics = transpose(statistics)
    colnames(statistics) = "Value"
    statistics[, Statistic := stat.names]
    setcolorder(statistics, c("Statistic", "Value"))
    # formatC last four columns
    statistics[9:12, Value := formatC(as.numeric(statistics[9:12, Value]))]
    # clean up
    if (testFactor(dtDataset[[resps]])) {
      statistics = statistics[-c(1, 10, 11), ]
    } else {
      statistics = statistics[-c(1, 9), ]
    }
    importances = transpose(importances)
    colnames(importances) = "Importance"
    importances = importances[-1, ]
    importances[, Importance := as.numeric(Importance)]
    importances[, Variable := preds]
    setcolorder(importances, c("Variable", "Importance"))
    importances = importances[order(-Importance)]
  }
  
  # Export to Cornerstone
  cs.out.dataset(statistics, "Statistics")
  cs.out.dataset(importances, "Variable Importances")
  cs.out.dataset(predictions, "Predictions", brush = TRUE)
  for (i in names(confusions)) {
    cs.out.dataset(confusions[[i]]
                   , paste0("Confusion Table", ifelse(length(resps) == 1, "", paste0(" (", i, ")")))
                   )
  }
  
  # return results
  if (return.results) {
    res = list(statistics = statistics, importances = importances
               , predictions = predictions, confusions = confusions
               )
    return(res)
  } else {
    invisible(TRUE)
  }
}
