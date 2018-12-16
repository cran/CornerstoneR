#' @title Mosaic Plot
#' @description
#'   Plot extended mosaic via \code{\link[vcd]{mosaic}}.
#' @template dataset
#' @template predictors
#' @template responses
#' @export
#' @importFrom graphics plot
#' @importFrom stats as.formula na.omit predict
#' @examples
#' # Draw mosaic plot from 'Titanic' data:
#' mosaicPlot(as.data.frame(Titanic), c("Class", "Sex", "Age", "Survived"), "Freq")
mosaicPlot = function(dataset = cs.in.dataset()
                      , preds = cs.in.predictors(), resps = cs.in.responses()
                      ) {
  # sanity checks
  assertDataFrame(dataset)
  assertCharacter(preds, any.missing = FALSE, min.len = 1)
  assertCharacter(resps, any.missing = FALSE, len = 1)
  assertSubset(names(dataset), choices = c(preds, resps))

  # convert to data.table
  dtDataset = as.data.table(dataset)
  # update to valid names
  preds = make.names(preds)
  resps = make.names(resps)
  colnames(dtDataset) = make.names(colnames(dtDataset))
  
  # create formula
  frml = as.formula(paste0(resps, "~", paste0(preds, collapse = "+")))
  # plotting
  cs.out.png("Mosaic Plot (PNG)")
  vcd::mosaic(frml, data = dtDataset, highlighting_fill = c("gray", "red"))
  cs.out.emf("Mosaic Plot (EMF)")
  vcd::mosaic(frml, data = dtDataset, highlighting_fill = c("gray", "red"))
  
  # return
  invisible(TRUE)
}
