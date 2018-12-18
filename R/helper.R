createCSEnvir = function(dfData, blnBrush = NULL, blnExcluded = NULL
                         , strPreds = character(), strResps = character(), strGroups = character()
                         , lstScriptVars = list()
                         , dfSubsets = data.frame(NULL), strSubset = ""
                         , env = parent.frame()) {
  # sanity check
  assertDataFrame(dfData, col.names = "named")
  nrows = nrow(dfData)
  if (testNull(blnBrush)) {blnBrush = logical(nrows)}
  assertLogical(blnBrush, len = nrows)
  if (testNull(blnExcluded)) {blnExcluded = logical(nrows)}
  assertLogical(blnExcluded, len = nrows)
  assert(testCharacter(strPreds), testSubset(strPreds, colnames(dfData)), combine = "and")
  assert(testCharacter(strResps), testSubset(strResps, colnames(dfData)), combine = "and")
  assert(testCharacter(strGroups), testSubset(strGroups, colnames(dfData)), combine = "and")
  assertList(lstScriptVars, null.ok = TRUE)
  assertDataFrame(dfSubsets, types = "logical", col.names = "named")
  assertString(strSubset)
  assertEnvironment(env)
  
  # create environment
  cs.session.test = new.env()
  cs.session.test$auxiliaries = character()
  cs.session.test$dataset = dfData
  cs.session.test$brushed = blnBrush
  cs.session.test$excluded = blnExcluded
  cs.session.test$groupvars= strGroups
  cs.session.test$predictors = strPreds
  cs.session.test$responses = strResps
  cs.session.test$scriptvars = lstScriptVars
  cs.session.test$subsets = dfSubsets
  cs.session.test$subsets.current = strSubset
  # FIXME: check defaults
  cs.session.test$graphs.active = FALSE
  cs.session.test$graphs.files = character()
  cs.session.test$graphs.names = character()
  
  assign("cs.session.test", cs.session.test, envir = env)
  
  invisible(TRUE)
}

createCSFunctions = function(env = parent.frame()) {
  assertEnvironment(env)
  
  # docu of cs.* functions in localInterface.R
  
  # due to notes about undefined 'cs.session.test' in R CMD check
  if (!testEnvironment(env, contains = "cs.session.test")) {
    cs.session.test = new.env()
    assign("cs.session.test", cs.session.test, envir = env)
  }
  
  env$cs.in.auxiliaries = function(quote = FALSE) {
    assertFlag(quote)
    if (quote) {
      as.character(sapply(cs.session.test$auxiliaries, cs.quote))
    } else {
      cs.session.test$auxiliaries
    }
  }
  env$cs.in.brushed = function() {cs.session.test$brushed}
  env$cs.in.dataset = function() {cs.session.test$dataset}
  env$cs.in.excluded = function() {cs.session.test$excluded}
  env$cs.in.groupvars = function(quote = FALSE) {
    assertFlag(quote)
    if (quote) {
      as.character(sapply(cs.session.test$groupvars, cs.quote))
    } else {
      cs.session.test$groupvars
    }
  }
  env$cs.in.predictors = function(quote = FALSE) {
    assertFlag(quote)
    if (quote) {
      as.character(sapply(cs.session.test$predictors, cs.quote))
    } else {
      cs.session.test$predictors
    }
  }
  env$cs.in.responses = function(quote = FALSE) {
    assertFlag(quote)
    if (quote) {
      as.character(sapply(cs.session.test$responses, cs.quote))
    } else {
      cs.session.test$responses
    }
  }
  env$cs.in.scriptvars = function(name = NA) {
    assertString(name, na.ok = TRUE)
    if (is.na(name)) {
      cs.session.test$scriptvars
    } else {
      cs.session.test$scriptvars[[name]]
    }
  }
  env$cs.in.subsets = function() {cs.session.test$subsets}
  env$cs.in.subsets.current = function() {cs.session.test$subsets.current}
  env$cs.quote = function(x = NULL) {
    assertString(x, null.ok = TRUE)
    if (length(grep('[^a-zA-Z0-9_.]|^[0-9_.]', x, useBytes = TRUE, perl = TRUE))) {
      paste('`', gsub('`', '\\`', gsub('\\','\\\\', x, fixed = TRUE), fixed = TRUE), '`', sep = '')
    } else {
      x
    }
  }
  
  # following functions have a different definition than in CS-R
  env$cs.out.dataset = function(data, name = NA, brush = FALSE) {
    assertDataFrame(data)
    assertCharacter(name)
    assertFlag(brush)
    invisible(TRUE)
  }
  env$cs.out.emf = function(name = NULL, width = 10, height = 10) {
    assertCharacter(name)
    assertNumber(width)
    assertNumber(height)
    invisible(TRUE)
  }
  env$cs.out.png = function(name = NULL, width = 10, height = 10) {
    assertCharacter(name)
    assertNumber(width)
    assertNumber(height)
    invisible(TRUE)
  }

  invisible(TRUE)
}
