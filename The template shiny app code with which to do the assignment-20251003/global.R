library(shiny)
library(shinyBS) # Additional Bootstrap Controls (tooltips)
library(plyr)
library(DT)
library(corrgram)
library(visdat)
library(shinycssloaders) # busy spinner
library(recipes) # The recipes package is central to preprocessing
library(doParallel) # We employ a form of parallelism that works for MAC/Windows/Ubuntu
library(caret) # This code implements the CARET framework: see http://topepo.github.io/caret/index.html for details
library(rlang)
library(devtools)
library(BiocManager)
if (!library("mixOmics", logical.return = TRUE)) {
  BiocManager::install("mixOmics", update = FALSE, ask = FALSE)  # This has moved from CRAN to Bioconductor
}
library(mixOmics)
library(rlist)
library(ggplot2)
library(butcher)
library(here)

options(digits = 3)

glmnet_initial <- c("naomit", "month", "dummy") # <-- These are arbitrary starting values. Set these to your best recommendation
pls_initial <- c("impute_knn","dow", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
rpart_initial <- c("dow", "month") # <-- These are arbitrary starting values. Set these to your best recommendation
# maintenance point ---------------------------------------------------------------------------------------------------------------------------

# helper * explanation ---------------------------------------------------------------------------------------------------------------------------

makeExplanation <- function(title = "Diagnostic Explanations", bullets) {
  renderUI({
    tagList(
      h4(title),
      tags$ul(lapply(bullets, function(b) tags$li(b)))
    )
  })
}

# Build a seeds list sized for a specific grid size, without touching the global getTrControl()
.make_local_seeds <- function(n_resamples = 25, grid_size = 8, min_seed = 1000L, max_seed = 5000L) {
  set.seed(673)  # keep your original seed base
  seeds <- vector("list", length = n_resamples + 1L)
  for (i in seq_len(n_resamples)) {
    seeds[[i]] <- as.integer(runif(n = grid_size, min = min_seed, max = max_seed))
  }
  seeds[[n_resamples + 1L]] <- as.integer(runif(n = 1, min = min_seed, max = max_seed))
  seeds
}




# add further preprocessing choices for the new methods here
baseline_initial <- c("impute_knn", "zv", "center", "scale", "month", "dow", "dummy")
lm_initial     <- c(baseline_initial, "corr")
# PCR does PCA inside the method; do NOT add step_pca in the recipe.
pcr_initial <- c("impute_knn", "zv", "center", "scale", "month", "dow", "dummy")
# Tree/rule-based model: no need to center/scale/corr. Keep missing-value handling + factors + date feats.
cubist_initial <- c("impute_knn", "zv", "month", "dow", "dummy")
# Optionally try an alternative preset with bag imputation for top contenders:
cubist_initial_bag <- c("impute_bag", "zv", "month", "dow", "dummy")
rf_initial <- c("impute_knn", "zv", "month", "dow", "dummy")
# You may also test a variant with bagging imputation for finalists:
# rf_initial_bag <- c("impute_bag", "zv", "month", "dow", "dummy")
# ranger
ranger_initial <- c("month", "dow", "impute_bag", "zv", "dummy")
# gbm
gbm_initial <- c("indicate_na", "month", "dow", "impute_bag", "impute_mode", "zv", "other", "unknown", "dummy")

# SVM needs scaling; keep imputation + dummies; drop zero-variance
svmR_initial <- c("impute_knn", "zv", "center", "scale", "month", "dow", "dummy")
# SVM-Poly needs scaling; keep impute + dummies; drop zero-variance; add date features if desired
svmP_initial <- c("impute_knn", "zv", "center", "scale", "month", "dow", "dummy")
# Robust default for MARS (earth):
# 1) create NA indicators → 2) date features → 3) impute (bag for num + mode for cats)
# 4) drop zero-variance → 5) dummy encode
earth_initial <- c("indicate_na", "month", "dow", "impute_bag", "impute_mode", "zv", "dummy")
# Bagged MARS (GCV): keep the same robust NA-safe pipeline as earth
bagEarthGCV_initial <- c("indicate_na", "month", "dow", "impute_bag", "impute_mode", "zv", "dummy")
# Neural Network (nnet): numerical scaling is critical; use imputation + dummy encoding
nnet_initial <- c("indicate_na", "impute_bag", "impute_mode", "zv", "center", "scale", "dummy")
# avNNet benefits from normalized numerics + robust NA handling + dummies
avNNet_initial <- c("indicate_na", "month", "dow", "impute_bag", "impute_mode", "zv", "center", "scale", "dummy")
mlp_initial    <- c("impute_knn", "zv", "center", "scale", "month", "dow", "dummy")
monmlp_initial <- c("impute_knn", "zv", "center", "scale", "month", "dow", "dummy")

# KNN 
knn_initial <- c("impute_knn", "zv", "center", "scale", "month", "dow", "dummy")
# BayesGLM benefits from: (a) robust NA handling, (b) removing degenerate columns,
# (c) reducing extreme collinearity, (d) standardizing scales, and
# (e) representing categorical/date info as dummies.
bayesglm_initial <- c("indicate_na", "impute_knn", "zv", "corr", "center", "scale", "month", "dow", "dummy")
# BayesGLM: handle degenerate cols first, then impute; map factor NAs to "unknown" before dummies
# Order: zv -> indicate_na -> impute_knn -> corr -> center -> scale -> month -> dow -> dummy
bayesglm_initial <- c("zv", "indicate_na", "impute_knn", "corr", "center", "scale", "month", "dow", "dummy")
# Bayesian Lasso: linear model with L1 shrinkage—benefits from scaling;
# also remove highly correlated columns to stabilize.
blasso_initial <- c(  "impute_knn",  "zv",  "center", "scale",  "corr",  "month", "dow",  "dummy")
# Bayesian Bridge (family incl. ridge/bridge priors): same prep as blasso.
bridge_initial <- c(  "impute_knn",  "zv",  "center", "scale",  "corr",  "month", "dow",  "dummy")
# M5 requires numeric predictors and benefits from normalization.Keep dummy variables and scale to stabilize linear models in leaves.
m5rules_initial <- c("impute_knn", "zv", "center", "scale", "dummy")
# SVM-Linear (LiblineaR): impute NAs, remove zero-variance cols, standardize (center/scale),# add date features (month, dow), then one-hot encode factors.
svmL2_initial <- c(  "impute_bag",  "zv",  "center",  "scale",  "month", "dow",  "dummy")
# Elastic Net needs standardized numeric predictors and dummy-coded factors.
# We keep month/dow signals then drop the raw date in the server recipe.
enet_initial <- c("impute_bag", "zv", "center", "scale", "month", "dow", "dummy")
# RBF nets like numeric, standardized inputs + dummies for factors; keep date signals.
rbfdda_initial <- c("impute_bag", "zv", "center", "scale", "month", "dow", "dummy")
# DENFIS requires numeric, non-missing input; center/scale for stability; dummy for factors
denfis_initial <- c("impute_bag", "zv", "month", "dow", "dummy", "center", "scale")
# GAM: use imputation + drop ZV + keep month/dow + dummy for factors
gam_initial <- c("impute_bag","zv","month","dow","dummy")
# GaussprRadial: typical numeric preprocessing (center, scale, dummy for factors)
gaussprRadial_initial <- c("impute_bag","zv","center","scale","dummy")
# GAMBoost prefers complete numerical design (impute→zv→date features→dummies→(optional) center/scale)
gamboost_initial <- c("indicate_na","impute_mode","impute_bag","zv","month","dow","dummy","center","scale","naomit")
# Trees handle raw factors; we only need robust imputation, optional date features, and guard against constants.
rfRules_initial <- c("impute_bag","month","dow","other","zv")
# Treebag requires numeric or dummy-coded features; prefers centered/scaled inputs
treebag_initial <- c("impute_median", "zv", "center", "scale", "dummy")

# Binary predictors required → discretize numerics, one-hot factors, drop constants/dates
logreg_logic_initial <- c("impute_bag","month","dow","dummy","zv","rm")
# RBF kernel needs centered/scaled numerics and dummies; drop dates, remove constants
rvmRadial_initial <- c("impute_bag","month","dow","dummy","center","scale","zv","rm")
# BRNN needs numeric matrix; impute -> date signals -> dummies -> center/scale -> drop constants
brnn_initial <- c("impute_bag","month","dow","dummy","center","scale","zv","rm")
# KRLS needs numeric matrix, so impute → dummy → center/scale → remove constants
krls_initial <- c("impute_bag","dummy","center","scale","zv","rm")

# end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
startMode <- function(Parallel = TRUE) {
  if (Parallel) {
    clus <- makeCluster(min(c(3,detectCores(all.tests = FALSE, logical = TRUE))))
    registerDoParallel(clus)
    clus
  } else {
    NULL
  }
}

stopMode <- function(clus) {
  if (!is.null(clus)) {
    stopCluster(clus)
    registerDoSEQ()
  }
}

ppchoices <- c("impute_knn", "impute_bag", "impute_median", "impute_mode", "YeoJohnson", "naomit", "pca", "pls", "ica", "center", "scale", "month", "dow", "dateDecimal", "nzv", "zv","other", "dummy","poly", "interact", "indicate_na", "corr")

# This function turns the method's selected preprocessing into a recipe that honours the same order. 
# You are allowed to add more recipe steps to this.
dynamicSteps <- function(recipe, preprocess) {
  if (is.null(preprocess)) {
    stop("The preprocess list is NULL - check that you are using the correct control identifier")
  }
  for (s in preprocess) {
    if (s == "impute_knn") {
      recipe <- step_impute_knn(recipe, all_numeric_predictors(), all_nominal_predictors(), neighbors = 5) # 5 is a reasonable guess
    } else if (s == "impute_bag") {
      recipe <- step_impute_bag(recipe, all_numeric_predictors(), all_nominal_predictors(), trees = 25)
    } else if (s == "impute_median") {
      recipe <- step_impute_median(recipe, all_numeric_predictors())
    } else if (s == "impute_mode") {
      recipe <- recipes::step_impute_mode(recipe, all_nominal_predictors())
    } else if (s == "YeoJohnson") {
      recipe <- recipes::step_YeoJohnson(recipe, all_numeric_predictors())
    } else if (s == "naomit") {
      recipe <- recipes::step_naomit(recipe, all_predictors(), skip = TRUE)
    } else if (s == "pca") {
      recipe <- recipes::step_pca(recipe, all_numeric_predictors(), num_comp = 25)
    } else if (s == "pls") {
      recipe <- recipes::step_pls(recipe, all_numeric_predictors(), outcome = "Response", num_comp = 25)
    } else if (s == "ica") {
      recipe <- recipes::step_ica(recipe, all_numeric_predictors(), num_comp = 25)
    } else if (s == "center") {
      recipe <- recipes::step_center(recipe, all_numeric_predictors())
    } else if (s == "scale") {
      recipe <- recipes::step_scale(recipe, all_numeric_predictors())
    } else if (s == "month") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("month"), ordinal = FALSE)
    } else if (s == "dow") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("dow"), ordinal = FALSE)
    } else if (s == "dateDecimal") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("decimal"), ordinal = FALSE)
    } else if (s == "zv") {
      recipe <- recipes::step_zv(recipe, all_predictors())
    } else if (s == "nzv") {
      recipe <- recipes::step_nzv(recipe, all_predictors(), freq_cut = 95/5, unique_cut = 10)
    } else if (s == "other") {
      recipe <- recipes::step_other(recipe, all_nominal_predictors(), threshold = 0.05)
    } else if (s == "dummy") {
      recipe <- recipes::step_dummy(recipe, all_nominal_predictors(), one_hot = FALSE)
    } else if (s == "poly") {
      recipe <- recipes::step_poly(recipe, all_numeric_predictors(), degree = 2)
    } else if (s == "interact") {
      recipe <- recipes::step_interact(recipe, terms = ~ all_numeric_predictors():all_numeric_predictors())
    } else if (s == "corr") {
      recipe <- recipes::step_corr(recipe, all_numeric_predictors(), threshold = 0.9)
    } else if (s == "indicate_na") {
      recipe <- recipes::step_indicate_na(recipe, all_predictors()) #shadow variables (this needs to precede dealing with NA)
    } else if (s == "rm") {
      # intentionally blank
    } else {
      stop(paste("Attempting to use an unknown recipe step:", s))
    }
  }
  recipe
}

description <- function(name) {
  regexName <- paste0("^", name, "$") # force an regular expression exact match
  mlist <- caret::getModelInfo(model = regexName)[[name]]
  line1 <- paste0("Method \"", name, "\" is able to do ", paste(collapse = " and ", mlist$type), ".")
  line2 <- paste0("It uses parameters: ", paste0(collapse = ", ", mlist$parameters$parameter), ".")
  line3 <- paste0("Its characteristics are: ", paste0(collapse = ", ", mlist$tags))
  paste(sep = "\n", line1, line2, line3)
}


# attempts to keep the model file size small by not saving the global environment with each model
saveToRds <- function(model, name) {
  try(
    # ensure that WEKA based models can be restored
    if (!is.null(model$finalModel$classifier)) {
      rJava::.jcache(model$finalModel$classifier)
    }, silent = TRUE
  )
  
  file <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, name, ".rds")
  #model2 <- butcher::axe_env(model, verbose = TRUE) # strip environments from the object to keep its size small
  #print(butcher::weigh(model2, threshold = 5, units = "MB"))
  saveRDS(model, file)
}

loadRds <- function(name, session) {
  rdsfile <- file.path(".","SavedModels", paste0(name, ".rds"))
  if (!file.exists(rdsfile)) {
    showNotification("Model needs to be trained first", session = session, duration = 3)
    return(NULL)
  }
  showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
  model <- readRDS(file = rdsfile)
  
  # try to update the preprocessing steps with the ones that were used
  steps <- model$recipe$steps
  seld <- c()
  for (step in steps) {
    s <- gsub(pattern = "step_", replacement = "", x = class(step)[1])
    if (s == "date") {
      s <- step$features[1]
    }
    seld <- c(seld, s)
  }
  preprocessingInputId <- paste0(name, "_Preprocess")
  updateSelectizeInput(session = session, inputId = preprocessingInputId, choices = ppchoices, selected = seld)
  if (length(seld) > 0) {
    showNotification(paste("Setting preprocessing for", name, "to", paste(seld, collapse = ",")), session = session, duration = 5)
  }
  model
}

deleteRds <- function(name) {
  rdsfile <- file.path(".","SavedModels", paste0(name, ".rds"))
  if (file.exists(rdsfile)) {
    ok <- unlink(rdsfile, force = TRUE)
  } else {
    ok <- TRUE
  }
  ok
}
