shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models

    
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  shiny::onSessionEnded(stopApp)

  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    # d$ObservationDate <- as.Date(d$ObservationDate, "%d/%m/%Y")
    d$ObservationDate <- as.Date(d$ObservationDate, format = "%Y-%m-%d")  # modified by yxi75
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Response"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 55, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                 trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  # reactive getResamples ----
  getResamples <- reactive({
    models2 <- reactiveValuesToList(models) %>% 
      rlist::list.clean( fun = is.null, recursive = FALSE)
    req(length(models2) > 1)
    results <- caret::resamples(models2)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    
    # hide results worse than null model
    subset <- rep(TRUE, length(models2))
    if (input$HideWorse & NullModel %in% names(models2)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model3 in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model3, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models, selected = "svmRadial")  ## change the value parameter to your best method
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # Range for charts
  getResidualRange <- reactive({
    d1 <- getTrainResults()
    d1$residuals <- d1$obs - d1$pred
    d2 <- getTestResults()
    d2$residuals <- d2$obs - d2$pred
    d <- c(d1$residuals, d2$residuals)
    range(d, na.rm = TRUE)
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.null(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical", ) +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$null_Load,
    {
      method  <- "null"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$null_Delete,
    {
      models[["null"]] <- NULL
      gc()
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output null_Recipe ---
  output$null_Recipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
  library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmnet_Go ----
  observeEvent(
    input$glmnet_Go,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
     }
  )
  
  observeEvent(
    input$glmnet_Load,
    {
      method  <- "glmnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmnet_Delete,
    {
      models[["glmnet"]] <- NULL
      gc()
    }
  )
  
  # output glmnet_ModelSummary (text) ----
  output$glmnet_ModelSummary0 <- renderText({
    description("glmnet")   # Use the caret method name here
  })
  
  # output glmnet_Metrics (table) ----
  output$glmnet_Metrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output glmnet_ModelPlots (plot) ----
  output$glmnet_ModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })

  # output glmnet_Recipe (print) ----
  output$glmnet_Recipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output glmnet_ModelSummary2 (print) ----
  output$glmnet_ModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })

  # output glmnet_Coef (print) ----
  output$glmnet_Coef <- renderTable({
    req(models$glmnet)
    co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df[1] != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  # ---- glmnet_Explanation ----
  output$glmnet_Explanation <- makeExplanation(
    bullets = c(
      "Tuning Profile: RMSE across α (mixing) and λ (penalty) guides bias–variance trade-off.",
      "Coefficient sparsity: many coefficients shrink to 0; improves generalization & interpretability.",
      "Predicted vs Actual: 45° alignment indicates overall fit; systematic bias = under/over-penalization."
    )
  )
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pls_Go ----
  observeEvent(
    input$pls_Go,
    {
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pls_Load,
    {
      method  <- "pls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pls_Delete,
    {
      models[["pls"]] <- NULL
      gc()
    }
  )
  
  # output pls_ModelSummary0 (text) ----
  output$pls_ModelSummary0 <- renderText({
    description("pls")   # Use the caret method name here
  })

  # output pls_Metrics (table) ----
  output$pls_Metrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output pls_ModelPlots (plot) ----
  output$pls_ModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output pls_Recipe (print) ----
  output$pls_Recipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output pls_ModelSummary2 (print) ----
  output$pls_ModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  
  # output pls_Coef (print) ----
  output$pls_Coef <- renderTable({
    req(models$pls)
    co <- coef(models$pls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  # ---- pls_Explanation ----
  output$pls_Explanation <- makeExplanation(
    bullets = c(
      "Tuning Profile: RMSE vs number of latent components; stop when validation error bottoms out.",
      "Latent factors: mitigate multicollinearity by projecting X to a low-dimensional space.",
      "Predicted vs Actual: square 1:1 view to judge calibration and spread of errors."
    )
  )
  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
  library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(rpart.plot)
  
  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$rpart_Go,
    {
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartRecipe(),
                              data = getTrainData(),
                              method = method,
                              metric = "RMSE",
                              trControl = getTrControl(),
                              tuneLength = 5, 
                              na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$rpart_Load,
    {
      method  <- "rpart"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rpart_Delete,
    {
      models[["rpart"]] <- NULL
      gc()
    }
  )
  
  # output rpart_ModelSummary0 (print) ----
  output$rpart_ModelSummary0 <- renderText({
    description("rpart")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$rpart_Metrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$rpart_Recipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$rpart_ModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output rpart_ModelTree (plot) ----
  output$rpart_ModelTree <- renderPlot({
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  
  # ---- rpart_Explanation ----
  output$rpart_Explanation <- makeExplanation(
    bullets = c(
      "Complexity pruning (cp): controls tree size; watch tuning curve for sweet spot.",
      "Tree plot: rules are interpretable; look for overly deep branches (overfit risk).",
      "Predicted vs Actual: systematic bends hint at missing interactions or nonlinearity depth."
    )
  )
  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  

  # METHOD * lm  -----------------------------------------------------------------------------------------------------------------------
  # reactive getLmRecipe 
  getLmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lm_Preprocess) %>%  # use <method>_Preprocess
      step_rm(has_type("date"))              
  })
  
  # observeEvent lm_Go 
  observeEvent(
    input$lm_Go,
    {
      method <- "lm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"),
                       session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(
          getLmRecipe(),
          data      = getTrainData(),
          method    = method,
          metric    = "RMSE",
          trControl = getTrControl(),
          na.action = na.pass     # 与 glmnet/pls 一致地显式放宽 NA 处理
        )
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # observeEvent lm_Load 
  observeEvent(
    input$lm_Load,
    {
      method  <- "lm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  # observeEvent lm_Delete 
  observeEvent(
    input$lm_Delete,
    {
      models[["lm"]] <- NULL
      gc()
    }
  )
  
  # output lm_ModelSummary0 (text) 
  output$lm_ModelSummary0 <- renderText({
    description("lm")   
  })
  
  # output lm_Metrics (table) 
  output$lm_Metrics <- renderTable({
    req(models$lm)
    models$lm$results[ which.min(models$lm$results[, "RMSE"]), ]
  })
  
  # output lm_ModelPlots (plot) 
  output$lm_ModelPlots <- renderPlot({
    req(models$lm)
    fm <- models$lm$finalModel
    dat <- getTestData()
    preds <- predict(models$lm, newdata = dat)
    d <- data.frame(Y = dat$Response, Yhat = preds)
    rang <- range(c(d$Y, d$Yhat), na.rm = TRUE)
    
    par(mfrow = c(1, 3))
    
    # 1. Residuals vs Fitted
    plot(fm, which = 1, main = "Residuals vs Fitted")
    
    # 2. Normal Q-Q
    plot(fm, which = 2, main = "Normal Q-Q")
    
    # 3. Predicted vs Actual
    plot(d$Yhat, d$Y,
         xlab = "Predicted", ylab = "Actual",
         xlim = rang, ylim = rang, main = "Pred vs Actual")
    abline(0, 1, col = "blue", lty = 2, lwd = 2)
  })
  
  # lm_plot Explanation
  output$lm_Explanation <- renderUI({
    tagList(
      h4("Diagnostic Plot Explanations"),
      tags$ul(
        tags$li(strong("Residuals vs Fitted:"), " checks linearity & variance. Here: mostly fine, mild curvature."),
        tags$li(strong("Normal Q-Q:"), " checks normality of residuals. Here: approx normal, few outliers."),
        tags$li(strong("Pred vs Actual:"), " checks predictive accuracy. Here: R² ~ 0.75, reasonable fit.")
      )
    )
  })
  
  # output lm_Recipe (print) 
  output$lm_Recipe <- renderPrint({
    req(models$lm)
    models$lm$recipe
  })
  
  # output lm_ModelSummary2 (print) 
  output$lm_ModelSummary2 <- renderPrint({
    req(models$lm)
    print(models$lm)
  })
  
  # output lm_Coef (table) 
  output$lm_Coef <- renderTable({
    req(models$lm)
    fm <- models$lm$finalModel
    cf <- summary(fm)$coefficients
    as.data.frame(cf) |>
      tibble::rownames_to_column("Term")
  }, rownames = FALSE)
  
  # lm_Explanation ---
  output$lm_Explanation <- makeExplanation(
    bullets = c(
      "Residuals vs Fitted: checks linearity and homoscedasticity; patterns imply mis-specification.",
      "Normal Q-Q: checks residual normality; tail deviations suggest outliers/heavy tails.",
      "Predicted vs Actual: 45° alignment indicates accuracy; scatter shows error magnitude."
    )
  )
  
  
  
  
  

# METHOD * pcr ------------------------------------------------------------------------------------------------------------------  
library(pls)   # PCR is implemented via pls::pcr

# Build the recipe for PCR (no step_pca here; PCR does PCA internally)
getPcrRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$pcr_Preprocess) %>%    # keep order from UI
    step_rm(has_type("date"))                 # remove raw date columns after feature extraction
})

# Train
observeEvent(
  input$pcr_Go,
  {
    method <- "pcr"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(
        getPcrRecipe(),
        data      = getTrainData(),
        method    = method,
        metric    = "RMSE",
        trControl = getTrControl(),
        tuneLength = 25,           # explore up to 25 components
        na.action = na.pass
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# Load
observeEvent(
  input$pcr_Load,
  {
    method <- "pcr"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  }
)

# Forget
observeEvent(
  input$pcr_Delete,
  {
    models[["pcr"]] <- NULL
    gc()
  }
)

# Method summary (short text from caret metadata)
output$pcr_ModelSummary0 <- renderText({
  description("pcr")
})

# Best-row from resampled results (minimum RMSE)
output$pcr_Metrics <- renderTable({
  req(models$pcr)
  models$pcr$results[ which.min(models$pcr$results[, "RMSE"]), ]
})

# Tuning profile: RMSE vs ncomp (caret's standard plot)
output$pcr_ModelPlots <- renderPlot({
  req(models$pcr)
  plot(models$pcr)   # shows RMSE (and other metrics) across ncomp
})

# Explained variance table for X and Y (cumulative)
output$pcr_ExplainedVar <- renderTable({
  req(models$pcr)
  fm <- models$pcr$finalModel  # mvr object
  
  # X variance explained (% per component)
  xvar <- tryCatch(pls::explvar(fm), error = function(e) numeric(0))
  if (!length(xvar)) {
    return(data.frame(Message = "Explained variance not available for this fit."))
  }
  
  x_cum <- cumsum(xvar) / 100  # cumulative proportion of X
  
  # Y R2 (cumulative) across ncomp on the training set
  r2arr <- pls::R2(fm, estimate = "train")$val
  # Defensive indexing over dimnames
  dn1 <- dimnames(r2arr)[[1]]  # stats, e.g. "R2", "adjR2"
  dn2 <- dimnames(r2arr)[[2]]  # data set label
  # Pick "R2" row if present, else first
  i_stat <- if (!is.null(dn1)) match("R2", dn1, nomatch = 1) else 1
  i_data <- 1
  y_r2  <- as.numeric(r2arr[i_stat, i_data, , drop = TRUE])  # length = ncomp in the fit
  
  # Align lengths (some fits may not include all PCs)
  n <- min(length(xvar), length(y_r2))
  data.frame(
    ncomp            = seq_len(n),
    X_var_explained  = round(xvar[seq_len(n)]/100, 4),
    X_var_cum        = round(x_cum[seq_len(n)], 4),
    Y_R2_cum         = round(y_r2[seq_len(n)], 4),
    check.names = FALSE,
    row.names   = NULL
  )
}, striped = TRUE, hover = TRUE, spacing = "s")

# Predicted vs Actual plot on the held-out test partition (square aspect, 45° line)
output$pcr_PredPlot <- renderPlot({
  req(models$pcr)
  dat   <- getTestData()
  preds <- predict(models$pcr, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rang <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rang, ylim = rang, main = "Predicted vs Actual (PCR)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# Show the recipe object used
output$pcr_Recipe <- renderPrint({
  req(models$pcr)
  models$pcr$recipe
})

# Verbose print of the caret::train object
output$pcr_ModelSummary2 <- renderPrint({
  req(models$pcr)
  print(models$pcr)
})

# Coefficients at the best number of components
output$pcr_Coef <- renderTable({
  req(models$pcr)
  fm    <- models$pcr$finalModel
  bestK <- models$pcr$bestTune$ncomp
  
  cf <- stats::coef(fm, ncomp = bestK, intercept = TRUE)  # array [p x 1 x 1]
  v  <- as.numeric(drop(cf))                               # plain numeric vector
  # Try to get names from dimnames first, fall back to names(drop())
  dn <- dimnames(cf)
  nm <- if (!is.null(dn) && length(dn) >= 1 && !is.null(dn[[1]])) dn[[1]] else names(drop(cf))
  if (is.null(nm) || length(nm) != length(v)) nm <- paste0("V", seq_along(v))
  
  data.frame(
    Term        = nm,
    Coefficient = v,
    check.names = FALSE
  )
}, rownames = FALSE)


# ---- pcr_Explanation
output$pcr_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: RMSE vs #PCs; pick smallest PCs achieving minimal error to avoid overfit.",
    "Explained Variance: X variance cumulative and Y R² cumulative clarify how many PCs suffice.",
    "Predicted vs Actual: 45° reference line checks accuracy on the held-out split."
  )
)


# METHOD * cubist ------------------------------------------------------------------------------------------------------------------  
library(Cubist)

# Build recipe (no center/scale by default; Cubist is not scale-sensitive)
getCubistRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$cubist_Preprocess) %>%  # keep the order from UI
    step_rm(has_type("date"))                  # remove raw date columns after feature extraction
})

# Train
observeEvent(
  input$cubist_Go,
  {
    method <- "cubist"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      # Use caret defaults; you can also pass a custom tuneGrid if desired
      model <- caret::train(
        getCubistRecipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        trControl  = getTrControl(),
        tuneLength = 9,        # explore multiple (committees, neighbors) combos
        na.action  = na.pass
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# Load
observeEvent(
  input$cubist_Load,
  {
    method <- "cubist"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
      # If you persist the selected preprocess steps, restore them here:
      # updateSelectizeInput(session, "cubist_Preprocess", selected = model$<your_saved_steps>)
    }
  }
)

# Forget
observeEvent(
  input$cubist_Delete,
  {
    models[["cubist"]] <- NULL
    gc()
  }
)

# Method summary (short description from caret metadata helper)
output$cubist_ModelSummary0 <- renderText({
  description("cubist")
})

# Best resampled row
output$cubist_Metrics <- renderTable({
  req(models$cubist)
  models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
})

# Tuning profile across committees/neighbors (caret's plot method)
output$cubist_ModelPlots <- renderPlot({
  req(models$cubist)
  plot(models$cubist)
})

# Predicted vs Actual on test split (square aspect with 45° line)
output$cubist_PredPlot <- renderPlot({
  req(models$cubist)
  dat   <- getTestData()
  preds <- predict(models$cubist, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rang <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rang, ylim = rang, main = "Predicted vs Actual (Cubist)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# Variable usage (Cubist reports rule/model usage; caret::varImp wraps it)
output$cubist_VarImp <- renderTable({
  req(models$cubist)
  vi <- caret::varImp(models$cubist)
  # Ensure plain data.frame with atomic cols
  df <- as.data.frame(vi$importance)
  df <- tibble::rownames_to_column(df, "Predictor")
  # Order by importance descending
  df[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

# Recipe printout
output$cubist_Recipe <- renderPrint({
  req(models$cubist)
  models$cubist$recipe
})

# Verbose model print (includes committees & neighbor setting)
output$cubist_ModelSummary2 <- renderPrint({
  req(models$cubist)
  print(models$cubist)
})

# ---- cubist_Explanation 
output$cubist_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: committees (ensembles) and neighbors (instance adjustment) trade bias–variance.",
    "Variable Usage: rule/model usage highlights influential predictors across committees.",
    "Predicted vs Actual: ensemble smoothing should reduce scatter vs a single rule set."
  )
)


# METHOD * rf ------------------------------------------------------------------------------------------------------------------  

library(randomForest)

# Recipe for RF: no need to center/scale/corr; keep imputation + factor handling + date features
getRfRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$rf_Preprocess) %>%  # honor UI order
    step_rm(has_type("date"))              # remove raw date cols after extracting features
})

# Train
observeEvent(
  input$rf_Go,
  {
    method <- "rf"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      # caret will infer a sensible mtry grid; adjust tuneLength as needed
      model <- caret::train(
        getRfRecipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        trControl  = getTrControl(),
        tuneLength = 8,        # explore mtry values
        na.action  = na.pass   # we impute upstream; keep consistent with other methods
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# Load
observeEvent(
  input$rf_Load,
  {
    method <- "rf"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
      # If you persist preprocess steps, restore via:
      # updateSelectizeInput(session, "rf_Preprocess", selected = model$<your_saved_steps>)
    }
  }
)

# Forget
observeEvent(
  input$rf_Delete,
  {
    models[["rf"]] <- NULL
    gc()
  }
)

# Method summary text
output$rf_ModelSummary0 <- renderText({
  description("rf")
})

# Best resampled row (lowest RMSE)
output$rf_Metrics <- renderTable({
  req(models$rf)
  models$rf$results[ which.min(models$rf$results[, "RMSE"]), ]
})

# Tuning profile (caret's plot: RMSE vs mtry)
output$rf_ModelPlots <- renderPlot({
  req(models$rf)
  plot(models$rf)
})

# Predicted vs Actual on test split (square aspect + 45° line)
output$rf_PredPlot <- renderPlot({
  req(models$rf)
  dat   <- getTestData()
  preds <- predict(models$rf, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rang <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rang, ylim = rang, main = "Predicted vs Actual (RF)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# Variable importance (for regression: %IncMSE or IncNodePurity → caret flattens to Overall)
output$rf_VarImp <- renderTable({
  req(models$rf)
  vi <- caret::varImp(models$rf)
  df <- as.data.frame(vi$importance)
  df <- tibble::rownames_to_column(df, "Predictor")
  df[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

# Show the recipe object used
output$rf_Recipe <- renderPrint({
  req(models$rf)
  models$rf$recipe
})

# Verbose print of the caret::train object
output$rf_ModelSummary2 <- renderPrint({
  req(models$rf)
  print(models$rf)
})

# ---- rf_Explanation 
output$rf_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: RMSE vs mtry; larger mtry lowers bias but can increase variance.",
    "Variable Importance: highlights predictors driving splits; robust to collinearity.",
    "Predicted vs Actual: bagging stabilizes predictions; look for underfit at extremes."
  )
)


# METHOD * ranger -----------------------------------------------------------------------------------------------------------------------
library(ranger)

# reactive getRangerRecipe 
getRangerRecipe <- reactive({
  form <- formula(Response ~ .)
  # NOTE: order matters:
  # 1) create date features (no scaling needed for trees)
  # 2) impute (bag handles numeric + nominal)
  # 3) drop zero-variance
  # 4) dummy-encode nominal
  # 5) remove raw date
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$ranger_Preprocess) %>%
    step_rm(has_type("date"))
})

# observeEvent ranger_Go 
observeEvent(
  input$ranger_Go,
  {
    method <- "ranger"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      # Keep tuning simple & safe for seeds; tuneLength lets caret pick a reasonable mtry sequence.
      # splitrule: stay with "variance" (most stable for regression)
      # min.node.size: add a small grid via tuneGrid to improve bias/variance tradeoff
      tg <- NULL
      tg <- expand.grid(
        mtry          = NA,                 # NA -> caret fills via tuneLength var_seq
        splitrule     = "variance",
        min.node.size = c(1, 5, 10)
      )
      # caret fills mtry if it is NA only when using tuneLength; so pass tuneLength (>1)
      model <- caret::train(
        getRangerRecipe(),
        data       = getTrainData(),
        method     = "ranger",
        metric     = "RMSE",
        trControl  = getTrControl(),   # keep your global control
        tuneLength = 5,                # caret will generate a safe mtry sequence
        importance = "impurity",
        num.trees  = 1000,
        na.action  = na.omit,          # prevents NA preds -> NA metrics
        respect.unordered.factors = "order"
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

observeEvent(
  input$ranger_Load,
  {
    method <- "ranger"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  }
)

observeEvent(
  input$ranger_Delete,
  {
    models[["ranger"]] <- NULL
    gc()
  }
)

# output ranger_ModelSummary0 (text) 
output$ranger_ModelSummary0 <- renderText({
  description("ranger")
})

# output ranger_Metrics (table) 
output$ranger_Metrics <- renderTable({
  req(models$ranger)
  models$ranger$results[ which.min(models$ranger$results[, "RMSE"]), ]
})

# output ranger_ModelPlots (plot) 
output$ranger_ModelPlots <- renderPlot({
  req(models$ranger)
  plot(models$ranger)   # RMSE vs mtry (faceted by min.node.size if present)
})

# output ranger_PredPlot (plot) 
output$ranger_PredPlot <- renderPlot({
  req(models$ranger)
  dat   <- getTestData()
  preds <- predict(models$ranger, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rang <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rang, ylim = rang, main = "Predicted vs Actual (Ranger)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# output ranger_VarImp (table) 
output$ranger_VarImp <- renderTable({
  req(models$ranger)
  vi <- caret::varImp(models$ranger)
  df <- as.data.frame(vi$importance)
  df <- tibble::rownames_to_column(df, "Predictor")
  df[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

# output ranger_Recipe (print) 
output$ranger_Recipe <- renderPrint({
  req(models$ranger)
  models$ranger$recipe
})

# output ranger_ModelSummary2 (print) 
output$ranger_ModelSummary2 <- renderPrint({
  req(models$ranger)
  print(models$ranger)
})

# compact bullets (same style as others)
output$ranger_Explanation <- makeExplanation(
  bullets = c(
    "Tuning: mtry (random subspace size) controls bias/variance; min.node.size regularizes leaves.",
    "Pred vs Actual: tight 45° alignment = good fit; systematic curves imply missing interactions.",
    "Variable Importance: impurity-based ranks splits; use as a heuristic, not causal evidence."
  )
)


# ---- ranger_Explanation 
output$ranger_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: RMSE across mtry/splitrule/min.node.size; pick stable low-error region.",
    "Variable Importance: surfacing key splitters; sanity-check against domain knowledge.",
    "Predicted vs Actual: square 1:1 plot to assess calibration and error spread."
  )
)




# METHOD * xgbTree -----------------------------------------------------------------------------------------------------------------------
library(xgboost)  # caret will call into xgboost under the hood

# reactive getXgbRecipe 
getXgbRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$xgb_Preprocess) %>%  # honor UI order
    step_rm(has_type("date")) %>%           # remove raw date columns after feature extraction
    step_zv(all_predictors())               # guard against constant predictors
})

# observeEvent xgb_Go 
observeEvent(
  input$xgb_Go,
  {
    method <- "xgbTree"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      # 1) Clone the global trControl, but neutralize seeds ONLY for xgbTree
      ctrl <- getTrControl()
      # ctrl$seeds <- NA  # <-- IMPORTANT: let caret size seeds correctly for xgbTree's looped grid
      # (Optional) deterministic randomness for the whole train call:
      # set.seed(20251004)
      
      model <- caret::train(
        getXgbRecipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        trControl  = ctrl,      # <-- use the LOCAL ctrl
        tuneLength = 8,
        na.action  = na.pass
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)
# observeEvent xgb_Load 
observeEvent(
  input$xgb_Load,
  {
    method <- "xgbTree"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
      # If you store the preprocess steps with the model object, restore them here via updateSelectizeInput(...)
    }
  }
)

# observeEvent xgb_Delete 
observeEvent(
  input$xgb_Delete,
  {
    models[["xgbTree"]] <- NULL
    gc()
  }
)

# output xgb_ModelSummary0 (text) 
output$xgb_ModelSummary0 <- renderText({
  description("xgbTree")   # caret method name
})

# output xgb_Metrics (table) 
output$xgb_Metrics <- renderTable({
  req(models$xgbTree)
  models$xgbTree$results[ which.min(models$xgbTree$results[, "RMSE"]), ]
})

# output xgb_ModelPlots (plot) 
output$xgb_ModelPlots <- renderPlot({
  req(models$xgbTree)
  plot(models$xgbTree)   # RMSE across the tuned parameters
})

# output xgb_PredPlot (plot) 
output$xgb_PredPlot <- renderPlot({
  req(models$xgbTree)
  dat   <- getTestData()
  preds <- predict(models$xgbTree, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rang <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rang, ylim = rang, main = "Predicted vs Actual (XGBoost)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# output xgb_VarImp (table) 
output$xgb_VarImp <- renderTable({
  req(models$xgbTree)
  vi <- caret::varImp(models$xgbTree)
  df <- as.data.frame(vi$importance)
  df <- tibble::rownames_to_column(df, "Predictor")
  df[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

# output xgb_Recipe (print) 
output$xgb_Recipe <- renderPrint({
  req(models$xgbTree)
  models$xgbTree$recipe
})

# output xgb_ModelSummary2 (print) 
output$xgb_ModelSummary2 <- renderPrint({
  req(models$xgbTree)
  print(models$xgbTree)
})

# ---- xgb_Explanation 
output$xgb_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: tracks RMSE over nrounds/depth/eta; stop where validation error stops improving.",
    "Predicted vs Actual: 45° alignment indicates accuracy; large scatter suggests under/over-regularization.",
    "Variable Importance: gain-based importance reveals features most used to reduce loss."
  )
)





# METHOD * svmRadial -------------------------------------------------------------------------------------------------------------------
library(kernlab)

# reactive getSvmRRecipe 
getSvmRRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$svmR_Preprocess) %>%  # ensure center/scale included
    step_rm(has_type("date")) %>%            # remove raw date columns after feature engineering
    step_zv(all_predictors())                # guard against constants
})

# observeEvent svmR_Go 
observeEvent(
  input$svmR_Go,
  {
    method <- "svmRadial"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      # Keep global control, but right-size seeds locally to avoid caret 'Bad seeds' issues.
      # svmRadial's grid size = tuneLength (no loop). We'll use a modest len = 11.
      ctrl <- getTrControl()
      local_len <- 11L
      # helper from earlier: .make_local_seeds(n_resamples, grid_size, ...)
      ctrl$seeds <- .make_local_seeds(n_resamples = 25, grid_size = local_len)
      
      model <- caret::train(
        getSvmRRecipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        trControl  = ctrl,
        tuneLength = local_len,   # matches local seeds
        na.action  = na.pass
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# observeEvent svmR_Load 
observeEvent(
  input$svmR_Load,
  {
    method <- "svmRadial"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  }
)

# observeEvent svmR_Delete 
observeEvent(
  input$svmR_Delete,
  {
    models[["svmRadial"]] <- NULL
    gc()
  }
)

# output svmR_ModelSummary0 (text) 
output$svmR_ModelSummary0 <- renderText({
  description("svmRadial")
})

# output svmR_Metrics (table) 
output$svmR_Metrics <- renderTable({
  req(models$svmRadial)
  models$svmRadial$results[ which.min(models$svmRadial$results[, "RMSE"]), ]
})

# output svmR_ModelPlots (plot) 
output$svmR_ModelPlots <- renderPlot({
  req(models$svmRadial)
  plot(models$svmRadial)   # RMSE across (sigma, C)
})

# output svmR_PredPlot (plot) 
output$svmR_PredPlot <- renderPlot({
  req(models$svmRadial)
  dat   <- getTestData()
  preds <- predict(models$svmRadial, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rang <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rang, ylim = rang, main = "Predicted vs Actual (SVM-RBF)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# output svmR_VarImp (table) 
output$svmR_VarImp <- renderTable({
  req(models$svmRadial)
  vi <- try(caret::varImp(models$svmRadial), silent = TRUE)
  if (inherits(vi, "try-error") || is.null(vi)) {
    data.frame(Message = "varImp not available for this fit; consider permutation importance.")
  } else {
    df <- as.data.frame(vi$importance)
    df <- tibble::rownames_to_column(df, "Predictor")
    df[order(df[[ncol(df)]], decreasing = TRUE), , drop = FALSE]
  }
}, rownames = FALSE)

# output svmR_Recipe (print) 
output$svmR_Recipe <- renderPrint({
  req(models$svmRadial)
  models$svmRadial$recipe
})

# output svmR_ModelSummary2 (print) 
output$svmR_ModelSummary2 <- renderPrint({
  req(models$svmRadial)
  print(models$svmRadial)
})

# ---- SVM-RBF Diagnostic Explanation (compact bullets) 
output$svmR_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: trades off kernel width (sigma) and regularization (C); watch RMSE surface.",
    "Predicted vs Actual: 45° line indicates calibration; curvature or large spread implies under/overfitting.",
    "Variable Importance: may be limited for kernel SVM; consider permutation importance for robust insights."
  )
)








# METHOD * svmPoly -----------------------------------------------------------------------------------------------------------------------
library(kernlab)

# reactive getSvmPRecipe 
getSvmPRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$svmP_Preprocess) %>%  # ensure center/scale included for kernel stability
    step_rm(has_type("date")) %>%            # remove raw date columns after feature engineering
    step_zv(all_predictors())                # guard against constants
})

# observeEvent svmP_Go 
observeEvent(
  input$svmP_Go,
  {
    method <- "svmPoly"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      # Keep global control but right-size seeds locally to avoid 'Bad seeds' errors.
      # CARET grid size for svmPoly (no loop) = min(len,3) * len * len
      ctrl <- getTrControl()
      local_len  <- 5L                         # reasonable coverage without heavy runtime
      grid_size  <- min(local_len, 3L) * local_len * local_len
      ctrl$seeds <- .make_local_seeds(n_resamples = 25, grid_size = grid_size)
      
      # Notes & tips
      # •	Scaling is critical for polynomial kernels → keep center/scale in svmP_initial.
      # •	Seeds: The local seeds use the correct grid size:
      # ${grid\_size}$ = min({len}, 3) \times {len} \times {len}$
      # which matches caret’s grid() for svmPoly.
      # •	Speed: Increase/decrease local_len to trade off coverage vs time. If you set local_len <- 7L, also updates seeds via grid_size = min(7,3)*7*7 = 147.
      
      model <- caret::train(
        getEarthRecipe(),          # recipe only; no preProcess=
        data       = getTrainData(),
        method     = "earth",
        metric     = "RMSE",
        trControl  = ctrl,         # your local seeds OK
        tuneLength = 12,
        na.action  = na.fail       # earth requires this
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# observeEvent svmP_Load 
observeEvent(
  input$svmP_Load,
  {
    method <- "svmPoly"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  }
)

# observeEvent svmP_Delete 
observeEvent(
  input$svmP_Delete,
  {
    models[["svmPoly"]] <- NULL
    gc()
  }
)

# output svmP_ModelSummary0 (text) 
output$svmP_ModelSummary0 <- renderText({
  description("svmPoly")
})

# output svmP_Metrics (table) 
output$svmP_Metrics <- renderTable({
  req(models$svmPoly)
  models$svmPoly$results[ which.min(models$svmPoly$results[, "RMSE"]), ]
})

# output svmP_ModelPlots (plot) 
output$svmP_ModelPlots <- renderPlot({
  req(models$svmPoly)
  m <- models$svmPoly
  res <- m$results
  
  # Defensive: check that numeric columns exist and are finite
  if (nrow(res) == 0 || all(is.na(res$RMSE))) {
    plot.new()
    title("No valid RMSE results to display")
    return(invisible())
  }
  
  # caret::plot.train sometimes fails with invalid digits if the grid is degenerate
  tryCatch({
    plot(m)
  }, error = function(e) {
    message("Caret plot failed: ", e$message)
    # fallback: custom ggplot for RMSE vs C, colored by degree, faceted by scale
    if (all(c("C","RMSE") %in% names(res))) {
      library(ggplot2)
      ggplot(res, aes(x = C, y = RMSE, color = factor(degree))) +
        geom_line() +
        geom_point() +
        facet_wrap(~ round(scale, 3)) +
        theme_minimal() +
        labs(
          title = "SVM-Poly Tuning Profile",
          x = "Cost (C)",
          y = "RMSE",
          color = "Degree"
        )
    } else {
      plot.new()
      title("Tuning profile unavailable (check RMSE values)")
    }
  })
})

# output svmP_PredPlot (plot) 
output$svmP_PredPlot <- renderPlot({
  req(models$svmPoly)
  dat   <- getTestData()
  preds <- predict(models$svmPoly, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  
  # Ensure numeric values and define common range
  rang <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  
  # Base plot with fixed 1:1 aspect ratio
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rang, ylim = rang,
       main = "Predicted vs Actual (SVM-Poly)",
       asp  = 1,                    # 👈 ensures the plot is square
       pch  = 19, col = "darkgray")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# output svmP_VarImp (table) 
output$svmP_VarImp <- renderTable({
  req(models$svmPoly)
  vi <- try(caret::varImp(models$svmPoly), silent = TRUE)
  if (inherits(vi, "try-error") || is.null(vi)) {
    data.frame(Message = "varImp not available for this kernel; consider permutation importance.")
  } else {
    df <- as.data.frame(vi$importance)
    df <- tibble::rownames_to_column(df, "Predictor")
    df[order(df[[ncol(df)]], decreasing = TRUE), , drop = FALSE]
  }
}, rownames = FALSE)

# output svmP_Recipe (print) 
output$svmP_Recipe <- renderPrint({
  req(models$svmPoly)
  models$svmPoly$recipe
})

# output svmP_ModelSummary2 (print) 
output$svmP_ModelSummary2 <- renderPrint({
  req(models$svmPoly)
  print(models$svmPoly)
})

# ---- SVM-Poly Diagnostic Explanation (compact bullets) 
output$svmP_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: explores degree/scale/C; higher degree increases interaction order and complexity.",
    "Predicted vs Actual: 45° agreement indicates calibration; curvature/spread suggests under/overfitting.",
    "Variable Importance: limited for kernel SVMs; use permutation importance for feature ranking."
  )
)


# METHOD * earth (MARS) ----------------------------------------------------------------------------------------------------------------------
library(earth)

# ---- Recipe for earth: robust, NA-safe, honors UI order via dynamicSteps() 
getEarthRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    # Keep user-chosen steps in the exact order (from UI earth_Preprocess)
    dynamicSteps(input$earth_Preprocess) %>%
    # Always drop raw date columns after month/dow feature extraction
    step_rm(has_type("date")) %>%
    # Safety: if user忘记加 zero-variance in UI，补一道
    step_zv(all_predictors())
})

# ---- Train (no preProcess=, local seeds right-sized, earth wants na.fail) 
observeEvent(
  input$earth_Go,
  {
    method <- "earth"
    models[[method]] <- NULL
    showNotification(id = method,
                     paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      # reuse global trControl; ONLY fix seeds size for this method
      ctrl <- getTrControl()
      # caret::earth with default loop -> 1 effective model per resample
      ctrl$seeds <- .make_local_seeds(n_resamples = 25, grid_size = 1L)
      
      model <- caret::train(
        getEarthRecipe(),
        data       = getTrainData(),  # let recipe handle all NA work
        method     = method,
        metric     = "RMSE",
        trControl  = ctrl,
        tuneLength = 12,              # explore nprune sequence (degree=1 by default grid)
        na.action  = na.fail          # earth requires na.fail
        # IMPORTANT: do NOT pass preProcess= here
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# ---- Load / Delete 
observeEvent(
  input$earth_Load,
  {
    method <- "earth"
    model <- loadRds(method, session)
    if (!is.null(model)) models[[method]] <- model
  }
)

observeEvent(
  input$earth_Delete,
  {
    models[["earth"]] <- NULL
    gc()
  }
)

# ---- Outputs 

# Summary text (caret description)
output$earth_ModelSummary0 <- renderText({
  description("earth")
})

# Best resampled metrics row
output$earth_Metrics <- renderTable({
  req(models$earth)
  models$earth$results[ which.min(models$earth$results[, "RMSE"]), ]
})

# Tuning profile (with robust fallback)
output$earth_ModelPlots <- renderPlot({
  req(models$earth)
  m <- models$earth
  res <- m$results
  if (nrow(res) == 0 || all(is.na(res$RMSE))) {
    plot.new(); title("No valid RMSE results to display for MARS"); return(invisible())
  }
  tryCatch({
    plot(m)  # RMSE vs nprune (and degree if present)
  }, error = function(e) {
    if (all(c("nprune","RMSE") %in% names(res))) {
      plot(res$nprune, res$RMSE, type = "b",
           xlab = "nprune (#terms after pruning)", ylab = "RMSE",
           main = "MARS Tuning Profile (fallback)")
    } else {
      plot.new(); title("Tuning profile unavailable (check RMSE values)")
    }
  })
})

# Predicted vs Actual (square aspect)
output$earth_PredPlot <- renderPlot({
  req(models$earth)
  dat   <- getTestData()
  preds <- predict(models$earth, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rang <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rang, ylim = rang,
       main = "Predicted vs Actual (MARS)",
       asp  = 1, pch = 19, col = "darkgray")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# Variable Importance (earth/evimp via caret::varImp)
output$earth_VarImp <- renderTable({
  req(models$earth)
  vi <- caret::varImp(models$earth)
  df <- as.data.frame(vi$importance)
  df <- tibble::rownames_to_column(df, "Predictor")
  df[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

# Print recipe actually used
output$earth_Recipe <- renderPrint({
  req(models$earth)
  models$earth$recipe
})

# Print full caret model
output$earth_ModelSummary2 <- renderPrint({
  req(models$earth)
  print(models$earth)
})

# Compact explanation panel (bullets)
output$earth_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: RMSE vs nprune (#terms kept after pruning). Choose the smallest nprune that achieves low error.",
    "Predicted vs Actual: square 1:1 view checks calibration; deviations from the 45° line show bias.",
    "Variable Importance: derived from earth/evimp, highlights variables with hinge terms after pruning."
  )
)


# METHOD * bagEarthGCV ----------------------------------------------------------------------------------------------------------------------
# caret method: "bagEarthGCV"  (Regression + GCV pruning; tunes 'degree')
library(earth)

# Recipe
getBagGCVRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$bagGCV_Preprocess) %>%  # honors chosen order
    step_rm(has_type("date")) %>%              # drop raw dates after month/dow
    step_zv(all_predictors())
})

# Train
observeEvent(
  input$bagGCV_Go,
  {
    method <- "bagEarthGCV"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      # Local seeds sized to grid rows; bagEarthGCV tunes only 'degree' (1..len)
      ctrl <- getTrControl()
      local_len <- 3L           # degrees 1..3 is a solid start
      ctrl$seeds <- .make_local_seeds(n_resamples = 25, grid_size = local_len)
      
      model <- caret::train(
        getBagGCVRecipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        trControl  = ctrl,
        tuneLength = local_len,   # degrees 1..local_len
        na.action  = na.fail      # keep parity with earth: enforce complete matrix post-recipe
        # IMPORTANT: do NOT pass preProcess= when using a recipe
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# Load / Delete
observeEvent(
  input$bagGCV_Load,
  {
    method <- "bagEarthGCV"
    model <- loadRds(method, session)
    if (!is.null(model)) models[[method]] <- model
  }
)
observeEvent(
  input$bagGCV_Delete,
  {
    models[["bagEarthGCV"]] <- NULL
    gc()
  }
)

# Outputs
output$bagGCV_ModelSummary0 <- renderText({
  description("bagEarthGCV")
})

output$bagGCV_Metrics <- renderTable({
  req(models$bagEarthGCV)
  models$bagEarthGCV$results[ which.min(models$bagEarthGCV$results[, "RMSE"]), ]
})

output$bagGCV_ModelPlots <- renderPlot({
  req(models$bagEarthGCV)
  m <- models$bagEarthGCV
  res <- m$results
  if (nrow(res) == 0 || all(is.na(res$RMSE))) {
    plot.new(); title("No valid RMSE results to display for Bagged MARS (GCV)"); return(invisible())
  }
  tryCatch({
    plot(m)  # RMSE vs degree
  }, error = function(e) {
    if (all(c("degree","RMSE") %in% names(res))) {
      plot(res$degree, res$RMSE, type = "b",
           xlab = "degree", ylab = "RMSE",
           main = "Bagged MARS (GCV) Tuning Profile (fallback)")
    } else {
      plot.new(); title("Tuning profile unavailable (check RMSE values)")
    }
  })
})

output$bagGCV_PredPlot <- renderPlot({
  req(models$bagEarthGCV)
  dat   <- getTestData()
  preds <- predict(models$bagEarthGCV, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rang <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rang, ylim = rang,
       main = "Predicted vs Actual (Bagged MARS, GCV)",
       asp  = 1, pch = 19, col = "darkgray")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

output$bagGCV_VarImp <- renderTable({
  req(models$bagEarthGCV)
  vi <- caret::varImp(models$bagEarthGCV)
  df <- as.data.frame(vi$importance)
  df <- tibble::rownames_to_column(df, "Predictor")
  df[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

output$bagGCV_Recipe <- renderPrint({
  req(models$bagEarthGCV)
  models$bagEarthGCV$recipe
})

output$bagGCV_ModelSummary2 <- renderPrint({
  req(models$bagEarthGCV)
  print(models$bagEarthGCV)
})

# Compact explanation panel
output$bagGCV_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: varies interaction degree; higher degree allows more complex hinge interactions.",
    "Predicted vs Actual: 1:1 square plot to check calibration and systematic bias.",
    "Variable Importance: aggregates across bagged base learners; hinge terms reflect nonlinear effects."
  )
)


# METHOD * nnet ----------------------------------------------------------------------------------------------------------------------------
library(nnet)

# --- Recipe builder
getNnetRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$nnet_Preprocess) %>%
    step_zv(all_predictors())
})

# --- Train
observeEvent(
  input$nnet_Go,
  {
    method <- "nnet"
    models[[method]] <- NULL
    showNotification(id = method, paste("Training", method, "model..."),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      ctrl <- getTrControl()
      local_len <- 3L
      
      model <- caret::train(
        getNnetRecipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        tuneLength = local_len,
        trControl  = ctrl,
        linout     = TRUE,    # regression output
        trace      = TRUE,   # suppress verbose training output
        maxit      = 100,     # increase iteration limit
        na.action  = na.fail
      )
      
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# --- Load / Delete
observeEvent(input$nnet_Load, {
  method <- "nnet"
  model <- loadRds(method, session)
  if (!is.null(model)) models[[method]] <- model
})
observeEvent(input$nnet_Delete, {
  models[["nnet"]] <- NULL
  gc()
})

# --- Outputs
output$nnet_ModelSummary0 <- renderText({
  description("nnet")
})

output$nnet_Metrics <- renderTable({
  req(models$nnet)
  models$nnet$results[ which.min(models$nnet$results[, "RMSE"]), ]
})

output$nnet_ModelPlots <- renderPlot({
  req(models$nnet)
  m <- models$nnet
  res <- m$results
  if (nrow(res) == 0 || all(is.na(res$RMSE))) {
    plot.new(); title("No valid RMSE results for Neural Network"); return(invisible())
  }
  tryCatch({
    plot(m)
  }, error = function(e) {
    if (all(c("size","RMSE") %in% names(res))) {
      plot(res$size, res$RMSE, type = "b",
           xlab = "Hidden Units", ylab = "RMSE",
           main = "nnet Tuning Profile (fallback)")
    } else {
      plot.new(); title("Tuning profile unavailable")
    }
  })
})

output$nnet_PredPlot <- renderPlot({
  req(models$nnet)
  dat   <- getTestData()
  preds <- predict(models$nnet, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rang <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rang, ylim = rang,
       main = "Predicted vs Actual (nnet)",
       asp  = 1, pch = 19, col = "gray40")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

output$nnet_VarImp <- renderTable({
  req(models$nnet)
  vi <- caret::varImp(models$nnet)
  df <- as.data.frame(vi$importance)
  df <- tibble::rownames_to_column(df, "Predictor")
  df[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

output$nnet_Recipe <- renderPrint({
  req(models$nnet)
  models$nnet$recipe
})

output$nnet_ModelSummary2 <- renderPrint({
  req(models$nnet)
  print(models$nnet)
})

# --- Compact explanation
output$nnet_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: explores combinations of hidden-unit count and L2 decay strength.",
    "Predicted vs Actual: checks fit tightness; systematic curvature indicates underfitting.",
    "Variable Importance: computed via Garson weights; higher values indicate stronger influence."
  )
)



# METHOD * avNNet ---------------------------------------------------------------------------------------------------------------------------
library(nnet)

# Recipe (honours UI step order)
getAvNNetRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$avNNet_Preprocess) %>%   # use <method>_Preprocess
    step_rm(has_type("date")) %>%               # drop raw dates after feature extraction
    step_zv(all_predictors())
})

# Train
observeEvent(
  input$avNNet_Go,
  {
    method <- "avNNet"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      ctrl <- getTrControl()
      local_len <- 5L                        # tune over 5 combos (size & decay; bag fixed in grid)
      # ctrl$seeds <- .make_local_seeds(n_resamples = 25, grid_size = local_len)
      
      model <- caret::train(
        getAvNNetRecipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        trControl  = ctrl,                   # local seeds only for this method
        tuneLength = local_len,
        linout     = TRUE,                   # regression output
        trace      = TRUE,                  # quiet training
        repeats    = 5,                      # number of nets to average (caret::avNNet default = 5)
        allowParallel = TRUE,
        na.action  = na.fail
      )
      
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# Load / Delete
observeEvent(input$avNNet_Load, {
  method <- "avNNet"
  model <- loadRds(method, session)
  if (!is.null(model)) models[[method]] <- model
})
observeEvent(input$avNNet_Delete, {
  models[["avNNet"]] <- NULL
  gc()
})

# Outputs
output$avNNet_ModelSummary0 <- renderText({
  description("avNNet")   # caret method name
})

output$avNNet_Metrics <- renderTable({
  req(models$avNNet)
  models$avNNet$results[ which.min(models$avNNet$results[, "RMSE"]), ]
})

output$avNNet_ModelPlots <- renderPlot({
  req(models$avNNet)
  m <- models$avNNet
  res <- m$results
  if (nrow(res) == 0 || all(is.na(res$RMSE))) {
    plot.new(); title("No valid RMSE results to display for avNNet"); return(invisible())
  }
  tryCatch({
    plot(m)    # caret’s tuning profile (RMSE vs size/decay; facets if needed)
  }, error = function(e) {
    if (all(c("size","RMSE") %in% names(res))) {
      plot(res$size, res$RMSE, type = "b",
           xlab = "Hidden Units (size)", ylab = "RMSE",
           main = "avNNet Tuning Profile (fallback)")
    } else {
      plot.new(); title("Tuning profile unavailable (check RMSE values)")
    }
  })
})

output$avNNet_PredPlot <- renderPlot({
  req(models$avNNet)
  dat   <- getTestData()
  preds <- predict(models$avNNet, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rang <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rang, ylim = rang,
       main = "Predicted vs Actual (avNNet)",
       asp  = 1, pch = 19, col = "gray40")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

output$avNNet_VarImp <- renderTable({
  req(models$avNNet)
  vi <- caret::varImp(models$avNNet)
  df <- as.data.frame(vi$importance)
  df <- tibble::rownames_to_column(df, "Predictor")
  df[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

output$avNNet_Recipe <- renderPrint({
  req(models$avNNet)
  models$avNNet$recipe
})

output$avNNet_ModelSummary2 <- renderPrint({
  req(models$avNNet)
  print(models$avNNet)
})

# Compact explanation panel (bullets), matching your style
output$avNNet_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: explores hidden units (size) and L2 decay; bagging optionally enabled.",
    "Predicted vs Actual: square 1:1 plot for calibration and bias checks.",
    "Variable Importance: Garson-style weights aggregated over the averaged nets."
  )
)

#  METHOD * mlp (RSNNS) ---------------------------------------------------------------------------------------------------------------------------
library(RSNNS)

# recipe
getMlpRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$mlp_Preprocess) %>%
    step_rm(has_type("date")) %>%      # remove raw date cols after month/dow created
    step_zv(all_predictors())          # guard again (cheap)
})

observeEvent(
  input$mlp_Go,
  {
    method <- "mlp"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
    
    # Typical RSNNS grid: size (hidden units) and learnFuncParams via caret tuning length
    # caret handles sensible defaults; we let tuneLength drive size/etc.
    model <- caret::train(
      getMlpRecipe(),
      data       = getTrainData(),
      method     = method,
      metric     = "RMSE",
      trControl  = getTrControl(),
      tuneLength = 5,
      na.action  = na.pass,
      trace = TRUE
    )
    deleteRds(method); saveToRds(model, method); models[[method]] <- model
  }
)

observeEvent(input$mlp_Load,   { if (!is.null(loadRds("mlp", session))) models[["mlp"]] <- loadRds("mlp", session) })
observeEvent(input$mlp_Delete, { models[["mlp"]] <- NULL; gc() })

output$mlp_ModelSummary0 <- renderText({ description("mlp") })
output$mlp_Metrics <- renderTable({ req(models$mlp); models$mlp$results[ which.min(models$mlp$results[, "RMSE"]), ] })

output$mlp_ModelPlots <- renderPlot({ req(models$mlp); plot(models$mlp) })

output$mlp_PredPlot <- renderPlot({
  req(models$mlp)
  dat   <- getTestData()
  preds <- predict(models$mlp, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual, xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (MLP)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

output$mlp_VarImp <- renderTable({
  req(models$mlp)
  vi <- try(caret::varImp(models$mlp), silent = TRUE)
  if (inherits(vi, "try-error")) {
    data.frame(Note = "Variable importance not available for this model.")
  } else {
    df <- as.data.frame(vi$importance)
    tibble::rownames_to_column(df, "Predictor")[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
  }
}, rownames = FALSE)

output$mlp_Recipe <- renderPrint({ req(models$mlp); models$mlp$recipe })
output$mlp_ModelSummary2 <- renderPrint({ req(models$mlp); print(models$mlp) })

output$mlp_Explanation <- makeExplanation(bullets = c(
  "Tuning Profile: follow RMSE across hidden units / learning params.",
  "Predicted vs Actual: tight 45° scatter = good fit; bowing = bias.",
  "VarImp (if available): heuristic connection-weight importance."
))


#  METHOD * monmlp ---------------------------------------------------------------------------------------------------------------------------
# Package: monmlp
library(monmlp)

getMonmlpRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$monmlp_Preprocess) %>%
    step_rm(has_type("date")) %>%
    step_zv(all_predictors())
})

observeEvent(
  input$monmlp_Go,
  {
    method <- "monmlp"  # caret method id
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
    
    # monmlp uses layer sizes; caret::train() exposes sensible grid via tuneLength
    model <- caret::train(
      getMonmlpRecipe(),
      data       = getTrainData(),
      method     = method,
      metric     = "RMSE",
      trControl  = getTrControl(),
      tuneLength = 5,
      na.action  = na.pass,
      trace = TRUE
    )
    deleteRds(method); saveToRds(model, method); models[[method]] <- model
  }
)

observeEvent(input$monmlp_Load,   { if (!is.null(loadRds("monmlp", session))) models[["monmlp"]] <- loadRds("monmlp", session) })
observeEvent(input$monmlp_Delete, { models[["monmlp"]] <- NULL; gc() })

output$monmlp_ModelSummary0 <- renderText({ description("monmlp") })
output$monmlp_Metrics <- renderTable({ req(models$monmlp); models$monmlp$results[ which.min(models$monmlp$results[, "RMSE"]), ] })

output$monmlp_ModelPlots <- renderPlot({ req(models$monmlp); plot(models$monmlp) })

output$monmlp_PredPlot <- renderPlot({
  req(models$monmlp)
  dat   <- getTestData()
  preds <- predict(models$monmlp, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual, xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (monmlp)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

output$monmlp_VarImp <- renderTable({
  req(models$monmlp)
  vi <- try(caret::varImp(models$monmlp), silent = TRUE)
  if (inherits(vi, "try-error")) {
    data.frame(Note = "Variable importance not available for this model.")
  } else {
    df <- as.data.frame(vi$importance)
    tibble::rownames_to_column(df, "Predictor")[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
  }
}, rownames = FALSE)

output$monmlp_Recipe <- renderPrint({ req(models$monmlp); models$monmlp$recipe })
output$monmlp_ModelSummary2 <- renderPrint({ req(models$monmlp); print(models$monmlp) })

output$monmlp_Explanation <- makeExplanation(bullets = c(
  "Tuning Profile: track RMSE across layer widths (capacity).",
  "Predicted vs Actual: systematic curvature implies under/over-fitting.",
  "VarImp: not always available; prefer partial dependency for insight."
))


# METHOD * knn ---------------------------------------------------------------------------------------------------------------------------
# caret’s method name: "knn"  (k-nearest neighbors regression)

# reactive getKnnRecipe 
getKnnRecipe <- reactive({
  form <- Response ~ .
  recipes::recipe(form, data = getTrainData()) %>%
    # Honor the UI-selected steps in the order chosen
    dynamicSteps(input$knn_Preprocess) %>%
    # Safety: remove raw date columns after feature engineering
    recipes::step_rm(has_type("date")) %>%
    # Extra guard: ensure all-predictor names are unique and non-empty after dummies
    recipes::step_zv(all_predictors())
})

# observeEvent knn_Go 
observeEvent(
  input$knn_Go,
  {
    method <- "knn"
    models[[method]] <- NULL
    showNotification(
      id = method,
      paste("Processing", method, "model using resampling"),
      session = session, duration = NULL
    )
    clus <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(
        getKnnRecipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        trControl  = getTrControl(),  # keep your global bootstrap control
        tuneLength = 10,              # try k across a reasonable span
        na.action  = na.pass
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# observeEvent knn_Load 
observeEvent(
  input$knn_Load,
  {
    method <- "knn"
    model  <- loadRds(method, session)
    if (!is.null(model)) models[[method]] <- model
  }
)

# observeEvent knn_Delete 
observeEvent(
  input$knn_Delete,
  {
    models[["knn"]] <- NULL
    gc()
  }
)

# output knn_ModelSummary0 (text) 
output$knn_ModelSummary0 <- renderText({
  description("knn")  # caret method name
})

# output knn_Metrics (table) 
output$knn_Metrics <- renderTable({
  req(models$knn)
  models$knn$results[ which.min(models$knn$results[, "RMSE"]), ]
})

# output knn_ModelPlots (plot) 
output$knn_ModelPlots <- renderPlot({
  req(models$knn)
  plot(models$knn)   # RMSE vs k (caret auto-facets if needed)
})

# output knn_PredPlot (plot) 
output$knn_PredPlot <- renderPlot({
  req(models$knn)
  dat   <- getTestData()
  preds <- predict(models$knn, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (KNN)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# output knn_Recipe (print) 
output$knn_Recipe <- renderPrint({
  req(models$knn)
  models$knn$recipe
})

# output knn_ModelSummary2 (print) 
output$knn_ModelSummary2 <- renderPrint({
  req(models$knn)
  print(models$knn)
})

# ---- KNN Diagnostic Explanation (compact bullets) 
output$knn_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: RMSE vs k; too-small k overfits, too-large k underfits.",
    "Predicted vs Actual: 45° line indicates accuracy; fan shape suggests heteroskedasticity.",
    "Preprocessing: KNN needs centered & scaled numerics and dummy-encoded factors."
  )
)


# METHOD * bayesglm ---------------------------------------------------------------------------------------------------------------------------
library(arm)   # for bayesglm

# reactive getBayesglmRecipe 
getBayesglmRecipe <- reactive({
  form <- Response ~ .
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$bayesglm_Preprocess) %>%
    # Ensure factor NA becomes "unknown" *before* dummies (prevents new-level warnings)
    recipes::step_unknown(all_nominal_predictors(), new_level = "unknown") %>%
    # Drop raw date columns after deriving month/dow:
    recipes::step_rm(has_type("date"))
})

# observeEvent bayesglm_Go 
observeEvent(
  input$bayesglm_Go,
  {
    method <- "bayesglm"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      # No tuning grid for bayesglm; caret will resample a single spec.
      # We keep your global getTrControl(). family defaults to gaussian() for numeric y.
      model <- caret::train(
        getBayesglmRecipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        trControl  = getTrControl(),
        na.action  = na.pass    # recipe handles NA; pass-through here is fine
        # you could add family = gaussian() here explicitly, but caret sets it already for regression
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

observeEvent(
  input$bayesglm_Load,
  {
    method <- "bayesglm"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  }
)

observeEvent(
  input$bayesglm_Delete,
  {
    models[["bayesglm"]] <- NULL
    gc()
  }
)

# output bayesglm_ModelSummary0 (text) 
output$bayesglm_ModelSummary0 <- renderText({
  description("bayesglm")  # caret method name
})

# output bayesglm_Metrics (table) 
output$bayesglm_Metrics <- renderTable({
  req(models$bayesglm)
  models$bayesglm$results[ which.min(models$bayesglm$results[, "RMSE"]), ]
})

# Model diagnostics (Residual vs Fitted, QQ)
output$bayesglm_ModelPlots <- renderPlot({
  req(models$bayesglm)
  fm <- models$bayesglm$finalModel
  
  # Remove NA residuals to avoid "non-numeric" issues
  fm$residuals <- stats::na.omit(fm$residuals)
  par(mfrow = c(1, 2))
  try(plot(fm, which = 1))  # Residuals vs Fitted
  try(plot(fm, which = 2))  # Normal Q-Q
})

# Pred vs Actual with square axes
output$bayesglm_PredPlot <- renderPlot({
  req(models$bayesglm)
  dat   <- getTestData()
  preds <- predict(models$bayesglm, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (BayesGLM)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
  par(pty = "s")  # enforce square plotting region
})

# Coefficients
output$bayesglm_Coef <- renderTable({
  req(models$bayesglm)
  sm <- summary(models$bayesglm$finalModel)
  
  co <- sm$coefficients
  # Coerce to a 2D matrix if needed
  if (is.null(dim(co))) {
    co <- matrix(co, nrow = 1,
                 dimnames = list("(Intercept)", names(co)))
  }
  df <- as.data.frame(co)
  df <- tibble::rownames_to_column(df, "Term")
  
  # keep whatever columns exist among these:
  keep <- intersect(colnames(df), c("Estimate","Std. Error","t value","z value","Pr(>|t|)","Pr(>|z|)"))
  df[, c("Term", keep), drop = FALSE]
}, rownames = FALSE)

# output bayesglm_Recipe (print) 
output$bayesglm_Recipe <- renderPrint({
  req(models$bayesglm)
  models$bayesglm$recipe
})

# output bayesglm_ModelSummary2 (print) 
output$bayesglm_ModelSummary2 <- renderPrint({
  req(models$bayesglm)
  print(models$bayesglm)
})

# compact explanation (bullets) 
output$bayesglm_Explanation <- makeExplanation(
  bullets = c(
    "Bayesian GLM uses weakly-informative priors to stabilize coefficients when predictors are collinear or poorly scaled.",
    "Diagnostics: Residuals–Fitted should look like random scatter; Q–Q should be near the diagonal (Gaussian errors).",
    "Predicted vs Actual: tight 45° alignment indicates good calibration; curvature suggests missing nonlinearity."
  )
)



# METHOD * blasso -----------------------------------------------------------------------------------------------------
suppressWarnings(suppressMessages(library(monomvn)))  # blasso/bridge live here

# reactive getBlassoRecipe 
getBlassoRecipe <- reactive({
  form <- Response ~ .
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$blasso_Preprocess) %>%
    recipes::step_unknown(all_nominal_predictors(), new_level = "unknown") %>%
    recipes::step_rm(has_type("date"))
})

# observeEvent blasso_Go 
observeEvent(
  input$blasso_Go,
  {
    method <- "blasso"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(
        getBlassoRecipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        trControl  = getTrControl(),
        tuneLength = 5,
        na.action  = na.pass
      )
      deleteRds(method); saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

observeEvent(input$blasso_Load,   { m <- loadRds("blasso", session); if(!is.null(m)) models[["blasso"]] <- m })
observeEvent(input$blasso_Delete, { models[["blasso"]] <- NULL; gc() })

output$blasso_ModelSummary0 <- renderText({ description("blasso") })
output$blasso_Metrics       <- renderTable({ req(models$blasso); models$blasso$results[ which.min(models$blasso$results[, "RMSE"]), ] })
output$blasso_ModelPlots    <- renderPlot({ req(models$blasso); plot(models$blasso) })

output$blasso_PredPlot <- renderPlot({
  req(models$blasso)
  dat   <- getTestData()
  preds <- predict(models$blasso, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  par(pty = "s")
  plot(d$Predicted, d$Actual, xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (blasso)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# Coefficients (robust to vector/matrix shape)
output$blasso_Coef <- renderTable({
  req(models$blasso)
  co <- try(stats::coef(models$blasso$finalModel), silent = TRUE)
  if (inherits(co, "try-error")) {
    return(data.frame(Message = "Coefficients not available for this fit"))
  }
  if (is.null(dim(co))) {
    co <- matrix(co, nrow = length(co), dimnames = list(names(co), "Estimate"))
  } else if (ncol(co) == 1 && is.null(colnames(co))) {
    colnames(co) <- "Estimate"
  }
  df <- as.data.frame(co)
  df <- tibble::rownames_to_column(df, "Term")
  df
}, rownames = FALSE)

output$blasso_VarImp          <- renderTable({
  req(models$blasso)
  vi <- caret::varImp(models$blasso)
  df <- as.data.frame(vi$importance)
  df <- tibble::rownames_to_column(df, "Predictor")
  df[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

output$blasso_Recipe          <- renderPrint({ req(models$blasso); models$blasso$recipe })
output$blasso_ModelSummary2   <- renderPrint({ req(models$blasso); print(models$blasso) })

output$blasso_Explanation <- makeExplanation(
  bullets = c(
    "Tuning: lambda is chosen by the Bayesian lasso routine; caret’s profile shows RMSE across priors.",
    "Pred vs Actual: look for linear scatter—systematic curvature ⇒ try interactions or MARS.",
    "Importance/Coefs: many small coefficients; a few large in magnitude identify key drivers."
  )
)



# METHOD * bridge -----------------------------------------------------------------------------------------------------
# monomvn already loaded above

# reactive getBridgeRecipe 
getBridgeRecipe <- reactive({
  form <- Response ~ .
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$bridge_Preprocess) %>%
    recipes::step_unknown(all_nominal_predictors(), new_level = "unknown") %>%
    recipes::step_rm(has_type("date"))
})

# observeEvent bridge_Go 
observeEvent(
  input$bridge_Go,
  {
    method <- "bridge"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(
        getBridgeRecipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        trControl  = getTrControl(),
        tuneLength = 5,
        na.action  = na.pass
      )
      deleteRds(method); saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

observeEvent(input$bridge_Load,   { m <- loadRds("bridge", session); if(!is.null(m)) models[["bridge"]] <- m })
observeEvent(input$bridge_Delete, { models[["bridge"]] <- NULL; gc() })

output$bridge_ModelSummary0 <- renderText({ description("bridge") })
output$bridge_Metrics       <- renderTable({ req(models$bridge); models$bridge$results[ which.min(models$bridge$results[, "RMSE"]), ] })
output$bridge_ModelPlots    <- renderPlot({ req(models$bridge); plot(models$bridge) })

output$bridge_PredPlot <- renderPlot({
  req(models$bridge)
  dat   <- getTestData()
  preds <- predict(models$bridge, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  par(pty = "s")
  plot(d$Predicted, d$Actual, xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (bridge)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# Coefficients (robust)
output$bridge_Coef <- renderTable({
  req(models$bridge)
  co <- try(stats::coef(models$bridge$finalModel), silent = TRUE)
  if (inherits(co, "try-error")) {
    return(data.frame(Message = "Coefficients not available for this fit"))
  }
  if (is.null(dim(co))) {
    co <- matrix(co, nrow = length(co), dimnames = list(names(co), "Estimate"))
  } else if (ncol(co) == 1 && is.null(colnames(co))) {
    colnames(co) <- "Estimate"
  }
  df <- as.data.frame(co)
  df <- tibble::rownames_to_column(df, "Term")
  df
}, rownames = FALSE)

output$bridge_VarImp          <- renderTable({
  req(models$bridge)
  vi <- caret::varImp(models$bridge)
  df <- as.data.frame(vi$importance)
  df <- tibble::rownames_to_column(df, "Predictor")
  df[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

output$bridge_Recipe          <- renderPrint({ req(models$bridge); models$bridge$recipe })
output$bridge_ModelSummary2   <- renderPrint({ req(models$bridge); print(models$bridge) })

output$bridge_Explanation <- makeExplanation(
  bullets = c(
    "Tuning: prior shape (bridge) controls shrinkage toward zero; monitor RMSE across settings.",
    "Pred vs Actual: linear trend expected; large structure in residuals ⇒ try interactions/PCR/MARS.",
    "Importance/Coefs: shrinkage stabilizes estimates with correlated Reagent_* block."
  )
)

# METHOD * M5Rules -----------------------------------------------------------------------------------------------------
library(Cubist)

getM5Recipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$m5_Preprocess) %>%
    step_rm(has_type("date")) %>%
    step_zv(all_predictors())
})

observeEvent(
  input$m5_Go,
  {
    method <- "M5Rules"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
    
    model <- caret::train(
      getM5Recipe(),
      data       = getTrainData(),
      method     = method,
      metric     = "RMSE",
      trControl  = getTrControl(),
      tuneLength = 5,
      na.action  = na.pass
    )
    deleteRds(method); saveToRds(model, method); models[[method]] <- model
  }
)

observeEvent(input$m5_Load,   { if (!is.null(loadRds("M5Rules", session))) models[["M5Rules"]] <- loadRds("M5Rules", session) })
observeEvent(input$m5_Delete, { models[["M5Rules"]] <- NULL; gc() })

output$m5_ModelSummary0 <- renderText({ description("M5Rules") })

output$m5_Metrics <- renderTable({
  req(models$M5Rules)
  models$M5Rules$results[ which.min(models$M5Rules$results[, "RMSE"]), ]
})

output$m5_ModelPlots <- renderPlot({ req(models$M5Rules); plot(models$M5Rules) })

output$m5_PredPlot <- renderPlot({
  req(models$M5Rules)
  dat   <- getTestData()
  preds <- predict(models$M5Rules, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (M5Rules)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

output$m5_VarImp <- renderTable({
  req(models$M5Rules)
  vi <- caret::varImp(models$M5Rules)
  df <- as.data.frame(vi$importance)
  tibble::rownames_to_column(df, "Predictor")[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

output$m5_Recipe <- renderPrint({ req(models$M5Rules); models$M5Rules$recipe })
output$m5_ModelSummary2 <- renderPrint({ req(models$M5Rules); print(models$M5Rules) })

output$m5_Explanation <- makeExplanation(bullets = c(
  "M5 builds linear models at rule leaves, combining tree structure with regression accuracy.",
  "Tuning: follow RMSE vs number of rules; fewer = simpler model.",
  "Predicted vs Actual: points near 45° show consistent rule fits.",
  "Variable Importance: reflects predictors driving rule splits and leaf regressions."
))


# --- METHOD * svmLinear2 ------------------------------------------------------
# recipe
getSvmL2Recipe <- reactive({
  recipes::recipe(Response ~ ., data = getTrainData()) %>%
    dynamicSteps(input$svmL2_Preprocess) %>%
    step_rm(has_type("date")) %>%     # raw date after feature eng.
    step_zv(all_predictors())         # double-guard
})

# train
observeEvent(
  input$svmL2_Go,
  {
    method <- "svmLinear2"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      stopifnot(is.numeric(getTrainData()[["Response"]]))  # regression guard
      
      model <- caret::train(
        getSvmL2Recipe(),
        data       = getTrainData(),
        method     = method,
        metric     = "RMSE",
        trControl  = getTrControl(),   # keep your global control (no local seeds needed)
        tuneLength = 8,                # caret grid over cost C
        na.action  = na.pass
      )
      
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  }
)

# load/delete
observeEvent(input$svmL2_Load,   { m <- loadRds("svmLinear2", session); if(!is.null(m)) models$svmLinear2 <- m })
observeEvent(input$svmL2_Delete, { models[["svmLinear2"]] <- NULL; gc() })

# outputs
output$svmL2_ModelSummary0 <- renderText({ description("svmLinear2") })
output$svmL2_Metrics <- renderTable({
  req(models$svmLinear2)
  models$svmLinear2$results[ which.min(models$svmLinear2$results$RMSE), ]
}, rownames = TRUE)

output$svmL2_ModelPlots <- renderPlot({
  req(models$svmLinear2)
  plot(models$svmLinear2)  # RMSE vs Cost (C)
})

output$svmL2_PredPlot <- renderPlot({
  req(models$svmLinear2)
  dat   <- getTestData()
  preds <- predict(models$svmLinear2, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  r <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  op <- par(pty = "s"); on.exit(par(op), add = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = r, ylim = r, main = "Predicted vs Actual (SVM Linear)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

output$svmL2_VarImp <- renderTable({
  req(models$svmLinear2)
  vi <- caret::varImp(models$svmLinear2)
  df <- tibble::rownames_to_column(as.data.frame(vi$importance), "Predictor")
  df[order(df$Overall, decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

output$svmL2_Recipe <- renderPrint({ req(models$svmLinear2); models$svmLinear2$recipe })
output$svmL2_ModelSummary2 <- renderPrint({ req(models$svmLinear2); print(models$svmLinear2) })

# Compact explanation bullets (same style)
output$svmL2_Explanation <- makeExplanation(
  bullets = c(
    "Tuning Profile: RMSE vs Cost (C); higher C fits harder but risks overfitting.",
    "Predicted vs Actual: closer to 45° is better; look for heteroskedastic scatter.",
    "Variable Importance: based on absolute linear weights after preprocessing."
  )
)


# METHOD * enet --------------------------------------------------------------------------------------------------------
library(elasticnet)

# reactive recipe
getEnetRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$enet_Preprocess) %>%   # preserve UI order
    step_rm(has_type("date")) %>%             # drop raw date after month/dow engineered
    step_zv(all_predictors())                 # guard against constants
})

# Train
observeEvent(
  input$enet_Go,
  {
    method <- "enet"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
    
    model <- caret::train(
      getEnetRecipe(),
      data       = getTrainData(),
      method     = method,
      metric     = "RMSE",
      trControl  = getTrControl(),
      tuneLength = 10,         # search over lambda & fraction
      na.action  = na.pass
    )
    deleteRds(method); saveToRds(model, method); models[[method]] <- model
  }
)

# Load/Forget
observeEvent(input$enet_Load,   { 
  m <- loadRds("enet", session); if (!is.null(m)) models[["enet"]] <- m 
})
observeEvent(input$enet_Delete, { models[["enet"]] <- NULL; gc() })

# Summary header
output$enet_ModelSummary0 <- renderText({ description("enet") })

# Metrics (best by RMSE)
output$enet_Metrics <- renderTable({
  req(models$enet)
  models$enet$results[ which.min(models$enet$results[, "RMSE"]), ]
})

# caret tuning plots
output$enet_ModelPlots <- renderPlot({
  req(models$enet)
  plot(models$enet)
})

# Pred vs Actual
output$enet_PredPlot <- renderPlot({
  req(models$enet)
  dat   <- getTestData()
  preds <- predict(models$enet, newdata = dat)
  d  <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (Elastic Net)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# Variable importance
output$enet_VarImp <- renderTable({
  req(models$enet)
  vi <- caret::varImp(models$enet)
  df <- as.data.frame(vi$importance)
  tibble::rownames_to_column(df, "Predictor")[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

# Recipe + full print
output$enet_Recipe        <- renderPrint({ req(models$enet); models$enet$recipe })
output$enet_ModelSummary2 <- renderPrint({ req(models$enet); print(models$enet) })

# Explanation
output$enet_Explanation <- makeExplanation(bullets = c(
  "Elastic Net blends L1/L2 penalties (λ controls strength; fraction selects along the regularization path).",
  "Tuning: look for RMSE minima across λ–fraction; higher fraction = more coefficients included.",
  "Predicted vs Actual: tight 45° alignment indicates good bias–variance tradeoff.",
  "Variable Importance: driven by standardized coefficient magnitudes after regularization."
))


# METHOD * rbfDDA ------------------------------------------------------------------------------------------------------
library(RSNNS)

# reactive recipe
getRbfRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$rbf_Preprocess) %>%  # preserve UI order
    step_rm(has_type("date")) %>%           # drop raw date after month/dow
    step_zv(all_predictors())
})

# Train
observeEvent(
  input$rbf_Go,
  {
    method <- "rbfDDA"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"),
                     session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
    
    model <- caret::train(
      getRbfRecipe(),
      data       = getTrainData(),
      method     = method,
      metric     = "RMSE",
      trControl  = getTrControl(),
      tuneLength = 6,       # explores negativeThreshold on log scale
      na.action  = na.pass
    )
    deleteRds(method); saveToRds(model, method); models[[method]] <- model
  }
)

# Load / Forget
observeEvent(input$rbf_Load,   { m <- loadRds("rbfDDA", session); if (!is.null(m)) models[["rbfDDA"]] <- m })
observeEvent(input$rbf_Delete, { models[["rbfDDA"]] <- NULL; gc() })

# Descriptions & metrics
output$rbf_ModelSummary0 <- renderText({ description("rbfDDA") })

output$rbf_Metrics <- renderTable({
  req(models$rbfDDA)
  models$rbfDDA$results[ which.min(models$rbfDDA$results[, "RMSE"]), ]
})

# caret tuning plot
output$rbf_ModelPlots <- renderPlot({
  req(models$rbfDDA)
  plot(models$rbfDDA)
})

# Predicted vs Actual
output$rbf_PredPlot <- renderPlot({
  req(models$rbfDDA)
  dat   <- getTestData()
  preds <- predict(models$rbfDDA, newdata = dat)
  d  <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (RBF-DDA)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# Variable importance (caret may not implement varImp for rbfDDA; fall back gracefully)
output$rbf_VarImp <- renderTable({
  req(models$rbfDDA)
  vi <- try(caret::varImp(models$rbfDDA), silent = TRUE)
  if (inherits(vi, "try-error")) {
    data.frame(Predictor = "(not available for rbfDDA)", Overall = NA_real_)
  } else {
    df <- as.data.frame(vi$importance)
    tibble::rownames_to_column(df, "Predictor")[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
  }
}, rownames = FALSE)

# Recipe & full model print
output$rbf_Recipe        <- renderPrint({ req(models$rbfDDA); models$rbfDDA$recipe })
output$rbf_ModelSummary2 <- renderPrint({ req(models$rbfDDA); print(models$rbfDDA) })

# Explanation
output$rbf_Explanation <- makeExplanation(bullets = c(
  "RBF-DDA incrementally builds radial units; the negativeThreshold controls class/conflict activation.",
  "Standardized numeric inputs and dummy-encoded factors stabilize RBF distances.",
  "Tuning: monitor RMSE vs negativeThreshold; too high can underfit, too low can overfit.",
  "Predicted vs Actual near the 45° line indicates good interpolation from radial bases."
))



# METHOD * GBM --------------------------------------------------------------------------------------------------------
library(gbm)

getGbmRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$gbm_Preprocess) %>%
    step_rm(has_type("date")) %>%
    step_zv(all_predictors())
})

observeEvent(input$gbm_Go, {
  method <- "gbm"
  models[[method]] <- NULL
  showNotification(id = method, paste("Training", method, "with resampling..."),
                   session = session, duration = NULL)
  clus <- startMode(input$Parallel)
  on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
  
  model <- caret::train(
    getGbmRecipe(),
    data       = getTrainData(),
    method     = method,
    metric     = "RMSE",
    trControl  = getTrControl(),
    tuneLength = 5,
    na.action  = na.pass
  )
  deleteRds(method); saveToRds(model, method); models[[method]] <- model
})

observeEvent(input$gbm_Load,   { if (!is.null(loadRds("gbm", session))) models[["gbm"]] <- loadRds("gbm", session) })
observeEvent(input$gbm_Delete, { models[["gbm"]] <- NULL; gc() })

output$gbm_ModelSummary0 <- renderText({ description("gbm") })
output$gbm_Metrics <- renderTable({
  req(models$gbm)
  models$gbm$results[ which.min(models$gbm$results$RMSE), ]
})
output$gbm_ModelPlots <- renderPlot({ req(models$gbm); plot(models$gbm) })

output$gbm_PredPlot <- renderPlot({
  req(models$gbm)
  dat   <- getTestData()
  preds <- predict(models$gbm, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (GBM)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

output$gbm_VarImp <- renderTable({
  req(models$gbm)
  vi <- caret::varImp(models$gbm)
  df <- as.data.frame(vi$importance)
  tibble::rownames_to_column(df, "Predictor")[order(df$Overall, decreasing = TRUE),]
}, rownames = FALSE)

output$gbm_Recipe <- renderPrint({ req(models$gbm); models$gbm$recipe })
output$gbm_ModelSummary2 <- renderPrint({ req(models$gbm); print(models$gbm) })

output$gbm_Explanation <- makeExplanation(bullets = c(
  "Gradient Boosting sequentially fits trees to residuals, reducing bias.",
  "Shrinkage and tree depth control learning rate and complexity.",
  "Tuning: watch RMSE vs n.trees — too few underfit, too many overfit.",
  "Variable Importance shows predictors most often splitting or reducing loss."
))


# METHOD * DENFIS -----------------------------------------------------------------------------------------------------
library(frbs)

# ---- Recipe definition 
getDenfisRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$denfis_Preprocess) %>%   # dynamically apply user-selected preprocessing steps
    step_rm(has_type("date")) %>%               # remove date-type variables (unsupported)
    step_zv(all_predictors())                   # remove zero-variance predictors
})

# ---- Training logic 
observeEvent(input$denfis_Go, {
  method <- "DENFIS"
  models[[method]] <- NULL
  showNotification(id = method, paste("Processing", method, "model using resampling"),
                   session = session, duration = NULL)
  clus <- startMode(input$Parallel)
  on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
  
  model <- caret::train(
    getDenfisRecipe(),
    data       = getTrainData(),
    method     = method,
    metric     = "RMSE",
    trControl  = getTrControl(),
    tuneLength = 5,
    na.action  = na.pass
  )
  
  deleteRds(method)
  saveToRds(model, method)
  models[[method]] <- model
})

# ---- Load/Delete buttons 
observeEvent(input$denfis_Load,   { if (!is.null(loadRds("DENFIS", session))) models[["DENFIS"]] <- loadRds("DENFIS", session) })
observeEvent(input$denfis_Delete, { models[["DENFIS"]] <- NULL; gc() })

# ---- Output summary 
output$denfis_ModelSummary0 <- renderText({ description("DENFIS") })

# ---- Metrics table 
output$denfis_Metrics <- renderTable({
  req(models$DENFIS)
  models$DENFIS$results[which.min(models$DENFIS$results[, "RMSE"]), ]
})

# ---- Model tuning plots 
output$denfis_ModelPlots <- renderPlot({ req(models$DENFIS); plot(models$DENFIS) })

# ---- Predicted vs Actual 
output$denfis_PredPlot <- renderPlot({
  req(models$DENFIS)
  dat   <- getTestData()
  preds <- predict(models$DENFIS, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (DENFIS)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# ---- Variable importance 
output$denfis_VarImp <- renderTable({
  req(models$DENFIS)
  vi <- caret::varImp(models$DENFIS)
  df <- as.data.frame(vi$importance)
  tibble::rownames_to_column(df, "Predictor")[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

# ---- Recipe + Model summary 
output$denfis_Recipe <- renderPrint({ req(models$DENFIS); models$DENFIS$recipe })
output$denfis_ModelSummary2 <- renderPrint({ req(models$DENFIS); print(models$DENFIS) })

# ---- Explanation 
output$denfis_Explanation <- makeExplanation(bullets = c(
  "DENFIS dynamically evolves fuzzy rules based on input–output patterns.",
  "Each rule adapts online, allowing flexible nonlinear mappings.",
  "Tuning focuses on 'Dthr' (rule granularity) and 'max.iter' (training depth).",
  "Variable Importance reflects which inputs dominate fuzzy partitioning."
))


# METHOD * GAM (mgcv) --------------------------------------------------------------------------------------------------
library(mgcv)

# recipe
getGamRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$gam_Preprocess) %>%   # honor UI order
    step_rm(has_type("date")) %>%            # drop raw date cols after feature engineering
    step_zv(all_predictors())                # guard against constant columns
})

# train
observeEvent(input$gam_Go, {
  method <- "gam"
  models[[method]] <- NULL
  showNotification(id = method, paste("Processing", method, "model using resampling"),
                   session = session, duration = NULL)
  clus <- startMode(input$Parallel)
  on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
  
  model <- caret::train(
    getGamRecipe(),
    data       = getTrainData(),
    method     = method,
    metric     = "RMSE",
    trControl  = getTrControl(),
    tuneLength = 2,        # select (TRUE/FALSE) × method ("GCV.Cp") per caret default grid
    na.action  = na.pass
  )
  
  deleteRds(method); saveToRds(model, method); models[[method]] <- model
})

# load / delete
observeEvent(input$gam_Load,   { if (!is.null(loadRds("gam", session))) models[["gam"]] <- loadRds("gam", session) })
observeEvent(input$gam_Delete, { models[["gam"]] <- NULL; gc() })

# summary (top)
output$gam_ModelSummary0 <- renderText({ description("gam") })

# metrics
output$gam_Metrics <- renderTable({
  req(models$gam)
  models$gam$results[which.min(models$gam$results[, "RMSE"]), ]
})

# tuning plot
output$gam_ModelPlots <- renderPlot({ req(models$gam); plot(models$gam) })

# predicted vs actual
output$gam_PredPlot <- renderPlot({
  req(models$gam)
  dat   <- getTestData()
  preds <- predict(models$gam, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (GAM)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# variable importance (−log10 p-values from mgcv summary)
output$gam_VarImp <- renderTable({
  req(models$gam)
  vi <- caret::varImp(models$gam)
  df <- as.data.frame(vi$importance)
  tibble::rownames_to_column(df, "Predictor")[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

# recipe + model print
output$gam_Recipe        <- renderPrint({ req(models$gam); models$gam$recipe })
output$gam_ModelSummary2 <- renderPrint({ req(models$gam); print(models$gam) })

# explanation
output$gam_Explanation <- makeExplanation(bullets = c(
  "GAM fits smooth splines for predictors with sufficient unique values; linear terms remain linear.",
  "Tuning uses 'select' (automatic term selection) and 'method' (GCV by default).",
  "Predicted vs Actual close to the 45° line indicates good calibration.",
  "Variable Importance reflects term significance from mgcv (−log10 p-values)."
))



# METHOD * gaussprRadial (Gaussian Process with RBF kernel) -----------------------------------------------
library(kernlab)

# recipe
getGaussprRadialRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$gaussprRadial_Preprocess) %>%  # apply steps selected in UI
    step_rm(has_type("date")) %>%                     # remove date columns
    step_zv(all_predictors())                         # drop zero-variance columns
})

# training process
observeEvent(input$gaussprRadial_Go, {
  method <- "gaussprRadial"
  models[[method]] <- NULL
  showNotification(id = method, paste("Processing", method, "model using resampling"),
                   session = session, duration = NULL)
  clus <- startMode(input$Parallel)
  on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
  
  model <- caret::train(
    getGaussprRadialRecipe(),
    data       = getTrainData(),
    method     = method,
    metric     = "RMSE",
    trControl  = getTrControl(),
    tuneLength = 3,     # reasonable grid length
    na.action  = na.pass
  )
  
  deleteRds(method); saveToRds(model, method); models[[method]] <- model
})

# load and delete
observeEvent(input$gaussprRadial_Load,   { if (!is.null(loadRds("gaussprRadial", session))) models[["gaussprRadial"]] <- loadRds("gaussprRadial", session) })
observeEvent(input$gaussprRadial_Delete, { models[["gaussprRadial"]] <- NULL; gc() })

# summary header
output$gaussprRadial_ModelSummary0 <- renderText({ description("gaussprRadial") })

# metrics table
output$gaussprRadial_Metrics <- renderTable({
  req(models$gaussprRadial)
  models$gaussprRadial$results[which.min(models$gaussprRadial$results[, "RMSE"]), ]
})

# tuning plot
output$gaussprRadial_ModelPlots <- renderPlot({ req(models$gaussprRadial); plot(models$gaussprRadial) })

# predicted vs actual scatter
output$gaussprRadial_PredPlot <- renderPlot({
  req(models$gaussprRadial)
  dat   <- getTestData()
  preds <- predict(models$gaussprRadial, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (gaussprRadial)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# variable importance (approximation)
output$gaussprRadial_VarImp <- renderTable({
  req(models$gaussprRadial)
  vi <- caret::varImp(models$gaussprRadial)
  df <- as.data.frame(vi$importance)
  tibble::rownames_to_column(df, "Predictor")[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

# recipe and model details
output$gaussprRadial_Recipe        <- renderPrint({ req(models$gaussprRadial); models$gaussprRadial$recipe })
output$gaussprRadial_ModelSummary2 <- renderPrint({ req(models$gaussprRadial); print(models$gaussprRadial) })

# explanation text
output$gaussprRadial_Explanation <- makeExplanation(bullets = c(
  "Gaussian Process Regression with a Radial Basis Function (RBF) kernel.",
  "Captures smooth nonlinear relationships without explicit feature engineering.",
  "Tuning parameter σ (sigma) controls kernel width: smaller → more wiggly fit, larger → smoother fit.",
  "Predicted vs Actual near 45° indicates good generalization; variable importance gives approximate relevance."
))



# METHOD * GAMBoost ---------------------------------------------------------------------------------
library(mboost)   # caret method = "gamboost"

# recipe
getGmbRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$gmb_Preprocess) %>%  # keep UI order
    step_rm(has_type("date")) %>%           # drop raw date after feature engineering
    step_zv(all_predictors())               # extra guard
})

# Train
observeEvent(input$gmb_Go, {
  method <- "gamboost"
  models[[method]] <- NULL
  showNotification(id = method, paste("Processing", method, "model using resampling"),
                   session = session, duration = NULL)
  clus <- startMode(input$Parallel)
  on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
  
  model <- caret::train(
    getGmbRecipe(),
    data       = getTrainData(),
    method     = method,
    metric     = "RMSE",
    trControl  = getTrControl(),
    tuneLength = 6,
    na.action  = na.pass
  )
  deleteRds(method); saveToRds(model, method); models[[method]] <- model
})

# Load / Forget
observeEvent(input$gmb_Load,   { m <- loadRds("gamboost", session); if (!is.null(m)) models[["gamboost"]] <- m })
observeEvent(input$gmb_Delete, { models[["gamboost"]] <- NULL; gc() })

# Header description
output$gmb_ModelSummary0 <- renderText({ description("gamboost") })

# Best resample row
output$gmb_Metrics <- renderTable({
  req(models$gamboost)
  models$gamboost$results[ which.min(models$gamboost$results[, "RMSE"]), ]
})

# caret tuning plot
output$gmb_ModelPlots <- renderPlot({
  req(models$gamboost)
  plot(models$gamboost)
})

# Predicted vs Actual
output$gmb_PredPlot <- renderPlot({
  req(models$gamboost)
  dat   <- getTestData()
  preds <- predict(models$gamboost, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (GAMBoost)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# Variable importance
output$gmb_VarImp <- renderTable({
  req(models$gamboost)
  vi <- suppressWarnings(caret::varImp(models$gamboost))
  df <- as.data.frame(vi$importance)
  tibble::rownames_to_column(df, "Predictor")[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

# Recipe & model print
output$gmb_Recipe <- renderPrint({ req(models$gamboost); models$gamboost$recipe })
output$gmb_ModelSummary2 <- renderPrint({ req(models$gamboost); print(models$gamboost) })

# Short guidance
output$gmb_Explanation <- makeExplanation(bullets = c(
  "Boosting additive base-learners (linear/spline) to capture non-linear yet interpretable effects.",
  "Tune via RMSE vs mstop/shrinkage; look for validation error flattening.",
  "Check variable importance and partial effects to communicate clinical relevance."
))


# METHOD * rfRules -----------------------------------------------------------------------------------------------------
# caret method: "rfRules" (rule-based model distilled from random forest)

# reactive recipe
getRfRulesRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$rfRules_Preprocess) %>%  # honor UI order
    step_rm(has_type("date")) %>%               # drop raw dates after feature engineering
    step_zv(all_predictors())                   # remove constants
})

# Train
observeEvent(input$rfRules_Go, {
  method <- "rfRules"
  models[[method]] <- NULL
  showNotification(id = method, paste("Processing", method, "model using resampling"),
                   session = session, duration = NULL)
  clus <- startMode(input$Parallel)
  on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
  
  model <- caret::train(
    getRfRulesRecipe(),
    data       = getTrainData(),
    method     = method,
    metric     = "RMSE",
    trControl  = getTrControl(),
    tuneLength = 5,
    na.action  = na.pass
  )
  deleteRds(method); saveToRds(model, method); models[[method]] <- model
})

# Load
observeEvent(input$rfRules_Load, {
  method <- "rfRules"
  model <- loadRds(method, session)
  if (!is.null(model)) models[[method]] <- model
})

# Forget
observeEvent(input$rfRules_Delete, {
  models[["rfRules"]] <- NULL
  gc()
})

# Summaries & outputs
output$rfRules_ModelSummary0 <- renderText({ description("rfRules") })

output$rfRules_Metrics <- renderTable({
  req(models$rfRules)
  models$rfRules$results[ which.min(models$rfRules$results[,"RMSE"]), ]
})

output$rfRules_ModelPlots <- renderPlot({
  req(models$rfRules)
  plot(models$rfRules)
})

output$rfRules_PredPlot <- renderPlot({
  req(models$rfRules)
  dat   <- getTestData()
  preds <- predict(models$rfRules, newdata = dat)
  d   <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (rfRules)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

output$rfRules_VarImp <- renderTable({
  req(models$rfRules)
  vi <- caret::varImp(models$rfRules)
  df <- as.data.frame(vi$importance)
  df <- tibble::rownames_to_column(df, "Predictor")
  df[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

output$rfRules_Recipe       <- renderPrint({ req(models$rfRules); models$rfRules$recipe })
output$rfRules_ModelSummary2<- renderPrint({ req(models$rfRules); print(models$rfRules) })

output$rfRules_Explanation <- makeExplanation(bullets = c(
  "rfRules extracts human-readable rules from random forests, balancing predictive power and interpretability.",
  "Tune primarily via #rules and rule depth proxies (through forest parameters).",
  "Predicted vs Actual: tight 45° alignment indicates consistent rule coverage.",
  "Variable importance summarizes features most frequently appearing in influential rules."
))



# METHOD * Treebag -----------------------------------------------------------------------------------------------------
getTreebagRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$treebag_Preprocess) %>%
    step_rm(has_type("date")) %>%
    step_zv(all_predictors())
})

observeEvent(input$treebag_Go, {
  method <- "treebag"
  models[[method]] <- NULL
  showNotification(id = method, paste("Processing", method, "model using resampling"),
                   session = session, duration = NULL)
  clus <- startMode(input$Parallel)
  on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
  
  model <- caret::train(
    getTreebagRecipe(),
    data       = getTrainData(),
    method     = method,
    metric     = "RMSE",
    trControl  = getTrControl(),
    tuneLength = 3,
    na.action  = na.pass
  )
  deleteRds(method); saveToRds(model, method); models[[method]] <- model
})

observeEvent(input$treebag_Load,   { if (!is.null(loadRds("treebag", session))) models[["treebag"]] <- loadRds("treebag", session) })
observeEvent(input$treebag_Delete, { models[["treebag"]] <- NULL; gc() })

output$treebag_ModelSummary0 <- renderText({ description("treebag") })

output$treebag_Metrics <- renderTable({
  req(models$treebag)
  models$treebag$results[ which.min(models$treebag$results[, "RMSE"]), ]
})

output$treebag_ModelPlots <- renderPlot({ req(models$treebag); plot(models$treebag) })

output$treebag_PredPlot <- renderPlot({
  req(models$treebag)
  dat   <- getTestData()
  preds <- predict(models$treebag, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (Treebag)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

output$treebag_VarImp <- renderTable({
  req(models$treebag)
  vi <- caret::varImp(models$treebag)
  df <- as.data.frame(vi$importance)
  tibble::rownames_to_column(df, "Predictor")[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
}, rownames = FALSE)

output$treebag_Recipe <- renderPrint({ req(models$treebag); models$treebag$recipe })
output$treebag_ModelSummary2 <- renderPrint({ req(models$treebag); print(models$treebag) })

output$treebag_Explanation <- makeExplanation(bullets = c(
  "Treebag (bootstrap aggregated trees) averages predictions from multiple unpruned trees.",
  "It reduces variance without increasing bias, improving model stability.",
  "No hyperparameters beyond number of trees — simpler yet often strong baseline.",
  "Variable importance reflects predictor usage frequency across bagged trees."
))



# METHOD * Logic Regression (LogicReg) --------------------------------------------------------------

library(LogicReg)
# Recipe: honor UI steps, then force binary predictors (discretize + one-hot), drop dates/constant cols
getLogicRegRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$logreg_Preprocess) %>%                                  # keep user order
    recipes::step_discretize(all_numeric_predictors(), num_breaks = 4,         # turn numerics into bins
                             min_unique = 5, id = "disc_for_logicreg") %>%
    recipes::step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%          # ensure pure 0/1
    recipes::step_rm(has_type("date")) %>%                                     # LogicReg needs binary only
    recipes::step_zv(all_predictors())
})

observeEvent(input$logreg_Go, {
  method <- "logreg"
  models[[method]] <- NULL
  showNotification(id = method, paste("Processing", "Logic Regression", "using resampling"),
                   session = session, duration = NULL)
  clus <- startMode(input$Parallel)
  on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
  
  model <- caret::train(
    getLogicRegRecipe(),
    data       = getTrainData(),
    method     = "logreg",
    metric     = if (is.factor(getTrainData()[["Response"]])) "Accuracy" else "RMSE",
    trControl  = getTrControl(),
    tuneLength = 5,
    na.action  = na.pass
  )
  
  deleteRds(method); saveToRds(model, method); models[[method]] <- model
})

observeEvent(input$logreg_Load,   { if (!is.null(loadRds("logreg", session))) models[["logreg"]] <- loadRds("logreg", session) })
observeEvent(input$logreg_Delete, { models[["logreg"]] <- NULL; gc() })

output$logreg_ModelSummary0 <- renderText({ description("logreg") })

output$logreg_Metrics <- renderTable({
  req(models$logreg)
  res <- models$logreg$results
  if ("Accuracy" %in% names(res)) res[which.max(res$Accuracy), , drop = FALSE]
  else if ("RMSE" %in% names(res)) res[which.min(res$RMSE), , drop = FALSE]
  else res[1, , drop = FALSE]
})

output$logreg_ModelPlots <- renderPlot({ req(models$logreg); plot(models$logreg) })

output$logreg_PredPlot <- renderPlot({
  req(models$logreg)
  dat   <- getTestData()
  preds <- predict(models$logreg, newdata = dat)
  if (is.factor(dat$Response)) {
    tbl <- table(Actual = dat$Response, Predicted = preds)
    fourfoldplot(tbl, conf.level = 0, col = c("gray90","gray60"),
                 main = "Confusion (Logic Regression)")
  } else {
    d <- data.frame(Actual = dat$Response, Predicted = preds)
    rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
    plot(d$Predicted, d$Actual, xlab = "Predicted", ylab = "Actual",
         xlim = rng, ylim = rng, main = "Predicted vs Actual (Logic Regression)")
    abline(0, 1, col = "blue", lty = 2, lwd = 2)
  }
})

# LogicReg doesn’t have a native varImp in caret; display selected logic terms instead, if available
output$logreg_VarImp <- renderTable({
  req(models$logreg)
  m <- models$logreg$finalModel
  if (!is.null(m$model$trees)) {
    extract <- function(tr) paste(vapply(tr$knot, function(k) if (k > 0) colnames(m$binary)[k] else NA_character_, character(1L)),
                                  collapse = " & ")
    rules <- vapply(m$model$trees, extract, character(1L))
    data.frame(Rule = seq_along(rules), Expression = rules, row.names = NULL, check.names = FALSE)
  } else {
    data.frame(Message = "Logic terms not available for display.")
  }
}, rownames = FALSE)

output$logreg_Recipe       <- renderPrint({ req(models$logreg); models$logreg$recipe })
output$logreg_ModelSummary2 <- renderPrint({ req(models$logreg); print(models$logreg) })

output$logreg_Explanation <- makeExplanation(bullets = c(
  "Logic Regression learns Boolean combinations of binary features to predict the outcome.",
  "Recipe forces binary predictors via discretization + one-hot encoding to meet method requirements.",
  "Inspect the extracted logic rules to understand how feature combinations influence predictions."
))


# METHOD * rvmRadial -------------------------------------------------------------------------------
# Recipe: honor UI order; then remove date columns and constant predictors
getRvmRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$rvm_Preprocess) %>%   # keep user-selected order
    recipes::step_rm(has_type("date")) %>%   # drop raw date columns after feature engineering
    recipes::step_zv(all_predictors())       # guard against constants
})

observeEvent(input$rvm_Go, {
  method <- "rvmRadial"
  models[[method]] <- NULL
  showNotification(id = method, paste("Processing", method, "model using resampling"),
                   session = session, duration = NULL)
  clus <- startMode(input$Parallel)
  on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
  
  model <- caret::train(
    getRvmRecipe(),
    data       = getTrainData(),
    method     = method,
    metric     = "RMSE",
    trControl  = getTrControl(),
    tuneLength = 5,
    na.action  = na.pass
  )
  
  deleteRds(method); saveToRds(model, method); models[[method]] <- model
})

observeEvent(input$rvm_Load,   { if (!is.null(loadRds("rvmRadial", session))) models[["rvmRadial"]] <- loadRds("rvmRadial", session) })
observeEvent(input$rvm_Delete, { models[["rvmRadial"]] <- NULL; gc() })

output$rvm_ModelSummary0 <- renderText({ description("rvmRadial") })

output$rvm_Metrics <- renderTable({
  req(models$rvmRadial)
  res <- models$rvmRadial$results
  res[which.min(res$RMSE), , drop = FALSE]
})

output$rvm_ModelPlots <- renderPlot({
  req(models$rvmRadial)
  plot(models$rvmRadial)   # RMSE vs sigma
})

output$rvm_PredPlot <- renderPlot({
  req(models$rvmRadial)
  dat   <- getTestData()
  preds <- predict(models$rvmRadial, newdata = dat)
  d <- data.frame(Actual = dat$Response, Predicted = preds)
  rng <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rng, ylim = rng, main = "Predicted vs Actual (rvmRadial)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# kernlab RVM has no native caret varImp; provide a graceful placeholder
output$rvm_VarImp <- renderTable({
  req(models$rvmRadial)
  data.frame(Message = "Variable importance is not available for rvmRadial.", check.names = FALSE)
}, rownames = FALSE)

output$rvm_Recipe        <- renderPrint({ req(models$rvmRadial); models$rvmRadial$recipe })
output$rvm_ModelSummary2 <- renderPrint({ req(models$rvmRadial); print(models$rvmRadial) })

output$rvm_Explanation <- makeExplanation(bullets = c(
  "RVM uses a sparse Bayesian formulation with an RBF kernel, often yielding very compact models.",
  "Preprocessing: imputation → date features → dummies → centering/scaling → remove constants.",
  "Tune sigma: too small may overfit; too large may underfit. Pick the RMSE minimum on resampling."
))



# METHOD * brnn ------------------------------------------------------------------------------------
# Recipe: honor UI order; then remove date columns and constant predictors
getBrnnRecipe <- reactive({
  form <- formula(Response ~ .)
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$brnn_Preprocess) %>%   # keep user-selected order
    recipes::step_rm(has_type("date")) %>%    # drop raw date columns after feature engineering
    recipes::step_zv(all_predictors())        # guard against constants
})

observeEvent(input$brnn_Go, {
  method <- "brnn"
  models[[method]] <- NULL
  showNotification(id = method, paste("Processing", method, "model using resampling"),
                   session = session, duration = NULL)
  clus <- startMode(input$Parallel)
  on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
  
  model <- caret::train(
    getBrnnRecipe(),
    data       = getTrainData(),
    method     = method,
    metric     = "RMSE",
    trControl  = getTrControl(),
    tuneLength = 5,          # neurons will be 1:5 by default grid
    na.action  = na.pass
  )
  
  deleteRds(method); saveToRds(model, method); models[[method]] <- model
})

observeEvent(input$brnn_Load,   { if (!is.null(loadRds("brnn", session))) models[["brnn"]] <- loadRds("brnn", session) })
observeEvent(input$brnn_Delete, { models[["brnn"]] <- NULL; gc() })

output$brnn_ModelSummary0 <- renderText({ description("brnn") })

output$brnn_Metrics <- renderTable({
  req(models$brnn)
  res <- models$brnn$results
  res[which.min(res$RMSE), , drop = FALSE]
})

output$brnn_ModelPlots <- renderPlot({
  req(models$brnn)
  plot(models$brnn)   # RMSE vs neurons
})

output$brnn_PredPlot <- renderPlot({
  req(models$brnn)
  dat   <- getTestData()
  preds <- predict(models$brnn, newdata = dat)
  d  <- data.frame(Actual = dat$Response, Predicted = preds)
  rg <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual,
       xlab = "Predicted", ylab = "Actual",
       xlim = rg, ylim = rg, main = "Predicted vs Actual (BRNN)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

# brnn has no built-in varImp in caret; show a friendly placeholder
output$brnn_VarImp <- renderTable({
  req(models$brnn)
  data.frame(Message = "Variable importance is not available for BRNN.", check.names = FALSE)
}, rownames = FALSE)

output$brnn_Recipe        <- renderPrint({ req(models$brnn); models$brnn$recipe })
output$brnn_ModelSummary2 <- renderPrint({ req(models$brnn); print(models$brnn) })

output$brnn_Explanation <- makeExplanation(bullets = c(
  "BRNN applies Bayesian regularization to a single hidden-layer neural net, reducing overfitting.",
  "Preprocessing: imputation → date features → dummies → centering/scaling → remove constants.",
  "Tune neurons: too few underfit; too many may overfit despite regularization—choose RMSE minimum."
))



# METHOD * KRLS (Radial)  -----------------------
# — with diagnostics & tolerant control
getKrlsRecipe <- reactive({
  form <- Response ~ .
  recipes::recipe(form, data = getTrainData()) %>%
    dynamicSteps(input$krls_Preprocess) %>%
    step_rm(has_type("date")) %>%
    step_zv(all_predictors())
})

observeEvent(input$krls_Go, {
  method <- "krlsRadial"
  models[[method]] <- NULL
  showNotification(id = method, paste("Processing", method, "model using resampling"),
                   session = session, duration = NULL)
  clus <- startMode(input$Parallel)
  on.exit({ removeNotification(id = method); stopMode(clus) }, add = TRUE)
  
  # ---- 1) bake once to catch data issues ahead of train() ----
  rec <- getKrlsRecipe()
  prep_obj <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
  baked <- recipes::bake(prep_obj, new_data = NULL)
  
  # outcome must be numeric and finite
  if (!is.numeric(baked$Response) || any(!is.finite(baked$Response))) {
    stop("KRLS: outcome Response must be numeric & finite after recipe.")
  }
  # predictors all numeric; no NA/Inf
  X <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
  bad_cols <- names(X)[vapply(X, function(v) !is.numeric(v) || any(!is.finite(v)), logical(1))]
  if (length(bad_cols)) {
    stop(paste("KRLS: non-numeric or non-finite predictors after recipe:", paste(bad_cols, collapse = ", ")))
  }
  if (ncol(X) == 0L) stop("KRLS: no predictors remain after preprocessing.")
  
  # ---- 2) make trainControl tolerant & verbose (avoid hard stop on NA folds) ----
  ctrl <- getTrControl()
  ctrl$errorHandling <- "continue"
  ctrl$returnData    <- TRUE
  ctrl$verboseIter   <- TRUE
  
  # ---- 3) light grid先试通（tuneLength 可再加大）----
  model <- caret::train(
    rec,
    data       = getTrainData(),
    method     = method,
    metric     = "RMSE",
    trControl  = ctrl,
    tuneLength = 2,
    na.action  = na.pass
  )
  
  # ---- 4) 若仍全 NA，给出可操作提示而不是直接崩 ----
  if (all(is.na(model$results$RMSE))) {
    msg <- paste(
      "KRLS resamples produced all-NA metrics.",
      "Try: (a) ensure recipe leaves only numeric finite predictors;",
      "(b) keep impute_bag before center/scale/dummy;",
      "(c) reduce dimensionality (e.g., step_pca or remove sparse dummies);",
      "(d) increase sample size in resampling or switch method='none' for a smoke test."
    )
    showNotification(msg, type = "error", duration = 8)
    stop(msg)
  }
  
  deleteRds(method); saveToRds(model, method); models[[method]] <- model
})

output$krls_ModelSummary0 <- renderText({ description("krlsRadial") })

output$krls_Metrics <- renderTable({
  req(models$krlsRadial)
  res <- models$krlsRadial$results
  res[which.min(res$RMSE), , drop = FALSE]
})

output$krls_ModelPlots <- renderPlot({ req(models$krlsRadial); plot(models$krlsRadial) })

output$krls_PredPlot <- renderPlot({
  req(models$krlsRadial)
  dat   <- getTestData()
  preds <- predict(models$krlsRadial, newdata = dat)
  d  <- data.frame(Actual = dat$Response, Predicted = preds)
  rg <- range(c(d$Actual, d$Predicted), na.rm = TRUE)
  plot(d$Predicted, d$Actual, xlab = "Predicted", ylab = "Actual",
       xlim = rg, ylim = rg, main = "Predicted vs Actual (KRLS)")
  abline(0, 1, col = "blue", lty = 2, lwd = 2)
})

output$krls_VarImp <- renderTable({
  req(models$krlsRadial)
  vi <- try(caret::varImp(models$krlsRadial), silent = TRUE)
  if (inherits(vi, "try-error")) {
    data.frame(Message = "Variable importance not supported for KRLS.")
  } else {
    df <- as.data.frame(vi$importance)
    tibble::rownames_to_column(df, "Predictor")[order(df[["Overall"]], decreasing = TRUE), , drop = FALSE]
  }
}, rownames = FALSE)

output$krls_Recipe        <- renderPrint({ req(models$krlsRadial); models$krlsRadial$recipe })
output$krls_ModelSummary2 <- renderPrint({ req(models$krlsRadial); print(models$krlsRadial) })

output$krls_Explanation <- makeExplanation(bullets = c(
  "KRLS uses an RBF kernel with ridge-style regularization for nonlinear regression.",
  "Preprocessing must yield numeric, finite predictors; impute before center/scale/dummy.",
  "If resamples still fail, reduce dimensionality or run a quick method='none' smoke test."
))






  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  
})
