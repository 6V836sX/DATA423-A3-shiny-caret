shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Yu Xia"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 2.6) # modified by yxi75 :outliers disappear when the IQR multiplier reaches 2.6 
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.3),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "null_Recipe")
                         ),
                         tabPanel("GLMnet Model",
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")

                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmnet_Recipe"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmnet_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("PLS Model",
                                  verbatimTextOutput(outputId = "pls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pls_ModelPlots"),
                                  verbatimTextOutput(outputId = "pls_Recipe"),
                                  verbatimTextOutput(outputId = "pls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rpart_ModelPlots"),
                                  verbatimTextOutput(outputId = "rpart_Recipe"),
                                  plotOutput(outputId = "rpart_ModelTree")   #  <- this tree-plot is unique to the rpart method
                         ),
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         # add further tabs (with controls) here
                         # ---- LM Model Tab ----
                         tabPanel("LM Model",
                                  verbatimTextOutput(outputId = "lm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId  = "lm_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(lm_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = lm_initial  # <-- 推荐默认（见下文 lm_initial）
                                           ),
                                           bsTooltip(id = "lm_Preprocess",
                                                     title = "这些步骤会在加载已保存模型时按保存顺序自动回填",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "lm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "lm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "lm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "lm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "lm_ModelPlots"),
                                  hr(),
                                  uiOutput("lm_Explanation"),
                                  verbatimTextOutput(outputId = "lm_Recipe"),
                                  verbatimTextOutput(outputId = "lm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "lm_Coef")
                                  ),
                                  
                                  hr(),
                                  uiOutput("lm_Explanation")
                                  
                         ),
                         
                         # ---- PCR Model Tab ----
                         tabPanel("PCR Model",
                                  verbatimTextOutput(outputId = "pcr_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id MUST be "<method>_Preprocess" for saved-model reload to work
                                           selectizeInput(
                                             inputId  = "pcr_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(pcr_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = pcr_initial   # <-- set in global.R (see note below)
                                           ),
                                           bsTooltip(id = "pcr_Preprocess",
                                                     title = "When a saved model is loaded, steps are restored in the saved order",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcr_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "pcr_Go", title = "Train or retrain (and save) the PCR model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcr_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "pcr_Load", title = "Reload the saved PCR model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcr_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "pcr_Delete", title = "Remove the PCR model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pcr_Metrics"),
                                  hr(),
                                  h4("Tuning Profile (RMSE vs #Components)"),
                                  plotOutput(outputId = "pcr_ModelPlots"),
                                  hr(),
                                  h4("Explained Variance (X & Y)"),
                                  tableOutput(outputId = "pcr_ExplainedVar"),
                                  hr(),
                                  h4("Predicted vs Actual"),
                                  plotOutput(outputId = "pcr_PredPlot"),
                                  hr(),
                                  verbatimTextOutput(outputId = "pcr_Recipe"),
                                  verbatimTextOutput(outputId = "pcr_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients (at best ncomp)"),
                                    tableOutput(outputId = "pcr_Coef")
                                  ),
                                  
                                  hr(),
                                  uiOutput("pcr_Explanation")
                         ),
                         # ---- CUBIST Model Tab ----
                         tabPanel("Cubist Model",
                                  # short intro (what it is / when to use)
                                  wellPanel(
                                    h4("Method Intro — Cubist"),
                                    tags$p(
                                      "Cubist is a rule-based model tree with linear models at leaves. ",
                                      "It builds committees (ensembles) of rule sets and can use instance-based correction via ",
                                      tags$code("neighbors"), ". It often performs well on tabular, non-linear data with mixed types, ",
                                      "and provides variable usage for interpretability."
                                    ),
                                    tags$ul(
                                      tags$li(tags$b("Key hyper-parameters:"), " ", tags$code("committees"), " (ensemble size), ",
                                              tags$code("neighbors"), " (instance-based adjustment)."),
                                      tags$li(tags$b("Strengths:"), " non-linearity, rule transparency, strong tabular performance."),
                                      tags$li(tags$b("Notes:"), " scaling not required; handle factors via dummy variables; ",
                                              "missingness can be imputed (e.g., KNN/bag).")
                                    )
                                  ),
                                  
                                  verbatimTextOutput(outputId = "cubist_ModelSummary0"),
                                  
                                  fluidRow(
                                    column(width = 4,
                                           # MUST be "<method>_Preprocess" for loading back saved models
                                           selectizeInput(
                                             inputId  = "cubist_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(cubist_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = cubist_initial_bag  # <-- set in global.R
                                           ),
                                           bsTooltip(id = "cubist_Preprocess",
                                                     title = "When a saved model is loaded, steps are restored in the saved order",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "cubist_Go", title = "Train or retrain (and save) the Cubist model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "cubist_Load", title = "Reload the saved Cubist model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "cubist_Delete", title = "Remove the Cubist model from memory")
                                    )
                                  ),
                                  
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "cubist_Metrics"),
                                  
                                  hr(),
                                  h4("Tuning Profile"),
                                  plotOutput(outputId = "cubist_ModelPlots"),   # RMSE vs (committees/neighbors)
                                  
                                  hr(),
                                  h4("Predicted vs Actual"),
                                  plotOutput(outputId = "cubist_PredPlot"),
                                  
                                  hr(),
                                  h4("Variable Usage (Importance-like)"),
                                  tableOutput(outputId = "cubist_VarImp"),
                                  
                                  hr(),
                                  verbatimTextOutput(outputId = "cubist_Recipe"),
                                  verbatimTextOutput(outputId = "cubist_ModelSummary2"),
                                  
                                  hr(),
                                  uiOutput("cubist_Explanation")
                         ),
                         
                         # ---- RF Model Tab ----
                         tabPanel("RF Model",
                                  # short intro
                                  wellPanel(
                                    h4("Method Intro — Random Forest"),
                                    tags$p(
                                      "Random Forest is a bagging ensemble of decision trees. ",
                                      "At each split it considers a random subset of predictors (", tags$code("mtry"), "). ",
                                      "It handles non-linearities and interactions, is robust to outliers, and provides variable importance."
                                    ),
                                    tags$ul(
                                      tags$li(tags$b("Key hyper-parameter:"), " ", tags$code("mtry"), " (number of predictors tried at each split)"),
                                      tags$li(tags$b("Strengths:"), " strong tabular performance, robust, little preprocessing required"),
                                      tags$li(tags$b("Notes:"), " scaling is unnecessary; categorical variables should be dummified; ",
                                              "impute missing values (KNN/bag) or use RF’s rough-fix upstream if needed.")
                                    )
                                  ),
                                  
                                  verbatimTextOutput(outputId = "rf_ModelSummary0"),
                                  
                                  fluidRow(
                                    column(width = 4,
                                           # MUST be "<method>_Preprocess" for saved-model round-trips
                                           selectizeInput(
                                             inputId  = "rf_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(rf_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = rf_initial  # <-- set in global.R (see below)
                                           ),
                                           bsTooltip(id = "rf_Preprocess",
                                                     title = "When a saved model is loaded, steps are restored in the saved order",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "rf_Go", title = "Train or retrain (and save) the RF model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "rf_Load", title = "Reload the saved RF model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "rf_Delete", title = "Remove the RF model from memory")
                                    )
                                  ),
                                  
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rf_Metrics"),
                                  
                                  hr(),
                                  h4("Tuning Profile (RMSE vs mtry)"),
                                  plotOutput(outputId = "rf_ModelPlots"),
                                  
                                  hr(),
                                  h4("Predicted vs Actual"),
                                  plotOutput(outputId = "rf_PredPlot"),
                                  
                                  hr(),
                                  h4("Variable Importance"),
                                  tableOutput(outputId = "rf_VarImp"),
                                  
                                  hr(),
                                  verbatimTextOutput(outputId = "rf_Recipe"),
                                  verbatimTextOutput(outputId = "rf_ModelSummary2"),
                                  
                                  hr(),
                                  uiOutput("rf_Explanation")
                         ),
                         
                         tabPanel("Ranger Model",
                                  verbatimTextOutput(outputId = "ranger_ModelSummary0"),
                                  fluidRow(
                                    column(
                                      width = 4,
                                      selectizeInput(
                                        inputId = "ranger_Preprocess",
                                        label   = "Pre-processing",
                                        choices = unique(c(ranger_initial, ppchoices)),
                                        multiple = TRUE,
                                        selected = ranger_initial
                                      ),
                                      bsTooltip(id = "ranger_Preprocess",
                                                title = "These entries will be populated in the correct order from a saved model once it loads",
                                                placement = "top")
                                    ),
                                    column(
                                      width = 1,
                                      actionButton(inputId = "ranger_Go",     label = "Train",  icon = icon("play")),
                                      bsTooltip(id = "ranger_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(
                                      width = 1,
                                      actionButton(inputId = "ranger_Load",   label = "Load",   icon = icon("file-arrow-up")),
                                      bsTooltip(id = "ranger_Load", title = "This will reload your saved model")
                                    ),
                                    column(
                                      width = 1,
                                      actionButton(inputId = "ranger_Delete", label = "Forget", icon = icon("trash-can")),
                                      bsTooltip(id = "ranger_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "ranger_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "ranger_ModelPlots"),
                                  plotOutput(outputId = "ranger_PredPlot"),
                                  tableOutput(outputId = "ranger_VarImp"),
                                  verbatimTextOutput(outputId = "ranger_Recipe"),
                                  verbatimTextOutput(outputId = "ranger_ModelSummary2"),
                                  hr(),
                                  h4("What to look for"),
                                  uiOutput(outputId = "ranger_Explanation")
                         ),
                         
                         
                         
                         # ---- SVM (Radial) Model Tab ----
                         tabPanel("SVM (Radial)",
                                  wellPanel(
                                    h4("Method Intro — SVM with RBF kernel (svmRadial)"),
                                    tags$p(
                                      "Nonlinear support vector regression using a Gaussian (RBF) kernel. ",
                                      "Key hyper-parameters: ", tags$code("sigma"), " (kernel width) and ", tags$code("C"),
                                      " (cost/regularization). Scaling is essential."
                                    ),
                                    tags$ul(
                                      tags$li(tags$b("Strengths:"), " handles nonlinearity, robust to outliers when tuned."),
                                      tags$li(tags$b("Notes:"), " center & scale numeric predictors; dummify categoricals; impute missing values.")
                                    )
                                  ),
                                  
                                  verbatimTextOutput(outputId = "svmR_ModelSummary0"),
                                  
                                  fluidRow(
                                    column(width = 4,
                                           # MUST be "<method>_Preprocess" for saved-model reload
                                           selectizeInput(
                                             inputId  = "svmR_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(svmR_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = svmR_initial
                                           ),
                                           bsTooltip(id = "svmR_Preprocess",
                                                     title = "When a saved model is loaded, steps are restored in the saved order",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmR_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "svmR_Go", title = "Train or retrain (and save) the SVM (RBF) model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmR_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "svmR_Load", title = "Reload the saved SVM (RBF) model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmR_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "svmR_Delete", title = "Remove the SVM (RBF) model from memory")
                                    )
                                  ),
                                  
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmR_Metrics"),
                                  
                                  hr(),
                                  h4("Tuning Profile"),
                                  plotOutput(outputId = "svmR_ModelPlots"),
                                  
                                  hr(),
                                  h4("Predicted vs Actual"),
                                  plotOutput(outputId = "svmR_PredPlot"),
                                  
                                  hr(),
                                  h4("Variable Importance (if available)"),
                                  tableOutput(outputId = "svmR_VarImp"),
                                  
                                  hr(),
                                  verbatimTextOutput(outputId = "svmR_Recipe"),
                                  verbatimTextOutput(outputId = "svmR_ModelSummary2"),
                                  
                                  hr(),
                                  uiOutput("svmR_Explanation")
                         ),
                         
                         # ---- SVM (Polynomial) Model Tab ----
                         tabPanel("SVM (Poly)",
                                  wellPanel(
                                    h4("Method Intro — SVM with Polynomial Kernel (svmPoly)"),
                                    tags$p(
                                      "Nonlinear support vector regression using a polynomial kernel with degree, scale, and cost (C). ",
                                      "Scaling is essential to get a sensible margin."
                                    ),
                                    tags$ul(
                                      tags$li(tags$b("Key hyper-parameters:"), " ", tags$code("degree"), ", ", tags$code("scale"), ", ", tags$code("C")),
                                      tags$li(tags$b("Strengths:"), " flexible nonlinear fits; degree controls interaction order."),
                                      tags$li(tags$b("Notes:"), " center & scale numeric predictors; dummify categoricals; impute missing values.")
                                    )
                                  ),
                                  
                                  verbatimTextOutput(outputId = "svmP_ModelSummary0"),
                                  
                                  fluidRow(
                                    column(width = 4,
                                           # MUST be "<method>_Preprocess" for saved-model round-trips
                                           selectizeInput(
                                             inputId  = "svmP_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(svmP_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = svmP_initial
                                           ),
                                           bsTooltip(id = "svmP_Preprocess",
                                                     title = "When a saved model is loaded, steps are restored in the saved order",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmP_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "svmP_Go", title = "Train or retrain (and save) the SVM (Poly) model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmP_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "svmP_Load", title = "Reload the saved SVM (Poly) model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmP_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "svmP_Delete", title = "Remove the SVM (Poly) model from memory")
                                    )
                                  ),
                                  
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmP_Metrics"),
                                  
                                  hr(),
                                  h4("Tuning Profile"),
                                  plotOutput(outputId = "svmP_ModelPlots"),
                                  
                                  hr(),
                                  h4("Predicted vs Actual"),
                                  plotOutput(outputId = "svmP_PredPlot"),
                                  
                                  hr(),
                                  h4("Variable Importance (if available)"),
                                  tableOutput(outputId = "svmP_VarImp"),
                                  
                                  hr(),
                                  verbatimTextOutput(outputId = "svmP_Recipe"),
                                  verbatimTextOutput(outputId = "svmP_ModelSummary2"),
                                  
                                  hr(),
                                  uiOutput("svmP_Explanation")
                         ),
                         
                         
                         # ---- MARS (earth) Model Tab ----
                         tabPanel("MARS (earth)",
                                  wellPanel(
                                    h4("Method Intro — Multivariate Adaptive Regression Splines (MARS, earth)"),
                                    tags$p(
                                      "Piecewise-linear basis functions with automatic knot selection and pruning. ",
                                      "Captures nonlinearities and interactions via hinge functions."
                                    ),
                                    tags$ul(
                                      tags$li(tags$b("Key hyper-parameters:"), " ",
                                              tags$code("nprune"), " (number of terms after pruning), ",
                                              tags$code("degree"), " (max interaction order)."),
                                      tags$li(tags$b("Strengths:"), " flexible, often strong on tabular data, implicit feature selection."),
                                      tags$li(tags$b("Notes:"), " dummify categoricals; centering/scaling optional; impute missing values.")
                                    )
                                  ),
                                  
                                  verbatimTextOutput(outputId = "earth_ModelSummary0"),
                                  
                                  fluidRow(
                                    column(width = 4,
                                           # MUST be "<method>_Preprocess" for saved-model round-trips
                                           selectizeInput(
                                             inputId  = "earth_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(earth_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = earth_initial
                                           ),
                                           bsTooltip(id = "earth_Preprocess",
                                                     title = "When a saved model is loaded, steps are restored in the saved order",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "earth_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "earth_Go", title = "Train or retrain (and save) the MARS model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "earth_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "earth_Load", title = "Reload the saved MARS model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "earth_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "earth_Delete", title = "Remove the MARS model from memory")
                                    )
                                  ),
                                  
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "earth_Metrics"),
                                  
                                  hr(),
                                  h4("Tuning Profile"),
                                  plotOutput(outputId = "earth_ModelPlots"),
                                  
                                  hr(),
                                  h4("Predicted vs Actual"),
                                  plotOutput(outputId = "earth_PredPlot"),
                                  
                                  hr(),
                                  h4("Variable Importance"),
                                  tableOutput(outputId = "earth_VarImp"),
                                  
                                  hr(),
                                  verbatimTextOutput(outputId = "earth_Recipe"),
                                  verbatimTextOutput(outputId = "earth_ModelSummary2"),
                                  
                                  hr(),
                                  uiOutput("earth_Explanation")
                         ),
                         
                         # ---- Bagged MARS (GCV) Model Tab ----
                         tabPanel("Bagged MARS (GCV)",
                                  wellPanel(
                                    h4("Method Intro — Bagged MARS with GCV Pruning (bagEarthGCV)"),
                                    tags$p("Ensembles multiple MARS base learners to reduce variance; GCV handles pruning."),
                                    tags$ul(
                                      tags$li(tags$b("Key hyper-parameter:"), " ", tags$code("degree"), " (max interaction order)."),
                                      tags$li(tags$b("Why use it:"), " typically more stable than a single MARS, strong on tabular data with nonlinearities."),
                                      tags$li(tags$b("Preprocessing:"), " impute, unknown-level handling, dummy encoding; scaling optional.")
                                    )
                                  ),
                                  
                                  verbatimTextOutput(outputId = "bagGCV_ModelSummary0"),
                                  
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId  = "bagGCV_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(bagEarthGCV_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = bagEarthGCV_initial
                                           ),
                                           bsTooltip(id = "bagGCV_Preprocess",
                                                     title = "These steps are saved/loaded with the model (order preserved).",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagGCV_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "bagGCV_Go", title = "Train or retrain (and save) the model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagGCV_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "bagGCV_Load", title = "Reload the saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagGCV_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "bagGCV_Delete", title = "Remove the model from memory")
                                    )
                                  ),
                                  
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bagGCV_Metrics"),
                                  
                                  hr(),
                                  h4("Tuning Profile"),
                                  plotOutput(outputId = "bagGCV_ModelPlots"),
                                  
                                  hr(),
                                  h4("Predicted vs Actual"),
                                  plotOutput(outputId = "bagGCV_PredPlot"),
                                  
                                  hr(),
                                  h4("Variable Importance"),
                                  tableOutput(outputId = "bagGCV_VarImp"),
                                  
                                  hr(),
                                  verbatimTextOutput(outputId = "bagGCV_Recipe"),
                                  verbatimTextOutput(outputId = "bagGCV_ModelSummary2"),
                                  
                                  hr(),
                                  uiOutput("bagGCV_Explanation")
                         ),
                         
                         tabPanel("Neural Network (nnet)",
                                  wellPanel(
                                    h4("Method Intro — Neural Network (nnet)"),
                                    tags$p("A single-hidden-layer feedforward neural network, trained via backpropagation with weight decay (L2 regularization)."),
                                    tags$ul(
                                      tags$li(tags$b("Key hyper-parameters:"), " ", tags$code("size"), " (hidden units) and ", tags$code("decay"), " (L2 regularization)."),
                                      tags$li(tags$b("Why use it:"), " handles nonlinear relationships while controlling overfitting via decay."),
                                      tags$li(tags$b("Preprocessing:"), " normalization and dummy encoding are essential; scaling mandatory.")
                                    )
                                  ),
                                  
                                  verbatimTextOutput(outputId = "nnet_ModelSummary0"),
                                  
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId  = "nnet_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(nnet_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = nnet_initial
                                           ),
                                           bsTooltip(id = "nnet_Preprocess", title = "Ordered recipe steps for nnet", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton("nnet_Go", "Train", icon = icon("play")),
                                           bsTooltip("nnet_Go", "Train or retrain nnet model")
                                    ),
                                    column(width = 1,
                                           actionButton("nnet_Load", "Load", icon = icon("file-arrow-up")),
                                           bsTooltip("nnet_Load", "Reload the saved model")
                                    ),
                                    column(width = 1,
                                           actionButton("nnet_Delete", "Forget", icon = icon("trash-can")),
                                           bsTooltip("nnet_Delete", "Remove nnet model from memory")
                                    )
                                  ),
                                  
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput("nnet_Metrics"),
                                  
                                  hr(),
                                  h4("Tuning Profile"),
                                  plotOutput("nnet_ModelPlots"),
                                  
                                  hr(),
                                  h4("Predicted vs Actual"),
                                  plotOutput("nnet_PredPlot"),
                                  
                                  hr(),
                                  h4("Variable Importance"),
                                  tableOutput("nnet_VarImp"),
                                  
                                  hr(),
                                  verbatimTextOutput("nnet_Recipe"),
                                  verbatimTextOutput("nnet_ModelSummary2"),
                                  
                                  hr(),
                                  uiOutput("nnet_Explanation")
                         ),
                         
                         # ---- Averaged Neural Network (avNNet) ----
                         tabPanel("Averaged Neural Net (avNNet)",
                                  wellPanel(
                                    h4("Method Intro — Model Averaged Neural Network (avNNet)"),
                                    tags$p("Ensemble of small neural networks trained on bootstrap samples; predictions are averaged."),
                                    tags$ul(
                                      tags$li(tags$b("Key hyper-parameters:"), " ", tags$code("size"), " (hidden units), ",
                                              tags$code("decay"), " (L2 regularization), ", tags$code("bag"), " (internal bagging)."),
                                      tags$li(tags$b("Preprocessing:"), " impute + dummy + center/scale are essential for stable training.")
                                    )
                                  ),
                                  
                                  verbatimTextOutput(outputId = "avNNet_ModelSummary0"),
                                  
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId  = "avNNet_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(avNNet_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = avNNet_initial
                                           ),
                                           bsTooltip(id = "avNNet_Preprocess",
                                                     title = "These entries will be populated in order from a saved model once it loads",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "avNNet_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "avNNet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "avNNet_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "avNNet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "avNNet_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "avNNet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "avNNet_Metrics"),
                                  
                                  hr(),
                                  h4("Tuning Profile"),
                                  plotOutput(outputId = "avNNet_ModelPlots"),
                                  
                                  hr(),
                                  h4("Predicted vs Actual"),
                                  plotOutput(outputId = "avNNet_PredPlot"),
                                  
                                  hr(),
                                  h4("Variable Importance"),
                                  tableOutput(outputId = "avNNet_VarImp"),
                                  
                                  hr(),
                                  verbatimTextOutput(outputId = "avNNet_Recipe"),
                                  verbatimTextOutput(outputId = "avNNet_ModelSummary2"),
                                  
                                  hr(),
                                  uiOutput("avNNet_Explanation")
                         ),
                         
                         
                         # ---- MLP (RSNNS) ----
                         
                         tabPanel("MLP (RSNNS)",
                                  verbatimTextOutput(outputId = "mlp_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "mlp_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(mlp_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = mlp_initial
                                           ),
                                           bsTooltip(id = "mlp_Preprocess",
                                                     title = "These entries will be populated in the correct order from a saved model once it loads",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "mlp_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "mlp_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "mlp_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "mlp_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "mlp_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "mlp_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "mlp_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "mlp_ModelPlots"),       # caret tuning profile
                                  plotOutput(outputId = "mlp_PredPlot"),         # Pred vs Actual
                                  tableOutput(outputId = "mlp_VarImp"),          # may be empty if not supported
                                  verbatimTextOutput(outputId = "mlp_Recipe"),
                                  verbatimTextOutput(outputId = "mlp_ModelSummary2"),
                                  uiOutput(outputId = "mlp_Explanation")
                         ),
                         
                         
                         # ---- monmlp ----
                         tabPanel("monmlp",
                                  verbatimTextOutput(outputId = "monmlp_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "monmlp_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(monmlp_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = monmlp_initial
                                           ),
                                           bsTooltip(id = "monmlp_Preprocess",
                                                     title = "These entries will be populated in the correct order from a saved model once it loads",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "monmlp_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "monmlp_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "monmlp_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "monmlp_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "monmlp_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "monmlp_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "monmlp_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "monmlp_ModelPlots"),
                                  plotOutput(outputId = "monmlp_PredPlot"),
                                  tableOutput(outputId = "monmlp_VarImp"),
                                  verbatimTextOutput(outputId = "monmlp_Recipe"),
                                  verbatimTextOutput(outputId = "monmlp_ModelSummary2"),
                                  uiOutput(outputId = "monmlp_Explanation")
                         ),
                         
                         
                         
                         # ---- KNN Model tab ----
                         tabPanel("KNN Model",
                                  verbatimTextOutput(outputId = "knn_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id MUST be "<method>_Preprocess" so saved models reload properly
                                           selectizeInput(
                                             inputId = "knn_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(knn_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = knn_initial
                                           ),
                                           bsTooltip(id = "knn_Preprocess",
                                                     title = "These entries will be populated from a saved model once it loads",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "knn_Go", title = "Train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "knn_Load", title = "Reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "knn_Delete", title = "Remove the model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "knn_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "knn_ModelPlots"),
                                  plotOutput(outputId = "knn_PredPlot"),
                                  verbatimTextOutput(outputId = "knn_Recipe"),
                                  verbatimTextOutput(outputId = "knn_ModelSummary2"),
                                  hr(),
                                  h4("What to look for"),
                                  uiOutput("knn_Explanation")
                         ),
                         
                         
                         tabPanel("BayesGLM Model",
                                  verbatimTextOutput(outputId = "bayesglm_ModelSummary0"),
                                  fluidRow(
                                    column(
                                      width = 4,
                                      # The id MUST be "<method>_Preprocess"
                                      selectizeInput(
                                        inputId = "bayesglm_Preprocess",
                                        label   = "Pre-processing",
                                        choices = unique(c(bayesglm_initial, ppchoices)),
                                        multiple = TRUE,
                                        selected = bayesglm_initial
                                      ),
                                      bsTooltip(id = "bayesglm_Preprocess",
                                                title = "These entries will be populated in the correct order from a saved model once it loads",
                                                placement = "top")
                                    ),
                                    column(
                                      width = 1,
                                      actionButton(inputId = "bayesglm_Go",     label = "Train",  icon = icon("play")),
                                      bsTooltip(id = "bayesglm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(
                                      width = 1,
                                      actionButton(inputId = "bayesglm_Load",   label = "Load",   icon = icon("file-arrow-up")),
                                      bsTooltip(id = "bayesglm_Load", title = "This will reload your saved model")
                                    ),
                                    column(
                                      width = 1,
                                      actionButton(inputId = "bayesglm_Delete", label = "Forget", icon = icon("trash-can")),
                                      bsTooltip(id = "bayesglm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bayesglm_Metrics"),
                                  hr(),
                                  # if no hyper-grid, caret::plot shows resample distribution; we also show diagnostics
                                  plotOutput(outputId = "bayesglm_ModelPlots"),
                                  plotOutput(outputId = "bayesglm_PredPlot"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "bayesglm_Coef")
                                  ),
                                  verbatimTextOutput(outputId = "bayesglm_Recipe"),
                                  verbatimTextOutput(outputId = "bayesglm_ModelSummary2"),
                                  hr(),
                                  h4("What to look for"),
                                  uiOutput(outputId = "bayesglm_Explanation")
                         ),
                         
                         
                         
                         # ---------- blasso ----------
                         tabPanel("blasso Model",
                                  verbatimTextOutput(outputId = "blasso_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "blasso_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(blasso_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = blasso_initial
                                           ),
                                           bsTooltip(id = "blasso_Preprocess",
                                                     title = "These entries will be populated in the correct order from a saved model once it loads",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "blasso_Go", label = "Train", icon = icon("play"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "blasso_Load", label = "Load", icon = icon("file-arrow-up"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "blasso_Delete", label = "Forget", icon = icon("trash-can"))
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "blasso_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "blasso_ModelPlots"),
                                  plotOutput(outputId = "blasso_PredPlot"),
                                  tableOutput(outputId = "blasso_VarImp"),
                                  verbatimTextOutput(outputId = "blasso_Recipe"),
                                  verbatimTextOutput(outputId = "blasso_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "blasso_Coef")
                                  ),
                                  hr(),
                                  h4("What to look for"),
                                  uiOutput("blasso_Explanation")
                         ),
                         
                         
                         # ---------- bridge ----------
                         tabPanel("bridge Model",
                                  verbatimTextOutput(outputId = "bridge_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "bridge_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(bridge_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = bridge_initial
                                           ),
                                           bsTooltip(id = "bridge_Preprocess",
                                                     title = "These entries will be populated in the correct order from a saved model once it loads",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bridge_Go", label = "Train", icon = icon("play"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bridge_Load", label = "Load", icon = icon("file-arrow-up"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bridge_Delete", label = "Forget", icon = icon("trash-can"))
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bridge_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "bridge_ModelPlots"),
                                  plotOutput(outputId = "bridge_PredPlot"),
                                  tableOutput(outputId = "bridge_VarImp"),
                                  verbatimTextOutput(outputId = "bridge_Recipe"),
                                  verbatimTextOutput(outputId = "bridge_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "bridge_Coef")
                                  ),
                                  hr(),
                                  h4("What to look for"),
                                  uiOutput("bridge_Explanation")
                         ),
                         
                         # ---------- M5 Rules ----------
                         tabPanel("M5 Rules",
                                  verbatimTextOutput(outputId = "m5_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "m5_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(m5rules_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = m5rules_initial
                                           ),
                                           bsTooltip(id = "m5_Preprocess",
                                                     title = "These entries will be populated in the correct order from a saved model once it loads",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "m5_Go", label = "Train", icon = icon("play"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "m5_Load", label = "Load", icon = icon("file-arrow-up"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "m5_Delete", label = "Forget", icon = icon("trash-can"))
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "m5_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "m5_ModelPlots"),
                                  plotOutput(outputId = "m5_PredPlot"),
                                  tableOutput(outputId = "m5_VarImp"),
                                  verbatimTextOutput(outputId = "m5_Recipe"),
                                  verbatimTextOutput(outputId = "m5_ModelSummary2"),
                                  uiOutput(outputId = "m5_Explanation")
                         ),
                         
                         # ---------- SVM Linear (LiblineaR) ----------
                         tabPanel("SVM Linear 2",
                                  verbatimTextOutput(outputId = "svmL2_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "svmL2_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(svmL2_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = svmL2_initial
                                           ),
                                           bsTooltip(id = "svmL2_Preprocess",
                                                     title = "Order of preprocessing matters — these steps will be applied sequentially in recipe construction.",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmL2_Go", label = "Train", icon = icon("play"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmL2_Load", label = "Load", icon = icon("file-arrow-up"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmL2_Delete", label = "Forget", icon = icon("trash-can"))
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmL2_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "svmL2_ModelPlots"),
                                  plotOutput(outputId = "svmL2_PredPlot"),
                                  tableOutput(outputId = "svmL2_VarImp"),
                                  verbatimTextOutput(outputId = "svmL2_Recipe"),
                                  verbatimTextOutput(outputId = "svmL2_ModelSummary2"),
                                  uiOutput(outputId = "svmL2_Explanation")
                         ),
                         
                         # ---------- Elastic Net ----------
                         tabPanel("Elastic Net",
                                  verbatimTextOutput(outputId = "enet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId  = "enet_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(enet_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = enet_initial
                                           ),
                                           bsTooltip(
                                             id = "enet_Preprocess",
                                             title = "These entries will be populated in the correct order from a saved model once it loads",
                                             placement = "top"
                                           )
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "enet_Go",     label = "Train",  icon = icon("play"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "enet_Load",   label = "Load",   icon = icon("file-arrow-up"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "enet_Delete", label = "Forget", icon = icon("trash-can"))
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "enet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "enet_ModelPlots"),
                                  plotOutput(outputId = "enet_PredPlot"),
                                  tableOutput(outputId = "enet_VarImp"),
                                  verbatimTextOutput(outputId = "enet_Recipe"),
                                  verbatimTextOutput(outputId = "enet_ModelSummary2"),
                                  uiOutput(outputId = "enet_Explanation")
                         ),
                         
                         # ---------- RBF-DDA ----------
                         tabPanel("RBF-DDA",
                                  verbatimTextOutput(outputId = "rbf_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId  = "rbf_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(rbfdda_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = rbfdda_initial
                                           ),
                                           bsTooltip(
                                             id = "rbf_Preprocess",
                                             title = "These entries will be populated in the correct order from a saved model once it loads",
                                             placement = "top"
                                           )
                                    ),
                                    column(width = 1, actionButton(inputId = "rbf_Go",     label = "Train",  icon = icon("play"))),
                                    column(width = 1, actionButton(inputId = "rbf_Load",   label = "Load",   icon = icon("file-arrow-up"))),
                                    column(width = 1, actionButton(inputId = "rbf_Delete", label = "Forget", icon = icon("trash-can")))
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rbf_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rbf_ModelPlots"),
                                  plotOutput(outputId = "rbf_PredPlot"),
                                  tableOutput(outputId = "rbf_VarImp"),
                                  verbatimTextOutput(outputId = "rbf_Recipe"),
                                  verbatimTextOutput(outputId = "rbf_ModelSummary2"),
                                  uiOutput(outputId = "rbf_Explanation")
                         ),
                         
                         # ---------- GBM ----------
                         tabPanel("GBM",
                                  verbatimTextOutput(outputId = "gbm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId  = "gbm_Preprocess",
                                             label    = "Pre-processing",
                                             choices  = unique(c(gbm_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = gbm_initial
                                           ),
                                           bsTooltip(id = "gbm_Preprocess",
                                                     title = "Order matters — these preprocessing steps are applied before GBM training.",
                                                     placement = "top")
                                    ),
                                    column(width = 1, actionButton(inputId = "gbm_Go",     label = "Train",  icon = icon("play"))),
                                    column(width = 1, actionButton(inputId = "gbm_Load",   label = "Load",   icon = icon("file-arrow-up"))),
                                    column(width = 1, actionButton(inputId = "gbm_Delete", label = "Forget", icon = icon("trash-can")))
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "gbm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "gbm_ModelPlots"),
                                  plotOutput(outputId = "gbm_PredPlot"),
                                  tableOutput(outputId = "gbm_VarImp"),
                                  verbatimTextOutput(outputId = "gbm_Recipe"),
                                  verbatimTextOutput(outputId = "gbm_ModelSummary2"),
                                  uiOutput(outputId = "gbm_Explanation")
                         ),
                         
                         # ---------- DENFIS ----------
                         tabPanel("DENFIS",
                                  verbatimTextOutput(outputId = "denfis_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "denfis_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(denfis_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = denfis_initial
                                           ),
                                           bsTooltip(
                                             id = "denfis_Preprocess",
                                             title = "These entries will be populated in the correct order from a saved model once it loads",
                                             placement = "top"
                                           )
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "denfis_Go",    label = "Train",  icon = icon("play"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "denfis_Load",  label = "Load",   icon = icon("file-arrow-up"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "denfis_Delete",label = "Forget", icon = icon("trash-can"))
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "denfis_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "denfis_ModelPlots"),
                                  plotOutput(outputId = "denfis_PredPlot"),
                                  tableOutput(outputId = "denfis_VarImp"),
                                  verbatimTextOutput(outputId = "denfis_Recipe"),
                                  verbatimTextOutput(outputId = "denfis_ModelSummary2"),
                                  uiOutput(outputId = "denfis_Explanation")
                         ),
                         
                         
                         # ---------- GAM (mgcv) ----------
                         tabPanel(
                           "GAM (mgcv)",
                           verbatimTextOutput(outputId = "gam_ModelSummary0"),
                           fluidRow(
                             column(
                               width = 4,
                               selectizeInput(
                                 inputId  = "gam_Preprocess",
                                 label    = "Pre-processing",
                                 choices  = unique(c(gam_initial, ppchoices)),
                                 multiple = TRUE,
                                 selected = gam_initial
                               ),
                               bsTooltip(
                                 id = "gam_Preprocess",
                                 title = "Order matters and will be restored from a loaded model.",
                                 placement = "top"
                               )
                             ),
                             column(width = 1, actionButton(inputId = "gam_Go",     label = "Train",  icon = icon("play"))),
                             column(width = 1, actionButton(inputId = "gam_Load",   label = "Load",   icon = icon("file-arrow-up"))),
                             column(width = 1, actionButton(inputId = "gam_Delete", label = "Forget", icon = icon("trash-can")))
                           ),
                           hr(),
                           h3("Resampled performance:"),
                           tableOutput(outputId = "gam_Metrics"),
                           hr(),
                           plotOutput(outputId = "gam_ModelPlots"),
                           plotOutput(outputId = "gam_PredPlot"),
                           tableOutput(outputId = "gam_VarImp"),
                           verbatimTextOutput(outputId = "gam_Recipe"),
                           verbatimTextOutput(outputId = "gam_ModelSummary2"),
                           uiOutput(outputId = "gam_Explanation")
                         ),
                         
                         # ---------- GaussprRadial ----------
                         tabPanel(
                           "GaussprRadial",
                           verbatimTextOutput(outputId = "gaussprRadial_ModelSummary0"),
                           fluidRow(
                             column(
                               width = 4,
                               selectizeInput(
                                 inputId  = "gaussprRadial_Preprocess",
                                 label    = "Pre-processing",
                                 choices  = unique(c(gaussprRadial_initial, ppchoices)),
                                 multiple = TRUE,
                                 selected = gaussprRadial_initial
                               ),
                               bsTooltip(
                                 id = "gaussprRadial_Preprocess",
                                 title = "The recipe steps are applied in order; they will restore automatically when loading saved model.",
                                 placement = "top"
                               )
                             ),
                             column(width = 1, actionButton(inputId = "gaussprRadial_Go",     label = "Train",  icon = icon("play"))),
                             column(width = 1, actionButton(inputId = "gaussprRadial_Load",   label = "Load",   icon = icon("file-arrow-up"))),
                             column(width = 1, actionButton(inputId = "gaussprRadial_Delete", label = "Forget", icon = icon("trash-can")))
                           ),
                           hr(),
                           h3("Resampled performance:"),
                           tableOutput(outputId = "gaussprRadial_Metrics"),
                           hr(),
                           plotOutput(outputId = "gaussprRadial_ModelPlots"),
                           plotOutput(outputId = "gaussprRadial_PredPlot"),
                           tableOutput(outputId = "gaussprRadial_VarImp"),
                           verbatimTextOutput(outputId = "gaussprRadial_Recipe"),
                           verbatimTextOutput(outputId = "gaussprRadial_ModelSummary2"),
                           uiOutput(outputId = "gaussprRadial_Explanation")
                         ),
                         
                         # ---------- GAMBoost ----------
                         tabPanel("GAMBoost",
                                  verbatimTextOutput(outputId = "gmb_ModelSummary0"),
                                  fluidRow(
                                    column(
                                      width = 4,
                                      selectizeInput(
                                        inputId = "gmb_Preprocess",
                                        label   = "Pre-processing",
                                        choices = unique(c(gamboost_initial, ppchoices)),
                                        multiple = TRUE,
                                        selected = gamboost_initial
                                      ),
                                      bsTooltip(
                                        id = "gmb_Preprocess",
                                        title = "Order matters and will be honored when a model is loaded.",
                                        placement = "top"
                                      )
                                    ),
                                    column(width = 1, actionButton(inputId = "gmb_Go",     label = "Train",  icon = icon("play"))),
                                    column(width = 1, actionButton(inputId = "gmb_Load",   label = "Load",   icon = icon("file-arrow-up"))),
                                    column(width = 1, actionButton(inputId = "gmb_Delete", label = "Forget", icon = icon("trash-can")))
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "gmb_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "gmb_ModelPlots"),
                                  plotOutput(outputId = "gmb_PredPlot"),
                                  tableOutput(outputId = "gmb_VarImp"),
                                  verbatimTextOutput(outputId = "gmb_Recipe"),
                                  verbatimTextOutput(outputId = "gmb_ModelSummary2"),
                                  uiOutput(outputId = "gmb_Explanation")
                         ),
                         
                         # ---------- rfRules ----------
                         tabPanel("rfRules",
                                  verbatimTextOutput(outputId = "rfRules_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "rfRules_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(rfRules_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = rfRules_initial
                                           ),
                                           bsTooltip(id = "rfRules_Preprocess",
                                                     title = "These entries will be populated in the correct order from a saved model once it loads",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rfRules_Go",    label = "Train",  icon = icon("play"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rfRules_Load",  label = "Load",   icon = icon("file-arrow-up"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rfRules_Delete",label = "Forget", icon = icon("trash-can"))
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rfRules_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rfRules_ModelPlots"),
                                  plotOutput(outputId = "rfRules_PredPlot"),
                                  tableOutput(outputId = "rfRules_VarImp"),
                                  verbatimTextOutput(outputId = "rfRules_Recipe"),
                                  verbatimTextOutput(outputId = "rfRules_ModelSummary2"),
                                  uiOutput(outputId = "rfRules_Explanation")
                         ),
                         
                         # ---------- Treebag ----------
                         tabPanel("Treebag",
                                  verbatimTextOutput(outputId = "treebag_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "treebag_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(ppchoices)),
                                             multiple = TRUE,
                                             selected = treebag_initial
                                           ),
                                           bsTooltip(id = "treebag_Preprocess",
                                                     title = "Select preprocessing steps (order matters). Treebag handles numeric predictors well after centering/scaling.",
                                                     placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "treebag_Go", label = "Train", icon = icon("play"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "treebag_Load", label = "Load", icon = icon("file-arrow-up"))
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "treebag_Delete", label = "Forget", icon = icon("trash-can"))
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "treebag_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "treebag_ModelPlots"),
                                  plotOutput(outputId = "treebag_PredPlot"),
                                  tableOutput(outputId = "treebag_VarImp"),
                                  verbatimTextOutput(outputId = "treebag_Recipe"),
                                  verbatimTextOutput(outputId = "treebag_ModelSummary2"),
                                  uiOutput(outputId = "treebag_Explanation")
                         ),
                         
                         # ---------- Logic Regression (LogicReg) ----------
                         tabPanel("Logic Regression",
                                  verbatimTextOutput(outputId = "logreg_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "logreg_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(logreg_logic_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = logreg_logic_initial
                                           ),
                                           bsTooltip(id = "logreg_Preprocess",
                                                     title = "Order matters; will be restored when loading a saved model.",
                                                     placement = "top")
                                    ),
                                    column(width = 1, actionButton(inputId = "logreg_Go",     label = "Train",  icon = icon("play"))),
                                    column(width = 1, actionButton(inputId = "logreg_Load",   label = "Load",   icon = icon("file-arrow-up"))),
                                    column(width = 1, actionButton(inputId = "logreg_Delete", label = "Forget", icon = icon("trash-can")))
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "logreg_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "logreg_ModelPlots"),
                                  plotOutput(outputId = "logreg_PredPlot"),
                                  tableOutput(outputId = "logreg_VarImp"),
                                  verbatimTextOutput(outputId = "logreg_Recipe"),
                                  verbatimTextOutput(outputId = "logreg_ModelSummary2"),
                                  uiOutput(outputId = "logreg_Explanation")
                         ),
                         
                         # ---------- RVM (Radial) ----------
                         tabPanel("RVM (Radial)",
                                  verbatimTextOutput(outputId = "rvm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "rvm_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(rvmRadial_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = rvmRadial_initial
                                           ),
                                           bsTooltip(id = "rvm_Preprocess",
                                                     title = "Order matters and will be restored when loading a saved model.",
                                                     placement = "top")
                                    ),
                                    column(width = 1, actionButton(inputId = "rvm_Go",     label = "Train",  icon = icon("play"))),
                                    column(width = 1, actionButton(inputId = "rvm_Load",   label = "Load",   icon = icon("file-arrow-up"))),
                                    column(width = 1, actionButton(inputId = "rvm_Delete", label = "Forget", icon = icon("trash-can")))
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rvm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rvm_ModelPlots"),
                                  plotOutput(outputId = "rvm_PredPlot"),
                                  tableOutput(outputId = "rvm_VarImp"),
                                  verbatimTextOutput(outputId = "rvm_Recipe"),
                                  verbatimTextOutput(outputId = "rvm_ModelSummary2"),
                                  uiOutput(outputId = "rvm_Explanation")
                         ),
                         
                         
                         # ---------- BRNN ----------
                         tabPanel("BRNN",
                                  verbatimTextOutput(outputId = "brnn_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "brnn_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(brnn_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = brnn_initial
                                           ),
                                           bsTooltip(id = "brnn_Preprocess",
                                                     title = "Order matters and will be restored when loading a saved model.",
                                                     placement = "top")
                                    ),
                                    column(width = 1, actionButton(inputId = "brnn_Go",     label = "Train",  icon = icon("play"))),
                                    column(width = 1, actionButton(inputId = "brnn_Load",   label = "Load",   icon = icon("file-arrow-up"))),
                                    column(width = 1, actionButton(inputId = "brnn_Delete", label = "Forget", icon = icon("trash-can")))
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "brnn_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "brnn_ModelPlots"),
                                  plotOutput(outputId = "brnn_PredPlot"),
                                  tableOutput(outputId = "brnn_VarImp"),
                                  verbatimTextOutput(outputId = "brnn_Recipe"),
                                  verbatimTextOutput(outputId = "brnn_ModelSummary2"),
                                  uiOutput(outputId = "brnn_Explanation")
                         ),
                         
                         
                         # ---------- KRLS (Radial) ----------
                         tabPanel("KRLS (Radial)",
                                  verbatimTextOutput(outputId = "krls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "krls_Preprocess",
                                             label   = "Pre-processing",
                                             choices = unique(c(krls_initial, ppchoices)),
                                             multiple = TRUE,
                                             selected = krls_initial
                                           ),
                                           bsTooltip(id = "krls_Preprocess",
                                                     title = "KRLS expects numeric-only inputs; order matters when loading models.",
                                                     placement = "top")
                                    ),
                                    column(width = 1, actionButton(inputId = "krls_Go",     label = "Train",  icon = icon("play"))),
                                    column(width = 1, actionButton(inputId = "krls_Load",   label = "Load",   icon = icon("file-arrow-up"))),
                                    column(width = 1, actionButton(inputId = "krls_Delete", label = "Forget", icon = icon("trash-can")))
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "krls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "krls_ModelPlots"),
                                  plotOutput(outputId = "krls_PredPlot"),
                                  tableOutput(outputId = "krls_VarImp"),
                                  verbatimTextOutput(outputId = "krls_Recipe"),
                                  verbatimTextOutput(outputId = "krls_ModelSummary2"),
                                  uiOutput(outputId = "krls_Explanation")
                         )
                         
                         
                         
                         
                        
                         
                         
                         
                         # end of maintenance point
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 2.6, step = 0.1),
    )
  )
))
