library(shiny)
library(shinydashboard)
library(shinyjs)
library(meta)
library(DT)
library(ggplot2)
library(dplyr)
library(robvis)
library(officer)
library(flextable)


z_score = function(p) {
        qnorm(1 - p/2)
}

t_score = function(p, df) {
        qt(1 - p/2, df)
}

iv_meta = function(mean, sd, n, se=NULL){
        if(is.null(se)){
                weighted_mean = sum(mean*(1/(sd/sqrt(n))^2))/sum(1/(sd/sqrt(n))^2)
                return(weighted_mean)
        } else {
                weighted_mean = sum(mean*(1/se^2))/sum(1/se^2)
                weighted_se = sqrt(1/sum(1/se^2))
                ci_lb = weighted_mean - qnorm(0.975) * weighted_se
                ci_ub = weighted_mean + qnorm(0.975) * weighted_se
                return(c(weighted_mean, ci_lb, ci_ub))
        }
}

ui <- dashboardPage(
        dashboardHeader(title = "Metanalysis"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Data Input", tabName = "data", icon = icon("database")),
                        menuItem("Analysis", tabName = "analysis", icon = icon("calculator")),
                        menuItem("Results", tabName = "results", icon = icon("chart-bar")),
                        menuItem("Meta-Regression", tabName = "metareg", icon = icon("line-chart")),
                        menuItem("Subgroup Analysis", tabName = "subgroup", icon = icon("object-group")),
                        menuItem("Risk of Bias", tabName = "rob", icon = icon("exclamation-triangle")),
                        menuItem("GRADE Assessment", tabName = "grade", icon = icon("balance-scale")),
                        menuItem("Imputation", tabName = "imputation", icon = icon("calculator")),
                        menuItem("Responder Analysis", tabName = "responder", icon = icon("percent")),
                        menuItem("About", tabName = "about", icon = icon("info-circle"))
                )
        ),
        dashboardBody(
                useShinyjs(),
                tabItems(
                        tabItem(tabName = "data",
                                tabBox(
                                        title = "Data Input",
                                        id = "data_tabs",
                                        width = 12,
                                        tabPanel("Manual Entry",
                                                 radioButtons("data_type", "Data type:", 
                                                              choices = c("Binary", "Continuous"),
                                                              selected = "Binary"),
                                                 numericInput("n_studies", "Number of studies", value = 5, min = 2),
                                                 uiOutput("manual_data_entry"),
                                                 actionButton("load_manual_data", "Load Data"),
                                                 br(),
                                                 h4("Sample Data:"),
                                                 verbatimTextOutput("sample_data")
                                        ),
                                        tabPanel("File Upload",
                                                 fileInput("file", "Choose CSV file", accept = ".csv"),
                                                 actionButton("load_file_data", "Load Data")
                                        )
                                )
                        ),
                        tabItem(tabName = "analysis",
                                tabBox(
                                        title = "Analysis Settings",
                                        id = "analysis_tabs",
                                        width = 12,
                                        tabPanel("Model Settings",
                                                 uiOutput("summary_measure_ui"),
                                                 uiOutput("method_ui"),
                                                 selectInput("method_tau", "tau^2 estimator:",
                                                             choices = c("DerSimonian-Laird" = "DL",
                                                                         "Restricted maximum-likelihood" = "REML",
                                                                         "Maximum-likelihood" = "ML",
                                                                         "Paule-Mandel" = "PM",
                                                                         "Empirical Bayes" = "EB")),
                                                 selectInput("method_ci", "Confidence interval method:",
                                                             choices = c("Wald-type" = "classic",
                                                                         "Hartung-Knapp" = "HK",
                                                                         "Kenward-Roger" = "KR")),
                                                 checkboxInput("prediction", "Show prediction interval", FALSE)
                                        )
                                ),
                                actionButton("run_analysis", "Run Analysis")
                        ),
                        tabItem(tabName = "results",
                                tabBox(
                                        title = "Results",
                                        id = "results_tabs",
                                        width = 12,
                                        tabPanel("Forest Plot", 
                                                 plotOutput("forest_plot_random", height = "600px"),
                                                 downloadButton("download_forest", "Download Forest Plot")),
                                        tabPanel("Funnel Plot", 
                                                 plotOutput("funnel_plot", height = "600px"),
                                                 downloadButton("download_funnel", "Download Funnel Plot")),
                                        tabPanel("Summary", verbatimTextOutput("results")),
                                        tabPanel("Data Table", DTOutput("data_table"))
                                )
                        ),
                        tabItem(tabName = "metareg",
                                fluidRow(
                                        box(
                                                title = "Define New Variables",
                                                width = 12,
                                                actionButton("add_metareg_var", "Add New Variable"),
                                                uiOutput("metareg_var_inputs"),
                                                actionButton("save_metareg_vars", "Save Variables")
                                        )
                                ),
                                fluidRow(
                                        box(
                                                title = "Run Meta-Regression",
                                                width = 12,
                                                selectizeInput("metareg_vars", "Select Variables for Meta-Regression", 
                                                               choices = NULL, multiple = TRUE),
                                                actionButton("run_metareg", "Run Meta-Regression"),
                                                verbatimTextOutput("metareg_results")
                                        )
                                )
                        ),
                        tabItem(tabName = "subgroup",
                                fluidRow(
                                        box(
                                                title = "Define New Variables",
                                                width = 12,
                                                actionButton("add_subgroup_var", "Add New Variable"),
                                                uiOutput("subgroup_var_inputs"),
                                                actionButton("save_subgroup_vars", "Save Variables")
                                        )
                                ),
                                fluidRow(
                                        box(
                                                title = "Run Subgroup Analysis",
                                                width = 12,
                                                selectInput("subgroup", "Select Variable for Subgroup Analysis", 
                                                            choices = c("None"), selected = "None"),
                                                actionButton("run_subgroup", "Run Subgroup Analysis")
                                        )
                                ),
                                fluidRow(
                                        box(
                                                title = "Subgroup Analysis Results",
                                                width = 12,
                                                plotOutput("subgroup_plot", height = "600px"),
                                                verbatimTextOutput("subgroup_results")
                                        )
                                )
                        ),
                        tabItem(tabName = "rob",
                                fluidRow(
                                        box(
                                                title = "Risk of Bias Assessment (RoB v2)",
                                                width = 12,
                                                uiOutput("rob_input"),
                                                actionButton("save_rob", "Save RoB Assessment"),
                                                actionButton("generate_rob_plot", "Generate RoB Plot")
                                        )
                                ),
                                fluidRow(
                                        box(
                                                title = "Risk of Bias Plot",
                                                width = 12,
                                                plotOutput("rob_plot", height = "600px"),
                                                downloadButton("download_rob_plot", "Download RoB Plot")
                                        )
                                )
                        ),
                        tabItem(tabName = "grade",
                                fluidRow(
                                        box(
                                                title = "GRADE Assessment",
                                                width = 12,
                                                numericInput("num_studies", "Number of studies", value = 1, min = 1),
                                                selectInput("risk_of_bias", "Risk of bias", 
                                                            choices = c("Not serious" = "Not serious", 
                                                                        "Serious" = "Serious", 
                                                                        "Very serious" = "Very serious")),
                                                selectInput("inconsistency", "Inconsistency", 
                                                            choices = c("Not serious" = "Not serious", 
                                                                        "Serious" = "Serious", 
                                                                        "Very serious" = "Very serious")),
                                                selectInput("indirectness", "Indirectness", 
                                                            choices = c("Not serious" = "Not serious", 
                                                                        "Serious" = "Serious", 
                                                                        "Very serious" = "Very serious")),
                                                selectInput("imprecision", "Imprecision", 
                                                            choices = c("Not serious" = "Not serious", 
                                                                        "Serious" = "Serious", 
                                                                        "Very serious" = "Very serious")),
                                                selectInput("publication_bias", "Publication bias", 
                                                            choices = c("Undetected" = "Undetected", 
                                                                        "Strongly suspected" = "Strongly suspected")),
                                                numericInput("n_exp", "Number of patients in experimental group", value = 0, min = 0),
                                                numericInput("n_ctrl", "Number of patients in control group", value = 0, min = 0),
                                                textInput("relative_effect", "Relative effect (95% CI)"),
                                                textInput("absolute_effect", "Absolute effect (95% CI)"),
                                                actionButton("calculate_grade", "Calculate GRADE"),
                                                textOutput("certainty_output"),
                                                hr(),
                                                textAreaInput("rob_explanation", "Risk of bias explanation", rows = 3),
                                                textAreaInput("inconsistency_explanation", "Inconsistency explanation", rows = 3),
                                                textAreaInput("indirectness_explanation", "Indirectness explanation", rows = 3),
                                                textAreaInput("imprecision_explanation", "Imprecision explanation", rows = 3),
                                                textAreaInput("publication_bias_explanation", "Publication bias explanation", rows = 3),
                                                downloadButton("download_grade", "Download GRADE Assessment")
                                        )
                                ),
                                fluidRow(
                                        box(
                                                title = "Explanations",
                                                width = 12,
                                                verbatimTextOutput("explanations_output")
                                        )
                                )
                        ),
                        tabItem(tabName = "imputation",
                                tabBox(
                                        title = "Imputation Circumstances",
                                        id = "imputation_tabs",
                                        width = 12,
                                        tabPanel("1. Standard Error",
                                                 numericInput("se_n", "Sample Size", value = 100),
                                                 numericInput("se_se", "Standard Error", value = 1),
                                                 actionButton("se_calc", "Calculate"),
                                                 verbatimTextOutput("se_result")
                                        ),
                                        tabPanel("2. Confidence Interval",
                                                 numericInput("ci_n", "Sample Size", value = 100),
                                                 numericInput("ci_lower", "Lower CI", value = 0),
                                                 numericInput("ci_upper", "Upper CI", value = 2),
                                                 numericInput("ci_conf", "Confidence Level (e.g., 0.95)", value = 0.95),
                                                 actionButton("ci_calc", "Calculate"),
                                                 verbatimTextOutput("ci_result")
                                        ),
                                        tabPanel("3. Descriptive Statistics",
                                                 selectInput("desc_type", "Available Data",
                                                             choices = c("Median, Min, Max", "Median, Q1, Q3", "Min, Q1, Median, Q3, Max")),
                                                 numericInput("desc_n", "Sample Size", value = 100),
                                                 numericInput("desc_min", "Minimum", value = 0),
                                                 numericInput("desc_q1", "Q1", value = 25),
                                                 numericInput("desc_median", "Median", value = 50),
                                                 numericInput("desc_q3", "Q3", value = 75),
                                                 numericInput("desc_max", "Maximum", value = 100),
                                                 actionButton("desc_calc", "Calculate"),
                                                 verbatimTextOutput("desc_result")
                                        ),
                                        tabPanel("4. Pooled SD",
                                                 numericInput("pool_n1", "Sample Size 1", value = 50),
                                                 numericInput("pool_sd1", "SD 1", value = 10),
                                                 numericInput("pool_n2", "Sample Size 2", value = 50),
                                                 numericInput("pool_sd2", "SD 2", value = 12),
                                                 numericInput("pool_mean1", "Mean 1", value = 100),
                                                 numericInput("pool_mean2", "Mean 2", value = 105),
                                                 actionButton("pool_calc", "Calculate"),
                                                 verbatimTextOutput("pool_result")
                                        ),
                                        tabPanel("5. SD for Change Score",
                                                 numericInput("change_n", "Sample Size", value = 100),
                                                 numericInput("change_sd_baseline", "SD Baseline", value = 10),
                                                 numericInput("change_sd_post", "SD Post", value = 12),
                                                 numericInput("change_corr", "Correlation", value = 0.5),
                                                 actionButton("change_calc", "Calculate"),
                                                 verbatimTextOutput("change_result")
                                        ),
                                        tabPanel("6. SE of Difference",
                                                 numericInput("diff_n1", "Sample Size 1", value = 50),
                                                 numericInput("diff_n2", "Sample Size 2", value = 50),
                                                 numericInput("diff_se", "SE of Difference", value = 2),
                                                 actionButton("diff_calc", "Calculate"),
                                                 verbatimTextOutput("diff_result")
                                        ),
                                        tabPanel("7. Effect Estimate with CI",
                                                 numericInput("effect_n1", "Sample Size 1", value = 50),
                                                 numericInput("effect_n2", "Sample Size 2", value = 50),
                                                 numericInput("effect_est", "Effect Estimate", value = 5),
                                                 numericInput("effect_lower", "Lower CI", value = 2),
                                                 numericInput("effect_upper", "Upper CI", value = 8),
                                                 numericInput("effect_conf", "Confidence Level (e.g., 0.95)", value = 0.95),
                                                 actionButton("effect_calc", "Calculate"),
                                                 verbatimTextOutput("effect_result")
                                        ),
                                        tabPanel("8. Effect Estimate with Z-score",
                                                 numericInput("z_n1", "Sample Size 1", value = 50),
                                                 numericInput("z_n2", "Sample Size 2", value = 50),
                                                 numericInput("z_est", "Effect Estimate", value = 5),
                                                 numericInput("z_score", "Z-score", value = 1.96),
                                                 actionButton("z_calc", "Calculate"),
                                                 verbatimTextOutput("z_result")
                                        ),
                                        tabPanel("9. Effect Estimate with t-value",
                                                 numericInput("t_n1", "Sample Size 1", value = 50),
                                                 numericInput("t_n2", "Sample Size 2", value = 50),
                                                 numericInput("t_est", "Effect Estimate", value = 5),
                                                 numericInput("t_value", "t-value", value = 2),
                                                 actionButton("t_calc", "Calculate"),
                                                 verbatimTextOutput("t_result")
                                        ),
                                        tabPanel("10. Effect Estimate with p-value",
                                                 numericInput("p_n1", "Sample Size 1", value = 50),
                                                 numericInput("p_n2", "Sample Size 2", value = 50),
                                                 numericInput("p_est", "Effect Estimate", value = 5),
                                                 numericInput("p_value", "p-value", value = 0.05),
                                                 radioButtons("p_dist", "Distribution", choices = c("z", "t"), selected = "z"),
                                                 actionButton("p_calc", "Calculate"),
                                                 verbatimTextOutput("p_result")
                                        )
                                )
                        ),
                        tabItem(tabName = "responder",
                                fluidRow(
                                        box(
                                                title = "Responder Analysis",
                                                width = 12,
                                                numericInput("mid_input", "Enter MID:", value = 1, step = 0.1),
                                                actionButton("run_responder", "Run Responder Analysis", class = "btn-primary")
                                        )
                                ),
                                fluidRow(
                                        box(
                                                title = "Results",
                                                width = 12,
                                                verbatimTextOutput("responder_check"),
                                                DTOutput("responder_results"),
                                                DTOutput("individual_results"),
                                                downloadButton("download_responder_results", "Download Aggregated Results"),
                                                downloadButton("download_individual_results", "Download Individual Results")
                                        )
                                )
                        ),
                        tabItem(tabName = "about",
                                fluidRow(
                                        box(
                                                title = "About the Creator",
                                                width = 12,
                                                p("Creator: Ahmad Sofi-Mahmudi"),
                                                br(),
                                                p("Social Media:"),
                                                tags$div(
                                                        tags$a("Blog", href = "https://choxos.com", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("GitHub", href = "https://github.com/choxos", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("Twitter", href = "https://twitter.com/ASofiMahmudi", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("LinkedIn", href = "https://www.linkedin.com/in/asofimahmudi/", target = "_blank")
                                                )
                                        )
                                )
                        )
                )
        )
)

server <- function(input, output, session) {
        
        data <- reactiveVal()
        ma_result <- reactiveVal()
        metareg_vars <- reactiveVal(list())
        rob_data <- reactiveVal(NULL)
        
        output$manual_data_entry <- renderUI({
                n <- input$n_studies
                if(input$data_type == "Binary") {
                        lapply(1:n, function(i) {
                                fluidRow(
                                        column(2, textInput(paste0("studlab_", i), "Study label", value = paste("Study", i))),
                                        column(2, numericInput(paste0("e_", i), "Events (exp)", value = round(runif(1, 10, 50)))),
                                        column(2, numericInput(paste0("n_", i), "Total (exp)", value = round(runif(1, 80, 120)))),
                                        column(2, numericInput(paste0("c_", i), "Events (ctrl)", value = round(runif(1, 10, 50)))),
                                        column(2, numericInput(paste0("nc_", i), "Total (ctrl)", value = round(runif(1, 80, 120))))
                                )
                        })
                } else {
                        lapply(1:n, function(i) {
                                fluidRow(
                                        column(2, textInput(paste0("studlab_", i), "Study label", value = paste("Study", i))),
                                        column(2, numericInput(paste0("n1_", i), "n (exp)", value = round(runif(1, 80, 120)))),
                                        column(2, numericInput(paste0("mean1_", i), "Mean (exp)", value = round(runif(1, 20, 30), 1))),
                                        column(2, numericInput(paste0("sd1_", i), "SD (exp)", value = round(runif(1, 5, 10), 1))),
                                        column(2, numericInput(paste0("n2_", i), "n (ctrl)", value = round(runif(1, 80, 120)))),
                                        column(2, numericInput(paste0("mean2_", i), "Mean (ctrl)", value = round(runif(1, 15, 25), 1))),
                                        column(2, numericInput(paste0("sd2_", i), "SD (ctrl)", value = round(runif(1, 5, 10), 1)))
                                )
                        })
                }
        })
        
        output$sample_data <- renderPrint({
                if(input$data_type == "Binary") {
                        data.frame(
                                studlab = paste("Study", 1:5),
                                e = round(runif(5, 10, 50)),
                                n = round(runif(5, 80, 120)),
                                c = round(runif(5, 10, 50)),
                                nc = round(runif(5, 80, 120))
                        )
                } else {
                        data.frame(
                                studlab = paste("Study", 1:5),
                                n1 = round(runif(5, 80, 120)),
                                mean1 = round(runif(5, 20, 30), 1),
                                sd1 = round(runif(5, 5, 10), 1),
                                n2 = round(runif(5, 80, 120)),
                                mean2 = round(runif(5, 15, 25), 1),
                                sd2 = round(runif(5, 5, 10), 1)
                        )
                }
        })
        
        observeEvent(input$load_manual_data, {
                n <- input$n_studies
                if(input$data_type == "Binary") {
                        df <- data.frame(
                                studlab = sapply(1:n, function(i) input[[paste0("studlab_", i)]]),
                                e = sapply(1:n, function(i) input[[paste0("e_", i)]]),
                                n = sapply(1:n, function(i) input[[paste0("n_", i)]]),
                                c = sapply(1:n, function(i) input[[paste0("c_", i)]]),
                                nc = sapply(1:n, function(i) input[[paste0("nc_", i)]])
                        )
                } else {
                        df <- data.frame(
                                studlab = sapply(1:n, function(i) input[[paste0("studlab_", i)]]),
                                n1 = sapply(1:n, function(i) input[[paste0("n1_", i)]]),
                                mean1 = sapply(1:n, function(i) input[[paste0("mean1_", i)]]),
                                sd1 = sapply(1:n, function(i) input[[paste0("sd1_", i)]]),
                                n2 = sapply(1:n, function(i) input[[paste0("n2_", i)]]),
                                mean2 = sapply(1:n, function(i) input[[paste0("mean2_", i)]]),
                                sd2 = sapply(1:n, function(i) input[[paste0("sd2_", i)]])
                        )
                }
                data(df)
                
                showNotification("Manual data loaded successfully", type = "message")
        })
        
        observeEvent(input$load_file_data, {
                req(input$file)
                df <- read.csv(input$file$datapath)
                data(df)
                
                showNotification("File data loaded successfully", type = "message")
        })
        
        output$summary_measure_ui <- renderUI({
                if(input$data_type == "Binary") {
                        selectInput("sm", "Summary measure:",
                                    choices = c("Odds Ratio" = "OR",
                                                "Risk Ratio" = "RR",
                                                "Risk Difference" = "RD",
                                                "Arcsine Difference" = "ASD"))
                } else {
                        selectInput("sm", "Summary measure:",
                                    choices = c("Mean Difference" = "MD",
                                                "Standardized Mean Difference" = "SMD",
                                                "Ratio of Means" = "ROM"))
                }
        })
        
        output$method_ui <- renderUI({
                if(input$data_type == "Binary") {
                        selectInput("method", "Method:",
                                    choices = c("Inverse variance" = "Inverse",
                                                "Mantel-Haenszel" = "MH",
                                                "Peto" = "Peto"))
                } else {
                        return(NULL)  # No method selection for continuous data
                }
        })
        
        observeEvent(input$run_analysis, {
                req(data())
                
                if(input$data_type == "Binary") {
                        result <- metabin(event.e = data()$e, n.e = data()$n,
                                          event.c = data()$c, n.c = data()$nc,
                                          studlab = data()$studlab,
                                          sm = input$sm,
                                          method = input$method,
                                          method.tau = input$method_tau,
                                          method.random.ci = input$method_ci,
                                          random = TRUE,
                                          prediction = input$prediction)
                } else {
                        result <- metacont(n.e = data()$n1, mean.e = data()$mean1, sd.e = data()$sd1,
                                           n.c = data()$n2, mean.c = data()$mean2, sd.c = data()$sd2,
                                           studlab = data()$studlab,
                                           sm = input$sm,
                                           method.tau = input$method_tau,
                                           method.random.ci = input$method_ci,
                                           random = TRUE,
                                           prediction = input$prediction)
                }
                
                ma_result(result)
                
                output$results <- renderPrint({
                        summary(result)
                })
                
                output$forest_plot_random <- renderPlot({
                        forest(result, main = "Random Effects Model")
                }, res = 120)
                
                output$funnel_plot <- renderPlot({
                        funnel(ma_result())
                }, res = 120)
                
                output$data_table <- renderDT({
                        datatable(data())
                })
                
                showNotification("Analyses done successfully.", type = "message")
        })
        
        output$download_forest <- downloadHandler(
                filename = function() {
                        paste("forest_plot_", Sys.Date(), ".png", sep = "")
                },
                content = function(file) {
                        png(file, width = 10, height = 8, units = "in", res = 300)
                        forest(ma_result(), main = "Random Effects Model")
                        dev.off()
                }
        )
        
        output$download_funnel <- downloadHandler(
                filename = function() {
                        paste("funnel_plot_", Sys.Date(), ".png", sep = "")
                },
                content = function(file) {
                        png(file, width = 10, height = 8, units = "in", res = 300)
                        funnel(ma_result())
                        dev.off()
                }
        )
        
        # Meta-regression functionality
        observeEvent(input$add_metareg_var, {
                req(data())
                current_vars <- metareg_vars()
                new_var_name <- paste0("New_Variable_", length(current_vars) + 1)
                current_vars[[new_var_name]] <- rep("", nrow(data()))
                metareg_vars(current_vars)
        })
        
        output$metareg_var_inputs <- renderUI({
                req(data())
                vars <- metareg_vars()
                study_labels <- data()$studlab
                
                lapply(seq_along(vars), function(i) {
                        var_name <- names(vars)[i]
                        tagList(
                                textInput(paste0("metareg_var_name_", i), "Variable Name", value = var_name),
                                lapply(seq_along(study_labels), function(j) {
                                        textInput(paste0("metareg_var_", i, "_", j), 
                                                  paste("Value for", study_labels[j]), 
                                                  value = vars[[var_name]][j])
                                }),
                                hr()
                        )
                })
        })
        
        # Meta-regression tab
        observeEvent(input$save_metareg_vars, {
                req(data())
                vars <- metareg_vars()
                study_labels <- data()$studlab
                
                new_vars <- list()
                for (i in seq_along(vars)) {
                        new_name <- input[[paste0("metareg_var_name_", i)]]
                        new_values <- sapply(seq_along(study_labels), function(j) {
                                input[[paste0("metareg_var_", i, "_", j)]]
                        })
                        new_vars[[new_name]] <- new_values
                }
                
                metareg_vars(new_vars)
                updateSelectizeInput(session, "metareg_vars", 
                                     choices = names(new_vars), 
                                     selected = NULL)
                
                showNotification("Meta-regression variables saved successfully", type = "message")
        })
        
        
        observeEvent(input$run_metareg, {
                req(ma_result(), metareg_vars(), input$metareg_vars)
                result <- ma_result()
                vars <- metareg_vars()
                
                # Combine new variables with main dataset
                for (var_name in input$metareg_vars) {
                        result$data[[var_name]] <- vars[[var_name]]
                }
                
                # Check for empty values
                empty_counts <- colSums(result$data[, input$metareg_vars, drop = FALSE] == "")
                if (any(empty_counts > 0)) {
                        output$metareg_results <- renderPrint({
                                cat("Error: The following variables contain empty values:\n")
                                print(empty_counts[empty_counts > 0])
                                cat("\nPlease fill in all values before running the meta-regression.")
                        })
                        return()
                }
                
                # Try to convert to numeric, if possible
                for (var_name in input$metareg_vars) {
                        if (all(!is.na(suppressWarnings(as.numeric(result$data[[var_name]]))))) {
                                result$data[[var_name]] <- as.numeric(result$data[[var_name]])
                        }
                }
                
                # Create formula for meta-regression
                formula <- as.formula(paste("~", paste(input$metareg_vars, collapse = " + ")))
                
                # Run meta-regression
                metareg_result <- try(metareg(result, formula))
                
                output$metareg_results <- renderPrint({
                        if (inherits(metareg_result, "try-error")) {
                                cat("Error in meta-regression:", metareg_result)
                        } else {
                                print(metareg_result)
                        }
                })
        })
        
        # Subgroup analysis functionality
        subgroup_vars <- reactiveVal(list())
        
        observeEvent(input$add_subgroup_var, {
                req(data())
                current_vars <- subgroup_vars()
                new_var_name <- paste0("New_Variable_", length(current_vars) + 1)
                current_vars[[new_var_name]] <- rep("", nrow(data()))
                subgroup_vars(current_vars)
        })
        
        output$subgroup_var_inputs <- renderUI({
                req(data())
                vars <- subgroup_vars()
                study_labels <- data()$studlab
                
                lapply(seq_along(vars), function(i) {
                        var_name <- names(vars)[i]
                        tagList(
                                textInput(paste0("subgroup_var_name_", i), "Variable Name", value = var_name),
                                lapply(seq_along(study_labels), function(j) {
                                        textInput(paste0("subgroup_var_", i, "_", j), 
                                                  paste("Value for", study_labels[j]), 
                                                  value = vars[[var_name]][j])
                                }),
                                hr()
                        )
                })
        })
        
        # Subgroup Analysis tab
        observeEvent(input$save_subgroup_vars, {
                req(data())
                vars <- subgroup_vars()
                study_labels <- data()$studlab
                
                new_vars <- list()
                for (i in seq_along(vars)) {
                        new_name <- input[[paste0("subgroup_var_name_", i)]]
                        new_values <- sapply(seq_along(study_labels), function(j) {
                                input[[paste0("subgroup_var_", i, "_", j)]]
                        })
                        new_vars[[new_name]] <- new_values
                }
                
                subgroup_vars(new_vars)
                updateSelectInput(session, "subgroup", 
                                  choices = c("None", names(new_vars)), 
                                  selected = "None")
                
                showNotification("Subgroup analysis variables saved successfully", type = "message")
        })
        
        observeEvent(input$run_subgroup, {
                req(ma_result(), subgroup_vars(), input$subgroup, input$subgroup != "None")
                result <- ma_result()
                vars <- subgroup_vars()
                
                # Combine new variable with main dataset
                result$data[[input$subgroup]] <- vars[[input$subgroup]]
                
                # Check for empty values
                if (any(result$data[[input$subgroup]] == "")) {
                        output$subgroup_results <- renderPrint({
                                cat("Error: The selected variable contains empty values. Please fill in all values before running the subgroup analysis.")
                        })
                        output$subgroup_plot <- renderPlot({
                                plot.new()
                                text(0.5, 0.5, "No data available for subgroup analysis", cex = 1.5)
                        })
                        return()
                }
                
                # Run subgroup analysis
                subgroup_result <- try(update(result, subgroup = result$data[[input$subgroup]]))
                
                output$subgroup_plot <- renderPlot({
                        if (inherits(subgroup_result, "try-error")) {
                                plot.new()
                                text(0.5, 0.5, "Error in subgroup analysis", cex = 1.5)
                        } else {
                                forest(subgroup_result, subgroup = TRUE, main = "Random Effects Model - Subgroup Analysis")
                        }
                }, res = 120)
                
                output$subgroup_results <- renderPrint({
                        if (inherits(subgroup_result, "try-error")) {
                                cat("Error in subgroup analysis:", subgroup_result)
                        } else {
                                print(subgroup_result, subgroup = TRUE)
                        }
                })
        })
        
        # RoB2
        output$rob_input <- renderUI({
                req(data())
                study_labels <- data()$studlab
                
                tagList(
                        lapply(seq_along(study_labels), function(i) {
                                fluidRow(
                                        column(2, textInput(paste0("rob_studlab_", i), "Study label", value = study_labels[i])),
                                        column(1, selectInput(paste0("rob_D1_", i), "D1", choices = c("Low", "Some concerns", "High"))),
                                        column(1, selectInput(paste0("rob_D2_", i), "D2", choices = c("Low", "Some concerns", "High"))),
                                        column(1, selectInput(paste0("rob_D3_", i), "D3", choices = c("Low", "Some concerns", "High"))),
                                        column(1, selectInput(paste0("rob_D4_", i), "D4", choices = c("Low", "Some concerns", "High"))),
                                        column(1, selectInput(paste0("rob_D5_", i), "D5", choices = c("Low", "Some concerns", "High"))),
                                        column(1, selectInput(paste0("rob_Overall_", i), "Overall", choices = c("Low", "Some concerns", "High")))
                                )
                        }),
                        tags$script(HTML(paste0("$(document).ready(function() {",
                                                paste(sprintf("$('#rob_studlab_%d').prop('readonly', true);", seq_along(study_labels)), collapse = "\n"),
                                                "});")
                        ))
                )
        })
        
        observeEvent(input$save_rob, {
                req(data())
                study_labels <- data()$studlab
                
                rob_df <- data.frame(
                        Study = study_labels,
                        D1 = sapply(seq_along(study_labels), function(i) input[[paste0("rob_D1_", i)]]),
                        D2 = sapply(seq_along(study_labels), function(i) input[[paste0("rob_D2_", i)]]),
                        D3 = sapply(seq_along(study_labels), function(i) input[[paste0("rob_D3_", i)]]),
                        D4 = sapply(seq_along(study_labels), function(i) input[[paste0("rob_D4_", i)]]),
                        D5 = sapply(seq_along(study_labels), function(i) input[[paste0("rob_D5_", i)]]),
                        Overall = sapply(seq_along(study_labels), function(i) input[[paste0("rob_Overall_", i)]])
                )
                
                rob_data(rob_df)
                showNotification("Risk of Bias assessment saved successfully", type = "message")
        })
        
        rob_plot <- reactiveVal()
        
        observeEvent(input$generate_rob_plot, {
                req(rob_data())
                
                plot <- rob_traffic_light(rob_data(), tool = "ROB2")
                rob_plot(plot)
                
                output$rob_plot <- renderPlot({
                        plot
                })
                
                showNotification("Risk of Bias plot generated successfully", type = "message")
        })
        
        output$download_rob_plot <- downloadHandler(
                filename = function() {
                        paste("rob_plot_", Sys.Date(), ".png", sep = "")
                },
                content = function(file) {
                        req(rob_plot())
                        ggsave(file, plot = rob_plot(), device = "png", width = 10, height = 8, units = "in", dpi = 300)
                }
        )
        
        # GRADE
        grade_result <- reactiveVal(NULL)
        
        observeEvent(input$calculate_grade, {
                # Convert descriptive terms to numeric values for calculation
                term_to_value <- function(term) {
                        switch(term,
                               "Not serious" = 0,
                               "Serious" = -1,
                               "Very serious" = -2,
                               "Undetected" = 0,
                               "Strongly suspected" = -1,
                               0)  # default case
                }
                
                total_downgrade <- term_to_value(input$risk_of_bias) + 
                        term_to_value(input$inconsistency) + 
                        term_to_value(input$indirectness) + 
                        term_to_value(input$imprecision) + 
                        term_to_value(input$publication_bias)
                
                certainty <- case_when(
                        total_downgrade == 0 ~ "⨁⨁⨁⨁ High",
                        total_downgrade == -1 ~ "⨁⨁⨁◯ Moderate",
                        total_downgrade == -2 ~ "⨁⨁◯◯ Low",
                        total_downgrade <= -3 ~ "⨁◯◯◯ Very low"
                )
                
                grade_result(list(
                        num_studies = input$num_studies,
                        risk_of_bias = input$risk_of_bias,
                        inconsistency = input$inconsistency,
                        indirectness = input$indirectness,
                        imprecision = input$imprecision,
                        publication_bias = input$publication_bias,
                        n_exp = input$n_exp,
                        n_ctrl = input$n_ctrl,
                        relative_effect = input$relative_effect,
                        absolute_effect = input$absolute_effect,
                        certainty = certainty,
                        rob_explanation = input$rob_explanation,
                        inconsistency_explanation = input$inconsistency_explanation,
                        indirectness_explanation = input$indirectness_explanation,
                        imprecision_explanation = input$imprecision_explanation,
                        publication_bias_explanation = input$publication_bias_explanation
                ))
                
                output$certainty_output <- renderText({
                        paste("Certainty of evidence:", certainty)
                })
                
                output$explanations_output <- renderText({
                        result <- grade_result()
                        explanations <- c()
                        if (result$risk_of_bias != "Not serious") explanations <- c(explanations, paste("Risk of bias:", result$rob_explanation))
                        if (result$inconsistency != "Not serious") explanations <- c(explanations, paste("Inconsistency:", result$inconsistency_explanation))
                        if (result$indirectness != "Not serious") explanations <- c(explanations, paste("Indirectness:", result$indirectness_explanation))
                        if (result$imprecision != "Not serious") explanations <- c(explanations, paste("Imprecision:", result$imprecision_explanation))
                        if (result$publication_bias != "Undetected") explanations <- c(explanations, paste("Publication bias:", result$publication_bias_explanation))
                        paste(explanations, collapse = "\n\n")
                })
        })
        
        # Update the download handler to use these descriptive terms
        output$download_grade <- downloadHandler(
                filename = function() {
                        paste("grade_assessment_", Sys.Date(), ".docx", sep = "")
                },
                content = function(file) {
                        result <- grade_result()
                        
                        if (is.null(result) || length(result) == 0) {
                                stop("No GRADE assessment data available. Please calculate GRADE first.")
                        }
                        
                        tryCatch({
                                doc <- read_docx()
                                doc <- body_add_par(doc, "GRADE Assessment", style = "heading 1")
                                
                                # Create table data in wide format
                                tab_data <- data.frame(
                                        `# studies` = result$num_studies,
                                        `Risk of bias` = result$risk_of_bias,
                                        Inconsistency = result$inconsistency,
                                        Indirectness = result$indirectness,
                                        Imprecision = result$imprecision,
                                        `Publication bias` = result$publication_bias,
                                        `# patients (exp)` = result$n_exp,
                                        `# patients (ctrl)` = result$n_ctrl,
                                        `Relative effect` = result$relative_effect,
                                        `Absolute effect` = result$absolute_effect,
                                        Certainty = result$certainty
                                )
                                
                                # Create flextable
                                ft <- flextable(tab_data)
                                ft <- autofit(ft)
                                ft <- theme_box(ft)
                                ft <- set_table_properties(ft, layout = "autofit")
                                ft <- bold(ft, part = "header")
                                ft <- width(ft, width = 1)  # Set table width to 100% of page width
                                ft <- fontsize(ft, size = 6.5, part = "all")  # Reduce font size if needed
                                
                                # Adjust column widths if necessary
                                for (col in names(tab_data)) {
                                        ft <- width(ft, j = col, width = 0.8)  # Adjust this value as needed
                                }
                                
                                # Add table to document
                                doc <- body_add_flextable(doc, ft)
                                
                                doc <- body_add_par(doc, "Explanations", style = "heading 2")
                                if (result$risk_of_bias != "Not serious") doc <- body_add_par(doc, paste("Risk of bias:", result$rob_explanation))
                                if (result$inconsistency != "Not serious") doc <- body_add_par(doc, paste("Inconsistency:", result$inconsistency_explanation))
                                if (result$indirectness != "Not serious") doc <- body_add_par(doc, paste("Indirectness:", result$indirectness_explanation))
                                if (result$imprecision != "Not serious") doc <- body_add_par(doc, paste("Imprecision:", result$imprecision_explanation))
                                if (result$publication_bias != "Undetected") doc <- body_add_par(doc, paste("Publication bias:", result$publication_bias_explanation))
                                
                                print(doc, target = file)
                        }, error = function(e) {
                                stop(paste("Error generating GRADE assessment document:", e$message))
                        })
                }
        )
        
        # Imputation
        observeEvent(input$se_calc, {
                sd = sqrt(input$se_n) * input$se_se
                output$se_result = renderPrint({
                        cat("Estimated SD:", round(sd, 4))
                })
        })
        
        observeEvent(input$ci_calc, {
                if (input$ci_n >= 60) {
                        z = z_score((1 - input$ci_conf) / 2)
                        se = (input$ci_upper - input$ci_lower) / (2 * z)
                } else {
                        t = t_score((1 - input$ci_conf) / 2, input$ci_n - 1)
                        se = (input$ci_upper - input$ci_lower) / (2 * t)
                }
                sd = sqrt(input$ci_n) * se
                output$ci_result = renderPrint({
                        cat("Estimated SD:", round(sd, 4))
                })
        })
        
        observeEvent(input$desc_calc, {
                n = input$desc_n
                if (input$desc_type == "Median, Min, Max") {
                        a = input$desc_min
                        m = input$desc_median
                        b = input$desc_max
                        est_mean = if (n < 25) (a + 2*m + b) / 4 else m
                        est_sd = if (n <= 15) sqrt(((b-a)^2 + (a-2*m+b)^2) / 12)
                        else if (n <= 70) (b-a) / 4
                        else (b-a) / 6
                } else if (input$desc_type == "Median, Q1, Q3") {
                        q1 = input$desc_q1
                        m = input$desc_median
                        q3 = input$desc_q3
                        est_mean = (0.7 + 0.39/n) * (q1+q3)/2 + (0.3 - 0.39/n) * m
                        est_sd = (q3-q1) / (2 * qnorm((0.75*n - 0.125) / (n + 0.25)))
                } else {
                        a = input$desc_min
                        q1 = input$desc_q1
                        m = input$desc_median
                        q3 = input$desc_q3
                        b = input$desc_max
                        w = 2.2 / (2.2 + n^0.75)
                        est_mean = w * (a+b)/2 + (0.7 - 0.72/n^0.55) * (q1+q3)/2 + (0.3 + 0.72/n^0.55 - w) * m
                        est_sd = (b-a)/(4*qnorm((n-0.375)/(n+0.25))) + (q3-q1)/(4*qnorm((0.75*n-0.125)/(n+0.25)))
                }
                output$desc_result = renderPrint({
                        cat("Estimated Mean:", round(est_mean, 4), "\n")
                        cat("Estimated SD:", round(est_sd, 4))
                })
        })
        
        observeEvent(input$pool_calc, {
                n1 = input$pool_n1
                n2 = input$pool_n2
                sd1 = input$pool_sd1
                sd2 = input$pool_sd2
                m1 = input$pool_mean1
                m2 = input$pool_mean2
                pooled_sd = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2 + n1*n2/(n1+n2)*(m1^2+m2^2-2*m1*m2)) / (n1+n2-1))
                output$pool_result = renderPrint({
                        cat("Pooled SD:", round(pooled_sd, 4))
                })
        })
        
        observeEvent(input$change_calc, {
                sd_change = sqrt(input$change_sd_baseline^2 + input$change_sd_post^2 - 
                                         2 * input$change_corr * input$change_sd_baseline * input$change_sd_post)
                output$change_result = renderPrint({
                        cat("SD for Change Score:", round(sd_change, 4))
                })
        })
        
        observeEvent(input$diff_calc, {
                sd_avg = input$diff_se / sqrt(1/input$diff_n1 + 1/input$diff_n2)
                output$diff_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        observeEvent(input$effect_calc, {
                if (input$effect_n1 + input$effect_n2 >= 60) {
                        z = z_score((1 - input$effect_conf) / 2)
                        se = (input$effect_upper - input$effect_lower) / (2 * z)
                } else {
                        t = t_score((1 - input$effect_conf) / 2, input$effect_n1 + input$effect_n2 - 2)
                        se = (input$effect_upper - input$effect_lower) / (2 * t)
                }
                sd_avg = se / sqrt(1/input$effect_n1 + 1/input$effect_n2)
                output$effect_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        observeEvent(input$z_calc, {
                se = abs(input$z_est / input$z_score)
                sd_avg = se / sqrt(1/input$z_n1 + 1/input$z_n2)
                output$z_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        observeEvent(input$t_calc, {
                se = abs(input$t_est / input$t_value)
                sd_avg = se / sqrt(1/input$t_n1 + 1/input$t_n2)
                output$t_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        observeEvent(input$p_calc, {
                if (input$p_dist == "z") {
                        score = z_score(input$p_value)
                } else {
                        score = t_score(input$p_value, input$p_n1 + input$p_n2 - 2)
                }
                se = abs(input$p_est / score)
                sd_avg = se / sqrt(1/input$p_n1 + 1/input$p_n2)
                output$p_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        # Preprocess data for responder analysis
        preprocess_data_for_responder <- function(data) {
                responder_data <- data.frame(study = data$studlab)  # Assuming 'study' column exists
                
                # Calculate change scores
                responder_data$change_e <- data$mean1
                responder_data$change_c <- data$mean2
                
                # Calculate SDs for change
                responder_data$sd_e <- data$sd1
                responder_data$sd_c <- data$sd2
                
                # Add sample sizes
                responder_data$n_e <- data$n1
                responder_data$n_c <- data$n2
                
                return(responder_data)
        }
        
        # Update the validation function to check for these specific columns
        is_valid_continuous_data <- function(data) {
                required_columns <- c("studlab", "mean1", "mean2", "sd1", "sd2", "n1", "n2")
                
                missing_columns <- setdiff(required_columns, names(data))
                if (length(missing_columns) > 0) {
                        return(paste("Missing columns:", paste(missing_columns, collapse = ", ")))
                }
                
                numeric_columns <- setdiff(required_columns, "studlab")
                non_numeric_cols <- numeric_columns[!sapply(data[numeric_columns], is.numeric)]
                if (length(non_numeric_cols) > 0) {
                        return(paste("The following columns should be numeric:", paste(non_numeric_cols, collapse = ", ")))
                }
                
                return(TRUE)
        }
        
        # Responder Analysis
        observeEvent(input$run_responder, {
                # Use the main data input
                current_data <- data()
                
                if (is.null(current_data)) {
                        output$responder_check <- renderText("No data available. Please load data first.")
                        return()
                }
                
                validation_result <- is_valid_continuous_data(current_data)
                
                if (is.character(validation_result)) {
                        output$responder_check <- renderText(paste("Error:", validation_result))
                        return()
                }
                
                output$responder_check <- renderText("Data is valid. Preprocessing data for responder analysis...")
                
                # Preprocess data
                tryCatch({
                        responder_data <- preprocess_data_for_responder(current_data)
                        
                        output$responder_check <- renderText("Data preprocessed. Performing responder analysis...")
                        
                        mid <- input$mid_input
                        
                        # Median method
                        pc_median <- (1 - pnorm((mid - median(responder_data$change_c)) / median(responder_data$sd_c))) * 100
                        pe_median <- (1 - pnorm((mid - median(responder_data$change_e)) / median(responder_data$sd_e))) * 100
                        rd_median <- pe_median - pc_median
                        var_rd_median <- (((pe_median/100) * (1 - (pe_median/100))) / sum(responder_data$n_e)) + 
                                (((pc_median/100) * (1 - (pc_median/100))) / sum(responder_data$n_c))
                        ci_rd_median <- rd_median + c(-1, 1) * qnorm(0.975) * sqrt(var_rd_median)
                        
                        # Unweighted average method
                        pe_unweighted_mean <- (1 - pnorm((mid - mean(responder_data$change_e)) / mean(responder_data$sd_e))) * 100
                        rd_unweighted_mean <- pe_unweighted_mean - pc_median
                        var_rd_unweighted_mean <- (((pe_unweighted_mean/100) * (1 - (pe_unweighted_mean/100))) / sum(responder_data$n_e)) + 
                                (((pc_median/100) * (1 - (pc_median/100))) / sum(responder_data$n_c))
                        ci_rd_unweighted_mean <- rd_unweighted_mean + c(-1, 1) * qnorm(0.975) * sqrt(var_rd_unweighted_mean)
                        
                        # Weighted average method
                        weighted_avg_mean_e <- iv_meta(responder_data$change_e, responder_data$sd_e, responder_data$n_e)
                        weighted_avg_sd_e <- iv_meta(responder_data$sd_e, responder_data$sd_e, responder_data$n_e)
                        pe_weighted_mean <- (1 - pnorm((mid - weighted_avg_mean_e) / weighted_avg_sd_e)) * 100
                        rd_weighted_mean <- pe_weighted_mean - pc_median
                        var_rd_weighted_mean <- (((pe_weighted_mean/100) * (1 - (pe_weighted_mean/100))) / sum(responder_data$n_e)) + 
                                (((pc_median/100) * (1 - (pc_median/100))) / sum(responder_data$n_c))
                        ci_rd_weighted_mean <- rd_weighted_mean + c(-1, 1) * qnorm(0.975) * sqrt(var_rd_weighted_mean)
                        
                        # Individual method
                        responder_data$pe <- (1 - pnorm((mid - responder_data$change_e) / responder_data$sd_e)) * 100
                        responder_data$pc <- (1 - pnorm((mid - responder_data$change_c) / responder_data$sd_c)) * 100
                        summary_ind <- data.frame(pe = responder_data$pe, pc = responder_data$pc, n_e = responder_data$n_e, n_c = responder_data$n_c)
                        summary_ind$RD <- summary_ind$pe - summary_ind$pc
                        summary_ind$SE <- sqrt(((summary_ind$pe * (100 - summary_ind$pe) / summary_ind$n_e) +
                                                        (summary_ind$pc * (100 - summary_ind$pc) / summary_ind$n_c)))
                        rd_ind_result <- iv_meta(mean = summary_ind$RD, se = summary_ind$SE)
                        rd_ind <- rd_ind_result[1]
                        ci_rd_ind <- rd_ind_result[2:3]
                        
                        # Create results dataframe
                        results <- data.frame(
                                Method = c("Median", "Unweighted Mean", "Weighted Mean", "Individual"),
                                PE = c(pe_median, pe_unweighted_mean, pe_weighted_mean, NA),
                                PC = c(pc_median, pc_median, pc_median, NA),
                                RD = c(rd_median, rd_unweighted_mean, rd_weighted_mean, rd_ind),
                                CI_Lower = c(ci_rd_median[1], ci_rd_unweighted_mean[1], ci_rd_weighted_mean[1], ci_rd_ind[1]),
                                CI_Upper = c(ci_rd_median[2], ci_rd_unweighted_mean[2], ci_rd_weighted_mean[2], ci_rd_ind[2])
                        )
                        
                        results[, 2:6] <- round(results[, 2:6], 2)
                        results$RD_with_CI <- sprintf("%.2f (%.2f-%.2f)", results$RD, results$CI_Lower, results$CI_Upper)
                        
                        output$responder_results <- renderDT({
                                datatable(results[, c("Method", "PE", "PC", "RD_with_CI")], 
                                          options = list(pageLength = 5, dom = 't'),
                                          rownames = FALSE)
                        })
                        
                        # Individual study results
                        individual_results <- responder_data[, c("study", "pe", "pc")]
                        individual_results$RD <- individual_results$pe - individual_results$pc
                        individual_results <- data.frame(
                                Study = individual_results$study,
                                PE = round(individual_results$pe, 2),
                                PC = round(individual_results$pc, 2),
                                RD = round(individual_results$RD, 2)
                        )
                        
                        output$individual_results <- renderDT({
                                datatable(individual_results, 
                                          options = list(pageLength = 10, dom = 't'),
                                          rownames = FALSE)
                        })
                        
                        # Download handlers
                        output$download_responder_results <- downloadHandler(
                                filename = function() {
                                        "responder_analysis_results.csv"
                                },
                                content = function(file) {
                                        write.csv(results, file, row.names = FALSE)
                                }
                        )
                        
                        output$download_individual_results <- downloadHandler(
                                filename = function() {
                                        "individual_study_results.csv"
                                },
                                content = function(file) {
                                        write.csv(individual_results, file, row.names = FALSE)
                                }
                        )
                        
                        showNotification("Responder analysis completed successfully", type = "message")
                }, error = function(e) {
                        output$responder_check <- renderText(paste("Error in responder analysis:", e$message))
                        showNotification(paste("Error in responder analysis:", e$message), type = "error")
                })
        })
}

shinyApp(ui = ui, server = server)