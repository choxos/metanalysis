library(shiny)
library(shinydashboard)
library(meta)
library(DT)
library(ggplot2)

ui <- dashboardPage(
        dashboardHeader(title = "Metanalysis"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Data Input", tabName = "data", icon = icon("database")),
                        menuItem("Analysis", tabName = "analysis", icon = icon("calculator")),
                        menuItem("Results", tabName = "results", icon = icon("chart-bar")),
                        menuItem("About", tabName = "about", icon = icon("info-circle"))
                )
        ),
        dashboardBody(
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
                                        tabPanel("Meta-Analysis Type",
                                                 selectInput("ma_type", "Meta-analysis type:",
                                                             choices = c("Binary outcome" = "metabin",
                                                                         "Continuous outcome" = "metacont")),
                                                 conditionalPanel(
                                                         condition = "input.ma_type == 'metabin'",
                                                         selectInput("sm_binary", "Summary measure:",
                                                                     choices = c("Odds Ratio" = "OR",
                                                                                 "Risk Ratio" = "RR",
                                                                                 "Risk Difference" = "RD",
                                                                                 "Arcsine Difference" = "ASD")),
                                                         selectInput("method_binary", "Method:",
                                                                     choices = c("Inverse variance" = "Inverse",
                                                                                 "Mantel-Haenszel" = "MH",
                                                                                 "Peto" = "Peto"))
                                                 ),
                                                 conditionalPanel(
                                                         condition = "input.ma_type == 'metacont'",
                                                         selectInput("sm_cont", "Summary measure:",
                                                                     choices = c("Mean Difference" = "MD",
                                                                                 "Standardized Mean Difference" = "SMD",
                                                                                 "Ratio of Means" = "ROM"))
                                                 )
                                        ),
                                        tabPanel("Model Settings",
                                                 checkboxInput("random", "Random effects", TRUE),
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
                                        ),
                                        tabPanel("Subgroup Analysis",
                                                 selectInput("subgroup", "Subgroup variable:", choices = NULL)
                                        ),
                                        tabPanel("Meta-Regression",
                                                 selectInput("metareg_var", "Meta-regression variable:", choices = NULL)
                                        ),
                                        tabPanel("Sensitivity Analysis",
                                                 checkboxInput("leave_one_out", "Leave-one-out sensitivity analysis", FALSE)
                                        )
                                ),
                                actionButton("run_analysis", "Run Analysis")
                        ),
                        tabItem(tabName = "results",
                                tabBox(
                                        title = "Results",
                                        id = "results_tabs",
                                        width = 12,
                                        tabPanel("Summary", verbatimTextOutput("results")),
                                        tabPanel("Forest Plot", 
                                                 plotOutput("forest_plot", height = "600px"),
                                                 downloadButton("download_forest", "Download Forest Plot")),
                                        tabPanel("Funnel Plot", 
                                                 plotOutput("funnel_plot", height = "600px"),
                                                 downloadButton("download_funnel", "Download Funnel Plot")),
                                        tabPanel("Subgroup Analysis", verbatimTextOutput("subgroup_results")),
                                        tabPanel("Meta-Regression", verbatimTextOutput("metareg_results")),
                                        tabPanel("Sensitivity Analysis", verbatimTextOutput("sensitivity_results")),
                                        tabPanel("Data Table", DTOutput("data_table"))
                                )
                        ),
                        # About tab
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
        })
        
        observeEvent(input$load_file_data, {
                req(input$file)
                df <- read.csv(input$file$datapath)
                data(df)
        })
        
        observeEvent(input$run_analysis, {
                req(data())
                
                if(input$ma_type == "metabin") {
                        result <- metabin(event.e = data()$e, n.e = data()$n,
                                          event.c = data()$c, n.c = data()$nc,
                                          studlab = data()$studlab,
                                          sm = input$sm_binary,
                                          method = input$method_binary,
                                          method.tau = input$method_tau,
                                          method.random.ci = input$method_ci,
                                          random = input$random,
                                          prediction = input$prediction)
                } else if(input$ma_type == "metacont") {
                        result <- metacont(n.e = data()$n1, mean.e = data()$mean1, sd.e = data()$sd1,
                                           n.c = data()$n2, mean.c = data()$mean2, sd.c = data()$sd2,
                                           studlab = data()$studlab,
                                           sm = input$sm_cont,
                                           method.tau = input$method_tau,
                                           method.random.ci = input$method_ci,
                                           random = input$random,
                                           prediction = input$prediction)
                }
                
                ma_result(result)
                
                output$results <- renderPrint({
                        summary(result)
                })
                
                output$forest_plot <- renderPlot({
                        forest(ma_result())
                }, res = 120)
                
                output$funnel_plot <- renderPlot({
                        funnel(ma_result())
                }, res = 120)
                
                if(input$subgroup != "None") {
                        subgroup_result <- update(result, subgroup = data()[[input$subgroup]])
                        output$subgroup_results <- renderPrint({
                                print(subgroup_result, subgroup = TRUE)
                        })
                }
                
                if(input$metareg_var != "None") {
                        metareg_result <- metareg(result, formula.tau2 = as.formula(paste("~", input$metareg_var)))
                        output$metareg_results <- renderPrint({
                                print(metareg_result)
                        })
                }
                
                if(input$leave_one_out) {
                        sensitivity_result <- metainf(result)
                        output$sensitivity_results <- renderPrint({
                                print(sensitivity_result)
                        })
                }
                
                output$data_table <- renderDT({
                        datatable(data())
                })
        })
        
        output$download_forest <- downloadHandler(
                filename = function() {
                        paste("forest_plot_", Sys.Date(), ".png", sep = "")
                },
                content = function(file) {
                        png(file, width = 10, height = 8, units = "in", res = 300)
                        forest(ma_result())
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
        
        observe({
                req(data())
                updateSelectInput(session, "subgroup", choices = c("None", names(data())))
                updateSelectInput(session, "metareg_var", choices = c("None", names(data())))
        })
}

shinyApp(ui = ui, server = server)