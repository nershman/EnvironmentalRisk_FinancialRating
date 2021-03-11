library(readxl)
library(mgcv)
library(broom)
library(assertive.base)
library(sjPlot)

server <- function(input, output, session) {
  load("base_no_dupli.RData")  
  
  data <- reactive({
    file1 <- input$file1
    if (is.null(file1)) { 
      return(base_no_dupli)
    } 
    data <- read_excel(file1$datapath)
    data
  })
  
  observe({
    updateSelectInput(
      session,
      "response",
      choices = names(data()[-1]),
      selected = "Financialrating"
    )
  })
  
  observe({
    updateCheckboxGroupInput(
      session,
      "covariate",
      choices = names(data()[-1]),
      selected = c("Turnover", "ebitda")
    )
  })
  
  output$tbl <- DT::renderDataTable({
    DT::datatable(data())
  })
  
  output$pred <- renderTable({
    lm <- load(file = "quali.RData")
    predict(lm_quali)[1:50]
  })
  
  output$print <- renderTable({
    input$covariate
  })
  
  output$reg <- renderPrint({
    rep_form <- paste(input$response, "~ ", sep = " ")
    cov_form <- paste(paste0("s", parenthesise(input$covariate)), collapse = "+")
    formula <- paste(rep_form, cov_form)
    fit <- gam(as.formula(formula), data = data())
    summary(fit)
  })

  
  
}
