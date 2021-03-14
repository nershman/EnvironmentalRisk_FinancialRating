library(readxl)
library(mgcv)
library(broom)
library(assertive.base)

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
  
  
  # observe button 1 press.
  observe({
    input$runbutton
    if(input$response == "Financial Rating"){
     
    }
    
    if(input$response == "Qualitative Rating"){
      
    }
    
    
    
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
  
   output$gamplot <- renderPlot({
     rep_form <- paste(input$response, "~ ", sep = " ")
     cov_form <- paste(paste0("s", parenthesise(input$covariate)), collapse = "+")
     formula <- paste(rep_form, cov_form)
     fit <- gam(as.formula(formula), data = data())
     plot(fit, pages=1)
     })
   
   selected_data <- reactive({
     ## input$numeric_var is a character vector, so we cast it to a list of symbols
     var_list <- syms(c(input$response, input$covariate))
     
     ## Now we evaluate it with !!!
     out_col <- data() %>% select(!!!var_list)
   })

    output$boxplot <- renderPlot({
      #compute natural log + 1
      boxplot(log1p(selected_data()))
    })
    
    output$histo <- renderPlot({
      hist(data()[[input$response]], 
           main = paste("Histogram of", input$response),
           xlab = input$response)
    })
  
}
