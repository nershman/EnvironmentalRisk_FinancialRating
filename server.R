library(readxl)
library(mgcv)
library(broom)
library(assertive.base)
library(ggplot2)
library(gridExtra)

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
    if(input$response == "Qualitativerating"){
      DATA <- append(input$response, input$covariate)
      DT::datatable(subset(data(), select=DATA))
    }
    else if(input$response == "Financialrating"){
      DATA <- append(input$response, input$covariate)
      DT::datatable(subset(data(), select=DATA))      
    }    
  })
  
  observe(({
    input$SaveDatabutton
    save(data, file= "data_saved.RData")
  }))
  
  output$pred <- renderTable({
    if(input$response == "Qualitativerating"){
      load(file = "./models/quali.RData")
      lm_qualitative <- predict(lm_quali)[1:20]
      data.frame(lm_qualitative)
    }
    else if(input$response == "Financialrating"){
      load(file = "./models/ind_gam.RData")
      load(file = "./models/con_gam.RData")
      financial_industrie <- predict(ind_gam)[1:20]
      financial_conseil <- predict(con_gam)[1:20]
      data.frame(financial_industrie,financial_conseil)
    }
  })
  
  
  output$pairplot <- renderPlot({
    nplot<-length(input$covariate)
    covar <- input$covariate
    myplots <- list()
    for ( i in 1:nplot) {
      p1<-  ggplot(data(), aes_string(y = input$response, x = covar[i])) + 
        geom_point() 
      myplots[[i]] <- (p1)
    } 
    grid.arrange(grobs=myplots, ncol=3)
  })
  
  output$print <- renderTable({
    input$covariate
  })
  
  output$reg <- renderPrint({
    rep_form <- paste(input$response, "~ ", sep = " ")
    if(input$response == "Financialrating") {
      cov_form <- paste(paste0("s", parenthesise(input$covariate)), collapse = "+")
      formula <- paste(rep_form, cov_form)
      if (input$family == "gaussian") {
        fit <- gam(as.formula(formula), data = data(),
                 family = gaussian())
      }
      else if (input$family == "poisson") {
        fit <- gam(as.formula(formula), data = data(),
                   family = poisson())
      }
      else if (input$family == "binomia") {
        fit <- gam(as.formula(formula), data = data(),
                    family = binomial())
      }
      else {
        fit <- gam(as.formula(formula), data = data(),
                   family = nb())
      }
    }
    else {
      formula <- paste(rep_form, paste(input$covariate, 
                                       collapse = "+"))
      fit <- lm(as.formula(formula), data = data())
    }
    print(formula)
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
