library(readxl)
library(mgcv)
library(broom)
library(assertive.base)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(plyr)
library(caret)
library(DescTools)
library(forcats)
library(prediction)

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
    save(data, file= "Data.xlsx")
  }))
  
  table <- reactive({
    set.seed(666)
    if(input$response == "Qualitativerating"){
      train_idx <- createDataPartition(data()$Qualitativerating,p=0.8,list=FALSE)
      training <- data()[train_idx,]
      test <- data()[-train_idx,]
      rep_form <- paste(input$response, "~ ", sep = " ")
      cov_form <- paste(input$covariate, collapse = "+")
      formula <- paste(rep_form, cov_form)
      fit <- lm(as.formula(formula), data = training)
      lm_qualitative <- predict(fit, test)
      table <- data.frame(True_Qualitativerating = test$Qualitativerating, 
                          Predict_Qualitativerating = lm_qualitative)
    }
    else if(input$response == "Financialrating"){
      ## Conseil Droit
      conseil <- data() %>% filter(group_name_mixed == "Conseil droit") 
      train_idx <- createDataPartition(conseil$Financialrating,p=0.8,list=FALSE)
      training_con <- conseil[train_idx,]
      test_con <- conseil[-train_idx,]
      rep_form <- paste(input$response, "~ ", sep = " ")
      cov_form <- paste(paste0("s", parenthesise(input$covariate)), collapse = "+")
      formula <- paste(rep_form, cov_form)
      con_gam <- gam(as.formula(formula),
                     data = training_con)
      temp_con <- predict.gam(con_gam, newdata = test_con,
                              type = "response")
      
      ## industrie
      industrie <- data() %>% filter(group_name_mixed == "Industrie") 
      train_idx <- createDataPartition(industrie$Financialrating,p=0.8,list=FALSE)
      training <- industrie[train_idx,]
      test <- industrie[-train_idx,]
      rep_form <- paste(input$response, "~ ", sep = " ")
      cov_form <- paste(paste0("s", parenthesise(input$covariate)), collapse = "+")
      formula <- paste(rep_form, cov_form)
      ind_gam <- gam(as.formula(formula),
                     data = training)
      temp_ind <- predict.gam(ind_gam, newdata = test,
                              type = "response")


      table <- data.frame(True_Financialrating_conseildroit = test_con$Financialrating[1:500], 
                          Predict_Financialrating_conseildroit = temp_con[1:500], 
                          True_Financialrating_industrie = test$Financialrating[1:500], 
                          Predict_Financialrating_industrie = temp_ind[1:500])
      

    }
    table
  })
  
  
  output$pred <- renderTable({
    data.frame(pred = table()[1:20,])
  })
  
  observe(({
    input$SaveDatabuttonpredict
    write.table(table(), file = "Predict.csv", sep = ",")
    write.csv(table(), "Predict.csv")
  }))
  
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
      else if (input$family == "binomial") {
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
}
