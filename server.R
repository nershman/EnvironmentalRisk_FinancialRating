

server <- function(input, output, session) {
  
  data <- reactive({
    file1 <- input$file1
    if (is.null(file1)) { 
      return() 
    } 
    data <- read_excel(file1$datapath)
    data
  })
  
  observe({
    updateSelectInput(
      session,
      "response",
      choices = names(data()[-1])
    )
  })
  
  observe({
    updateCheckboxGroupInput(
      session,
      "covariate",
      choices = names(data()[-1])
    )
  })
  
  output$tbl <- DT::renderDataTable({
    DT::datatable(data())
  })
  
  
  output$cam <- renderPlot({
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
