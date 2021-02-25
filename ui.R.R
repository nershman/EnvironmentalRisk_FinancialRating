ui <- fluidPage(
  titlePanel("Statistical Consulting"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
    selectInput("response", "Response Variable",
                choices = c("Financial Rating",
                            "Qualitative Rating "), 
                selected = "Financial Rating", selectize = TRUE),
    checkboxGroupInput("covariate", "Choose Covariates:",
                c("Qualitative rating about transparency", 
                  "Qualitative rating about shareholder's contribution",
                  "Favourable economic market",
                  "Sector will increase?",
                  "Management Quality",
                  "Hold by a bigger firm",
                  "CEO Involved",
                  "Help from the group on legal",
                  "Assets",
                  "Liability",
                  "Turnover",
                  "EBITDA",
                  "Debt on equity",
                  "Gross Operating Surplus Global Costs",
                  "Gross Operating Surplus Turnover 100"),
                textOutput("number")
    )
    ),
    
  mainPanel(
    
  )
  )
)