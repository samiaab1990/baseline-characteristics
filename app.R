#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyjs)
library(dplyr)
library(gtsummary)
library(gt)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Use shinyjs
  shinyjs::useShinyjs(),
  
  # Stylesheets 
  tags$style("body {font-family: 'Roboto', sans-serif;}"),
  
  # Application title
  tags$head(HTML("<h1><b>Baseline Characteristics Table Generator</b></h1>")),
  
  # Initial app tags plus description
  tags$h2(HTML("<b>Upload Dataset</b>")),
  tags$p(HTML("Upload your dataset here. Make ensure that your dataset is <b>formatted so that each cell corresponds to one row and one column</b> (no nested columns) and is <b>saved as a .csv or .xlsx file</b>. <b>Please de-identify data</b>, <b> filter to only columns needed for the baseline characteristics </b> and <b>ensure there are no password protections on your file</b>. If your data contains sensitive information or a rare situation which can make a patient easily identifiable, please use an alternative method for generating baseline characteristics.")), 
  fileInput("upload", NULL, buttonLabel = "Upload File 📁", accept=c(".csv",".xlsx")),

  # Data preview
  # hide until file is uploaded
  hidden(tags$h2(HTML("<b>Preview Data</b>"),id="preview_data")),
  hidden(numericInput("n", "Show Number of Rows", value = 5, min = 1, step = 1)),
  tableOutput("head"),
  
  # Table specs
  # hide until file is uploaded 
  hidden(tags$h2(HTML("<b>Table Specifications</b>"), id="specs_title")),
  
  # covariates and group, will show when rendered
  ui<-fluidPage(
    uiOutput("covariates")
  ),
  
  ui<-fluidPage(
    uiOutput("group")
  ),
  
  # test input
  ui<-fluidPage(
    hidden(selectInput(
      "test",
      "Test Type (for continuous data)",
      choices = c("wilcox.test","t.test"),
      selected = "wilcox.test",
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  )),
  
  hidden(actionButton("generate", "Click to Generate Table")),

  
  gt_output("baseline_characteristics"),
  

  hidden(downloadButton("download"))
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           xlsx = readxl::read_xlsx(input$upload$datapath),
           validate("Invalid file; Please upload a .csv or .xlsx file")
    )
    
  })
  

  observeEvent(input$upload, 
    {
      if(length(data()) != 0)
      {
        show("preview_data")
        show("n")
        show("specs_title")
        show("test")
        show("generate")
      }
                 
    })
  
  output$head <- renderTable({
    head(data(), input$n)})
      
      output$covariates<-renderUI({
        selectInput(
          "characteristics",
          "Covariates",
          choices = colnames(data()),
          selected = NULL,
          multiple = TRUE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        )})
      
      output$group<-renderUI({
        selectInput(
          "group",
          "Group",
          choices = colnames(data()),
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        )}
      )
      
      
      final_table<-eventReactive(
        input$generate,
        {
          req(input$characteristics)
          req(input$group)
          
          data() %>% 
            tbl_summary(
              include = input$characteristics,
              by = input$group,
              type = list(where(is.numeric) ~ "continuous2",
                          all_dichotomous() ~ "categorical"),
              statistic = 
               where(is.numeric) ~ c("{mean} ({sd})", "{median} ({p25}, {p75})"),
              missing_text = "Missing", 
            ) %>%
            add_overall(last=TRUE) %>%
            add_p(
              test = list(where(is.numeric) ~ input$test) 
            ) %>%
            bold_labels()
          
          
        })
      
      output$baseline_characteristics<-gt::render_gt(
        final_table() %>% as_gt())
      
      observeEvent(input$generate,
                   
      {
        if(length(final_table()) != 0)
          show("download")
      })
      
      output$download<-
        downloadHandler(
          
          filename = function()
            {"baseline_characteristics.docx"},
          
          content = function(file)
            gt::gtsave(final_table() %>% as_gt(),file)
          
        )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

