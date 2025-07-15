###############################################
#
# Define server for the Shiny app
#
##############################################
server <- function(input, output, session) {
  
  #disable(selector = "#somevalue button:eq(1)")
  runjs("$(\"input[name='SelectCategory'][value='BEC_ZONE']\").parent().attr('disabled', true);")
  #shinyjs::runjs("$(\"input[name='SelectCategory'][value='BEC_ZONE']\").parents('button').prop('disabled', true);")
  #disable(selector = "#SelectCategory button:eq(1)")
  #observeEvent(input$SelectCategory, {
  #  shinyjs::disable("text")  # toggle is a shinyjs function
  #})
  #if(TRUE) {
  #updateRadioButtons(
  #  session = session, 
  #  inputId = "SelectCategory",
  #  disabledChoices = c("BEC_ZONE", "TFL")
  #)
  #}
  observeEvent(input$link_to_rope, {
    updateTabsetPanel(session, "demo", "Stand Summaries")
  })
  # Call reactive values and data
  source(file.path("serverscripts/react.R"), local = TRUE) 
  
  # Source files with server code for each tab -----------------------------------------
  source(file.path("serverscripts/overview.R"), local = TRUE)$value 
  source(file.path("serverscripts/stat.R"), local = TRUE)$value 
  source(file.path("serverscripts/leadingsp.R"), local = TRUE)$value 
  source(file.path("serverscripts/fireimpact.R"), local = TRUE)$value 
  source(file.path("serverscripts/gn.R"), local = TRUE)$value 
  source(file.path("serverscripts/foresthealth.R"), local = TRUE)$value 
  source(file.path("serverscripts/prj.R"), local = TRUE)$value 

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0('Mature_Inventory_', title(), "_", Sys.Date(), switch(
        input$format, PDF = '.pdf', HTML = '.html')
      )
    },
    
    content = function(file) {
      withProgress(message = 'Exporting report..', {
      src <- normalizePath('report.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      ## Create a Progress object
      #progress <- shiny::Progress$new()
      ## Make sure it closes when we exit this reactive, even if there's an error
      #on.exit(progress$close())
      #progress$set(message = "Creating report", value = 10)
      
      out <- rmarkdown::render('report.Rmd', switch(
        input$format,
        HTML = rmarkdown::html_document(),
        PDF = rmarkdown::pdf_document()
        #PDF = rmarkdown::pandoc_convert(rmarkdown::render('report.Rmd', "html_document"), output = 'report.pdf')
      ))
      })
      file.rename(out, file)
    }
  )
  
  
}

## END 