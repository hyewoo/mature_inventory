###############################################
#
# Define server for the Shiny app
#
##############################################
server <- function(input, output, session) {
  
  observeEvent(input$link_to_rope, {
    updateTabsetPanel(session, "demo", "Stand Summaries")
  })
  observeEvent(input$link_to_sp, {
    updateTabsetPanel(session, "demo", "Species Composition")
  })
  observeEvent(input$link_to_gn, {
    updateTabsetPanel(session, "demo", "Tree Species and Damage Agents")
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
      
        out <- rmarkdown::render('report.Rmd', output_format = "html_document"
                                 #switch(
                                 #  input$format,
                                 #  HTML = rmarkdown::html_document(),
                                 #  PDF = rmarkdown::pdf_document()
                                 #  #PDF = rmarkdown::pandoc_convert(rmarkdown::render('report.Rmd', "html_document"), output = 'report.pdf')
                                 #)
                                 )
      })
      file.rename(out, file)
    }
  )
  
  
}

## END 