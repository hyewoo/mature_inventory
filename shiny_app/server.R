###############################################
#
# Define server for the Shiny app
#
##############################################
server <- function(input, output, session) {
  
  
  
  
  # Call reactive values and data
  source(file.path("serverscripts/react.R"), local = TRUE) 
  
  # Source files with server code for each tab -----------------------------------------
  source(file.path("serverscripts/overview.R"), local = TRUE)$value 
  source(file.path("serverscripts/stat.R"), local = TRUE)$value 
  #source(file.path("server/lsServer.R"), local = TRUE)$value 
  #source(file.path("server/residServer.R"), local = TRUE)$value 

  #output$downloadReport <- downloadHandler(
  #  filename = function() {
  #    paste0('YSM_report_', title(), "_", Sys.Date(), switch(
  #      input$format, PDF = '.pdf', HTML = '.html')
  #    )
  #  },
  #  
  #  content = function(file) {
  #    src <- normalizePath('report.Rmd')
  #    # temporarily switch to the temp dir, in case you do not have write
  #    # permission to the current working directory
  #    owd <- setwd(tempdir())
  #    on.exit(setwd(owd))
  #    file.copy(src, 'report.Rmd', overwrite = TRUE)
  #    
  #    ## Create a Progress object
  #    #progress <- shiny::Progress$new()
  #    ## Make sure it closes when we exit this reactive, even if there's an error
  #    #on.exit(progress$close())
  #    #progress$set(message = "Creating report", value = 10)
  #    
  #    out <- rmarkdown::render('report.Rmd', switch(
  #      input$format,
  #      HTML = rmarkdown::html_document(),
  #      PDF = rmarkdown::pdf_document()
  #      #PDF = rmarkdown::pandoc_convert(rmarkdown::render('report.Rmd', "html_document"), output = 'report.pdf')
  #    ))
  #    
  #    file.rename(out, file)
  #  }
  #)
  
  
}

## END 