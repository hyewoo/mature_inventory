## UI
# Use a fluid Bootstrap layout
ui <- dashboardPage(
  
  title = "Mature Inventory Assessment", # Browser title
  
  dashboardHeader(
    tags$li(a(href = 'https://gov.bc.ca',
              img(src = 'logo-banner.png',
                  title = "Home", height = "41px"),
              style = "padding-top:10px; padding-bottom:10px;
                            background-color: #036; margin-right: 10px;"),
            class = "dropdown"),
    title = div('Mature Inventory Assessment', style = "color: white; font-weight: bold; font-size: 24px;
                              font-family: 'BCSans', 'Noto Sans', Verdana, Arial, sans-serif;
                              padding-top:10px;")
    #titleWidth='100%',
    #title = span(
    #  tags$img(src="logo-banner.png", title = "Home", height = "41px"), 
    #  column(12, class="title-box", 
    #         tags$h1(class="primary-title", style="color: white; font-weight: bold; font-size: 24px;
    #                          font-family: 'BCSans', 'Noto Sans', Verdana, Arial, sans-serif;
    #                          padding-top:10px;", 'YSM Technical Report')
    #         #tags$h2(class="primary-subtitle", style='margin-top:10px;', 'EXPERT ELICITATION FOR ADAPTIVE MANAGEMENT')
    #  )
    #              )
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    
    tags$head(tags$style("body{min-height: 800px;  height: auto;  max-width: 1296px;  margin: auto;
                         background-color: #b3b1b3}")),
    
    fluidPage(    
      
      waiter::use_waiter(),
      #waiter::waiter_show_on_load(html = waiter_html("Loading App")),
      
      # BC gov custom css
      includeCSS("bcgov2.css"),
      
      # Overwrite shinydashboard color
      tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {background-color: #036; width: 800px;}  
        .skin-blue .main-header .logo:hover {background-color: #036;}
        .skin-blue .main-header .navbar {background-color: #036; margin-left: 100px;}
        
        .well {background-color: #fff;  border: 1px solid #5a7dab; border-radius: 4px;
        -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
        box-shadow: inset 0 1px 1px rgba(0,0,0,.05);}
        
        /*.nav-tabs>li>a {font-family: "BCSans", "Noto Sans", Verdana, Arial, sans-serif; color:#036;}
        .nav-tabs>li.active>a {font-family: "BCSans", "Noto Sans", Verdana, Arial, sans-serif; color:#036;}*/
        
        /*.navbar{color: #036;}
        .navbar-default .navbar-brand {color: #cc3f3f;}*/
        
        .content-wrapper, .right-side {background-color: #FFFFFF;}
        
        /*.footer {
             border-top: 2px solid #fcba19;
             color: #fff;
             position: absolute;
             font-family: ‘BCSans’, ‘Noto Sans’, Verdana, Arial, sans-serif; 
             bottom: 0;
             width: 100%;
             height: 60px; 
             background-color: #036;}*/
             
        .shiny-options-group { 
          /*height: 100px;*/
          width: 600px;
          -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
            -moz-column-count: 2;    /* Firefox */ 
            
            row-count: 2;
          -webkit-column-fill: auto;
          -moz-column-fill: auto;
          column-fill: auto;
          margin-top: 0px;
        } 
        
        .control-label {
          padding-bottom: 5px;
        }
        
        div.radio {
          margin-top: 5px;
          margin-bottom: 0px;
          padding-bottom: 5px;
        }
      '))),
      
      box(title ="Note: This site is currently under development.", 
          solidHeader = T, collapsible = T,status = "warning", width = NULL, collapsed=TRUE
          #p(strong("*Note that this site is currently under development."),style = "color:red"),
          ),
      
      box(title = "Select the area of interest", #background = "light-blue", 
          solidHeader = TRUE, status = "primary", width = NULL,
          
          column(3, offset = 1, selectInput(inputId = "SelectVar",
                                            label = "Select",
                                            choices = c(Choose = "", tsa_list)), 
                 HTML("<font size='-1'>*only n&ge;10 are selectable.</font>")),
          
          column(3, offset = 1, downloadButton("downloadReport", "Download report"), br(),
                 radioButtons("format", "Document format", c("HTML", "PDF"), inline = TRUE))
          
      ), # box
      
      column(12, 
             
             navlistPanel(
               
               #tags$style("t{color:blue;}"), 
               
               "Overview",
               tabPanel(title = "Overview",
                        #hr(),
                        uiOutput('overview_header'),
                        uiOutput("description"),
                        br(),
                        leafletOutput("samplemap"),
                        #br(),
                        #uiOutput("overviewflex"),
                        br()
               ),
               
               tabPanel(title = "Summary of Key Results",
                        h4("Sample Size & Measurement Year by Ground Sample Design"),
                        uiOutput("samplesize"),
                        fluidRow(
                          column(6,
                                 h4("Leading Species Agreement (Inventory vs. Ground)"),
                                 uiOutput("spcagree1"),
                                 br()),
                          column(6,
                                 h4("Overall Species Agreement (Inventory vs. Ground)"),
                                 uiOutput("spcagree2"),
                                 br())
                        ),
                        
                        h4("Listing of those Attributes where Ground: Inventory ratio of means are practically different (Y) or not practically different (N) from 1.0. Attributes which are not listed here have inconclusive (I) results."),
                                                fluidRow(
                          column(4,
                                 uiOutput("test1"),
                                 br()),
                          column(4,
                                 uiOutput("test2"),
                                 br()),
                          column(4,
                                 uiOutput("test3"),
                                 br())
                        ),
                        br(),
                        plotOutput("fig2", width = "300px")
                        #h3("Leading Species Agreement (Inventory vs. Ground)"),
                        #leafletOutput("spcagree1"),
                        #h3("Overall Species Agreement (Inventory vs. Ground)"),
                        #leafletOutput("spcagree2"),
               ),
               
               "Descriptive Statistics",
               tabPanel(title = "Stand Summaries",
                        uiOutput("description_text"),
                        br(),
                        uiOutput("table1"),
                        br(),
                        uiOutput("table2"),
                        br(),
                        uiOutput("table3"),
                        br(),
                        uiOutput("table4"),
                        br(),
               ),
               
               "Components of Bias",
               tabPanel(title = "Model vs Attribute Bias",
                        
                        uiOutput("bias_comp"),
                        br(),
                        plotOutput("fig3", width = "800px", height = "300px"),
                        fluidRow(
                          column(6,
                                 uiOutput("fig3_1")),
                          column(6,
                                 uiOutput("fig3_2"))
                        )
               ),
               
               "Species Comparisons",
               tabPanel(title = "Leading Species",
                        uiOutput("spcomp_text"),
                        br(),
                        uiOutput("table5a"),
                        br(),
                        uiOutput("table5b"),
                        br(),
               ),
               tabPanel(title = "Overall Species",
                        
                        plotOutput("fig4", width = "800px", height = "400px"),
                        br(),
                        plotOutput("fig5", width = "800px", height = "400px")
               ),
               
               "Other Attributes",
               tabPanel(title = "Ground vs. Inventory",
                        uiOutput("scatter_text"),
                        br(),
                        plotOutput("fig6", width = "800px", height = "1000px"),
                        br(),
               ),
               
               "Inventory Standards of Data Collection",
               tabPanel(title = "Inventory Standard Code",
                        uiOutput("stdcode_text"),
                        br(),
                        plotOutput("fig7", width = "600px", height = "200px"),
                        br(),
               ),
               
               "Ground Samples Impacted by Recent Wildfires",
               tabPanel(title = "Impact of 2017, 2018, and 2021 wildfires",
                        uiOutput("fire_text"),
                        br(),
                        plotOutput("fig8", width = "600px"),
                        br()
               ),
               
               "General Notes",
               tabPanel(title = "Disclaimer",
                        uiOutput('disclaimer'),
                        
               ),
               tabPanel(title = "Reference",
                        h4("Reference for Analyses of Past VRI Phase II / VPIP Projects"),
                        uiOutput("ref")
               ),
               
             ),  # navlistPanel 
             br(),
             
      ), # navlist column
      br(),
      
    ), #fluidPage
    
    br(),
    br(),
    br(),
    div(class = "footer",
        includeHTML("footer.html")
    )
  ) # dashboardBody
  
) # dashboardPage