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
    useShinyjs(), 
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
        
        .navbar-brand {color:#38598a;}
        
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
        
        a:hover {
        color: #4b5e7e !important;
        }
        
        footer ul li a:hover {
        color: #FFFFFF !important;
        }
        
        h5 {
        font-family: Arial, sans-serif;
        font-weight: bold;  /* 400 */
        color: black;
        }
      '))),
      
      box(title ="Note: This site is currently under development.", 
          "The information provided is preliminary and subject to change.",
          solidHeader = T, collapsible = T,status = "warning", width = NULL, collapsed=TRUE
          ),
      
      box(title = "Select the area of interest", 
          solidHeader = TRUE, status = "primary", width = NULL,
          
          column(4, #offset = 0, 
                 #list(tags$div(align = 'left', 
                 #              class = 'multicol', 
                 #              radioButtons("SelectCategory", "Strata",
                 #                           choiceNames = c("By TSA", "By BEC", "By TFL"),
                 #                           choiceValues = c("TSA_DESC", "BEC_ZONE", "TFL"),
                 #                           #choices = list("By TSA" = "TSA_DESC", 
                 #                           #               "By BEC" = "BEC_ZONE",
                 #                           #               "By TFL" = "TFL"), 
                 #                           selected = "TSA_DESC"),
                 #              style = "font-size:100%", align = "center"))
                 
                 column(5, offset = 0, 
                        HTML('<div align="left" class="multicol" style="font-size:100%; text-align:left;">
  <label for="SelectCategory">Strata</label><br>
  <div style="line-height:1; margin-bottom:1px;">
  <input type="radio" id="tsa" name="SelectCategory" value="TSA_DESC" checked>
  <label for="tsa" style="font-weight:normal;">By TSA</label><br>
  
  <input type="radio" id="bec" name="SelectCategory" value="BEC_ZONE" disabled>
  <label for="bec" style="font-weight:normal;">By BEC zone</label><br>
  
  <input type="radio" id="flp" name="SelectCategory" value="FLP" disabled >
  <label for="flp" style="font-weight:normal;">By FLP</label>
  </div>  
</div>'
                        )
                 ),
                 
                 column(7, offset = 0, 
                        div(
                          style = "margin-top: 25px;",
                          checkboxInput(inputId = "nonVT",
                                        label = HTML("<font size='-1'>Include non-treed samples</font>
                                                     <i class='glyphicon glyphicon-info-sign' 
            style='color:#494949;' 
            title='Uncheck for TSR'></i>"),
                                        value = FALSE),
                        )
                 )
          ), 
          
          
          column(3, offset = 0, selectInput(inputId = "SelectVar",
                                            label = "Select",
                                            choices = c(Choose = "", tsa_list)), 
                 HTML("<font size='-1'>*only n&ge;8 are selectable.</font>")),
          
          column(2, offset = 1, downloadButton("downloadReport", "Download report"), 
                 br(),
                 radioButtons("format", #label = tags$span(
                   "Document format", 
                   #tags$i(
                   #  class = "glyphicon glyphicon-info-sign", 
                   #  style = "color:#494949;",
                   #  title = "Optimized for HTML"
                   #)
                   #), 
                   c("HTML"), inline = TRUE))
                 
          ), # box
      
      column(12, 
             
             navlistPanel(
               id = "demo",
               #tags$style("t{color:blue;}"), 
               
               "Overview",
               tabPanel(title = "Overview",
                        #hr(),
                        uiOutput('overview_header'),
                        uiOutput("description"),
                        br(),
                        uiOutput("samplemap_caption"),
                        withSpinner(leafletOutput("samplemap", height = "600px")),
                        br(),
                        uiOutput("samplenum"),
                        br(),
                        plotOutput("bec_dist", height = "350px"),
                        br()
               ),
               
               tabPanel(title = "Summary of Key Results",
                        br(),
                        withSpinner(uiOutput("samplesize")),
                        br(),
                        fluidRow(
                          column(6,
                                 uiOutput("spcagree1"),
                                 br()),
                          column(6,
                                 uiOutput("spcagree2"),
                                 br())
                        ),
                        
                        plotOutput("fig2", height = "350px"),
                        uiOutput("fig2_desc"),
                        
                        br(),
                        uiOutput("test_caption"),
                        uiOutput("test1"),
                        br(),
                        uiOutput("test2"),
                        br(),
                        uiOutput("test3"),
                        
                        uiOutput("rope_desc"),
                        br(),
               ),
               
               "Ground vs. Inventory",
               tabPanel(title = "Stand Summaries",
                        uiOutput("description_text"),
                        br(),
                        withSpinner(uiOutput("table1")),
                        br(),
                        uiOutput("table2"),
                        br(),
                        uiOutput("table3"),
                        br(),
                        uiOutput("table4"),
                        br(),
               ),
               tabPanel(title = "Species Composition",
                        withSpinner(uiOutput("spcomp_text")),
                        br(),
                        uiOutput("table5a"),
                        br(),
                        uiOutput("table5b"),
                        br(),
                        div(withSpinner(plotOutput("fig4", width = "800px", height = "400px")), align = "center"),
                        uiOutput("fig4_flex"),
                        uiOutput("fig4_caption"),
                        br(),
                        plotOutput("fig5", width = "800px", height = "400px"),
                        uiOutput("fig5_caption"),
                        br(),
                        plotOutput("stock_table", width = "800px", height = "400px"),
                        br(),
               ),
               #tabPanel(title = "Overall Species",
               #         
               #         div(withSpinner(plotOutput("fig4", width = "800px", height = "400px")), align = "center"),
               #         uiOutput("fig4_flex"),
               #         uiOutput("fig4_caption"),
               #         br(),
               #         plotOutput("fig5", width = "800px", height = "400px"),
               #         uiOutput("fig5_caption"),
               #         br()
               #),
               tabPanel(title = "Age, BA, Height, and Volume",
                        actionLink("link_to_rope", "For ROPE test results, go to Table 3."),
                        #uiOutput("scatter_text"),
                        br(),
                        div(withSpinner(plotOutput("fig6", width = "800px", height = "1200px")), align = "center"),
                        uiOutput("fig6_caption"),
                        br(),
               ),
               
               "Comparison with TSR",
               tabPanel(title = "Current Volumes",
                        h3("Comparing Current Volumes: TSR Predicted Yield Tables vs. GRID Actual Measurements"),
                        uiOutput("curvoltext"),
                        br(),
                        div(plotOutput("maturetsr", width = "700px"),
                            br(),
                            withSpinner(plotOutput("vol_bias", width = "700px")), align = "center"),
                        br(),
               ),
               tabPanel(title = "Periodic Annual Increment",
                        h3("Test to Compare TSR vs. Re-measured GRID Sample Periodic Annual Increment"),
                        uiOutput("paitext"),
                        br(),
                        fluidRow(
                          column(6,
                                 uiOutput("tsr_pai_flex1"),
                                 br()),
                          column(6,
                                 uiOutput("tsr_pai_flex2"),
                                 br())
                        ),
                        br(),
                        div(withSpinner(plotOutput("pai_diff", height = "200px", width = "400px")), align = "center"),
                        br(),
               ),  
               
               
               
               
               "Standards of Data Collection",
               tabPanel(title = "VRI Standard",
                        uiOutput("stdcode_text"),
                        br(),
                        div(withSpinner(plotOutput("fig7", width = "600px", height = "200px")), align = "center"),
                        br(),
                        uiOutput("invyear_flex"),
                        br(),
                        uiOutput("invyear_text"),
                        br(),
               ),
               
               "Wildfire Impact",
               tabPanel(title = "Ground Samples Impacted by Recent Wildfires",
                        uiOutput("fire_text"),
                        br(),
                        div(plotOutput("fig8", width = "500px", height = "300px"), align = "center"),
                        br(),
                        withSpinner(leafletOutput("firemap")),
                        br()
               ),
               
               "Forest Health",
               tabPanel(title = "Growth and Mortality",
                        uiOutput("coctext"),
                        br(),
                        div(withSpinner(plotOutput("cocfig", width = "600px")), align = "center"),
                        br(),
                        #leafletOutput("firemap"),
                        #br()
               ),
               tabPanel(title = "Current Forest Health Incidence",
                        uiOutput("health_inci"),
                        br(),
                        div(withSpinner(plotOutput("curr_fh_inci")),
                        br(),
                        plotOutput("sevplot", width = "500px", height = "300px"), align = "center"),
                        br()
               ),
               tabPanel(title = "Change in Forest Health Incidence",
                        uiOutput("fhcoctext1"),
                        br(),
                        div(withSpinner(plotOutput("cocfhplot")), align = "center"),
                        br(),
                        uiOutput("fhcoctext2"),
                        br(),
                        uiOutput("fhcocflex"),
                        br()
               ),
               
               "Comparison with VDYP",
               tabPanel(title = "Current Volume and PAI",
                        uiOutput("growth_text"),
                        br(),
                        div(withSpinner(plotOutput("fig7_5", width = "600px")), align = "center"),
                        br(),
                        uiOutput("pai_text"),
                        br(),
                        fluidRow(
                          column(6,
                                 uiOutput("pai_table1")),
                          column(6,
                                 uiOutput("pai_table2"))
                        ),
                        br(),
                        div(plotOutput("paidiff", height = "200px", width = "400px"), align = "center"),
                        br()
               ),
               
               "Components of Bias",
               tabPanel(title = "Model vs Attribute Bias",
                        uiOutput("bias_comp"),
                        #br(),
                        div(withSpinner(plotOutput("fig3", width = "800px", height = "300px")), align = "center"),
                        fluidRow(
                          column(6,
                                 uiOutput("fig3_1")),
                          column(6,
                                 uiOutput("fig3_2"))
                        ),
                        uiOutput("fig3_caption"),
                        br(),
               ),
               
               "General Notes",
               tabPanel(title = "Disclaimer",
                        uiOutput('disclaimer'),
                        br()
               ),
               tabPanel(title = "Tree Species and Damage Agents",
                        uiOutput('sp_dam_header'),
                        fluidRow(
                          column(width = 6,
                                 h4("Tree Species Codes / Names"),
                                 DT::dataTableOutput("sp_table")),
                          column(width = 6,
                                 h4("Damage Agent Codes / Names"),
                                 DT::dataTableOutput('dam_table'))
                        ),
                        br(),
                        br(),
                        uiOutput('damsevtable'),
                        br()
               ),
               tabPanel(title = "Reference",
                        h4("Reference for Analyses of Past VRI Phase II / VPIP Projects"),
                        #uiOutput("ref"),
                        dataTableOutput("ref"),
                        br()
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