
fire_text <- reactive({
  
  fire_text <- paste0("<p>Recent wildfire activity has impacted a number of 
                      ground samples across some management units. All 
                      design-based ground samples were revisited following 
                      three notable fire seasons (2017, 2018, 2021) for 
                      live/dead tree status and their mortality impacts were 
                      computed. This ensures consistency when comparing ground 
                      measurements against inventory attributes, since the 
                      published VRI also incorporates attribute adjustments by 
                      burn severity class for these recent wildfires.</p>")
  
  return(fire_text)
})

output$fire_text <- renderUI({
  HTML(fire_text())
})



firesample <- reactive({
  req(input$SelectVar)
  
  firesample <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    filter(!is.na(fire_year)) %>%
    select(fire_year, ba_mortality) %>%
    mutate(fire_year_f = factor(fire_year, 
                                levels = c(2017, 2018, 2021)),
           ba_mortality_f = ifelse(ba_mortality < 0.25, "0-25", ifelse(
             ba_mortality < 0.5, "25-50", ifelse(ba_mortality < 0.75, "50-75", "75-100")
           ))) %>%
    mutate(ba_mortality_f = factor(ba_mortality_f, 
                                   levels = c("75-100", "50-75", "25-50", "0-25")))
  
  return(firesample)
})


fig8 <- reactive({
  
  firesample <- firesample()
  
  fig8 <- if(nrow(firesample) > 0){
    
    ggplot(firesample) +
      geom_bar(aes(fill=ba_mortality_f, x=fire_year_f), width = 0.7, show.legend=TRUE) +
      scale_fill_manual(name = "%BA \nMortality", 
                        values = c(hcl.colors(4, palette = "YlOrRd")), drop = F) +
      scale_y_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_x_discrete(drop = F) +
      labs(x = "Wildfire Season", y = "# Ground Samples",
           title = "Ground Samples Impacted by Recent Wildfires") +
      theme(rect = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    
    
  } else {
    ggplot() + 
      theme_void() +
      geom_text(aes(0,0,label='No sample impacted by fire')) +
      xlab(NULL)
  }
  
  return(fig8)
})



output$fig8 <- renderPlot({
  
  fig8()
  
})
