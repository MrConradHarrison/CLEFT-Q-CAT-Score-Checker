shinyServer(function(input, output) {

  # Single score checker
  score_dist <- reactive({
    (data %>%
       filter(
         data$Scale == input$scale,
         data$Age %in% (if (input$age != "All ages (8-29)") {
           input$age
         } else {
           c("8-11", "12-15", "16-19", "20+", NA)
         }),
         data$Sex %in% (if (input$Sex != "All genders") {
           input$Sex
         } else {
           c("Male", "Female", NA)
         }),
         data$Cleft_type %in% (if (input$Cleft_type != "All cleft types") {
           input$Cleft_type
         } else {
           c(
             "Cleft lip", "Cleft palate",
             "Cleft lip and alveolus",
             "Cleft lip, alveolus and palate", NA
           )
         }),
         data$Laterality %in% (if (input$Laterality != "Unilateral or Bilateral") {
           input$Laterality
         } else {
           c("Unilateral", "Bilateral", NA)
         })
       ) %>%
       select(Score))
  })

  percentile <- reactive({
    getPercent(score_dist(), input$score) %>%
      round()
  })

  output$Plot <- renderPlot({
    makePlot(score_dist(), input$score, percentile())
  })

  output$Text <- renderText({
    glue::glue("A score of {input$score} on the CLEFT-Q {input$scale} scale is higher than {percentile()}% of scores reported by people with similar phenotypes, in the CLEFT-Q field test, based on a sample size of {nrow(score_dist())}.")
  })

  radardata <- reactive({
    data <- data %>%
      filter(
        data$Age %in% (if (input$age_radar != "All ages (8-29)") {
          input$age_radar
        } else {
          c("8-11", "12-15", "16-19", "20+", NA)
        }),
        data$Sex %in% (if (input$Sex_radar != "All genders") {
          input$Sex_radar
        } else {
          c("Male", "Female", NA)
        }),
        data$Cleft_type %in% (if (input$Cleft_type_radar != "All cleft types") {
          input$Cleft_type_radar
        } else {
          c(
            "Cleft lip", "Cleft palate",
            "Cleft lip and alveolus",
            "Cleft lip, alveolus and palate", NA
          )
        }),
        data$Laterality %in% (if (input$Laterality_radar != "Unilateral or Bilateral") {
          input$Laterality_radar
        } else {
          c("Unilateral", "Bilateral", NA)
        })
      )
  })

  scores_df <- reactive({tibble(
    Scale = c("Face", "Nose", "Nostrils", "Jaws", "Lips", "Teeth", "Scar", "School",
              "Social Function", "Psychological Function", "Speech function", "Speech distress"),
    score = c(input$facescore, input$nosescore, input$nostrilscore, input$jawscore,
              input$lipscore, input$teethscore, input$scarscore, input$schoolscore, input$socialscore,
              input$psychscore, input$sfscore, input$sdscore)
  )})

  output$RadarPlot <- renderPlot({
    radarPlot(
      data = radardata(),
      scores_df = scores_df()
    )}, height = 625, width = 800
  )
})
