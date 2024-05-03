shinyUI(
  navbarPage("CLEFT-Q CAT Score Checker",
    tabPanel(
      "Radar Plot",
      fluidPage(
        titlePanel("Radar Plot"),
        # Text
        br(),
        h4("Please use this Score Checker to compare CLEFT-Q CAT scores to median scores
     from the CLEFT-Q field test (Klassen", em("et al.,"), "2018). The intended use of the Score Checker is to
     aid in the visualisation and interpretation of this dataset. It is not a registered medical device and should not be used to influence treatment decisions."),
        fluidRow(
          plotOutput(outputId = "RadarPlot") %>% withSpinner(color = "darkmagenta")
        ),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        fluidRow(
          column(
            3,
            selectInput(
              inputId = "age_radar",
              label = "Age range (years)",
              choices = c("All ages (8-29)", "8-11", "12-15", "16-19", "20+")
            ),
            selectInput(
              inputId = "Sex_radar",
              label = "Gender",
              choices = c("All genders", "Male", "Female")
            ),
            selectInput(
              inputId = "Cleft_type_radar",
              label = "Cleft type",
              choices = c(
                "All cleft types", "Cleft lip", "Cleft palate",
                "Cleft lip and alveolus",
                "Cleft lip, alveolus and palate"
              )
            ),
            selectInput(
              inputId = "Laterality_radar",
              label = "Laterality",
              choices = c("Unilateral or Bilateral", "Unilateral", "Bilateral")
            )
          ),
          column(3,
            numericInput(
              inputId = "facescore",
              label = "Face Score",
              value = 50,
              min = 1,
              max = 100
            ),
            numericInput(
              inputId = "nosescore",
              label = "Nose Score",
              value = 50,
              min = 1,
              max = 100
            ),
            numericInput(
              inputId = "nostrilscore",
              label = "Nostrils Score",
              value = 50,
              min = 1,
              max = 100
            ),
            numericInput(
              inputId = "jawscore",
              label = "Jaws Score",
              value = 50,
              min = 1,
              max = 100
            )
          ),
          column(3,
            numericInput(
              inputId = "lipscore",
              label = "Lips Score",
              value = 50,
              min = 1,
              max = 100
            ),
            numericInput(
              inputId = "teethscore",
              label = "Teeth Score",
              value = 50,
              min = 1,
              max = 100
            ),
            numericInput(
              inputId = "scarscore",
              label = "Scar Score",
              value = 50,
              min = 1,
              max = 100
            ),
            numericInput(
              inputId = "schoolscore",
              label = "School Score",
              value = 50,
              min = 1,
              max = 100
            )
          ),
          column(3,
            numericInput(
              inputId = "socialscore",
              label = "Social Function Score",
              value = 50,
              min = 1,
              max = 100
            ),
            numericInput(
              inputId = "psychscore",
              label = "Psychological Function Score",
              value = 50,
              min = 1,
              max = 100
            ),
            numericInput(
              inputId = "sfscore",
              label = "Speech Function Score",
              value = 50,
              min = 1,
              max = 100
            ),
            numericInput(
              inputId = "sdscore",
              label = "Speech Distress Score",
              value = 50,
              min = 1,
              max = 100
            )
          )
        )
      )
    ),
    tabPanel(
      "Population Density",
      fluidPage(

        # Application title
        titlePanel("Population Density"),

        # Text
        br(),
        h4("Please use this Score Checker to compare a CLEFT-Q CAT score to scores
     obtained in the CLEFT-Q field test (Klassen", em("et al.,"), "2018). The intended use of the Score Checker is to
     aid in the visualisation and interpretation of this dataset. It is not a registered medical device and should not be used to influence treatment decisions."),
        br(),
        br(),

        # Sidebar
        sidebarLayout(
          sidebarPanel(
            numericInput(
              inputId = "score",
              label = "Score",
              value = 50,
              min = 1,
              max = 100
            ),
            selectInput(
              inputId = "scale",
              label = "CLEFT-Q CAT scale",
              choices = c(
                "Face", "Nose", "Nostrils", "Jaws", "Teeth",
                "Lips", "Scar", "School", "Speech function",
                "Speech distress", "Psychological function",
                "Social function"
              )
            ),
            selectInput(
              inputId = "age",
              label = "Age range (years)",
              choices = c("All ages (8-29)", "8-11", "12-15", "16-19", "20+")
            ),
            selectInput(
              inputId = "Sex",
              label = "Gender",
              choices = c("All genders", "Male", "Female")
            ),
            selectInput(
              inputId = "Cleft_type",
              label = "Cleft type",
              choices = c(
                "All cleft types", "Cleft lip", "Cleft palate",
                "Cleft lip and alveolus",
                "Cleft lip, alveolus and palate"
              )
            ),
            selectInput(
              inputId = "Laterality",
              label = "Laterality",
              choices = c("Unilateral or Bilateral", "Unilateral", "Bilateral")
            )
          ),
          # Show a plot of the generated distribution
          mainPanel(
            plotOutput("Plot") %>% withSpinner(color = "darkmagenta"),
            div(textOutput("Text"), style = "font-size:20px;")
          )
        )
      )
    ),
    footer = tags$footer(column(12,
                                p("Klassen, A. F. et al. (2018) ‘Psychometric findings and normative values for the CLEFT-Q
        based on 2434 children and young adult patients with cleft lip and/or palate from 12 countries’,",
        em("CMAJ"), ". doi: 10.1503/cmaj.170289.",
        style = "font-size:13px;"
                                ),
        br(),
        p("The CLEFT-Q CAT Score Checker has been developed by Conrad Harrison at the University of Oxford.
              This represents independent research funded by the NIHR.
              The views expressed are those of the author and not necessarily those of
              the University of Oxford, the NHS, the NIHR or the Department of Health and Social Care.
              The CLEFT-Q CAT Score Checker is provided with absolutely no warranty. The author, the University of Oxford
              and the NIHR disclaim any and all express and implied warranties including without limitation the
              implied warranties of title, fitness for a particular
              purpose, merchantability and noninfringement. The CLEFT-Q Score Checker is
              licensed under BSD_3_clause + file LICENSE. To obtain a copy of the license please",
          a(href = "https://github.com/MrConradHarrison/CLEFT-Q-CAT-Score-Checker/blob/main/LICENSE", "click here"), "or contact the author on conrad.harrison@medsci.ox.ac.uk.",
          style = "font-size:11px;")
        ))
  )
)
