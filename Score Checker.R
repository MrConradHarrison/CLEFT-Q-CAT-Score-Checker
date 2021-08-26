

# bugs to solve = kernel density is quite slow
# drop down boxes for second tab are not reactive, but they were when this was a stand-alone app



install.packages("shinycssloaders")
install.packages("fmsb")

library(shiny)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(fmsb)

data <- readRDS("data/data.RDS")


#### Functions ####

positionLabelx <- function(a) {
  if (a > 85){
    x = -5
  } else {
    x = 5
  }
  return(x)
}

makePlot <- function(score_distribution, score, percentile){
  
  
  ggplot(score_distribution) +
    geom_density(aes(x = Score), alpha = 0.1, colour = "darkmagenta", fill = "darkmagenta") +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16)) +
    ylab(glue::glue("Frequency (n = {nrow(score_distribution)})")) +
    geom_vline(aes(xintercept= score), color = "darkblue", linetype = "longdash") +
    geom_label(
      label=glue::glue("Percentile {percentile}"), 
      x=score + positionLabelx(score),
      y = 0.75*(max((density(score_distribution$Score)$y))),
      label.padding = unit(0.55, "lines"), # Rectangle size around label
      label.size = 0.35,
      color = "black",
      size = 5
    )
  
  
}



getPercent <- function (score_distribution, score) {
  getPercentile <- ecdf(score_distribution$Score)
  percentile <- getPercentile(score)*100
  return(percentile)
}



radarPlot <- function(data, facescore, nosescore, nostrilscore, jawscore, lipscore, teethscore,
                      scarscore, schoolscore, socialscore, psychscore, sfscore, sdscore) {
  
  dataframe <- data_frame(
    "Face" = facescore,
    "Nose" = nosescore,
    "Nostrils" = nostrilscore,
    "Jaws" = jawscore,
    "Lips" = lipscore,
    "Teeth" = teethscore,
    "Scar" = scarscore,
    "School" = schoolscore,
    "Social" = socialscore,
    "Psychological" = psychscore,
    "Speech function" = sfscore,
    "Speech distress" = sdscore
  )
  
  facedata <- data %>%
    filter(Scale == "Face")
  
  if (nrow(facedata) > 0){
    medianface <- median(facedata$Score)
  }else{
    medianface <- 0
  }
  
  nosedata <- data %>%
    filter(Scale == "Nose")
  
  if (nrow(nosedata) > 0){
    mediannose <- median(nosedata$Score)
  }else{
    mediannose <- 0
  }
  
  
  nostrilsdata <- data %>%
    filter(Scale == "Nostrils")
  
  if (nrow(nostrilsdata) > 0){
    mediannostrils <- median(nostrilsdata$Score)
  }else{
    mediannostrils <- 0
  }
  
  jawdata <- data %>%
    filter(Scale == "Jaws")
  if (nrow(jawdata) > 0){
    medianjaws <- median(jawdata$Score)
  }else{
    medianjaws <- 0
  }
  
  teethdata <- data %>%
    filter(Scale == "Teeth")
  if (nrow(teethdata) > 0){
    medianteeth <- median(teethdata$Score)
  }else{
    medianteeth <- 0
  }
  
  lipsdata <- data %>%
    filter(Scale == "Lips")
  
  if (nrow(lipsdata) > 0){
    medianlips <- median(lipsdata$Score)
  }else{
    medianlips <- 0
  }
  
  scardata <- data %>%
    filter(Scale == "Scar")
  if (nrow(scardata) > 0){
    medianscar <- median(scardata$Score)
  }else{
    medianscar <- 0
  }
  
  schooldata <- data %>%
    filter(Scale == "School")
  
  if (nrow(schooldata) > 0){
    medianschool <- median(schooldata$Score)
  }else{
    medianschool <- 0
  }
  
  sfdata <- data %>%
    filter(Scale == "Speech function")
  
  if (nrow(sfdata) > 0){
    mediansf <- median(sfdata$Score)
  }else{
    mediansf <- 0
  }
  
  sddata <- data %>%
    filter(Scale == "Speech distress")
  
  if (nrow(sddata) > 0){
    mediansd <- median(sddata$Score)
  }else{
    mediansd <- 0
  }
  
  psychdata <- data %>%
    filter(Scale == "Psychological Function")
  
  if (nrow(psychdata) > 0){
    medianpsych <- median(psychdata$Score)
  }else{
    medianpsych <- 0
  }
  
  socialdata <- data %>%
    filter(Scale == "Social Function")
  
  if (nrow(socialdata) > 0){
    mediansocial <- median(socialdata$Score)
  }else{
    mediansocial <- 0
  }
  
  averages <- c(medianface, 
                mediannose, 
                mediannostrils, 
                medianjaws, 
                medianlips,
                medianteeth, 
                medianscar,
                medianschool,
                mediansocial,
                medianpsych,
                mediansf,
                mediansd)
  
  dataframe <- rbind(rep(100,12), rep(0,12), averages, dataframe)
  
  
  radarchart(dataframe, axistype = 1, caxislabels = c("0", "25", "50", "75", "100"),maxmin = T,
             pcol=c("red", "darkmagenta"), pfcol = c(alpha("red",0.000001),alpha("darkmagenta",0.2)), plwd=2 , plty=c(2,1),
             #custom the grid
             cglcol="lightgrey", cglty=1, axislabcol="black", cglwd=0.8, 
             #custom labels
             vlcex=1.1 , xaxs="i"
  )
  
  
  legend(x=1, legend = c("Median scores", "Respondent"), y=-0.95, bty = "n", lty = c(2,1), lwd = 2 , col= c("red", "darkmagenta"), text.col = "black", cex=1.1, pt.cex=1)
  
}



#### User Interface 1 (Single Score Checker) ####

ui1 <- fluidPage(
  
  # Application title
  titlePanel("Population Density"),
  
  # Text
  br(),
  
  h4("Please use this Score Checker to compare a CLEFT-Q CAT score to scores 
     obtained in the CLEFT-Q field test (Klassen", em("et al.,"), "2018)."),
  br(),
  br(),
  
  
  # Sidebar  
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "score",
                   label = "Score",
                   value = 50,
                   min = 1,
                   max = 100
      ),
      
      selectInput(inputId = "scale",
                  label = "CLEFT-Q CAT scale",
                  choices = c("Face", "Nose", "Nostrils", "Jaws", "Teeth",
                              "Lips", "Scar", "School", "Speech function",
                              "Speech distress", "Psychological function",
                              "Social function")),
      
      selectInput(inputId = "age",
                  label = "Age range (years)",
                  choices = c("All ages (8-29)", "8-11", "12-15", "16-19", "20+")),
      
      selectInput(inputId = "Sex",
                  label = "Gender",
                  choices = c("All genders", "Male", "Female")),
      
      selectInput(inputId = "Cleft_type",
                  label = "Cleft type",
                  choices = c("All cleft types","Cleft lip", "Cleft palate",
                              "Cleft lip and alveolus",
                              "Cleft lip, alveolus and palate")),
      
      selectInput(inputId = "Laterality",
                  label = "Laterality",
                  choices = c("Unilateral or Bilateral","Unilateral", "Bilateral"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot") %>% withSpinner(color="darkmagenta"),
      div(textOutput("Text"), style = "font-size:20px;")
      
    )),
  
  
  fluidRow(column(12,
                  br(),
                  br(),
                  
                  
                  p("Klassen, A. F. et al. (2018) ‘Psychometric findings and normative values for the CLEFT-Q 
        based on 2434 children and young adult patients with cleft lip and/or palate from 12 countries’,", 
                    em("CMAJ"),". doi: 10.1503/cmaj.170289.", 
                    style = "font-size:13px;"),
                  
                  br(),
                  
                  
                  p("The CLEFT-Q CAT Score Checker has been developed by Conrad Harrison 
              at the University of Oxford. 
              This represents independent research funded by the NIHR. 
              The views expressed are those of the author and not necessarily those of 
              the University of Oxford, the NHS, the NIHR or the Department of Health and Social Care.
              The CLEFT-Q CAT Score Checker is provided with absolutely no warranty. The author, the University of Oxford
              and the NIHR disclaim any and all express and implied warranties including without limitation the
              implied warranties of title, fitness for a particular
              purpose, merchantability and noninfringement. The CLEFT-Q Score Checker is 
              licensed under [INSERT LICENSE]. To obtain a copy of the license please
              click here [ADD HYPERLINK] or contact the author on conrad.harrison@medsci.ox.ac.uk.", 
                    style = "font-size:11px;")
                  
                  
  ))
  
)



#### User Interface 2 (Multi Score Checker) ####


ui2 <- fluidPage(
  
  # Application title
  titlePanel("Radar Plot"),
  
  # Text
  br(),
  
  h4("Please use this Score Checker to compare CLEFT-Q CAT scores to median scores 
     from the CLEFT-Q field test (Klassen", em("et al.,"), "2018)."),
  
  fluidRow(
    
    plotOutput(outputId = "RadarPlot") %>% withSpinner(color="darkmagenta"),
    
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
    
    
    column(3,
           
           selectInput(inputId = "age",
                       label = "Age range (years)",
                       choices = c("All ages (8-29)", "8-11", "12-15", "16-19", "20+")),
           
           selectInput(inputId = "Sex",
                       label = "Gender",
                       choices = c("All genders", "Male", "Female")),
           
           selectInput(inputId = "Cleft_type",
                       label = "Cleft type",
                       choices = c("All cleft types","Cleft lip", "Cleft palate",
                                   "Cleft lip and alveolus",
                                   "Cleft lip, alveolus and palate")),
           
           selectInput(inputId = "Laterality",
                       label = "Laterality",
                       choices = c("Unilateral or Bilateral","Unilateral", "Bilateral"))
           
           
    ),
    
    
    column(3,
           
           numericInput(inputId = "facescore",
                        label = "Face Score",
                        value = 50,
                        min = 1,
                        max = 100
           ),
           
           numericInput(inputId = "nosescore",
                        label = "Nose Score",
                        value = 50,
                        min = 1,
                        max = 100
           ),
           
           numericInput(inputId = "nostrilscore",
                        label = "Nostrils Score",
                        value = 50,
                        min = 1,
                        max = 100
           ),
           
           numericInput(inputId = "jawscore",
                        label = "Jaws Score",
                        value = 50,
                        min = 1,
                        max = 100
           )
           
           
    ),
    
    column(3,
           
           
           numericInput(inputId = "lipscore",
                        label = "Lips Score",
                        value = 50,
                        min = 1,
                        max = 100
           ),
           
           numericInput(inputId = "teethscore",
                        label = "Teeth Score",
                        value = 50,
                        min = 1,
                        max = 100
           ),
           
           numericInput(inputId = "scarscore",
                        label = "Scar Score",
                        value = 50,
                        min = 1,
                        max = 100
                        
           ),
           
           numericInput(inputId = "schoolscore",
                        label = "School Score",
                        value = 50,
                        min = 1,
                        max = 100
           )
           
           
    ),
    
    column(3,
           
           numericInput(inputId = "socialscore",
                        label = "Social Function Score",
                        value = 50,
                        min = 1,
                        max = 100
           ),
           
           numericInput(inputId = "psychscore",
                        label = "Psychological Function Score",
                        value = 50,
                        min = 1,
                        max = 100
           ),  
           
           numericInput(inputId = "sfscore",
                        label = "Speech Function Score",
                        value = 50,
                        min = 1,
                        max = 100
           ),  
           
           numericInput(inputId = "sdscore",
                        label = "Speech Distress Score",
                        value = 50,
                        min = 1,
                        max = 100
           )
    )
  ),
  
  
  fluidRow(column(11,
                  br(),
                  br(),
                  
                  
                  p("Klassen, A. F. et al. (2018) ‘Psychometric findings and normative values for the CLEFT-Q 
        based on 2434 children and young adult patients with cleft lip and/or palate from 12 countries’,", 
                    em("CMAJ"),". doi: 10.1503/cmaj.170289.", 
                    style = "font-size:13px;"),
                  
                  br(),
                  
                  
                  p("The CLEFT-Q CAT Score Checker has been developed by Conrad Harrison 
              at the University of Oxford. 
              This represents independent research funded by the NIHR. 
              The views expressed are those of the author and not necessarily those of 
              the University of Oxford, the NHS, the NIHR or the Department of Health and Social Care.
              The CLEFT-Q CAT Score Checker is provided with absolutely no warranty. The author, the University of Oxford
              and the NIHR disclaim any and all express and implied warranties including without limitation the
              implied warranties of title, fitness for a particular
              purpose, merchantability and noninfringement. The CLEFT-Q Score Checker is 
              licensed under [INSERT LICENSE]. To obtain a copy of the license please
              click here [ADD HYPERLINK] or contact the author on conrad.harrison@medsci.ox.ac.uk.", 
                    style = "font-size:11px;")
                  
                  
  ))
  
)


#### Combine ui 1 and 2 ####


ui <- tabsetPanel(
  
  tabPanel("Population Density", ui1),
  tabPanel("Radar Plot", ui2)
  
)






#### Server logic ####
server <- function(input, output) {
  
  # Single score checker
  
  score <- reactive({input$score})
  scale <- reactive({input$scale})
  
  score_dist <- reactive({(data %>%
                             filter(data$Scale == input$scale,
                                    data$Age %in% (if (input$age != "All ages (8-29)") {input$age} else {c("8-11", "12-15", "16-19", "20+", NA)}),
                                    data$Sex %in% (if (input$Sex != "All genders") {input$Sex} else {c("Male", "Female", NA)}),
                                    data$Cleft_type %in% (if (input$Cleft_type != "All cleft types") {input$Cleft_type} else{c("Cleft lip", "Cleft palate",
                                                                                                                               "Cleft lip and alveolus",
                                                                                                                               "Cleft lip, alveolus and palate", NA)}),
                                    data$Laterality %in% (if (input$Laterality != "Unilateral or Bilateral") {input$Laterality} else{c("Unilateral", "Bilateral", NA)})) %>%
                             select(Score))
  })
  
  
  
  percentile <- reactive({
    getPercent((data %>%
                  filter(data$Scale == input$scale,
                         data$Age %in% (if (input$age != "All ages (8-29)") {input$age} else {c("8-11", "12-15", "16-19", "20+", NA)}),
                         data$Sex %in% (if (input$Sex != "All genders") {input$Sex} else {c("Male", "Female", NA)}),
                         data$Cleft_type %in% (if (input$Cleft_type != "All cleft types") {input$Cleft_type} else{c("Cleft lip", "Cleft palate",
                                                                                                                    "Cleft lip and alveolus",
                                                                                                                    "Cleft lip, alveolus and palate", NA)}),
                         data$Laterality %in% (if (input$Laterality != "Unilateral or Bilateral") {input$Laterality} else{c("Unilateral", "Bilateral", NA)})) %>%
                  select(Score)), 
               input$score) %>%
      round(0)
  })
  
  
  output$Plot <- renderPlot({
    
    makePlot(score_dist(), score(), percentile())
    
  })
  
  output$Text <- renderText({
    
    glue::glue("A score of {score()} on the CLEFT-Q {scale()} scale is higher than {percentile()}% of scores reported by people with similar phenotypes, in the CLEFT-Q field test, based on a sample size of {nrow(score_dist())}.")
    
  })
  
  
  ### Multiscore checker
  
  
  facescore <- reactive({input$facescore})
  nosescore <- reactive({input$nosescore})
  nostrilscore <- reactive({input$nostrilscore})
  jawscore <- reactive({input$jawscore})
  lipscore <- reactive({input$lipscore})
  teethscore <- reactive({input$teethscore})
  scarscore <- reactive({input$scarscore})
  schoolscore <- reactive({input$schoolscore})
  socialscore <- reactive({input$socialscore})
  psychscore <- reactive({input$psychscore})
  sfscore <- reactive({input$sfscore})
  sdscore <- reactive({input$sdscore})
  
  
  radardata <- reactive({data = data %>%
    filter(data$Age %in% (if (input$age != "All ages (8-29)") {input$age} else {c("8-11", "12-15", "16-19", "20+", NA)}),
           data$Sex %in% (if (input$Sex != "All genders") {input$Sex} else {c("Male", "Female", NA)}),
           data$Cleft_type %in% (if (input$Cleft_type != "All cleft types") {input$Cleft_type} else{c("Cleft lip", "Cleft palate",
                                                                                                      "Cleft lip and alveolus",
                                                                                                      "Cleft lip, alveolus and palate", NA)}),
           data$Laterality %in% (if (input$Laterality != "Unilateral or Bilateral") {input$Laterality} else{c("Unilateral", "Bilateral", NA)}))
  })
  
  
  output$RadarPlot <- renderPlot({
    
    
    radarPlot(data = radardata(),
              facescore = facescore(),
              nosescore = nosescore(),
              nostrilscore = nostrilscore(),
              jawscore = jawscore(),
              lipscore = lipscore(),
              teethscore = teethscore(),
              scarscore = scarscore(),
              schoolscore = schoolscore(),
              socialscore = socialscore(),
              psychscore = psychscore(),
              sfscore = sfscore(),
              sdscore = sdscore()
              
    )
    
  }, height = 625, width = 800)
  
}

#### Run the application ####
shinyApp(ui = ui, server = server)

