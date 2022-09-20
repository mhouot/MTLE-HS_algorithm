#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# install.packages("anyLib")
# anyLib::anyLib(c("shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "ggplot2", "googleVis", "colourpicker"))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # tags$style(type="text/css",
    #            ".shiny-output-error { visibility: hidden; }",
    #            ".shiny-output-error:before { visibility: hidden; }"
    # ),
    # 
    # # Application title
    # 
    # # Thème bslib
    # theme = bs_theme (
    #     version = 5,
    #     bg = "#FFFFFF",
    #     fg = "#112446",
    #     primary = "#112446",
    #     "well-bg" = "#FFF",
    #     base_font = font_google("Poppins")
    # ),
    h3("Algorithm* from \"Early identification of seizure freedom with medical treatment in patients with mesial temporal lobe epilepsy and hippocampal sclerosis (MTLE HS)\"", align = "center"),
    br(),
    em("*Algorithm developped by the Department of Neurology and Institut du Cerveau et de la Moelle épinière (ICM) , AP-HP, Pitié-Salpêtrière Hospital, Paris, France.", style = "font-size:12px"),
    br(), br(),
    p("In order to prospectively validate this algorithm at broad international level, we encourage all users to send us their anonymised patient data as an Excel file to this email address:", strong("margaux.cheval@aphp.fr", style = "color:green;font-size:16px"), ". We may contact you to obtain follow-up data on these patients."),
    br(), 
    p(em("Notes. "), "This algorithm was developped in a cohort of MTLE-HS patients with median age at onset of 10 [", em("Q1;Q3"), ": 5;18] (range: 0-73)"),
   # tags$div(
   #     "Link to the publication: ",
   #     tags$a(href="https://pubmed.ncbi.nlm.nih.gov", 
   #            "https://pubmed.ncbi.nlm.nih.gov")
    #),
    
    br(),

    tags$head(
        # Custom CSS styles
        tags$link(rel="stylesheet", type="text/css", href="css_custom_styles.css"),
        tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 250px;
               left: 0px;
               width: 100%;
               height: 10%;
               padding: 200px 0px 200px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #FF6600; # CCFF66
               z-index: 105;
             }
          ")
    ),

    navbarPage(
        theme = "yeti",  # <--- To use a theme, uncomment this
        "Choose between : ",
        tabPanel("Select one patient's data",
                 sidebarLayout(
                 sidebarPanel(
                     fluidRow(column(5, 
                     selectInput("age.at.onset", "Age at onset of epilepsy", c('', 0:125), selected = NULL, multiple = F),
                     radioButtons("sex", "Sex", c("Man"="0", "Woman"="1"), selected = character(0)),
                     radioButtons("prematurity", "Prematurity or neonatal anoxia",  c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("family.history.of.epilepsy", "Family history of epilepsy (febrile convulsion or epilepsy)", c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("febrile.convulsion", "Febrile convulsion (complicated or simple)", c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("head.trauma", "Head Trauma", c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("meningitis.or.encephalitis", "Meningitis or encephalitis", c("No"="0", "Yes"="1"), selected = character(0)),
                     # radioButtons("aura", "Aura (all types)", c("No"="0", "Yes"="1"), selected = character(0)),
                     # radioButtons("abdominal.aura", "Abdominal aura (rising epigastric sensation, nausea, abdominal pain, dysphagia )", c("No"="0", "Yes"="1"), selected = character(0)),
                     # radioButtons("psycho.affective.aura", "Psycho affective aura (fear, anxiety, happiness, well-being, sadness, near death sensation )", c("No"="0", "Yes"="1"), selected = character(0)),
                     # radioButtons("autonomic.aura", "Autonomic aura (rising feeling of warmth, thoracic constriction, tachycardia, thrill-feeling of cold, breathless, pallor, rubor, urge to urinate, sweating, thirst, hunger)", c("No"="0", "Yes"="1"), selected = character(0))
                     radioButtons("gestural.automatisms", "Gestural automatisms", c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("oro.alimentary.automatisms", "Oro alimentary automatisms", c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("verbal.automatisms", "Verbal automatisms", c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("dystonia.of.a.limb", "Dystonia of a limb", c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("at.least.one.focal.to.bilateral.tonic.clonic.seizure", "At least one focal to bilateral tonic clonic seizure", c("No"="0", "Yes"="1"), selected = character(0))
                     ),
                     column(5, offset = 2,
                            radioButtons("aura", "Aura (all types)", c("No"="0", "Yes"="1"), selected = character(0)),
                            radioButtons("abdominal.aura", "Abdominal aura (rising epigastric sensation, nausea, abdominal pain, dysphagia )", c("No"="0", "Yes"="1"), selected = character(0)),
                            radioButtons("psycho.affective.aura", "Psycho affective aura (fear, anxiety, happiness, well-being, sadness, near death sensation )", c("No"="0", "Yes"="1"), selected = character(0)),
                            radioButtons("autonomic.aura", "Autonomic aura (rising feeling of warmth, thoracic constriction, tachycardia, thrill-feeling of cold, breathless, pallor, rubor, urge to urinate, sweating, thirst, hunger)", c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("experiential.aura", "Experiential aura (déjà-vu/déjà-vecu, reviviscence, dreamy state, premonition)", c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("visual.aura", "Visual aura (light variation, blurring, illusion)", c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("sensory.aura", "Sensory aura (tingling, pain)", c("No"="0", "Yes"="1"), selected = character(0)),
                     radioButtons("non.specific.aura", "Non specific aura (indescribable, numbness, faintness, headache, feeling of shaking, stiffness)", c("No"="0", "Yes"="1"), selected = character(0)),
                     actionButton("submit", "Submit", class = "btn-primary")
                     )), 
                     
                    
                     width = 5
#                      textInput("txt", "Text input:", "general"),
#                      sliderInput("slider", "Slider input:", 1, 100, 30),
#                      tags$h5("Default actionButton:"),
#                      actionButton("action", "Search"),
#                      
#                      tags$h5("actionButton with CSS class:"),
#                      actionButton("action2", "Action button", class = "btn-primary")
                 ),
                 mainPanel(
                     verbatimTextOutput("text_submit"),
                     verbatimTextOutput("text_resProba"),
                     column(7, align="center", plotOutput("Plot_proba")),
                     column(7, uiOutput("downloadData")), 
                     width = 7
                     )
                 )
        ),
        tabPanel("Upload a file of the data patients", 
                 
                 br(),  
                 tags$div(
                     "A test file named data_test.xlsx is available at the link: ",
                     tags$a(href="https://github.com/mhouot/MTLE-HS_algorithm/tree/main/data", 
                            "https://github.com/mhouot/MTLE-HS_algorithm/tree/main/data")),
                 br(), 
                 h3("Instructions for data file completion"),
                 p('Please find the dictionnary variables to complete the xlsx file: '),
                 DTOutput('dictionnary'), 
                 br(),
                 
                 h3("Upload the .xlsx file"),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Choose xlsx file',
                                   accept = c(".xlsx")
                         )
                     ),
                     mainPanel(
                         DTOutput('contents'))),
                 br(),

                 # MainPanel divided into many tabPanel
                 h3("Algorithm results"),
                     tabsetPanel(
                         # tabPanel("Export data",  uiOutput("downloadData_ImportFile")),# downloadButton("Download data")
                         br(),
                         tabPanel("Table", strong(paste0("\nCutoff is: ", round(cutoff, 4))),  br(),  br(), uiOutput("downloadData_ImportFile"), br(),br(), DTOutput('probaData')),
                         tabPanel("Plot", strong(paste0("\nCutoff is: ", round(cutoff, 4))),  br(),  br(), column(12, align="center", 
                                                 conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div("Loading...",id="loadmessage")), 
                                                                  plotOutput("Plot_proba_File", height = "auto")))
                     )
                 
                 )
    )
    
    
    # sidebarLayout(
    #     sidebarPanel(
    #         fileInput('file1', 'Choose xlsx file',
    #                   accept = c(".xlsx")
    #         )
    #     ),
    #     mainPanel(
    #         DTOutput('contents'))),
    # 
    # # MainPanel divided into many tabPanel
    # mainPanel(
    #     tabsetPanel(
    #         tabPanel("Export data",  downloadButton("Download data")),
    #         tabPanel("Table", DTOutput('data_clean'))#,
    #         # tabPanel("Plot", plotOutput("Plot_proba"))
    #     )
    # )
    
    # # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #         plotOutput("distPlot")
    #     )
    # )
    
    
    # ########## Troisième page : DATA ##########
    # 
    # tabPanel(
    #     title = "Data",
    #     downloadButton(outputId = "export_data", label = "Exporter", class = "mb-3"),
    #     reactableOutput(outputId = "tableau_data")
    # )
    
))
