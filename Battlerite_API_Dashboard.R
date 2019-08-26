library('shiny')
library('dplyr')
library('readr')
library('ggplot2')
library('forcats')
library('DT')
library('tidyr')
library('stringr')
library('fastDummies')
library('readxl')
library('leaflet')
library('schoolmath')

setwd('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects')

#Get crosswalk reference files
br_crosswalk <- read_csv('Reference Files/Battlerite_Crosswalk.csv')
pose_crosswalk <- read_csv('Reference Files/Pose_Crosswalk.csv')
outfit_crosswalk <- read_csv('Reference Files/Outfit_Crosswalk.csv')
attachment_crosswalk <- read_csv('Reference Files/Attachment_Crosswalk.csv')
mount_crosswalk <- read_csv('Reference Files/Mount_Crosswalk.csv')
champion_crosswalk <- read_csv('Reference Files/Champion_Crosswalk.csv')
avatar_crosswalk <- read_csv('Reference Files/Avatar_Crosswalk.csv')
region_coordinates <- read_excel('Reference Files/region.xlsx')

ui <- navbarPage('Navbar',
                 tabPanel('File Selection',
                          fluidRow(
                            column(width = 4,
                                   radioButtons(inputId = 'FileSelectAll',
                                                label = 'File Selection',
                                                choices = c('Use all data', 'Customize'),
                                                selected = 'Customize'),
                                   conditionalPanel(
                                     condition = "input.FileSelectAll == 'Customize'",
                                     selectInput(inputId = 'FileSelectDays',
                                                 label = 'Select Days Included:',
                                                 multiple = TRUE,
                                                 choices = 10:17,
                                                 selected = 10)
                                   )
                            ),
                            column(width = 4,
                                   selectInput(inputId = 'Customize',
                                               label = 'Customize File Selection For Each Day:',
                                               multiple = FALSE,
                                               choices = c('Use all data from each day', 'Customize'),
                                               selected =  'Use all data from each day')
                            ),
                            column(width = 4,
                                   actionButton(inputId = 'CreateData',
                                                label = 'Choose Selected Data'),
                                   htmlOutput('MessageDataCreated'))
                          ),
                          fluidRow(
                            column(width = 3,
                                   uiOutput('Interactive_Day10a'),
                                   uiOutput('Interactive_Day10b')),
                            column(width = 3,
                                   uiOutput('Interactive_Day11a'),
                                   uiOutput('Interactive_Day11b')),
                            column(width = 3,
                                   uiOutput('Interactive_Day12a'),
                                   uiOutput('Interactive_Day12b')),
                            column(width = 3,
                                   uiOutput('Interactive_Day13a'),
                                   uiOutput('Interactive_Day13b'))
                          ),
                          fluidRow(
                            column(width = 3,
                                   uiOutput('Interactive_Day14a'),
                                   uiOutput('Interactive_Day14b')),
                            column(width = 3,
                                   uiOutput('Interactive_Day15a'),
                                   uiOutput('Interactive_Day15b')),
                            column(width = 3,
                                   uiOutput('Interactive_Day16a'),
                                   uiOutput('Interactive_Day16b')),
                            column(width = 3,
                                   uiOutput('Interactive_Day17a'),
                                   uiOutput('Interactive_Day17b'))
                          ),
                          fluidRow(htmlOutput('UserGuide'),
                                   htmlOutput('Message'))
                 ),
                 tabPanel('Champion',
                          fluidRow(
                            column(width = 4,
                                   selectInput(inputId = 'champion',
                                               label = 'Champion',
                                               multiple = FALSE,
                                               choices = c('None', c("ALYSIA", "ASHKA", "BAKKO", "BLOSSOM", "CROAK",
                                                                     "DESTINY", "EZMO", "FREYA", "IVA", "JADE", "JAMILA", 
                                                                     "JUMONG", "LUCIE", "OLDUR", "PEARL", "PESTILUS", "POLOMA",
                                                                     "RAIGON", "ROOK", "RUH KAAN", "SHEN RAO", "SHIFU", "SIRIUS",
                                                                     "TAYA", "THORN", "ULRIC", "VARESH", "ZANDER" )),
                                               selected = 'None')
                                   
                            ),
                            column(width = 4,
                                   uiOutput('Interactive_ResetButton'))
                          ),
                          
                          fluidRow(
                            column(width = 4,
                                   radioButtons(inputId = 'measure',
                                                label = 'Measure',
                                                choices = c('Win Rate' = 'winrate',
                                                            'Win Rate(Adjusted)' = 'winrateadjusted',
                                                            'Pick Rate' = 'pickrate'))
                            ),
                            
                            column(width = 4,
                                   uiOutput(outputId = 'ChampImage')),
                            
                            column(width = 4,
                                   htmlOutput('OverallMeasure'),
                                   uiOutput('Pick_Rate_Adjusted_OverallMeasure'),
                                   uiOutput('Pick_Rate_OverallMeasure'))
                          ),
                          
                          fluidRow(
                            column(width = 12,
                                   div(DT::dataTableOutput('RegionGroup'), style = "font-size:75%"),
                                   uiOutput(outputId = 'Interactive_Unfilter_RegionGroup'),
                                   uiOutput('Pick_Rate_Adjusted_RegionGroup'),
                                   uiOutput('Pick_Rate_RegionGroup')
                            )
                          ),
                          
                          fluidRow(
                            column(width = 12,
                                   htmlOutput('Chosen_Region'),
                                   leafletOutput(outputId = 'Region'),
                                   uiOutput(outputId = 'Interactive_Unfilter_Region'),
                                   uiOutput('Pick_Rate_Adjusted_Region'),
                                   uiOutput('Pick_Rate_Region')
                            )
                          ),
                          
                          fluidRow(
                            column(width = 6,
                                   plotOutput(outputId = 'TotalTime',
                                              dblclick = 'filterTotalTime',
                                              click = 'unfilterTotalTime',
                                              hover = hoverOpts('hoverTotalTime'),
                                              height = '300px'),
                                   uiOutput('TotalTime_tooltip'),
                                   uiOutput('Pick_Rate_Adjusted_TotalTime'),
                                   uiOutput('Pick_Rate_TotalTime')
                            ),
                            column(width = 6,
                                   plotOutput(outputId = 'ChampionTime',
                                              dblclick = 'filterChampionTime',
                                              click = 'unfilterChampionTime',
                                              hover = hoverOpts('hoverChampionTime'),
                                              height = '300px'),
                                   uiOutput('ChampionTime_tooltip'),
                                   uiOutput('Pick_Rate_Adjusted_ChampionTime'),
                                   uiOutput('Pick_Rate_ChampionTime')
                            )
                          
                          ),
                          
                          fluidRow(
                            htmlOutput('BestOverallBattlerites'),
                            uiOutput('Pick_Rate_Adjusted_BestOverallBattlerites'),
                            uiOutput('Pick_Rate_BestOverallBattlerites')
                          ),
                          
                          fluidRow(
                            column(width = 6,
                                   fluidRow(
                                     htmlOutput('SelectedBattlerites'),
                                     uiOutput(outputId = 'Interactive_Repick_Battlerites'),
                                     plotOutput(outputId = 'Battlerites',
                                                dblclick = 'filterBattlerites',
                                                hover = hoverOpts('hoverBattlerites'),
                                                height = '600px'),
                                     uiOutput('Battlerites_tooltip'),
                                     uiOutput('Pick_Rate_Adjusted_Battlerites'),
                                     uiOutput('Pick_Rate_Battlerites')
                                   )
                            ),
                            
                            column(width = 6,
                                   fluidRow(width = 6,
                                            plotOutput(outputId = 'League',
                                                       dblclick = 'filterLeague',
                                                       click = 'unfilterLeague',
                                                       hover = hoverOpts('hoverLeague'),
                                                       height = '300px'),
                                            uiOutput('League_tooltip'),
                                            uiOutput('Pick_Rate_Adjusted_League'),
                                            uiOutput('Pick_Rate_League')
                                   ),
                                   fluidRow(width = 6,
                                            plotOutput(outputId = 'ServerType',
                                                       dblclick = 'filterServerType',
                                                       click = 'unfilterServerType',
                                                       hover = hoverOpts('hoverServerType'),
                                                       height = '300px'),
                                            uiOutput('ServerType_tooltip'),
                                            uiOutput('Pick_Rate_Adjusted_ServerType'),
                                            uiOutput('Pick_Rate_ServerType')
                                   )
                            )
                          ),
                          
                          fluidRow(
                            column(width = 6,
                                   plotOutput(outputId = 'PlayerType',
                                              dblclick = 'filterPlayerType',
                                              click = 'unfilterPlayerType',
                                              hover = hoverOpts('hoverPlayerType'),
                                              height = '300px'),
                                   uiOutput('PlayerType_tooltip'),
                                   uiOutput('Pick_Rate_Adjusted_PlayerType'),
                                   uiOutput('Pick_Rate_PlayerType')),
                            

                            column(width = 6,
                                   plotOutput(outputId = 'Date',
                                              dblclick = 'filterDate',
                                              click = 'unfilterDate',
                                              hover = hoverOpts('hoverDate'),
                                              height = '300px'),
                                   uiOutput('Date_tooltip'),
                                   uiOutput('Pick_Rate_Adjusted_Date'),
                                   uiOutput('Pick_Rate_Date'))
                          ),
                          
                          fluidRow(
                            column(width = 6,
                                   fluidRow(
                                     plotOutput(outputId = 'Map',
                                                dblclick = 'filterMap',
                                                click = 'unfilterMap',
                                                hover = hoverOpts('hoverMap'),
                                                height = '600px'),
                                     uiOutput('Map_tooltip'),
                                     uiOutput('Pick_Rate_Adjusted_Map'),
                                     uiOutput('Pick_Rate_Map')
                                   )
                            ),
                            
                            column(width = 6,
                                   fluidRow(width = 6,
                                            plotOutput(outputId = 'Casual',
                                                       dblclick = 'filterCasual',
                                                       click = 'unfilterCasual',
                                                       hover = hoverOpts('hoverCasual'),
                                                       height = '300px'),
                                            uiOutput('Casual_tooltip'),
                                            uiOutput('Pick_Rate_Adjusted_Casual'),
                                            uiOutput('Pick_Rate_Casual')
                                   ),
                                   fluidRow(width = 6,
                                            plotOutput(outputId = 'Ping',
                                                       dblclick = 'filterPing',
                                                       click = 'unfilterPing',
                                                       hover = hoverOpts('hoverPing'),
                                                       height = '300px'),
                                            uiOutput('Ping_tooltip'),
                                            uiOutput('Pick_Rate_Adjusted_Ping'),
                                            uiOutput('Pick_Rate_Ping')
                                   )
                            )
                          ),
                          
               
                          fluidRow(
                            column(width = 3,
                                   htmlOutput('StatsWinLabel'),
                                   div(DT::dataTableOutput('StatsWin'), style = "font-size:70%"),
                                   uiOutput('Pick_Rate_Adjusted_Round_Stats'),
                                   uiOutput('Pick_Rate_Round_Stats')
                            ),
                            
                            column(width = 3,
                                   htmlOutput('StatsLoseLabel'),
                                   div(DT::dataTableOutput('StatsLose'), style = "font-size:70%")
                            ),
                            
                            column(width = 6,
                                   
                                   fluidRow(
                                     column(width = 6,
                                            htmlOutput('BestCompsLabel'),
                                            div(tableOutput('BestComps'), style = "font-size:80%")),
                                     column(width = 6,
                                            htmlOutput('WorstCompsLabel'),
                                            div(tableOutput('WorstComps'), style = "font-size:80%"))
                                   ),
                                   uiOutput('Pick_Rate_Adjusted_Comps'),
                                   uiOutput('Pick_Rate_Comps'),
                                   fluidRow(htmlOutput('BestAllyRoles')),
                                   
                                   fluidRow(
                                     column(width = 6,
                                            htmlOutput('BestMatchupsLabel'),
                                            div(tableOutput('BestMatchups'), style = "font-size:80%")),
                                     column(width = 6,
                                            htmlOutput('WorstMatchupsLabel'),
                                            div(tableOutput('WorstMatchups'), style = "font-size:80%"))
                                   ),
                                   uiOutput('Pick_Rate_Adjusted_Comps_Matchups'),
                                   uiOutput('Pick_Rate_Comps_Matchups'),
                                   fluidRow(htmlOutput('WorstAllyRoles')),
                                   uiOutput('Pick_Rate_Adjusted_TeamRoles'),
                                   uiOutput('Pick_Rate_TeamRoles')
                          
                            )
                          ),
                          
                          fluidRow(
                            column(width = 4,
                                   plotOutput(outputId = 'Mount',
                                              dblclick = 'filterMount',
                                              click = 'unfilterMount',
                                              hover = hoverOpts('hoverMount'),
                                              height = '600px'),
                                   uiOutput('Mount_tooltip'),
                                   uiOutput('Interactive_Slider_Mount'),
                                   uiOutput('Pick_Rate_Adjusted_Mount'),
                                   uiOutput('Pick_Rate_Mount')
                            ),
                            
                            column(width = 4,
                                   plotOutput(outputId = 'Title',
                                              dblclick = 'filterTitle',
                                              click = 'unfilterTitle',
                                              hover = hoverOpts('hoverTitle'),
                                              height = '600px'),
                                   uiOutput('Title_tooltip'),
                                   uiOutput('Interactive_Slider_Title'),
                                   uiOutput('Pick_Rate_Adjusted_Title'),
                                   uiOutput('Pick_Rate_Title')
                            ),
                            
                            column(width = 4,
                                   plotOutput(outputId = 'Avatar',
                                              dblclick = 'filterAvatar',
                                              click = 'unfilterAvatar',
                                              hover = hoverOpts('hoverAvatar'),
                                              height = '600px'),
                                   uiOutput('Avatar_tooltip'),
                                   uiOutput('Interactive_Slider_Avatar'),
                                   uiOutput('Pick_Rate_Adjusted_Avatar'),
                                   uiOutput('Pick_Rate_Avatar')
                            )
                          ),
                          
                          fluidRow(
                            column(width = 4,
                                   plotOutput(outputId = 'Outfit',
                                              dblclick = 'filterOutfit',
                                              click = 'unfilterOutfit',
                                              hover = hoverOpts('hoverOutfit'),
                                              height = '300px'),
                                   uiOutput('Outfit_tooltip'),
                                   uiOutput('Pick_Rate_Adjusted_Outfit'),
                                   uiOutput('Pick_Rate_Outfit')
                            ),
                            
                            column(width = 4,
                                   plotOutput(outputId = 'Attachment',
                                              dblclick = 'filterAttachment',
                                              click = 'unfilterAttachment',
                                              hover = hoverOpts('hoverAttachment'),
                                              height = '300px'),
                                   uiOutput('Attachment_tooltip'),
                                   uiOutput('Pick_Rate_Adjusted_Attachment'),
                                   uiOutput('Pick_Rate_Attachment')
                            ),
                            
                            column(width = 4,
                                   plotOutput(outputId = 'Pose',
                                              dblclick = 'filterPose',
                                              click = 'unfilterPose',
                                              hover = hoverOpts('hoverPose'),
                                              height = '300px'),
                                   uiOutput('Pose_tooltip'),
                                   uiOutput('Pick_Rate_Adjusted_Pose'),
                                   uiOutput('Pick_Rate_Pose')
                            )
                          ),
                          
                          fluidRow(
                            column(width = 12,
                                   htmlOutput('Player_Filter_Info'),
                                   div(DT::dataTableOutput('Players'), style = 'font-size:75%'),
                                   uiOutput(outputId = 'Interactive_Filter_Player'),
                                   uiOutput(outputId = 'Interactive_Unfilter_Player'),
                                   div(DT::dataTableOutput('Players_Game'), style = "font-size:75%"),
                                   div(DT::dataTableOutput('Players_Round'), style = "font-size:75%"),
                                   div(DT::dataTableOutput('Round_Stats'), style = "font-size:75%"),
                                   div(DT::dataTableOutput('Player_Information'), style = "font-size:75%")
                            )
                          )
                 )
)

server <- function(input, output) {
  
  ####################
  ####################
  ###FILE SELECTION###
  ####################
  ####################
  
  ################################
  ###INTERACTIVE FILE SELECTION###
  ################################
  
  output$Interactive_Day10a <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Customize == 'Customize')
    req('10' %in% input$FileSelectDays)

    selectInput(inputId = 'Day10',
                label = 'Customize Day 10',
                choices = c('Use all data', 'Customize'),
                selected = 'Use all data')
    
  })
  
  output$Interactive_Day10b <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Day10 == 'Customize')
    req(input$Customize == 'Customize')

    selectInput(inputId = 'Day10Customize',
                label = 'Select files:',
                multiple = TRUE,
                choices = list.files(path = 'Data', pattern = '^Jul-10', recursive = TRUE)) 
    
  })
  
  output$Interactive_Day11a <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Customize == 'Customize')
    req('11' %in% input$FileSelectDays)
    
    selectInput(inputId = 'Day11',
                label = 'Customize Day 11',
                choices = c('Use all data', 'Customize'),
                selected = 'Use all data')
    
  })
  
  output$Interactive_Day11b <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Day11 == 'Customize')
    req(input$Customize == 'Customize')
    
    selectInput(inputId = 'Day11Customize',
                label = 'Select files:',
                multiple = TRUE,
                choices = list.files(path = 'Data', pattern = '^Jul-11', recursive = TRUE)) 
    
  })
  
  output$Interactive_Day12a <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Customize == 'Customize')
    req('12' %in% input$FileSelectDays)
    
    selectInput(inputId = 'Day12',
                label = 'Customize Day 12',
                choices = c('Use all data', 'Customize'),
                selected = 'Use all data')
    
  })
  
  output$Interactive_Day12b <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Day12 == 'Customize')
    req(input$Customize == 'Customize')
    
    selectInput(inputId = 'Day12Customize',
                label = 'Select files:',
                multiple = TRUE,
                choices = list.files(path = 'Data', pattern = '^Jul-12', recursive = TRUE)) 
    
  })
  
  output$Interactive_Day13a <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Customize == 'Customize')
    req('13' %in% input$FileSelectDays)
    
    selectInput(inputId = 'Day13',
                label = 'Customize Day 13',
                choices = c('Use all data', 'Customize'),
                selected = 'Use all data')
    
  })
  
  output$Interactive_Day13b <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Day13 == 'Customize')
    req(input$Customize == 'Customize')
    
    selectInput(inputId = 'Day13Customize',
                label = 'Select files:',
                multiple = TRUE,
                choices = list.files(path = 'Data', pattern = '^Jul-13', recursive = TRUE)) 
    
  })
  
  output$Interactive_Day14a <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Customize == 'Customize')
    req('14' %in% input$FileSelectDays)
    
    selectInput(inputId = 'Day14',
                label = 'Customize Day 14',
                choices = c('Use all data', 'Customize'),
                selected = 'Use all data')
    
  })
  
  output$Interactive_Day14b <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Day14 == 'Customize')
    req(input$Customize == 'Customize')
    
    selectInput(inputId = 'Day14Customize',
                label = 'Select files:',
                multiple = TRUE,
                choices = list.files(path = 'Data', pattern = '^Jul-14', recursive = TRUE)) 
    
  })
  
  output$Interactive_Day15a <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Customize == 'Customize')
    req('15' %in% input$FileSelectDays)
    
    selectInput(inputId = 'Day15',
                label = 'Customize Day 15',
                choices = c('Use all data', 'Customize'),
                selected = 'Use all data')
    
  })
  
  output$Interactive_Day15b <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Day15 == 'Customize')
    req(input$Customize == 'Customize')
    
    selectInput(inputId = 'Day15Customize',
                label = 'Select files:',
                multiple = TRUE,
                choices = list.files(path = 'Data', pattern = '^Jul-15', recursive = TRUE)) 
    
  })
  
  output$Interactive_Day16a <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Customize == 'Customize')
    req('16' %in% input$FileSelectDays)
    
    selectInput(inputId = 'Day16',
                label = 'Customize Day 16',
                choices = c('Use all data', 'Customize'),
                selected = 'Use all data')
    
  })
  
  output$Interactive_Day16b <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Day16 == 'Customize')
    req(input$Customize == 'Customize')
    
    selectInput(inputId = 'Day16Customize',
                label = 'Select files:',
                multiple = TRUE,
                choices = list.files(path = 'Data', pattern = '^Jul-16', recursive = TRUE)) 
    
  })
  
  output$Interactive_Day17a <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Customize == 'Customize')
    req('17' %in% input$FileSelectDays)
    
    selectInput(inputId = 'Day17',
                label = 'Customize Day 17',
                choices = c('Use all data', 'Customize'),
                selected = 'Use all data')
    
  })
  
  output$Interactive_Day17b <- renderUI({
    
    req(input$FileSelectAll == 'Customize')
    req(input$Day17 == 'Customize')
    req(input$Customize == 'Customize')
    
    selectInput(inputId = 'Day17Customize',
                label = 'Select files:',
                multiple = TRUE,
                choices = list.files(path = 'Data', pattern = '^Jul-17', recursive = TRUE)) 
    
  })
  
  #####################
  ###USER GUIDE LINK###
  #####################
  
  output$UserGuide <- renderUI({
    
    url <- a("See The User Guide For Instructions and Functionality", 
             href = 'https://github.com/Skywind555/Personal-Projects/blob/master/User_Guide.md',
             target="_blank")
    
    tagList(url)
    
  })
  
  output$Message <- renderUI({
    
    HTML('<font size = "4"><b>Keep in mind that it is normal to see nothing happen after selecting a champion. It takes a while to process. </font></b>')
    
  })
  
  ##########
  ###DATA###
  ##########
  
  df <- eventReactive(
    eventExpr = input$CreateData,
    valueExpr = {
      
    data <- c()
    
    if (input$FileSelectAll == 'Use all data') {
      
      withProgress(message = 'Reading in data', value = 0, {
      
      files <- list.files(path = 'Dashboard Data', pattern = '^Jul', recursive = TRUE)
      
      for (i in 1:length(files)) {
        
        data <- rbind(data, read_csv(paste0('Dashboard Data/', files[i])))
        
        incProgress(1/(length(files)), detail = paste('Concatenating file', i))
        
      }})
      
    } else {
      
      if (input$Customize == 'Use all data from each day') {
      
        withProgress(message = 'Reading in data', max = length(input$FileSelectDays), value = 0, {
        
        for (day in input$FileSelectDays) {
          
          files <- list.files(path = 'Dashboard Data', pattern = paste0('^Jul-', day), recursive = TRUE)
          
          for (i in 1:length(files)) {
            
            data <- rbind(data, read_csv(paste0('Dashboard Data/', files[i])))
            
            incProgress(1/(length(files)), detail = paste('Concatenating file', i, 'on day', day))
            
          }
          
        }})
        
      } else {
        
        if(!is.null(input$Day10)) {
        
        if(input$Day10 == 'Use all data') {
          
          files <- list.files(path = 'Dashboard Data', pattern = '^Jul-10', recursive = TRUE)
         
           withProgress(message = 'Reading in Day 10', value = 0, {
          
          for (i in 1:length(files)) {
            
            data <- rbind(data, read_csv(paste0('Dashboard Data/', files[i])))
            
            incProgress(1/length(files), detail = paste('Concatenating file', i))
            
          }})
          
        } else {
          
          if(!is.null(input$Day10Customize)) {
            
            withProgress(message = 'Reading in Day 10', value = 0, {
          
          for (i in 1:length(input$Day10Customize)) {
            
            data <- rbind(data, read_csv(paste0('Dashboard Data/', input$Day10Customize[i])))
            
            incProgress(1/length(input$Day10Customize), detail = paste('Concatenating file', i))
            
          }})}
          
        }}
        
        if(!is.null(input$Day11)) {
          
          if(input$Day11 == 'Use all data') {
            
            files <- list.files(path = 'Dashboard Data', pattern = '^Jul-11', recursive = TRUE)
            
            withProgress(message = 'Reading in Day 11', value = 0, {
              
              for (i in 1:length(files)) {
                
                data <- rbind(data, read_csv(paste0('Dashboard Data/', files[i])))
                
                incProgress(1/length(files), detail = paste('Concatenating file', i))
                
              }})
            
          } else {
            
            if(!is.null(input$Day11Customize)) {
              
              withProgress(message = 'Reading in Day 11', value = 0, {
                
                for (i in 1:length(input$Day11Customize)) {
                  
                  data <- rbind(data, read_csv(paste0('Dashboard Data/', input$Day11Customize[i])))
                  
                  incProgress(1/length(input$Day11Customize), detail = paste('Concatenating file', i))
                  
                }})}
            
          }}
        
        if(!is.null(input$Day12)) {
        
        if(input$Day12 == 'Use all data') {
          
          files <- list.files(path = 'Dashboard Data', pattern = '^Jul-12', recursive = TRUE)
          
          withProgress(message = 'Reading in Day 12', value = 0, {
            
            for (i in 1:length(files)) {
              
              data <- rbind(data, read_csv(paste0('Dashboard Data/', files[i])))
              
              incProgress(1/length(files), detail = paste('Concatenating file', i))
              
            }})
          
        } else {
          
          if(!is.null(input$Day12Customize)) {
            
            withProgress(message = 'Reading in Day 12', value = 0, {
              
              for (i in 1:length(input$Day12Customize)) {
                
                data <- rbind(data, read_csv(paste0('Dashboard Data/', input$Day12Customize[i])))
                
                incProgress(1/length(input$Day12Customize), detail = paste('Concatenating file', i))
                
              }})}
          
        }}
        
      if(!is.null(input$Day13)) {
        
      if(input$Day13 == 'Use all data') {
        
        files <- list.files(path = 'Dashboard Data', pattern = '^Jul-13', recursive = TRUE)
        
        withProgress(message = 'Reading in Day 13', value = 0, {
          
          for (i in 1:length(files)) {
            
            data <- rbind(data, read_csv(paste0('Dashboard Data/', files[i])))
            
            incProgress(1/length(files), detail = paste('Concatenating file', i))
            
          }})
        
      } else {
        
        if(!is.null(input$Day13Customize)) {
          
          withProgress(message = 'Reading in Day 13', value = 0, {
            
            for (i in 1:length(input$Day13Customize)) {
              
              data <- rbind(data, read_csv(paste0('Dashboard Data/', input$Day13Customize[i])))
              
              incProgress(1/length(input$Day13Customize), detail = paste('Concatenating file', i))
              
            }})}
        
      }}
        
    if(!is.null(input$Day14)) {
        
    if(input$Day14 == 'Use all data') {
      
      files <- list.files(path = 'Dashboard Data', pattern = '^Jul-14', recursive = TRUE)
      
      withProgress(message = 'Reading in Day 14', value = 0, {
        
        for (i in 1:length(files)) {
          
          data <- rbind(data, read_csv(paste0('Dashboard Data/', files[i])))
          
          incProgress(1/length(files), detail = paste('Concatenating file', i))
          
        }})
      
    } else {
      
      if(!is.null(input$Day14Customize)) {
        
        withProgress(message = 'Reading in Day 14', value = 0, {
          
          for (i in 1:length(input$Day14Customize)) {
            
            data <- rbind(data, read_csv(paste0('Dashboard Data/', input$Day14Customize[i])))
            
            incProgress(1/length(input$Day14Customize), detail = paste('Concatenating file', i))
            
          }})}
      
    }}
        
        if(!is.null(input$Day15)) {
          
          if(input$Day15 == 'Use all data') {
            
            files <- list.files(path = 'Dashboard Data', pattern = '^Jul-15', recursive = TRUE)
            
            withProgress(message = 'Reading in Day 15', value = 0, {
              
              for (i in 1:length(files)) {
                
                data <- rbind(data, read_csv(paste0('Dashboard Data/', files[i])))
                
                incProgress(1/length(files), detail = paste('Concatenating file', i))
                
              }})
            
          } else {
            
            if(!is.null(input$Day15Customize)) {
              
              withProgress(message = 'Reading in Day 15', value = 0, {
                
                for (i in 1:length(input$Day15Customize)) {
                  
                  data <- rbind(data, read_csv(paste0('Dashboard Data/', input$Day15Customize[i])))
                  
                  incProgress(1/length(input$Day15Customize), detail = paste('Concatenating file', i))
                  
                }})}
            
          }}
        
        if(!is.null(input$Day16)) {
          
          if(input$Day16 == 'Use all data') {
            
            files <- list.files(path = 'Dashboard Data', pattern = '^Jul-16', recursive = TRUE)
            
            withProgress(message = 'Reading in Day 16', value = 0, {
              
              for (i in 1:length(files)) {
                
                data <- rbind(data, read_csv(paste0('Dashboard Data/', files[i])))
                
                incProgress(1/length(files), detail = paste('Concatenating file', i))
                
              }})
            
          } else {
            
            if(!is.null(input$Day16Customize)) {
              
              withProgress(message = 'Reading in Day 16', value = 0, {
                
                for (i in 1:length(input$Day16Customize)) {
                  
                  data <- rbind(data, read_csv(paste0('Dashboard Data/', input$Day16Customize[i])))
                  
                  incProgress(1/length(input$Day16Customize), detail = paste('Concatenating file', i))
                  
                }})}
            
          }}
        
        if(!is.null(input$Day17)) {
          
          if(input$Day17 == 'Use all data') {
            
            files <- list.files(path = 'Dashboard Data', pattern = '^Jul-17', recursive = TRUE)
            
            withProgress(message = 'Reading in Day 17', value = 0, {
              
              for (i in 1:length(files)) {
                
                data <- rbind(data, read_csv(paste0('Dashboard Data/', files[i])))
                
                incProgress(1/length(files), detail = paste('Concatenating file', i))
                
              }})
            
          } else {
            
            if(!is.null(input$Day17Customize)) {
              
              withProgress(message = 'Reading in Day 17', value = 0, {
                
                for (i in 1:length(input$Day17Customize)) {
                  
                  data <- rbind(data, read_csv(paste0('Dashboard Data/', input$Day17Customize[i])))
                  
                  incProgress(1/length(input$Day17Customize), detail = paste('Concatenating file', i))
                  
                }})}
            
          }}
      }
    }
    
    if (is.null(data)) {
      
      data
      
    } else {
    
    df <- data
    
    df$Total_Time_Played <- factor(df$Total_Time_Played, levels = c('Under 10', '10-50', '50-100', 
                                                                    '100-200', '200-500', 'Over 500', 'Unknown'))
    df$Champion_Time_Played <- factor(df$Champion_Time_Played, levels = c('Under 10', '10-50', '50-100', 
                                                                          '100-200', '200-500', 'Over 500', 'Unknown'))

    df$Team_Roles <- factor(df$Team_Roles)
    df$Enemy_Roles <- factor(df$Enemy_Roles)
    df$Battlerites <- factor(df$Battlerites)
    
    df$Ping <- factor(df$Ping, levels = c('0', '10', '20', '30', '40', '50', '60', '70', '80', '90', '100',
                                          '110', '120', '130', '140', '150', '160', '170', '180', '190', '200',
                                          'Time out', 'Unknown'))
    df$Outfit <- factor(df$Outfit)
    df$Mount <- factor(df$Mount)
    df$Attachment <- factor(df$Attachment)
    df$Pose <- factor(df$Pose)
    df$Player_Type <- factor(df$Player_Type)
    df$Ranking_Type <- factor(df$Ranking_Type)
    df$Server_Type <- factor(df$Server_Type)
    df$User_ID <- as.character(df$User_ID)
    df$Date <- factor(df$Date)
    df$Title <- factor(df$Title)
    df$Avatar <- factor(df$Avatar)
    df$Region <- factor(df$Region)
    df$`Battlerite 1` <- factor(df$`Battlerite 1`)
    df$`Battlerite 2` <- factor(df$`Battlerite 2`)
    df$`Battlerite 3` <- factor(df$`Battlerite 3`)
    df$`Battlerite 4` <- factor(df$`Battlerite 4`)
    df$`Battlerite 5` <- factor(df$`Battlerite 5`)
    df$Team_Comp <- factor(df$Team_Comp)
    df$Enemy_Comp <- factor(df$Enemy_Comp)
    df$`Region Group` <- factor(df$`Region Group`)
    df$Map <- factor(df$Map)
    df$Champion <- factor(df$Champion)
    df$Region <- factor(df$Region)
    df$League <- factor(df$League, levels = rev(c('Grand Champion', 'Champion', 'Diamond', 'Platinum', 'Gold',
                                                  'Silver', 'Bronze')), ordered = TRUE)
    
    #Drop Duplicate rows of data (data collection error)
    df <- distinct(df, Game_ID, User_ID, Round, .keep_all = TRUE)
    
    
    data <- df
    
    }
    
    data
    
  })
  
  #####################
  ###DATA PROCESSING###
  #####################
  
  output$MessageDataCreated <- renderUI({
    
    if (!is.null(df())) {
    
    HTML(paste0('Data created with ', dim(df())[1], ' observations.'))
    
    } else {
      
      HTML(paste0('Please select at least one data file.'))
      
    }
    
  })
    

    
    #Compute total number of 'Pick' phases for pick rate. Total number of unique Game_ID's * 2
    Total_Picks_Overall <- reactive({
      
      length(unique(df()$Game_ID))*2
      
    })
    

    
    #Get Champion pick rate adjusted df
    #Get total games played for each user for df for pickrateadjusted
    User_Total_Games_df <- reactive({
      
      df() %>%
        group_by(Game_ID, User_ID) %>%
        ungroup() %>%
        group_by(User_ID) %>%
        summarize(Total_Games = n())
      
    })

  ###############
  ###############
  ###FUNCTIONS###
  ###############
  ###############
  
  
  #Filter data on champion select first
  
  
  create_filtered_data <- function(champ_df, input_champion, filtered_totaltime, filtered_championtime,
                                   filtered_region, filtered_league, filtered_servertype,
                                   filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                   filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                   filtered_playertype, filtered_date, filtered_battlerites, filtered_ping,
                                   calc_pick_size, filter_player, player_selection) {
    #Only include if have selected a champion
    req(input_champion != 'None')
    
    data <- champ_df
      
    #Filter on region
    if (filtered_region != 0) {
      data <- filter(data, Region %in% filtered_region)
    }
    
    #Region Group
    if (filtered_regiongroup != 0) {
      data <- filter(data, `Region Group` %in% filtered_regiongroup)
    }
      
    #Total Time Played
    if (filtered_totaltime != 0) {
      data <- filter(data, Total_Time_Played %in% filtered_totaltime)
    }
      
    #Champion Time Played
    if (filtered_championtime != 0) {
      data <- filter(data, Champion_Time_Played %in% filtered_championtime)
    }
    
    #Filter on League
    if (filtered_league != 0) {
      data <- filter(data, League %in% filtered_league)
    }
    
    #Filter player type
    if (filtered_playertype != 0) {
      data <- filter(data, Player_Type %in% filtered_playertype)
    }
    #Filter Date
    if (filtered_date != 0) {
      data <- filter(data, Date %in% filtered_date)
    }
    
    #Filter on Server Type (Need to add solo queue in one column in match type)
    if (filtered_servertype != 0) {
      data <- filter(data, Server_Type %in% filtered_servertype)
    }
    
    #Filter on Map played
    if (filtered_map != 0) {
      data <- filter(data, Map %in% filtered_map)
    }
    
    #Filter on casual vs ranked
    if (filtered_casual != 0) {
      data <- filter(data, Ranking_Type %in% filtered_casual)
    }
    
    #Filter on Mount
    if (filtered_mount != 0) {
      data <- filter(data, Mount %in% filtered_mount)
    }
    
    #Filter on Title
    if (filtered_title != 0) {
      data <- filter(data, Title %in% filtered_title)
    }
    
    #Filter on Avatar
    if (filtered_avatar != 0) {
      data <- filter(data, Avatar %in% filtered_avatar)
    }
    
    #Filter on Outfit
    if (filtered_outfit != 0) {
      data <- filter(data, Outfit %in% filtered_outfit)
    }
    
    #Filter on Attachment/Weapon
    if (filtered_attachment != 0) {
      data <- filter(data, Attachment %in% filtered_attachment)
    }
    
    #Filter on Pose
    if (filtered_pose != 0) {
      data <- filter(data, Pose %in% filtered_pose)
    }
      
    #Filter on Ping
    if (filtered_ping != -1) {
      data <- filter(data, Ping %in% filtered_ping)
    }
    
    if (filter_player != 0) {
      data <- filter(data, User_ID %in% player_selection)
    }
    
      
      if (calc_pick_size) {
        
        return(data)

      }
      
      if (length(filtered_battlerites) > 0) {
        
        for (br in 1:length(filtered_battlerites)) {
          
          var <- as.name(filtered_battlerites[br])
          
          data <- filter(data, !!var == 1)
          
          #Adjust output battlerite to not include selected battlerite
          if (filtered_battlerites[br] %in% names(data)) {
            
            remove_col <- names(data) %in% filtered_battlerites[br]
            data <- data[, !remove_col]
            
          }}}
      
      #Check for columns with all 0's and remove
      remove_cols <- c()
      for (battlerite in c(names(data)[77:length(data)])) {
        
        column <- names(data) %in% battlerite
        col_sum <- sum(data[, column])
        
        if (col_sum == 0) {
          
          remove_cols <- c(remove_cols, battlerite)
          
        }
        
      }
      
      remove_cols <- names(data) %in% remove_cols
      
      data <- data[, !remove_cols]

    
    return(data)
    
  }
  

  ###Observe Events###
  
  
  
  #Region
  OE_filterRegion <- function(filtered_region, input_Region_marker_click_id) {
    
    filtered_region(input_Region_marker_click_id)
    
  }
  
  
  #Common observe event filter for bar plots
  OE_Filter_common <- function(common_agg_df, filtered_variable, input_filter_y, variable) {
    
    data <- common_agg_df
    
    if (round(input_filter_y) > length(levels(fct_drop(data[[variable]])))) {
      
      filtered_variable(levels(fct_drop(data[[variable]]))[length(levels(fct_drop(data[[variable]])))])
      
    } else if (round(input_filter_y) <= 1) {
      
      filtered_variable(levels(fct_drop(data[[variable]]))[1])
      
    } else {
      
      filtered_variable(levels(fct_drop(data[[variable]]))[round(input_filter_y)])
      
    }
    
  }

  OE_Filter_Battlerites <- function(agg_df, filtered_variable, input_filter_y, variable) {
    
    data <- agg_df
    
    if (round(input_filter_y) > length(levels(fct_drop(data[[variable]])))) {
      
      filtered_variable <- c(filtered_variable, levels(fct_drop(data[[variable]]))[length(levels(fct_drop(data[[variable]])))])
      
    } else if (round(input_filter_y) <= 1) {
      
      filtered_variable <- c(filtered_variable, levels(fct_drop(data[[variable]]))[1])
      
    } else {
      
      filtered_variable <- c(filtered_variable, levels(fct_drop(data[[variable]]))[round(input_filter_y)])
      
    }
}
  
  ###Aggregates###
  
  pre_agg <- function(filtered_data, measure, variable, calc_baseline, pickrateadjusted) {
    
    req(!is.null(filtered_data))
    
    var <- as.name(variable)
    
    if (calc_baseline) {
      
      user_games_played <- Baseline_User_Games_Played()
      
    } else {
      
      user_games_played <- User_Games_Played()
      
    }
    
    
    data <- filtered_data %>%
      select(Game_ID, User_ID, Round_Won, !!var) %>%
      group_by(Game_ID, User_ID, !!var) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    
      if (measure == 'winrateadjusted') {
        
        data <- data %>%
          ungroup() %>%
          group_by(User_ID, !!var) %>%
          summarize(Game_Won = mean(Game_Won))
        
        data
      }
    
    if (pickrateadjusted == 1 & measure == 'pickrate') {
      
      data <- data %>%
        ungroup() %>%
        group_by(User_ID, !!var) %>%
        summarize(Num_Picks = n())
    
      data <- inner_join(x = data, y = user_games_played, by = c('User_ID' = 'User_ID'))
      
      categories <- unique(pull(data, variable))
      num_unique_users <- length(unique(data$User_ID))
      
      Category = c()
      Pick_Rate = c()
      Sample_Size = c()
      
      for (category in categories) {
        
        data2 <- filter(data, !!var == category)
        user_pick_rates <- c()
        
        #Check if user has picked any other category and compute probability accordingly
        for (userid in data2$User_ID) {
          
          user_filtered <- filter(data2, User_ID == userid)
          user_pick_rate <- user_filtered$Num_Picks/user_filtered$User_Total_Games
          user_pick_rates <- c(user_pick_rates, user_pick_rate)
            
            
        }
        
        category_pick_rate <- sum(user_pick_rates)/num_unique_users
        sample_size <- length(user_pick_rates)
        
        Category <- c(Category, category)
        Pick_Rate <- c(Pick_Rate, category_pick_rate)
        Sample_Size <- c(Sample_Size, sample_size)
      }
      
      data <- data.frame(Category, Pick_Rate, Sample_Size, stringsAsFactors = TRUE)
      
      if (is.factor(data$Category)) {
        
        data$Category <- factor(data$Category, levels = levels(pull(filtered_data, variable)))
        
      } 
      
      data <- mutate(data, Pick_Rate = round(Pick_Rate*100,2))
      colnames(data)[colnames(data) == 'Category'] <- variable 
      
      
      }
      
      return(data)
      

  }
  
  common_agg <- function(pre_agg_data, measure, variable, sort_descending, pickrateadjusted) {
   
    req(!is.null(pre_agg_data))
    var <- as.name(variable)
    
    if (!(measure == 'pickrate' & pickrateadjusted == 1)) {
      
    data <- pre_agg_data %>%
      ungroup() %>%
      select(!!var, Game_Won) %>%
      group_by(!!var) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100, 2),
                Pick_Rate = round(n()/Champion_Pick_Size()*100, 2),
                Sample_Size = n())
    
    } else {
      
      data <- pre_agg_data
      
    }
      
      if (sort_descending) {
        
        if (measure == 'winrate' |
            measure == 'winrateadjusted') {
        
        data <- mutate(data, !!var := fct_reorder(!!var, Win_Rate))
        
        } else {
          
          data <- mutate(data, !!var := fct_reorder(!!var, Pick_Rate))
          
        }
      }
    
    if (variable == 'Region Group') {
      
      if (measure == 'pickrate' & pickrateadjusted == 0) {
      
      data <- select(data, -Win_Rate)
      
      } else if (measure == 'winrate' | measure == 'winrateadjusted') {
        
        data <- select(data, -Pick_Rate)
        
      }
      
    }
    
    
    data
    return(data)
    
  }
  
  
  
  region_agg <- function(filtered_data, measure, pickrateadjusted) {
    req(!is.null(filtered_data))

      data <- filtered_data %>%
        select(Game_ID, User_ID, Round_Won, City, Longitude, Latitude, Region, `Region Group`) %>%
        group_by(Game_ID, User_ID, City, Longitude, Latitude, Region, `Region Group`) %>%
        summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0)) %>%
        ungroup()
      
      if (measure == 'winrateadjusted') {
        
        data <- data %>%
          group_by(User_ID, City, Longitude, Latitude, Region, `Region Group`) %>%
          summarize(Game_Won = mean(Game_Won)) %>%
          ungroup()
        
      }
      
      if (measure == 'pickrate' & pickrateadjusted == 1) {
        
        data2 <- data %>%
          ungroup() %>%
          group_by(User_ID, Region) %>%
          summarize(Num_Picks = n())
        
        extra_cols <- data %>%
          ungroup() %>%
          group_by(City, Longitude, Latitude, Region, `Region Group`) %>%
          summarize(Sample_Size = n())
        
        data <- inner_join(x = data2, y = User_Games_Played(), by = c('User_ID' = 'User_ID'))
        
        categories <- unique(pull(data, Region))
        num_unique_users <- length(unique(data$User_ID))
        
        Category = c()
        Pick_Rate = c()
        Sample_Size = c()
        
        for (category in categories) {
          
          data2 <- filter(data, Region == category)
          user_pick_rates <- c()
          
          #Check if user has picked any other category and compute probability accordingly
          for (userid in data2$User_ID) {
            
            user_filtered <- filter(data2, User_ID == userid)
            user_pick_rate <- user_filtered$Num_Picks/user_filtered$User_Total_Games
            user_pick_rates <- c(user_pick_rates, user_pick_rate)
            
            
          }
          
          category_pick_rate <- sum(user_pick_rates)/num_unique_users
          sample_size <- length(user_pick_rates)
          
          Category <- c(Category, category)
          Pick_Rate <- c(Pick_Rate, category_pick_rate)
          Sample_Size <- c(Sample_Size, sample_size)
        }
        
        data <- data.frame(Category, Pick_Rate, stringsAsFactors = TRUE) %>%
          mutate(Pick_Rate = round(Pick_Rate*100,2))
        colnames(data)[colnames(data) == 'Category'] <- 'Region'
        
        data$Region <- factor(data$Region, levels = levels(extra_cols$Region))
        data <- inner_join(x = data, y = extra_cols, by = c('Region' = 'Region'))
      
      }
      
      if (!(measure == 'pickrate' & pickrateadjusted == 1)) {
        data <- data %>%
          select(Region, `Region Group`, Longitude, Latitude, City, Game_Won) %>%
          group_by(Region, `Region Group`, Longitude, Latitude, City) %>%
          summarize(Win_Rate = round(mean(Game_Won)*100,2),
                    Pick_Rate = round(n()/Champion_Pick_Size()*100, 2),
                    Sample_Size = n())
      }
      

    return(data)
    
  }
  
  ###Dynamic Text###
  
  create_dynamic_text <- function(filtered_totaltime, filtered_championtime,
                                  filtered_region, filtered_league, filtered_servertype,
                                  filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                  filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                  filtered_playertype, filtered_date, filtered_battlerites, filtered_ping,
                                  filter_player, player_selection, barplot) {
    
    if (filtered_totaltime != 0 | filtered_championtime != 0 | filtered_region != 0 |
        filtered_league != 0 | filtered_servertype != 0 | filtered_map != 0 |
        filtered_casual != 0 | filtered_mount != 0 | filtered_title != 0 |
        filtered_avatar != 0 | filtered_outfit != 0 | filtered_attachment != 0 |
        filtered_pose != 0 | filtered_regiongroup != 0 | filtered_playertype != 0 |
        filtered_date != 0 | length(filtered_battlerites) > 0 | filtered_ping != -1 |
        filter_player != 0) {
      
      total_filtered <- list()
      
      if (filtered_totaltime != 0) {
        total_filtered <- c(total_filtered, list(c('Total_Time_Played', filtered_totaltime)))
      }
      
      if (filtered_championtime != 0) {
        total_filtered <- c(total_filtered, list(c('Champion_Time_Played', filtered_championtime)))
      }
      
      if (filtered_region != 0) {
        total_filtered <- c(total_filtered, list(c('Region', filtered_region)))
      }
      
      if (filtered_league != 0) {
        total_filtered <- c(total_filtered, list(c('League', filtered_league)))
      }
      
      if (filtered_servertype != 0) {
        total_filtered <- c(total_filtered, list(c('Server_Type', filtered_servertype)))
      }
      
      if (filtered_map != 0) {
        total_filtered <- c(total_filtered, list(c('Map', filtered_map)))
      }
      
      if (filtered_casual != 0) {
        total_filtered <- c(total_filtered, list(c('Ranking_Type', filtered_casual)))
      }
      
      if (filtered_mount != 0) {
        total_filtered <- c(total_filtered, list(c('Mount', filtered_mount)))
      }
      
      if (filtered_title != 0) {
        total_filtered <- c(total_filtered, list(c('Title', filtered_title)))
      }
      
      if (filtered_avatar != 0) {
        total_filtered <- c(total_filtered, list(c('Avatar', filtered_avatar)))
      }
      
      if (filtered_outfit != 0) {
        total_filtered <- c(total_filtered, list(c('Outfit', filtered_outfit)))
      }
      
      if (filtered_attachment != 0) {
        total_filtered <- c(total_filtered, list(c('Attachment', filtered_attachment)))
      }
      
      if (filtered_pose != 0) {
        total_filtered <- c(total_filtered, list(c('Pose', filtered_pose)))
      }
      
      if (filtered_regiongroup != 0) {
        total_filtered <- c(total_filtered, list(c('Region Group', toString(filtered_regiongroup))))
      }
      
      if (filtered_playertype != 0) {
        total_filtered <- c(total_filtered, list(c('Player_Type', filtered_playertype)))
      }
      
      if (filtered_date != 0) {
        total_filtered <- c(total_filtered, list(c('Date', filtered_date)))
      }
      
      if (length(filtered_battlerites) > 0) {
        total_filtered <- c(total_filtered, list(c('Battlerites', filtered_battlerites)))
      }
      
      if (filtered_ping != -1) {
        total_filtered <- c(total_filtered, list(c('Ping', filtered_ping)))
      }
      
      if (filter_player != 0) {
        total_filtered <- c(total_filtered, list(c('Name', player_selection)))
      }
      
      if (!is.null(total_filtered)) {
        
        for (x in total_filtered) {
          
          if (x[1] == total_filtered[[1]][1]) {
            
            if (x[1] == 'Battlerites') {
              
              if (!is.null(filtered_battlerites)) {
              
              if (length(filtered_battlerites) == 1) {
                
                txt <- paste0('Battlerite = ', filtered_battlerites)
                
              } else {
                
                txt <- paste0('Battlerites in (', toString(filtered_battlerites), ')')
                
              }}
              
            } else if (x[1] == 'Name'){
              
              filtered_player <- filter(player_agg(), User_ID == x[2])
              txt <- paste0('Name = ', filtered_player$Name)
              
            } else {
              
              txt <- paste0(x[1], ' = ', x[2])
              
            }
            
          } else {
            
            
            if(barplot) {
              
              txt <- paste0(txt, '\n')
              
            } else {
              
              txt <- paste0(txt, ' | ')
              
            }
            
            if (x[1] == 'Battlerites') {
              
              if (length(filtered_battlerites) == 1) {
                
                txt <- paste0(txt, 'Battlerite = ', filtered_battlerites)
                
              } else {
                
                txt <- paste0(txt, 'Battlerites in (', filtered_battlerites, ')')
                
              }
              
            } else if (x[1] == 'Name'){
              
              filtered_player <- filter(player_agg(), User_ID == x[2])
              txt <- paste0(txt, 'Name = ', filtered_player$Name)
              
            } else {
              
              txt <- paste0(txt, x[1], ' = ', x[2])
              
            }
          }
          
        }
        
        return(txt)
      }
      
    } else {
      
      return('')
      
    }
    
  }

  ###Plots###
  
  create_region_map <- function(region_df, input_champion, region_ref, measure) {
    req(!is.null(region_df))
    req(input_champion != 'None')

    pal <- colorFactor(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"), 
                       levels(region_ref$`Region Group`))
    
    if (measure == 'winrate' |
        measure == 'winrateadjusted') {
    
    if (dim(region_df)[1] == 1) {
      
      m <- leaflet(data = region_df) %>%
        addTiles() %>%
        addCircleMarkers(lng = ~Longitude,
                         lat = ~Latitude,
                         stroke = FALSE,
                         layerId = ~Region,
                         color = ~pal(`Region Group`),
                         fillOpacity = 1,
                         radius = 10,
                         label = paste('Server: ', region_df$Region,
                                       ' | City: ', region_df$City,
                                       ' | Win Rate: ', region_df$Win_Rate,
                                       ' | Sample Size: ', region_df$Sample_Size)
        )
      
    } else {

    m <- leaflet(data = region_df) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       stroke = FALSE,
                       layerId = ~Region,
                       color = ~pal(`Region Group`),
                       fillOpacity = 1,
                       radius = ~ifelse(Win_Rate <= 30, 3,
                                        ifelse(Win_Rate <= 40, 4,
                                               ifelse(Win_Rate <= 45, 5, 
                                                      ifelse(Win_Rate <= 50, 6,
                                                             ifelse(Win_Rate <= 55, 7, 
                                                                    ifelse(Win_Rate <= 60, 8, 
                                                                           ifelse(Win_Rate <= 65, 9,
                                                                                  ifelse(Win_Rate <= 70, 10,
                                                                                         ifelse(Win_Rate <= 75, 11, 13))))))))),
                       label = paste('Server: ', region_df$Region,
                                     ' | City: ', region_df$City,
                                     ' | Win Rate: ', region_df$Win_Rate,
                                     ' | Sample Size: ', region_df$Sample_Size)
      )
    }
    
    } else {
      
      if (dim(region_df)[1] == 1) {
      
      m <- leaflet(data = region_df) %>%
        addTiles() %>%
        addCircleMarkers(lng = ~Longitude,
                         lat = ~Latitude,
                         stroke = FALSE,
                         layerId = ~Region,
                         color = ~pal(`Region Group`),
                         fillOpacity = 1,
                         radius = ~10,
                         label = paste('Server: ', region_df$Region,
                                       ' | City: ', region_df$City,
                                       ' | Pick Rate: ', region_df$Pick_Rate,
                                       ' | Sample Size: ', region_df$Sample_Size)
        )
      
      } else {
        
        m <- leaflet(data = region_df) %>%
          addTiles() %>%
          addCircleMarkers(lng = ~Longitude,
                           lat = ~Latitude,
                           stroke = FALSE,
                           layerId = ~Region,
                           color = ~pal(`Region Group`),
                           fillOpacity = 1,
                           radius = ~ifelse(Pick_Rate <= quantile(region_df$Pick_Rate, 0.20), 3,
                                            ifelse(Pick_Rate <= quantile(region_df$Pick_Rate, 0.40), 5,
                                                   ifelse(Pick_Rate <= quantile(region_df$Pick_Rate, 0.60), 7, 
                                                          ifelse(Pick_Rate <= quantile(region_df$Pick_Rate, 0.8), 9, 11)))),
                           label = paste('Server: ', region_df$Region,
                                         ' | City: ', region_df$City,
                                         ' | Pick Rate: ', region_df$Pick_Rate,
                                         ' | Sample Size: ', region_df$Sample_Size)
          )
        
      }
      
    }
   
    return(m)
     
  }
  
  
  create_region_table <- function(data, row_name, input_champion, measure, filtered_totaltime, filtered_championtime,
                                  filtered_region, filtered_league, filtered_servertype,
                                  filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                  filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                  filtered_playertype, filtered_date, filtered_battlerites, filtered_ping,
                                  filter_player, player_selection, pickrateadjusted) {
    req(!is.null(data))
    
    if (measure == 'winrate') {
      
      initial_name <- paste0(input_champion, ' Win Rate Split by Region Group')
      
    } else if (measure == 'winrateadjusted') {
      
      initial_name <- paste0(input_champion, ' Win Rate (Adjusted) Split by Region Group')
      
    } else if (measure == 'pickrate' & pickrateadjusted == 0) {
      
      initial_name <- paste0(input_champion, ' Region Group Pick Rate')
      
    } else {
      
      initial_name <- paste0(input_champion, ' Region Group Pick Rate (Adjusted)')
      
    }
    
    dynamic_name <- create_dynamic_text(filtered_totaltime, filtered_championtime,
                                        filtered_region, filtered_league, filtered_servertype,
                                        filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                        filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                        filtered_playertype, filtered_date, filtered_battlerites, filtered_ping,
                                        filter_player, player_selection, barplot = FALSE)
    
    if (dynamic_name != '') {
      
      final_name <- paste0(initial_name, ' Filtered on: ', dynamic_name)
      
    } else {
      
      final_name <- initial_name
      
    }
    
    names(data)[names(data) == 'Region Group'] <- final_name
    names(data)[names(data) == 'Win_Rate'] <- 'Win Rate (Percent)'

    dat <- datatable(data, selection = list(mode = 'single', target = 'cell'), options = list(searching = FALSE,
                                                                                              paging = FALSE),
                     callback = JS(gsub("\n", "", paste0("table.on('click.dt', 'td', function() {
                                                         var row_=table.cell(this).index().row;
                                                         var col=table.cell(this).index().column;
                                                         var rnd= Math.random();
                                                         var data = [row_, col, rnd];
                                                         Shiny.onInputChange(", row_name, ",data);
  });"))) ) %>%
      formatStyle(final_name, target = 'row',
                  backgroundColor = styleEqual(c('USA', 'Europe', 'OCE', 'Asia', 'South America', 'Other'),
                                               c("#D55E00", "#56B4E9", "#009E73", "#E69F00", "#0072B2", "#F0E442")))
    
    #c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
    return(dat)
    
    
}
  
  #Group bars by 2v2, 3v3, solo queue etc stacked
  create_barplot <- function(agg_df, variable, measure, filtered_totaltime, filtered_championtime,
                             filtered_region, filtered_league, filtered_servertype,
                             filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                             filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                             filtered_playertype, filtered_date, filtered_battlerites, filtered_ping,
                             filter_player, player_selection, pickrateadjusted) {
    req(!is.null(agg_df))

    var <- as.name(variable)
    var2 <- var
    
    if (measure == 'winrate') {
      
      initial_title <- paste0(input$champion, ' Win Rate Split by ', variable)
      
    } else if (measure == 'winrateadjusted') {
      
      initial_title <- paste0(input$champion, ' Win Rate (Adjusted) Split by ', variable)
      
    } else if (measure == 'pickrate' & pickrateadjusted == 0) {
      
      if (variable %in% c('Mount', 'Outfit', 'Attachment', 'Pose', 'Server_Type', 'Ranking_Type',
                          'Title', 'Avatar')) {
        initial_title <- paste0(input$champion, ' ', variable, ' Pick Rate')
        
      } else {
        
        initial_title <- paste0(input$champion, ' ', variable, ' Distribution')
        
      }
      
    } else {
      
      if (variable %in% c('Mount', 'Outfit', 'Attachment', 'Pose', 'Server_Type', 'Ranking_Type',
                          'Title', 'Avatar')) {
        initial_title <- paste0(input$champion, ' ', variable, ' Pick Rate (Adjusted)')
        
      } else {
        
        initial_title <- paste0(input$champion, ' ', variable, ' Distribution (Adjusted)')
        
      }

    }
    
    sub_title <- create_dynamic_text(filtered_totaltime, filtered_championtime,
                                       filtered_region, filtered_league, filtered_servertype,
                                       filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                       filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                       filtered_playertype, filtered_date, filtered_battlerites, filtered_ping,
                                       filter_player, player_selection, barplot = TRUE)
    
    if (variable %in% c('League', 'Server_Type', 'Player_Type', 'Map', 'Ranking_Type', 'Pose',
                        'Mount', 'Outfit', 'Attachment', 'Ping', 'Total_Time_Played', 'Champion_Time_Played')) {
    
    if (variable == 'League') {
      
      Colors <- setNames(rev(c('#FFFF78', '#D59E26', '#5AF8FF', '#2C3F67', '#AC8A28', '#C0C0C0', '#A66232')),
                         levels(agg_df$League))
      Colors_subset <- Colors[as.vector(agg_df$League)]
      
    }
    
    if (variable == 'Server_Type') {
      
      Colors <- setNames(c("#E69F00", "#56B4E9", "#009E73"),
                         levels(agg_df$Server_Type))
      Colors_subset <- Colors[as.vector(agg_df$Server_Type)]
      
    }
    
    if (variable == 'Player_Type') {
      
      Colors <- setNames(c("#E69F00", "#56B4E9"),
                         levels(agg_df$Player_Type))
      Colors_subset <- Colors[as.vector(agg_df$Player_Type)]
      
    }
    
    if (variable == 'Map') {
      
      Colors <- setNames(c("#E69F00", "#56B4E9"),
                         levels(agg_df$Type))
      Colors_subset <- Colors[as.vector(agg_df$Type)]
      var2 <- as.name('Type')
      
    }
    
    if (variable == 'Ranking_Type') {
      
      Colors <- setNames(c("#E69F00", "#56B4E9"),
                         levels(agg_df$Ranking_Type))
      Colors_subset <- Colors[as.vector(agg_df$Ranking_Type)]
      
    }
      
    if (variable %in% c('Pose', 'Outfit', 'Mount', 'Attachment')) {
      
      Colors <- setNames(c('#C1C194', '#4CB7E1', '#DA40EE', '#FA8C26'),
                         levels(agg_df$Rarity))
      Colors_subset <- Colors[as.vector(agg_df$Rarity)]
      var2 <- as.name('Rarity')
      
    }
        
      if (variable == 'Ping') {
        
        pal = colorRampPalette(c('green', 'yellow'))
        Colors1 = setNames(pal(8), levels(agg_df$Ping)[1:8])
        pal = colorRampPalette(c('yellow', 'orange'))
        Colors2 = setNames(pal(5), levels(agg_df$Ping)[9:13])
        pal = colorRampPalette(c('orange', 'red'))
        Colors3 = setNames(pal(4), levels(agg_df$Ping)[14:17])
        

        Colors4 = setNames(c(rep('#FF0000', 4), '#6D756B', '#6D756B'), c('170', '180', '190', '200', 
                                                                         'Time out', 'Unknown'))
        Colors = c(Colors1, Colors2, Colors3, Colors4)
        
        Colors_subset <- Colors[as.vector(agg_df$Ping)]
        
      } 
      
      if (variable == 'Total_Time_Played') {
        
        Colors <- setNames(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#6D756B"),
                           levels(agg_df$Total_Time_Played))
        Colors_subset <- Colors[as.vector(agg_df$Total_Time_Played)]
        
      }
      
      if (variable == 'Champion_Time_Played') {
        
        Colors <- setNames(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#6D756B"),
                           levels(agg_df$Champion_Time_Played))
        Colors_subset <- Colors[as.vector(agg_df$Champion_Time_Played)]
        
      }
      
      
      if (measure == 'winrate' | 
          measure == 'winrateadjusted') {
      
      bar <- ggplot(data = agg_df, aes(x = !!var, y = Win_Rate, fill = !!var2)) +
        geom_bar(width = 1, stat = 'identity') +
        ylab('Win Rate (Percent)') +
        xlab(variable) +
        scale_fill_manual(values = Colors_subset) +
        theme(legend.position = 'none', plot.title = element_text(size = 16, face = 'bold'))
      
      } else {
        
        bar <- ggplot(data = agg_df, aes(x = !!var, y = Pick_Rate, fill = !!var2)) +
          geom_bar(width = 1, stat = 'identity') +
          ylab('Pick Rate (Percent)') +
          xlab(variable) +
          scale_fill_manual(values = Colors_subset) +
          theme(legend.position = 'none', plot.title = element_text(size = 16, face = 'bold'))
        
      }
      
      
    } else {
      
      if (variable %in% c('Date', 'Title', 'Avatar')) {
        
        Color <- "#E69F00"
        
        if (measure == 'winrate' |
            measure == 'winrateadjusted') {
        
        bar <- ggplot(data = agg_df, aes(x = !!var, y = Win_Rate)) +
          geom_bar(width = 1, stat = 'identity', fill = Color) +
          ylab('Win Rate (Percent)') +
          xlab(variable)+
          theme(plot.title = element_text(size = 16, face = 'bold'))
        
       
        } else {
          
          bar <- ggplot(data = agg_df, aes(x = !!var, y = Pick_Rate)) +
            geom_bar(width = 1, stat = 'identity', fill = Color) +
            ylab('Pick Rate (Percent)') +
            xlab(variable)+
            theme(plot.title = element_text(size = 16, face = 'bold'))
          
        }
        
      }
      
    }
    
    if (variable != 'Ping') {
      
      bar <- bar + coord_flip()
      
    }
    
    if (sub_title == '') {
      
      bar <- bar + ggtitle(initial_title, subtitle = waiver())
      
    } else {
      
      final_title <- paste0(initial_title, ' Filtered on:')
      
      bar <- bar + ggtitle(final_title, subtitle = sub_title)
      
    }
    
    
    
    return(bar)
    
  }
  
  
  ### Labels ###
  
  create_region_label <- function(measure, filtered_totaltime, filtered_championtime,
                                  filtered_region, filtered_league, filtered_servertype,
                                  filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                  filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                  filtered_playertype, filtered_date, filtered_battlerites, filtered_ping,
                                  filter_player, player_selection, pickrateadjusted) {
    
    if (measure == 'winrate') {
      
      txt <- paste0(input$champion, ' Win Rate Split by Region')
      
    } else if (measure == 'winrateadjusted') {
      
      txt <- paste0(input$champion, ' Win Rate (Adjusted) Split by Region')
      
    } else if (measure == 'pickrate' & pickrateadjusted == 0) {
      
      txt <- paste0(input$champion, ' Region Pick Rate')
      
    } else {
      
      txt <- paste0(input$champion, ' Region Pick Rate (Adjusted)')
      
    }
    
    new_txt <- create_dynamic_text(filtered_totaltime, filtered_championtime,
                                   filtered_region, filtered_league, filtered_servertype,
                                   filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                   filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                   filtered_playertype, filtered_date, filtered_battlerites, filtered_ping,
                                   filter_player, player_selection, barplot = FALSE)
    
    if (new_txt != '') {
      
      txt <- paste0('<font size = "4">', txt, ' Filtered on: </font>')
      txt <- paste0(txt, '<font size = "2">' , new_txt, '</font>')
      
    } else {
      
      txt <- paste0('<font size = "4">', txt, '</font>')
      
    }
      
    return(HTML(txt))
    
  }
  
  create_tooltip <- function(input_hover, count_aggregate_df, count_aggregate, measure, left_adjust, top_adjust) {
    
    hover <- input_hover
    if (is.null(hover)) return(NULL)
    
    if (round(hover$y) > length(levels(fct_drop(count_aggregate)))) {
      
      Name <- levels(fct_drop(count_aggregate))[length(levels(fct_drop(count_aggregate)))]
      
    } else if (round(hover$y) < 1) {
      
      Name <- levels(fct_drop(count_aggregate))[1]
      
    } else {
      
      Name <- levels(fct_drop(count_aggregate))[round(hover$y)]
      
    }
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.20); ",
                    "left:", left_px + left_adjust, "px; top:", top_px + top_adjust, "px;")
    
    
    if (measure == 'winrate' |
        measure == 'winrateadjusted') {
      
      value <- count_aggregate_df[count_aggregate == Name,]$Win_Rate
      
    } else {
      
      value <- count_aggregate_df[count_aggregate == Name,]$Pick_Rate
      
    }
    
    
    samplesize <- count_aggregate_df[count_aggregate == Name,]$Sample_Size
    samplesize
    return(list("Name" = Name, "Value" = value, "Size" = samplesize, "style" = style))
    
    
  }
  
  #Tooltip for ping graph uses hover$x instead of hover$y
  create_tooltip2 <- function(input_hover, count_aggregate_df, count_aggregate, measure, left_adjust, top_adjust) {
    
    hover <- input_hover
    if (is.null(hover)) return(NULL)
    
    if (round(hover$x) > length(levels(fct_drop(count_aggregate)))) {
      
      Name <- levels(fct_drop(count_aggregate))[length(levels(fct_drop(count_aggregate)))]
      
    } else if (round(hover$x) < 1) {
      
      Name <- levels(fct_drop(count_aggregate))[1]
      
    } else {
      
      Name <- levels(fct_drop(count_aggregate))[round(hover$x)]
      
    }
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.20); ",
                    "left:", left_px + left_adjust, "px; top:", top_px + top_adjust, "px;")
    
    if (measure == 'winrate' |
        measure == 'winrateadjusted') {
      
      value <- count_aggregate_df[count_aggregate == Name,]$Win_Rate
      
    } else {
      
      value <- count_aggregate_df[count_aggregate == Name,]$Pick_Rate
      
    }
    
    
    samplesize <- count_aggregate_df[count_aggregate == Name,]$Sample_Size
    return(list("Name" = Name, "Value" = value, "Size" = samplesize, "style" = style))
    
    
  }  
  
  
  
  ###############
  ###############
  ###CHAMPIONS### 
  ###############
  ###############
  
  #Set initial reactive values
  filtered_Region <- reactiveVal(0)
  filtered_Battlerites <- reactiveValues(battlerites = c())
  filtered_League <- reactiveVal(0)
  filtered_Servertype <- reactiveVal(0)
  filtered_Map <- reactiveVal(0)
  filtered_Casual <- reactiveVal(0)
  filtered_Mount <- reactiveVal(0)
  filtered_Title <- reactiveVal(0)
  filtered_Avatar <- reactiveVal(0)
  filtered_Outfit <- reactiveVal(0)
  filtered_Attachment <- reactiveVal(0)
  filtered_Pose <- reactiveVal(0)
  filtered_RegionGroup <- reactiveVal(0)
  filtered_PlayerType <- reactiveVal(0)
  filtered_Date <- reactiveVal(0)
  filtered_Championtime <- reactiveVal(0)
  filtered_Totaltime <- reactiveVal(0)
  filtered_Ping <- reactiveVal(-1)
  player_selection <- reactiveVal(0)
  filter_player <- reactiveVal(0)
  game_selection <- reactiveVal(0)
  round_selection <- reactiveVal(-1)
  round_player_selection <- reactiveVal(0)
  
  #Find total number of games played for each user-champion to compute pick rate adjusted
  User_Games_Played <- reactive({
    
    data <- filtered_data() %>%
      group_by(Game_ID, User_ID) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      select(-count) %>%
      group_by(User_ID) %>%
      summarize(User_Total_Games = n())
    
  })
  
  Baseline_User_Games_Played <- reactive({
    
    data <- filtered_data2() %>%
      group_by(Game_ID, User_ID) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      select(-count) %>%
      group_by(User_ID) %>%
      summarize(User_Total_Games = n())
    
  })
  

  #Filter on champion select first
  #Filter on initial champion select
  champ_df <- reactive({
    
    req(input$champion != 'None')
    req(!is.null(df()))
    Cdf <- filter(df(), Champion == input$champion)
    #Slice Cdf to include only Battlerite columns
    Cdf2 <- Cdf[, c('Battlerite 1', 'Battlerite 2', 'Battlerite 3', 'Battlerite 4', 'Battlerite 5')]
    
    #Get unique list of battlerites stored in vector
    all_battlerites <- unique(c(unique(Cdf2$`Battlerite 1`), unique(Cdf2$`Battlerite 2`), 
                                unique(Cdf2$`Battlerite 3`), unique(Cdf2$`Battlerite 4`), 
                                unique(Cdf2$`Battlerite 5`)))
    
    #Convert categories to indicator variables
    Cdf3 <- dummy_cols(Cdf2)
    
    #Initial column names without Battlerite 1, Battlerite 2, etc
    init_colnames <- colnames(Cdf3)[6:length(colnames(Cdf3))]
    
    #Subset to not include Battlerite 1, Battlerite 2 columns
    Cdf3 <- Cdf3[, init_colnames]
    
    #Rename columns to remove prefix introduced by dummy_cols
    colnames(Cdf3) <- str_replace(init_colnames, paste0(colnames(Cdf2)[1] , '_'), replacement = '')
    colnames(Cdf3) <- str_replace(colnames(Cdf3), paste0(colnames(Cdf2)[2] , '_'), replacement = '')
    colnames(Cdf3) <- str_replace(colnames(Cdf3), paste0(colnames(Cdf2)[3] , '_'), replacement = '')
    colnames(Cdf3) <- str_replace(colnames(Cdf3), paste0(colnames(Cdf2)[4] , '_'), replacement = '')
    colnames(Cdf3) <- str_replace(colnames(Cdf3), paste0(colnames(Cdf2)[5] , '_'), replacement = '')
    
    #Convert character variables to numeric to apply rowsums
    Cdf3 <- sapply(Cdf3, as.numeric)
    
    #Group by same Battlerite name and have final unique set of battlerite choices as indicator variables
    Cdf3 <- t(rowsum(t(Cdf3), group = colnames(Cdf3)))
    
    #Remove the current Battlerite 1, Battlerite 2, etc columns
    Cdf <- Cdf[, !(names(Cdf) %in% c('Battlerite 1', 'Battlerite 2', 'Battlerite 3', 'Battlerite 4', 'Battlerite 5'))]
    
    #Attach new Battlerite columns
    Cdf <- cbind(Cdf, Cdf3)
    Cdf
    
  })
  
  
  
  #Filter starting data frame with any filters applied
  filtered_data <- reactive({create_filtered_data(champ_df(),
                                                  input$champion,
                                                  filtered_Totaltime(),
                                                  filtered_Championtime(),
                                                  filtered_Region(),
                                                  filtered_League(),
                                                  filtered_Servertype(),
                                                  filtered_Map(),
                                                  filtered_Casual(),
                                                  filtered_Mount(),
                                                  filtered_Title(),
                                                  filtered_Avatar(),
                                                  filtered_Outfit(),
                                                  filtered_Attachment(),
                                                  filtered_Pose(),
                                                  filtered_RegionGroup(),
                                                  filtered_PlayerType(),
                                                  filtered_Date(),
                                                  filtered_Battlerites$battlerites,
                                                  filtered_Ping(),
                                                  FALSE,
                                                  filter_player(),
                                                  player_selection()
  )})
  
  #Filtered data without filters on battlerites to calculate champion pick size
  filtered_data2 <- reactive({create_filtered_data(champ_df(),
                                                   input$champion,
                                                   filtered_Totaltime(),
                                                   filtered_Championtime(),
                                                   filtered_Region(),
                                                   filtered_League(),
                                                   filtered_Servertype(),
                                                   filtered_Map(),
                                                   filtered_Casual(),
                                                   filtered_Mount(),
                                                   filtered_Title(),
                                                   filtered_Avatar(),
                                                   filtered_Outfit(),
                                                   filtered_Attachment(),
                                                   filtered_Pose(),
                                                   filtered_RegionGroup(),
                                                   filtered_PlayerType(),
                                                   filtered_Date(),
                                                   filtered_Battlerites$battlerites,
                                                   filtered_Ping(),
                                                   TRUE,
                                                   filter_player(),
                                                   player_selection()
  )})
  
 
  ##################
  ###RESET BUTTON###
  ##################
  
  observeEvent(
    eventExpr = input$ResetAll,
    handlerExpr = {
      
      filtered_Region(0)
      filtered_Battlerites$battlerites <- c()
      filtered_League(0)
      filtered_Servertype(0)
      filtered_Map(0)
      filtered_Casual(0)
      filtered_Mount(0)
      filtered_Title(0)
      filtered_Avatar(0)
      filtered_Outfit(0)
      filtered_Attachment(0)
      filtered_Pose(0)
      filtered_RegionGroup(0)
      filtered_PlayerType(0)
      filtered_Date(0)
      filtered_Championtime(0)
      filtered_Totaltime(0)
      filtered_Ping(-1)
      player_selection(0)
      filter_player(0)
      game_selection(0)
      round_selection(-1)
      round_player_selection(0)
      
      
    }
  )
  
  ####################
  ###CHAMPION IMAGE###
  ####################
  
  selected_champ <- reactive({
    
    req(input$champion != 'None')
    

    data <- filter(champion_crosswalk, Value == input$champion)
    filename <- data$`Icon File Name`
    imagelink <- paste0('https://github.com/Skywind555/Personal-Projects/blob/master/ImagesConverted/', 
                        filename, '.png?raw=true')
    

    
  })
  
  output$ChampImage <- renderUI({
    
    req(input$champion != 'None')

    tags$img(src = selected_champ())
    
  })
  
  ##############################
  ###PICKRATEADJUSTED BUTTONS###
  ##############################
  
  Pickrateadjusted_OverallMeasure <- reactiveVal(0)
  Pickrateadjusted_RegionGroup <- reactiveVal(0)
  Pickrateadjusted_Region <- reactiveVal(0)
  Pickrateadjusted_TotalTime <- reactiveVal(0)
  Pickrateadjusted_ChampionTime <- reactiveVal(0)
  Pickrateadjusted_BestOverallBattlerites <- reactiveVal(0)
  Pickrateadjusted_Battlerites <- reactiveVal(0)
  Pickrateadjusted_League <- reactiveVal(0)
  Pickrateadjusted_ServerType <- reactiveVal(0)
  Pickrateadjusted_PlayerType <- reactiveVal(0)
  Pickrateadjusted_Date <- reactiveVal(0)
  Pickrateadjusted_Map <- reactiveVal(0)
  Pickrateadjusted_Casual <- reactiveVal(0)
  Pickrateadjusted_Ping <- reactiveVal(0)
  Pickrateadjusted_Round_Stats <- reactiveVal(0)
  Pickrateadjusted_Comps <- reactiveVal(0)
  Pickrateadjusted_Comps_Matchups <- reactiveVal(0)
  Pickrateadjusted_TeamRoles <- reactiveVal(0)
  Pickrateadjusted_Mount <- reactiveVal(0)
  Pickrateadjusted_Title <- reactiveVal(0)
  Pickrateadjusted_Avatar <- reactiveVal(0)
  Pickrateadjusted_Outfit <- reactiveVal(0)
  Pickrateadjusted_Attachment <- reactiveVal(0)
  Pickrateadjusted_Pose <- reactiveVal(0)
  

  output$Pick_Rate_Adjusted_OverallMeasure <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_OverallMeasure() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_OverallMeasure_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_OverallMeasure <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_OverallMeasure() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_OverallMeasure_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Overall measure pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_OverallMeasure_Button,
    handlerExpr = {
      
      Pickrateadjusted_OverallMeasure(1)

    }
  )
  
  #Overall measure pick rate button
  observeEvent(
    eventExpr = input$Pickrate_OverallMeasure_Button,
    handlerExpr = {
      
      Pickrateadjusted_OverallMeasure(0)
      
    }
  )
  
  
  output$Pick_Rate_Adjusted_RegionGroup <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_RegionGroup() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_RegionGroup_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_RegionGroup <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_RegionGroup() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_RegionGroup_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Region Group pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_RegionGroup_Button,
    handlerExpr = {
      
      Pickrateadjusted_RegionGroup(1)
      
    }
  )
  
  #Region Group pick rate button
  observeEvent(
    eventExpr = input$Pickrate_RegionGroup_Button,
    handlerExpr = {
      
      Pickrateadjusted_RegionGroup(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_Region <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Region() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Region_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Region <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Region() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Region_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Region pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Region_Button,
    handlerExpr = {
      
      Pickrateadjusted_Region(1)
     
    }
  )
  
  #Region pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Region_Button,
    handlerExpr = {
      
      Pickrateadjusted_Region(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_TotalTime <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_TotalTime() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_TotalTime_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_TotalTime <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_TotalTime() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_TotalTime_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #TotalTime pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_TotalTime_Button,
    handlerExpr = {
      
      Pickrateadjusted_TotalTime(1)
     
    }
  )
  
  #TotalTime pick rate button
  observeEvent(
    eventExpr = input$Pickrate_TotalTime_Button,
    handlerExpr = {
      
      Pickrateadjusted_TotalTime(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_ChampionTime <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_ChampionTime() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_ChampionTime_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_ChampionTime <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_ChampionTime() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_ChampionTime_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #ChampionTime pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_ChampionTime_Button,
    handlerExpr = {
     
      Pickrateadjusted_ChampionTime(1)
    
    }
  )
  
  #ChampionTime pick rate button
  observeEvent(
    eventExpr = input$Pickrate_ChampionTime_Button,
    handlerExpr = {
      
      Pickrateadjusted_ChampionTime(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_BestOverallBattlerites <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_BestOverallBattlerites() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_BestOverallBattlerites_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_BestOverallBattlerites <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_BestOverallBattlerites() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_BestOverallBattlerites_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #BestOverallBattlerites pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_BestOverallBattlerites_Button,
    handlerExpr = {
     
      Pickrateadjusted_BestOverallBattlerites(1)
     
    }
  )
  
  #BestOverallBattlerites pick rate button
  observeEvent(
    eventExpr = input$Pickrate_BestOverallBattlerites_Button,
    handlerExpr = {
      
      Pickrateadjusted_BestOverallBattlerites(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_Battlerites <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Battlerites() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Battlerites_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Battlerites <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Battlerites() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Battlerites_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Battlerites pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Battlerites_Button,
    handlerExpr = {
    
      Pickrateadjusted_Battlerites(1)
    
    }
  )
  
  #Battlerites pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Battlerites_Button,
    handlerExpr = {
      
      Pickrateadjusted_Battlerites(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_League <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_League() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_League_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_League <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_League() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_League_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #League pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_League_Button,
    handlerExpr = {
      
      Pickrateadjusted_League(1)
    
    }
  )
  
  #League pick rate button
  observeEvent(
    eventExpr = input$Pickrate_League_Button,
    handlerExpr = {
      
      Pickrateadjusted_League(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_ServerType <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_ServerType() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_ServerType_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_ServerType <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_ServerType() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_ServerType_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #ServerType pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_ServerType_Button,
    handlerExpr = {
     
      Pickrateadjusted_ServerType(1)
     
    }
  )
  
  #ServerType pick rate button
  observeEvent(
    eventExpr = input$Pickrate_ServerType_Button,
    handlerExpr = {
      
      Pickrateadjusted_ServerType(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_PlayerType <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_PlayerType() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_PlayerType_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_PlayerType <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_PlayerType() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_PlayerType_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #PlayerType pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_PlayerType_Button,
    handlerExpr = {
    
      Pickrateadjusted_PlayerType(1)
     
    }
  )
  
  #PlayerType pick rate button
  observeEvent(
    eventExpr = input$Pickrate_PlayerType_Button,
    handlerExpr = {
      
      Pickrateadjusted_PlayerType(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_Date <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Date() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Date_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Date <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Date() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Date_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Date pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Date_Button,
    handlerExpr = {
     
      Pickrateadjusted_Date(1)
     
    }
  )
  
  #Date pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Date_Button,
    handlerExpr = {
      
      Pickrateadjusted_Date(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_Map <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Map() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Map_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Map <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Map() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Map_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Map pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Map_Button,
    handlerExpr = {
     
      Pickrateadjusted_Map(1)
    
    }
  )
  
  #Map pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Map_Button,
    handlerExpr = {
      
      Pickrateadjusted_Map(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_Casual <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Casual() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Casual_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Casual <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Casual() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Casual_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Casual pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Casual_Button,
    handlerExpr = {
      
      Pickrateadjusted_Casual(1)
    
    }
  )
  
  #Casual pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Casual_Button,
    handlerExpr = {
      
      Pickrateadjusted_Casual(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_Ping <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Ping() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Ping_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Ping <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Ping() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Ping_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Ping pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Ping_Button,
    handlerExpr = {
    
      Pickrateadjusted_Ping(1)
     
    }
  )
  
  #Ping pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Ping_Button,
    handlerExpr = {
      
      Pickrateadjusted_Ping(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_Round_Stats <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Round_Stats() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Round_Stats_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Round_Stats <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Round_Stats() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Round_Stats_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Round_Stats pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Round_Stats_Button,
    handlerExpr = {
    
      Pickrateadjusted_Round_Stats(1)
    
    }
  )
  
  #Round_Stats pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Round_Stats_Button,
    handlerExpr = {
      
      Pickrateadjusted_Round_Stats(0)
      
    }
  )
 
  output$Pick_Rate_Adjusted_Comps <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Comps() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Comps_Button',
                 label = 'Pick rate (adjusted) [long]',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Comps <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Comps() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Comps_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Comps pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Comps_Button,
    handlerExpr = {
    
      Pickrateadjusted_Comps(1)
    
    }
  )
  
  #Comps pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Comps_Button,
    handlerExpr = {
      
      Pickrateadjusted_Comps(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_Comps_Matchups <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Comps_Matchups() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Comps_Matchups_Button',
                 label = 'Pick rate (adjusted) [long]',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Comps_Matchups <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Comps_Matchups() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Comps_Matchups_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Comps_Matchups pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Comps_Matchups_Button,
    handlerExpr = {
    
      Pickrateadjusted_Comps_Matchups(1)
    
    }
  )
  
  #Comps_Matchups pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Comps_Matchups_Button,
    handlerExpr = {
      
      Pickrateadjusted_Comps_Matchups(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_TeamRoles <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_TeamRoles() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_TeamRoles_Button',
                 label = 'Pick rate (adjusted) [Both text blocks above]',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_TeamRoles <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_TeamRoles() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_TeamRoles_Button',
                 label = 'Pick rate [Both text blocks above]',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Team Roles Best pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_TeamRoles_Button,
    handlerExpr = {
      
      Pickrateadjusted_TeamRoles(1)
      
    }
  )
  
  #Team Roles Best pick rate button
  observeEvent(
    eventExpr = input$Pickrate_TeamRoles_Button,
    handlerExpr = {
      
      Pickrateadjusted_TeamRoles(0)
      
    }
  )
  
  output$Pick_Rate_Adjusted_Mount <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Mount() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Mount_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Mount <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Mount() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Mount_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Mount pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Mount_Button,
    handlerExpr = {
     
      Pickrateadjusted_Mount(1)
    
    }
  )
  
  #Mount pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Mount_Button,
    handlerExpr = {
      
      Pickrateadjusted_Mount(0)
      
    }
  ) 
  
  output$Pick_Rate_Adjusted_Title <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Title() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Title_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Title <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Title() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Title_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Title pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Title_Button,
    handlerExpr = {
    
      Pickrateadjusted_Title(1)
    
    }
  )
  
  #Title pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Title_Button,
    handlerExpr = {
      
      Pickrateadjusted_Title(0)
      
    }
  ) 
  
  output$Pick_Rate_Adjusted_Avatar <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Avatar() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Avatar_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Avatar <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Avatar() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Avatar_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Avatar pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Avatar_Button,
    handlerExpr = {
    
      Pickrateadjusted_Avatar(1)
    
    }
  )
  
  #Avatar pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Avatar_Button,
    handlerExpr = {
      
      Pickrateadjusted_Avatar(0)
      
    }
  ) 
  
  output$Pick_Rate_Adjusted_Outfit <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Outfit() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Outfit_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Outfit <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Outfit() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Outfit_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Outfit pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Outfit_Button,
    handlerExpr = {
    
      Pickrateadjusted_Outfit(1)
    
    }
  )
  
  #Outfit pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Outfit_Button,
    handlerExpr = {
      
      Pickrateadjusted_Outfit(0)
      
    }
  ) 
  
  output$Pick_Rate_Adjusted_Attachment <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Attachment() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Attachment_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Attachment <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Attachment() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Attachment_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Attachment pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Attachment_Button,
    handlerExpr = {
     
      Pickrateadjusted_Attachment(1)
     
    }
  )
  
  #Attachment pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Attachment_Button,
    handlerExpr = {
      
      Pickrateadjusted_Attachment(0)
      
    }
  ) 
  
  output$Pick_Rate_Adjusted_Pose <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Pose() == 0)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrateadjusted_Pose_Button',
                 label = 'Pick rate (adjusted)',
                 style='padding:4px; font-size:75%')
    
  })
  
  output$Pick_Rate_Pose <- renderUI({
    
    req(input$measure == 'pickrate')
    req(Pickrateadjusted_Pose() == 1)
    req(input$champion != 'None')
    
    actionButton(inputId = 'Pickrate_Pose_Button',
                 label = 'Pick rate',
                 style='padding:4px; font-size:75%')
    
  })
  
  #Pose pick rate adjusted button
  observeEvent(
    eventExpr = input$Pickrateadjusted_Pose_Button,
    handlerExpr = {
     
      Pickrateadjusted_Pose(1)
      
    }
  )
  
  #Pose pick rate button
  observeEvent(
    eventExpr = input$Pickrate_Pose_Button,
    handlerExpr = {
      
      Pickrateadjusted_Pose(0)
      
    }
  ) 
  
  
  
  #####################
  ###OVERALL WINRATE###
  #####################
  
  #Overall win rate or pick rate
  overallmeasure <- reactive({
    
    req(filtered_Region() == 0 & length(filtered_Battlerites$battlerites) == 0 & filtered_League() == 0 &
        filtered_Servertype() == 0 & filtered_Map() == 0 & filtered_Casual() == 0 &
        filtered_Mount() == 0 & filtered_Title() == 0 & filtered_Avatar() == 0 &
        filtered_Outfit() == 0 & filtered_Attachment() == 0 & filtered_Pose() == 0 &
        filtered_RegionGroup() == 0 & filtered_PlayerType() == 0 & filtered_Date() == 0 &
        filtered_Championtime() == 0 & filtered_Totaltime() == 0 & filtered_Ping() == -1,
        filter_player() == 0)
    
    
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won) %>%
      group_by(Game_ID, User_ID) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    
    if (input$measure == 'winrateadjusted') {
      
      data <- data %>%
        ungroup() %>%
        group_by(User_ID) %>%
        summarize(Game_Won = mean(Game_Won))
      
    }
    
    if (input$measure == 'pickrate') {
      
      champ_pick_size <- dim(data)[1]
      output <- round(champ_pick_size*100/Total_Picks_Overall(), 2)
      
    }
    
    if (input$measure == 'pickrate' & Pickrateadjusted_OverallMeasure() == 1) {
        
        data <- df() %>%
          group_by(Game_ID, User_ID, Champion) 
        
        data <- data %>%
          ungroup() %>%
          filter(Champion == input$champion) %>%
          group_by(User_ID, Champion) %>%
          summarize(Num_Picks = n())
        
        data <- left_join(x = User_Total_Games_df(), y = data, by = c('User_ID' = 'User_ID'))
        data <- mutate(data, Num_Picks = ifelse(is.na(Num_Picks), 0, Num_Picks))
        
        
        data <- mutate(data, Pick_Rate = Num_Picks/Total_Games)
        output <- round(mean(data$Pick_Rate)*100,2)
      
    }
    
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
    
    output <- round(mean(data$Game_Won)*100,2)
    
    }
    
    output
    
  })
  
  output$OverallMeasure <- renderUI({
    
    req(filtered_Region() == 0 & length(filtered_Battlerites$battlerites) == 0 & filtered_League() == 0 &
          filtered_Servertype() == 0 & filtered_Map() == 0 & filtered_Casual() == 0 &
          filtered_Mount() == 0 & filtered_Title() == 0 & filtered_Avatar() == 0 &
          filtered_Outfit() == 0 & filtered_Attachment() == 0 & filtered_Pose() == 0 &
          filtered_RegionGroup() == 0 & filtered_PlayerType() == 0 & filtered_Date() == 0 &
          filtered_Championtime() == 0 & filtered_Totaltime() == 0 & filtered_Ping() == -1 &
          filter_player() == 0) 
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
    
    HTML(paste0('<font size = "3"><b> Overall win rate: ',
                overallmeasure(),
                ' percent.</font></b>'))
      
    } else {
      
      HTML(paste0('<font size = "3"><b> Overall pick rate: ',
                  overallmeasure(),
                  ' percent.</font></b>'))
      
    }
    
  })
  
  #Separate reactive value for total pick size for the champion
  Champion_Pick_Size <- reactive({
    
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won) %>%
      group_by(Game_ID, User_ID) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    
    output <- dim(data)[1]

  })
  
  Baseline_Champion_Pick_Size <- reactive ({
    
    data <- filtered_data2() %>%
      select(Game_ID, User_ID, Round_Won) %>%
      group_by(Game_ID, User_ID) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    
    output <- dim(data)[1]
    
    
  })
  
  #######################################
  ###INTERACTIVE FILTER ACTION BUTTONS###
  #######################################
  
  output$Interactive_ResetButton <- renderUI({
    
    req(input$champion != 'None')
    
    req(filtered_Region() != 0 | length(filtered_Battlerites$battlerites) != 0 | filtered_League() != 0 |
          filtered_Servertype() != 0 | filtered_Map() != 0 | filtered_Casual() != 0 |
          filtered_Mount() != 0 | filtered_Title() != 0 | filtered_Avatar() != 0 |
          filtered_Outfit() != 0 | filtered_Attachment() != 0 | filtered_Pose() != 0 |
          filtered_RegionGroup() != 0 | filtered_PlayerType() != 0 | filtered_Date() != 0 |
          filtered_Championtime() != 0 | filtered_Totaltime() != 0 | filtered_Ping() != -1 |
          filter_player() != 0)
    
    actionButton(inputId = 'ResetAll',
                 label = 'Reset All Filters')
    
  })
  
  
  output$Interactive_Unfilter_RegionGroup <- renderUI({
    
    req(filtered_RegionGroup() != 0)
    
    actionButton(inputId = 'unfilterRegionGroup',
                 label = 'Unfilter Region Group',
                 style='padding:4px; font-size:75%')
  })
  
  output$Interactive_Unfilter_Region <- renderUI({
    
    req(filtered_Region() != 0)
    
    actionButton(inputId = 'unfilterRegion',
                 label = 'Unfilter Region',
                 style='padding:4px; font-size:75%')
  })
  
  output$Interactive_Filter_Player <- renderUI({
    
    req(player_selection() != 0)
    req(filter_player() == 0)
    
    actionButton(inputId = 'filterPlayer',
                 label = 'Filter Selected Player',
                 style='padding:4px; font-size:75%')
  })
  
  output$Interactive_Unfilter_Player <- renderUI({
    
    req(filter_player() != 0)
    
    actionButton(inputId = 'unfilterPlayer',
                 label = 'Unfilter Selected Player',
                 style='padding:4px; font-size:75%')
  })
  
  output$Interactive_Repick_Battlerites <- renderUI({
    
    req(length(filtered_Battlerites$battlerites) > 0) 
    
    actionButton(inputId = 'unfilterBattlerites',
                 label = 'Repick Battlerites',
                 style='padding:4px; font-size:75%')
    
  })
  
  ############
  ###REGION###
  ############
  
  
  #Create aggregate region data
  Region_df <- reactive({region_agg(filtered_data(),
                                    input$measure,
                                    Pickrateadjusted_Region())})
  
  
  
  #Filter Region observe event
  observeEvent(
    eventExpr = input$Region_marker_click,
    handlerExpr = {
      
      OE_filterRegion(filtered_Region, input$Region_marker_click$id)
      
    }
  )
  
  #Unfilter button
  observeEvent(
    eventExpr = input$unfilterRegion,
    handlerExpr = {
      
      filtered_Region(0)
      
    }
  )
  
  #Create Region plot
  output$Region <- renderLeaflet({create_region_map(Region_df(), input$champion, region_coordinates, input$measure)
  })
  
  #Filtered region label
  output$Chosen_Region <- renderUI({
    
    req(input$champion != 'None')
    
    create_region_label(input$measure, filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                                        filtered_League(), filtered_Servertype(), filtered_Map(),
                                                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                                                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                                        filter_player(), player_selection(), Pickrateadjusted_Region())
    
    })
  
  ##################
  ###REGION GROUP###
  ##################
  
  #Pre agg
  pre_agg_regiongroup <- reactive({pre_agg(filtered_data(),
                                           input$measure,
                                           'Region Group',
                                           FALSE,
                                           Pickrateadjusted_RegionGroup())})
  
  RegionGroup_df <- reactive({
    
    common_agg(pre_agg_regiongroup(),
               input$measure,
               'Region Group',
               FALSE,
               Pickrateadjusted_RegionGroup())
    
    
    })
  
  
  #Filter Region Group observe event
  observeEvent(
    eventExpr = input$RegionGroupRow,
    handlerExpr = {
      
      row <- input$RegionGroup_cell_clicked
      selected_regiongroup <- RegionGroup_df()[row$row, 'Region Group']$`Region Group`
      filtered_RegionGroup(selected_regiongroup)
      
    }
  )
  
  #Unfilter button
  observeEvent(
    eventExpr = input$unfilterRegionGroup,
    handlerExpr = {
      
      filtered_RegionGroup(0)
      filtered_Region(0)
      
    }
  )
  
  
  output$RegionGroup <- DT::renderDataTable(expr = {
    
    create_region_table(RegionGroup_df(), "'RegionGroupRow'", input$champion, input$measure,
                        filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                        filtered_League(), filtered_Servertype(), filtered_Map(),
                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                        filter_player(), player_selection(), Pickrateadjusted_RegionGroup())
    
  })
  
  #######################
  ###TOTAL TIME PLAYED###
  #######################
  
  pre_agg_totaltime <- reactive({pre_agg(filtered_data(),
                                    input$measure,
                                    'Total_Time_Played',
                                    FALSE,
                                    Pickrateadjusted_TotalTime())})
  
  Totaltime_df <- reactive({common_agg(pre_agg_totaltime(),
                                  input$measure,
                                  'Total_Time_Played',
                                  FALSE,
                                  Pickrateadjusted_TotalTime())})
  #Filter Totaltime observe event
  observeEvent(
    eventExpr = input$filterTotalTime,
    handlerExpr = {
      
      OE_Filter_common(Totaltime_df(), filtered_Totaltime, input$filterTotalTime$y, 'Total_Time_Played')
      
    }
  )
  
  #Unfilter Totaltime event
  observeEvent(
    eventExpr = input$unfilterTotalTime,
    handlerExpr = {
      
      filtered_Totaltime(0)
      
    }
  )
  
  #Create barplot
  
  output$TotalTime <- renderPlot({
    
    create_barplot(Totaltime_df(), 'Total_Time_Played', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_TotalTime())
    
  })
  
  #Totaltime hover label
  output$TotalTime_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverTotalTime, Totaltime_df(), Totaltime_df()$Total_Time_Played,
                           input$measure, 40, -30)
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  ##########################
  ###CHAMPION TIME PLAYED###
  ##########################
  
  pre_agg_championtime <- reactive({pre_agg(filtered_data(),
                                         input$measure,
                                         'Champion_Time_Played',
                                         FALSE,
                                         Pickrateadjusted_ChampionTime())})
  
  Championtime_df <- reactive({common_agg(pre_agg_championtime(),
                                       input$measure,
                                       'Champion_Time_Played',
                                       FALSE,
                                       Pickrateadjusted_ChampionTime())})
  #Filter Totaltime observe event
  observeEvent(
    eventExpr = input$filterChampionTime,
    handlerExpr = {
      
      OE_Filter_common(Championtime_df(), filtered_Championtime, input$filterChampionTime$y, 'Champion_Time_Played')
      
    }
  )
  
  #Unfilter Championtime event
  observeEvent(
    eventExpr = input$unfilterChampionTime,
    handlerExpr = {
      
      filtered_Championtime(0)
      
    }
  )
  
  #Create barplot
  
  output$ChampionTime <- renderPlot({
    
    create_barplot(Championtime_df(), 'Champion_Time_Played', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_ChampionTime())
    
  })
  
  #Totaltime hover label
  output$ChampionTime_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverChampionTime, Championtime_df(), Championtime_df()$Champion_Time_Played, 
                           input$measure, 40, -30)
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  
  
  ############
  ###LEAGUE###
  ############
  
  pre_agg_league <- reactive({pre_agg(filtered_data(),
                                      input$measure,
                                      'League',
                                      FALSE,
                                      Pickrateadjusted_League())})
  
  League_df <- reactive({common_agg(pre_agg_league(),
                                    input$measure,
                                    'League',
                                    FALSE,
                                    Pickrateadjusted_League())})
  
  #Filter League observe event
  observeEvent(
    eventExpr = input$filterLeague,
    handlerExpr = {
      
      OE_Filter_common(League_df(), filtered_League, input$filterLeague$y, 'League')
      
    }
  )
  
  #Unfilter League
  observeEvent(
    eventExpr = input$unfilterLeague,
    handlerExpr = {
      
      filtered_League(0)
      
    }
  )
  
  #League Bar plot
  output$League <- renderPlot({
    
    req(input$champion != 'None')
    create_barplot(League_df(), 'League', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_League())})
  
  #League hover label
  output$League_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverLeague, League_df(), League_df()$League,
                           input$measure, 40, -30)
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  #################
  ###PLAYER TYPE###
  #################
  
  pre_agg_playertype <- reactive({pre_agg(filtered_data(),
                                          input$measure,
                                          'Player_Type',
                                          FALSE,
                                          Pickrateadjusted_PlayerType())})
  
  PlayerType_df <- reactive({common_agg(pre_agg_playertype(),
                                        input$measure,
                                        'Player_Type',
                                        FALSE,
                                        Pickrateadjusted_PlayerType())})
  
  #Filter PlayerType observe event
  observeEvent(
    eventExpr = input$filterPlayerType,
    handlerExpr = {
      
      OE_Filter_common(PlayerType_df(), filtered_PlayerType, input$filterPlayerType$y, 'Player_Type')
      
    }
  )
  
  #Unfilter PlayerType event
  observeEvent(
    eventExpr = input$unfilterPlayerType,
    handlerExpr = {
      
      filtered_PlayerType(0)
      
    }
  )
  
  #Create barplot
  
  output$PlayerType <- renderPlot({
    
    create_barplot(PlayerType_df(), 'Player_Type', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_PlayerType())
    
  })
  
  #PlayerType hover label
  output$PlayerType_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverPlayerType, PlayerType_df(), PlayerType_df()$Player_Type, 
                           input$measure, 40, -30)
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  
  ##########
  ###DATE###
  ##########
  
  pre_agg_date <- reactive({pre_agg(filtered_data(),
                                          input$measure,
                                          'Date',
                                           FALSE,
                                    Pickrateadjusted_Date())})
  
  date_df <- reactive({common_agg(pre_agg_date(),
                                        input$measure,
                                        'Date',
                                        FALSE,
                                  Pickrateadjusted_Date())})
  #Filter Date observe event
  observeEvent(
    eventExpr = input$filterDate,
    handlerExpr = {
      
      OE_Filter_common(date_df(), filtered_Date, input$filterDate$y, 'Date')
      
    }
  )
  
  #Unfilter Date event
  observeEvent(
    eventExpr = input$unfilterDate,
    handlerExpr = {
      
      filtered_Date(0)
      
    }
  )
  
  #Create barplot
  
  output$Date <- renderPlot({
    
    create_barplot(date_df(), 'Date', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_Date())
    
  })
  
  #Date hover label
  output$Date_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverDate, date_df(), date_df()$Date,
                           input$measure, 40, -30)
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  ##########
  ###PING###
  ##########
  
  pre_agg_ping <- reactive({
    
    pre_agg(filtered_data(),
            input$measure,
            'Ping',
            FALSE,
            Pickrateadjusted_Ping())
    
  })
  
  agg_ping <- reactive({
    
    common_agg(pre_agg_ping(),
               input$measure,
               'Ping',
               FALSE,
               Pickrateadjusted_Ping())
    
  })
  
  
  observeEvent(
    eventExpr = input$filterPing,
    handlerExpr = {
      
      OE_Filter_common(agg_ping(), filtered_Ping, input$filterPing$x, 'Ping')
      
    }
  )
  
  observeEvent(
    eventExpr = input$unfilterPing,
    handlerExpr = {
      
      filtered_Ping(-1)
      
    }
  )
  
  output$Ping <- renderPlot({

    create_barplot(agg_ping(), 'Ping', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_Ping()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$Ping_tooltip <- renderUI({
    
      info <- create_tooltip2(input$hoverPing, agg_ping(), agg_ping()$Ping, 
                              input$measure, -25, 210)
      
      if (!is.null(info$Value)) {
        
        wellPanel(
          style = info$style,
          p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
        )
        
      }
      
    
  })
  
  
  ################
  ###SERVERTYPE###
  ################
  
  pre_agg_servertype <- reactive({pre_agg(filtered_data(),
                                          input$measure,
                                          'Server_Type',
                                          FALSE,
                                          Pickrateadjusted_ServerType())})
  
  Servertype_df <- reactive({common_agg(pre_agg_servertype(),
                                        input$measure,
                                        'Server_Type',
                                        FALSE,
                                        Pickrateadjusted_ServerType())})
  
  #Filter ServerType observe event
  observeEvent(
    eventExpr = input$filterServerType,
    handlerExpr = {
      
      OE_Filter_common(Servertype_df(), filtered_Servertype, input$filterServerType$y, 'Server_Type')
      
    }
  )
  
  #Unfilter ServerType event
  observeEvent(
    eventExpr = input$unfilterServerType,
    handlerExpr = {
      
      filtered_Servertype(0)
      
    }
  )
  
  #Create barplot
  
  output$ServerType <- renderPlot({
    
    create_barplot(Servertype_df(), 'Server_Type', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_ServerType())
    
  })
  
  #Matchtype hover label
  output$ServerType_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverServerType, Servertype_df(), Servertype_df()$Server_Type,
                           input$measure, 40, 268)
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  #########
  ###MAP###
  #########
  
  pre_agg_map <- reactive({pre_agg(filtered_data(),
                                   input$measure,
                                   'Map',
                                   FALSE,
                                   Pickrateadjusted_Map())})
  
  Map_df <- reactive({
    
    data <- common_agg(pre_agg_map(),
                       input$measure,
                       'Map',
                       TRUE,
                       Pickrateadjusted_Map()) %>%
      mutate(Type = ifelse(str_sub(Map, -3, -1) == 'DAY', 'DAY', 'NIGHT'))
    data$Type <- factor(data$Type)
    data

    })
  
  #Filter ServerType observe event
  observeEvent(
    eventExpr = input$filterMap,
    handlerExpr = {
      
      OE_Filter_common(Map_df(), filtered_Map, input$filterMap$y, 'Map')
      
    }
  )
  
  #Unfilter Map event
  observeEvent(
    eventExpr = input$unfilterMap,
    handlerExpr = {
      
      filtered_Map(0)
      
    }
  )
  
  #Create barplot
  
  output$Map <- renderPlot({
    
    create_barplot(Map_df(), 'Map', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_Map())
    
  })
  
  #Matchtype hover label
  output$Map_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverMap, Map_df(), Map_df()$Map,
                           input$measure, 40, -30)
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  ##################
  ###RANKING TYPE###
  ##################
  
  pre_agg_casual <- reactive({pre_agg(filtered_data(),
                                      input$measure,
                                      'Ranking_Type',
                                      FALSE,
                                      Pickrateadjusted_Casual())})
  
  Casual_df <- reactive({common_agg(pre_agg_casual(),
                                    input$measure,
                                    'Ranking_Type',
                                    FALSE,
                                    Pickrateadjusted_Casual())})
  
  #Filter Ranking_Type observe event
  observeEvent(
    eventExpr = input$filterCasual,
    handlerExpr = {
      
      OE_Filter_common(Casual_df(), filtered_Casual, input$filterCasual$y, 'Ranking_Type')
      
    }
  )
  
  #Unfilter Ranking_Type event
  observeEvent(
    eventExpr = input$unfilterCasual,
    handlerExpr = {
      
      filtered_Casual(0)
      
    }
  )
  
  #Create barplot
  
  output$Casual <- renderPlot({
    
    create_barplot(Casual_df(), 'Ranking_Type', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_Casual())
    
  })
  
  #Ranking_Type hover label
  output$Casual_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverCasual, Casual_df(), Casual_df()$Ranking_Type, 
                           input$measure, 40, -30)
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  #########################
  ###INTERACTIVE SLIDERS###
  #########################
  
  Avatar_breaks <- reactive ({
    
    ceiling(dim(Avatar_df())[1]/45)
    
  })
  
  output$Interactive_Slider_Avatar <- renderUI({
    
    req(!is.null(Avatar_df()))
    
      sliderInput(inputId = 'View_Avatar',
                  label = 'Range of Avatar Data',
                  min = 1,
                  max = Avatar_breaks(),
                  value = 1,
                  step = 1)

  })
  
  Title_breaks <- reactive({
    
    ceiling(dim(Title_df())[1]/45)
    
  })
  
  output$Interactive_Slider_Title <- renderUI({
    
    req(!is.null(Title_df()))
    
    sliderInput(inputId = 'View_Title',
                label = 'Range of Title Data',
                min = 1,
                max = Title_breaks(),
                value = 1,
                step = 1)
    
  })
  
  Mount_breaks <- reactive({
    
    ceiling(dim(Mount_df())[1]/45)
    
  })
  
  output$Interactive_Slider_Mount <- renderUI({
    
    req(!is.null(Mount_df()))
    
    sliderInput(inputId = 'View_Mount',
                label = 'Range of Mount Data',
                min = 1,
                max = Mount_breaks(),
                value = 1,
                step = 1)
    
  })
  
  
  
  ############
  ###AVATAR###
  ############
  
  pre_agg_avatar <- reactive({pre_agg(filtered_data(),
                                      input$measure,
                                      'Avatar',
                                      FALSE,
                                      Pickrateadjusted_Avatar())})
  
  Avatar_df <- reactive({
    
            common_agg(pre_agg_avatar(),
                       input$measure,
                       'Avatar',
                       TRUE,
                       Pickrateadjusted_Avatar())
    })
  
  Avatar_split_df <- reactive({
    
    req(!is.null(Avatar_df()))
    req(!is.null(input$View_Avatar))
    req(Avatar_breaks() > 1)
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      data <- arrange(Avatar_df(), desc(Win_Rate))
      
    } else {
      
      data <- arrange(Avatar_df(), desc(Pick_Rate))
      
    }
    
    
    if (input$View_Avatar == Avatar_breaks()) {
      
      data <- data[(45*(input$View_Avatar-1)+1):dim(data)[1],]
      
    } else {
    
      data <- data[(45*(input$View_Avatar-1)+1):(input$View_Avatar*45),]
      
    }
    
    data
    
  })
  
  #Filter Avatar observe event
  observeEvent(
    eventExpr = input$filterAvatar,
    handlerExpr = {
      
      OE_Filter_common(Avatar_df(), filtered_Avatar, input$filterAvatar$y, 'Avatar')
      
    }
  )
  
  #Unfilter Avatar event
  observeEvent(
    eventExpr = input$unfilterAvatar,
    handlerExpr = {
      
      filtered_Avatar(0)
      
    }
  )
  
  #Create barplot
  
  output$Avatar <- renderPlot({
    
    if (Avatar_breaks() > 1) {
      
      bar <- create_barplot(Avatar_split_df(), 'Avatar', input$measure,
                     filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                     filtered_League(), filtered_Servertype(), filtered_Map(),
                     filtered_Casual(), filtered_Mount(), filtered_Title(),
                     filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                     filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                     filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                     filter_player(), player_selection(), Pickrateadjusted_Avatar())
      
    } else {
    
    bar <- create_barplot(Avatar_df(), 'Avatar', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_Avatar())
   
    }
    
    bar
    
  })
  
  #Avatar hover label
  output$Avatar_tooltip <- renderUI({
    
    req(!is.null(input$hoverAvatar))
    
    if (Avatar_breaks() > 1) {
     
      info <- create_tooltip(input$hoverAvatar, Avatar_split_df(), Avatar_split_df()$Avatar, 
                             input$measure, 40, -30)
       
    } else {
    
    info <- create_tooltip(input$hoverAvatar, Avatar_df(), Avatar_df()$Avatar, 
                           input$measure, 40, -30)
    
    }
    selected_avatar <- info$Name
    filename <- filter(avatar_crosswalk, Value == selected_avatar)$`Avatar File Name`
    imagelink <- paste0('https://github.com/Skywind555/Personal-Projects/blob/master/ImagesConverted/', 
                        filename, '.png?raw=true')
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>"))),
        tags$img(src = imagelink)
      )
      
    }
    
  })
  
  ###########
  ###TITLE###
  ###########
  
  pre_agg_title <- reactive({pre_agg(filtered_data(),
                                     input$measure,
                                     'Title',
                                     FALSE,
                                     Pickrateadjusted_Title())})
  
  Title_df <- reactive({common_agg(pre_agg_title(),
                                   input$measure,
                                   'Title',
                                   TRUE,
                                   Pickrateadjusted_Title())})
  
  Title_split_df <- reactive({
    
    req(!is.null(Title_df()))
    req(!is.null(input$View_Title))
    req(Title_breaks() > 1)
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      data <- arrange(Title_df(), desc(Win_Rate))
      
    } else {
      
      data <- arrange(Title_df(), desc(Pick_Rate))
      
    }
    
    if (input$View_Title == Title_breaks()) {
      
      data <- data[(45*(input$View_Title-1)+1):dim(data)[1],]
      
    } else {
      
      data <- data[(45*(input$View_Title-1)+1):(input$View_Title*45),]
      
    }
    
    data
    
  })
  
  #Filter Title observe event
  observeEvent(
    eventExpr = input$filterTitle,
    handlerExpr = {
      
      OE_Filter_common(Title_df(), filtered_Title, input$filterTitle$y, 'Title')
      
    }
  )
  
  #Unfilter Title event
  observeEvent(
    eventExpr = input$unfilterTitle,
    handlerExpr = {
      
      filtered_Title(0)
      
    }
  )
  
  #Create barplot
  
  output$Title <- renderPlot({
    
    if (Title_breaks() > 1) {
      
      create_barplot(Title_split_df(), 'Title', input$measure,
                     filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                     filtered_League(), filtered_Servertype(), filtered_Map(),
                     filtered_Casual(), filtered_Mount(), filtered_Title(),
                     filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                     filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                     filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                     filter_player(), player_selection(), Pickrateadjusted_Title())
      
    } else {
    
    create_barplot(Title_df(), 'Title', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_Title())
      
    }
    
  })
  
  #Title hover label
  output$Title_tooltip <- renderUI({
    
    if (Title_breaks() > 1) {
      
      info <- create_tooltip(input$hoverTitle, Title_split_df(), Title_split_df()$Title, 
                             input$measure, 40, -30)
      
    } else {
    
    info <- create_tooltip(input$hoverTitle, Title_df(), Title_df()$Title, 
                           input$measure, 40, -30)
    
    }
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  ###########
  ###MOUNT###
  ###########
  
  pre_agg_mount <- reactive({pre_agg(filtered_data(),
                                     input$measure,
                                     'Mount',
                                     FALSE,
                                     Pickrateadjusted_Mount())})
  
  Mount_df <- reactive({
    data <- common_agg(pre_agg_mount(),
                       input$measure,
                       'Mount',
                       TRUE,
                       Pickrateadjusted_Mount())
    
    
    
    mount_crosswalk$Value <- factor(mount_crosswalk$Value, levels = levels(data$Mount))
    
    if (!(input$measure == 'pickrate' & Pickrateadjusted_Mount() == 1)) {
      
      data <- inner_join(x = data, y = mount_crosswalk, by = c('Mount' = 'Value'))[-5] 
      
    } else {
      
      data <- inner_join(x = data, y = mount_crosswalk, by = c('Mount' = 'Value'))[-4]
      
    }
    
    data$Rarity <- factor(data$Rarity, levels = c('Common', 'Rare', 'Epic', 'Legendary'))
    data <- data[!duplicated(data$Mount),]
    data
    
  })
  
  Mount_split_df <- reactive({
    
    req(!is.null(Mount_df()))
    req(!is.null(input$View_Mount))
    req(Mount_breaks() > 1)
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      data <- arrange(Mount_df(), desc(Win_Rate))
      
    } else {
      
      data <- arrange(Mount_df(), desc(Pick_Rate))
      
    }
    
    if (input$View_Mount == Mount_breaks()) {
      
      data <- data[(45*(input$View_Mount-1)+1):dim(data)[1],]
      
    } else {
      
      data <- data[(45*(input$View_Mount-1)+1):(input$View_Mount*45),]
      
    }
    
    data
    
  })
  
  #Filter Mount observe event
  observeEvent(
    eventExpr = input$filterMount,
    handlerExpr = {
      
      OE_Filter_common(Mount_df(), filtered_Mount, input$filterMount$y, 'Mount')
      
    }
  )
  
  #Unfilter Mount event
  observeEvent(
    eventExpr = input$unfilterMount,
    handlerExpr = {
      
      filtered_Mount(0)
      
    }
  )
  
  #Create barplot
  
  output$Mount <- renderPlot({
    
    if (Mount_breaks() > 1) {
      
      create_barplot(Mount_split_df(), 'Mount', input$measure,
                     filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                     filtered_League(), filtered_Servertype(), filtered_Map(),
                     filtered_Casual(), filtered_Mount(), filtered_Title(),
                     filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                     filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                     filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                     filter_player(), player_selection(), Pickrateadjusted_Mount())
      
    } else {
    
    create_barplot(Mount_df(), 'Mount', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_Mount())
    
    }
  })
  
  #Mount hover label
  output$Mount_tooltip <- renderUI({
    
    if (Mount_breaks() > 1) {
      
      info <- create_tooltip(input$hoverMount, Mount_split_df(), Mount_split_df()$Mount,
                             input$measure, 40, -30)
      
    } else {
    
    info <- create_tooltip(input$hoverMount, Mount_df(), Mount_df()$Mount,
                           input$measure, 40, -30)
    
    }
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  
  ############
  ###OUTFIT###
  ############
  
  pre_agg_outfit <- reactive({pre_agg(filtered_data(),
                                      input$measure,
                                      'Outfit',
                                      FALSE,
                                      Pickrateadjusted_Outfit())})
  
  Outfit_df <- reactive({
    
    data <- common_agg(pre_agg_outfit(),
                       input$measure,
                       'Outfit',
                       TRUE,
                       Pickrateadjusted_Outfit())
    
    outfit_crosswalk$Value <- factor(outfit_crosswalk$Value, levels = levels(data$Outfit))
    
    if (!(input$measure == 'pickrate' & Pickrateadjusted_Outfit() == 1)) {
      
      data <- inner_join(x = data, y = outfit_crosswalk, by = c('Outfit' = 'Value'))[-5] 
      
    } else {
      
      data <- inner_join(x = data, y = outfit_crosswalk, by = c('Outfit' = 'Value'))[-4]
      
    }
    
    data$Rarity <- factor(data$Rarity, levels = c('Common', 'Rare', 'Epic', 'Legendary'))
    data <- data[!duplicated(data$Outfit),]
    
    })
  
  #Filter Outfit observe event
  observeEvent(
    eventExpr = input$filterOutfit,
    handlerExpr = {
      
      OE_Filter_common(Outfit_df(), filtered_Outfit, input$filterOutfit$y, 'Outfit')
      
    }
  )
  
  #Unfilter Outfit event
  observeEvent(
    eventExpr = input$unfilterOutfit,
    handlerExpr = {
      
      filtered_Outfit(0)
      
    }
  )
  
  #Create barplot
  
  output$Outfit <- renderPlot({
    
    create_barplot(Outfit_df(), 'Outfit', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_Outfit())
    
  })
  
  #Outfit hover label
  output$Outfit_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverOutfit, Outfit_df(), Outfit_df()$Outfit,
                           input$measure, 40, -30)
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  
  ################
  ###ATTACHMENT###
  ################
  
  pre_agg_attachment <- reactive({pre_agg(filtered_data(),
                                          input$measure,
                                          'Attachment',
                                          FALSE,
                                          Pickrateadjusted_Attachment())})
  
  Attachment_df <- reactive({
    
    data <- common_agg(pre_agg_attachment(),
                       input$measure,
                       'Attachment',
                       TRUE,
                       Pickrateadjusted_Attachment())
    
    attachment_crosswalk$Value <- factor(attachment_crosswalk$Value, levels = levels(data$Attachment))
    
    if (!(input$measure == 'pickrate' & Pickrateadjusted_Attachment() == 1)) {
      
      data <- inner_join(x = data, y = attachment_crosswalk, by = c('Attachment' = 'Value'))[-5] 
      
    } else {
      
      data <- inner_join(x = data, y = attachment_crosswalk, by = c('Attachment' = 'Value'))[-4]
      
    }
    
    data$Rarity <- factor(data$Rarity, levels = c('Common', 'Rare', 'Epic', 'Legendary'))
    data <- data[!duplicated(data$Attachment),]
    
    })
  
  #Filter Attachment observe event
  observeEvent(
    eventExpr = input$filterAttachment,
    handlerExpr = {
      
      OE_Filter_common(Attachment_df(), filtered_Attachment, input$filterAttachment$y, 'Attachment')
      
    }
  )
  
  #Unfilter Attachment event
  observeEvent(
    eventExpr = input$unfilterAttachment,
    handlerExpr = {
      
      filtered_Attachment(0)
      
    }
  )
  
  #Create barplot
  
  output$Attachment <- renderPlot({
    
    create_barplot(Attachment_df(), 'Attachment', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_Attachment())
    
  })
  
  #Attachment hover label
  output$Attachment_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverAttachment, Attachment_df(), Attachment_df()$Attachment,
                           input$measure, 40, -30)
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  ##########
  ###POSE###
  ##########
  
  pre_agg_pose <- reactive({pre_agg(filtered_data(),
                                    input$measure,
                                    'Pose',
                                    FALSE,
                                    Pickrateadjusted_Pose())})
  
  Pose_df <- reactive({
    
    data <- common_agg(pre_agg_pose(),
                       input$measure,
                       'Pose',
                       TRUE,
                       Pickrateadjusted_Pose())
    
    pose_crosswalk$Value <- factor(pose_crosswalk$Value, levels = levels(data$Pose))
    
    if (!(input$measure == 'pickrate' & Pickrateadjusted_Pose() == 1)) {
      
      data <- inner_join(x = data, y = pose_crosswalk, by = c('Pose' = 'Value'))[-5] 
      
    } else {
      
      data <- inner_join(x = data, y = pose_crosswalk, by = c('Pose' = 'Value'))[-4]
      
    }
  
    data$Rarity <- factor(data$Rarity, levels = c('Common', 'Rare', 'Epic', 'Legendary'))
    data <- data[!duplicated(data$Pose),]
    
    })
  
  #Filter Pose observe event
  observeEvent(
    eventExpr = input$filterPose,
    handlerExpr = {
      
      OE_Filter_common(Pose_df(), filtered_Pose, input$filterPose$y, 'Pose')
      
    }
  )
  
  #Unfilter Pose event
  observeEvent(
    eventExpr = input$unfilterPose,
    handlerExpr = {
      
      filtered_Pose(0)
      
    }
  )
  
  #Create barplot
  
  output$Pose <- renderPlot({
    
    create_barplot(Pose_df(), 'Pose', input$measure,
                   filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                   filtered_League(), filtered_Servertype(), filtered_Map(),
                   filtered_Casual(), filtered_Mount(), filtered_Title(),
                   filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                   filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                   filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                   filter_player(), player_selection(), Pickrateadjusted_Pose())
    
  })
  
  #Pose hover label
  output$Pose_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverPose, Pose_df(), Pose_df()$Pose,
                           input$measure, 40, -30)
    
    if (!is.null(info$Value)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
      )
      
    }
    
  })
  
  ###################
  ###AVERAGE STATS###
  ###################
  
  stats_pre_agg <- reactive({
    
    req(input$champion != 'None')
    
    data <- filtered_data() %>%
      mutate(Survived = ifelse(!is.na(Time_Alive),
                               ifelse(Time_Alive < 0, 1, 0), Time_Alive)) %>%
      group_by(Game_ID, User_ID) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0),
                Kills = mean(Kills, na.rm = TRUE),
                Deaths = mean(Deaths, na.rm = TRUE),
                Damage = mean(Damage, na.rm = TRUE),
                Damage_Received = mean(Damage_Received, na.rm = TRUE),
                Protection = mean(Protection, na.rm = TRUE),
                Protection_Received = mean(Protection_Received, na.rm = TRUE),
                Control = mean(Control, na.rm = TRUE),
                Control_Received = mean(Control_Received, na.rm = TRUE),
                Energy_Used = mean(Energy_Used, na.rm = TRUE),
                Energy_Gained = mean(Energy_Gained, na.rm = TRUE),
                Abilities_Used = mean(Abilities_Used, na.rm = TRUE),
                Num_Energy_Used = mean(Num_Energy_Used, na.rm = TRUE),
                Orb_Kills = mean(Orb_Kills, na.rm = TRUE),
                First_Orb = mean(First_Orb, na.rm = TRUE),
                MVP = mean(MVP, na.rm = TRUE),
                Round_Length = mean(Round_Length, na.rm = TRUE),
                Total_Score = mean(Total_Score, na.rm = TRUE),
                Highest_Score = mean(Highest_Score, na.rm = TRUE),
                Health_Shards = mean(Health_Shards, na.rm = TRUE),
                Energy_Shards = mean(Energy_Shards, na.rm = TRUE),
                Survived = mean(Survived, na.rm = TRUE),
                Queue_Time = mean(Queue_Time, na.rm = TRUE))
    
    if (input$measure == 'winrateadjusted' |
        (input$measure == 'pickrate' & Pickrateadjusted_Round_Stats() == 1)) {
      
      #Group by game won and user id
      
      data <- data %>%
      ungroup() %>%
      group_by(Game_Won, User_ID) %>%
      summarize(Kills = mean(Kills),
                Deaths = mean(Deaths),
                Damage = mean(Damage),
                Damage_Received = mean(Damage_Received),
                Protection = mean(Protection),
                Protection_Received = mean(Protection_Received),
                Control = mean(Control),
                Control_Received = mean(Control_Received),
                Energy_Used = mean(Energy_Used),
                Energy_Gained = mean(Energy_Gained),
                Abilities_Used = mean(Abilities_Used),
                Num_Energy_Used = mean(Num_Energy_Used),
                Orb_Kills = mean(Orb_Kills),
                First_Orb = mean(First_Orb),
                MVP = mean(MVP),
                Round_Length = mean(Round_Length),
                Total_Score = mean(Total_Score),
                Highest_Score = mean(Highest_Score),
                Health_Shards = mean(Health_Shards),
                Energy_Shards = mean(Energy_Shards),
                Survived = mean(Survived),
                Queue_Time = mean(Queue_Time))
      
    }
    
    data
    
  })
  
  stats_agg <- reactive({
    
    req(input$champion != 'None')
    
    data <- stats_pre_agg() %>%
      ungroup() %>%
      group_by(Game_Won) %>%
      summarize(Kills = mean(Kills),
                Deaths = mean(Deaths),
                Damage = mean(Damage),
                Damage_Received = mean(Damage_Received),
                Protection = mean(Protection),
                Protection_Received = mean(Protection_Received),
                Control = mean(Control),
                Control_Received = mean(Control_Received),
                Energy_Used = mean(Energy_Used),
                Energy_Gained = mean(Energy_Gained),
                Abilities_Used = mean(Abilities_Used),
                Num_Energy_Used = mean(Num_Energy_Used),
                Orb_Kills = mean(Orb_Kills),
                First_Orb = mean(First_Orb),
                MVP = mean(MVP),
                Round_Length = mean(Round_Length),
                Total_Score = mean(Total_Score),
                Highest_Score = mean(Highest_Score),
                Health_Shards = mean(Health_Shards),
                Energy_Shards = mean(Energy_Shards),
                Survived = mean(Survived),
                Queue_Time = mean(Queue_Time))

    data
    
  })
  
  stats_agg_lose <- reactive({
    
    req(input$champion != 'None')
    
    req(input$measure == 'winrate' |
        input$measure == 'winrateadjusted')
    
    data <- filter(stats_agg(), Game_Won == 0)
    
  })
  
  stats_agg_win <- reactive({
   
    req(input$champion != 'None')
    
    data <- filter(stats_agg(), Game_Won == 1)
    
  })
  
  stats_win_df <- reactive({
    
    req(input$champion != 'None')
    
    #Remove Game_Won column
    data <- stats_agg_win()[,2:length(stats_agg_win())]
    data <- gather(data, key = 'Measure', value = 'Value')
    data$Value <- round(data$Value, 2)
    data

  })
  
  stats_lose_df <- reactive({
    
    req(input$champion != 'None')
    
    req(input$measure == 'winrate' |
          input$measure == 'winrateadjusted')
    
    #Remove Game_Won column
    data <- stats_agg_lose()[,2:length(stats_agg_lose())]
    data <- gather(data, key = 'Measure', value = 'Value')
    data$Value <- round(data$Value, 2)
    data

  })
  
  stats_average_df <- reactive({
    
    req(input$champion != 'None')
    
    req(input$measure == 'pickrate')
    
    data <- stats_agg()
    Value <- colMeans(data)
    data <- data.frame(Value)
    data$Measure <- rownames(data)
    rownames(data) <- 1:length(rownames(data))
    data$Value <- round(data$Value, 2)
    #Remove game_won
    data <- data[2:dim(data)[1],]
    data <- select(data, Measure, Value)
    data
    
  })
  
  #For wins view and overall(when pickrate selected)
  output$StatsWinLabel <- renderUI({
    
    req(input$champion != 'None')
    
    dynamic_name <- create_dynamic_text(filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                        filtered_League(), filtered_Servertype(), filtered_Map(),
                                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                        filter_player(), player_selection(), barplot = FALSE)
    
    if(input$measure == 'winrate' |
          input$measure == 'winrateadjusted') {
      
      initial_text <- 'Average Stats Per Round For Game Wins'
      
      if (dynamic_name == '') {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, '</font></b></u>')
        
      } else {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, ' While Filtering on: ',
                             dynamic_name, '</font></b></u>')
        
      }
    
    } else {
      
      initial_text <- 'Average Stats Per Round Overall'
      
      if (dynamic_name == '') {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, '</font></b></u>')
        
      } else {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, ' While Filtering on: ',
                             dynamic_name, '</font></b></u>')
        
      }
      
    }
    
    HTML(final_text)
      
  })
  
  output$StatsLoseLabel <- renderUI({
    
    req(input$champion != 'None')
    
    req(input$measure == 'winrate' |
          input$measure == 'winrateadjusted')
    
    dynamic_name <- create_dynamic_text(filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                        filtered_League(), filtered_Servertype(), filtered_Map(),
                                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                        filter_player(), player_selection(), barplot = FALSE)
    
    initial_text <- 'Average Stats Per Round For Game Losses'
    
    if (dynamic_name == '') {
      
      final_text <- paste0('<font size = "2"><b><u>', initial_text, '</font></b></u>')
      
    } else {
      
      final_text <- paste0('<font size = "2"><b><u>', initial_text, ' While Filtering on: ',
                           dynamic_name, '</font></b></u>')
      
    }
    
    HTML(final_text)
    
  })
  
  
  output$StatsWin <- DT::renderDataTable(expr = {
    
    if(input$measure == 'winrate' |
          input$measure == 'winrateadjusted') {
    
    datatable(stats_win_df(), options = list(searching = FALSE,
                                             paging = FALSE))
      
    } else {
      
      datatable(stats_average_df(), options = list(searching = FALSE,
                                                   paging = FALSE))
      
    }
    
  })
  
  output$StatsLose <- DT::renderDataTable(expr = {
    
    req(input$measure == 'winrate' |
          input$measure == 'winrateadjusted')
    
    datatable(stats_lose_df(), options = list(searching = FALSE,
                                              paging = FALSE))
    
  })
  
  ##############
  ###MATCHUPS###
  ##############
  
  ally_comp_pre_agg <- reactive({
    
    withProgress(message = 'Preliminary computations', value = 0, {
    
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won, Team_Comp, Server_Type, Team_Roles) %>%
      group_by(Game_ID, User_ID, Team_Comp, Server_Type, Team_Roles) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    
    if (input$measure == 'winrateadjusted') {
      
      data <- data %>%
        ungroup() %>%
        group_by(User_ID, Team_Comp, Server_Type, Team_Roles) %>%
        summarize(Game_Won = mean(Game_Won))
    
    }
    
    if (input$measure == 'pickrate' & Pickrateadjusted_Comps() == 1) {
      
      data2 <- data %>%
        ungroup() %>%
        group_by(User_ID, Team_Comp) %>%
        summarize(Num_Picks = n())
      
      extra_cols <- data %>%
        ungroup() %>%
        group_by(Team_Comp, Server_Type, Team_Roles) %>%
        summarize(Sample_Size = n())
      
      data <- inner_join(x = data2, y = User_Games_Played(), by = c('User_ID' = 'User_ID'))
      
      #Too many categories for larger data sets. To reduce computation time will only take a subset for larger
      #data sets 

      if (dim(data)[1] > 50) {
        
        subset_comps <- data %>%
          ungroup() %>%
          group_by(Team_Comp) %>%
          summarize(Count = n()) %>%
          arrange(desc(Count))
        subset_comps <- head(subset_comps, 50)
        
        data <- filter(data, Team_Comp %in% subset_comps$Team_Comp)
        
      }
      
      categories <- unique(pull(data, Team_Comp))
      num_unique_users <- length(unique(data$User_ID))
      
      Category = c()
      Pick_Rate = c()
      Sample_Size = c()
      
      for (i in 1:length(categories)) {
        
        incProgress(1/(length(categories)), detail = paste0('Computing Pick_Rate for ', categories[i], ' (',
                                                           i, ' of ', length(categories)))
        
        data2 <- filter(data, Team_Comp == categories[i])
        user_pick_rates <- c()
        
        #Check if user has picked any other category and compute probability accordingly
        for (userid in data2$User_ID) {
          
          user_filtered <- filter(data2, User_ID == userid)
          user_pick_rate <- user_filtered$Num_Picks/user_filtered$User_Total_Games
          user_pick_rates <- c(user_pick_rates, user_pick_rate)
          
          
        }
        
        category_pick_rate <- sum(user_pick_rates)/num_unique_users
        sample_size <- length(user_pick_rates)
        
        Category <- c(Category, as.character(categories[i]))
        Pick_Rate <- c(Pick_Rate, category_pick_rate)
        Sample_Size <- c(Sample_Size, sample_size)
      }
      
      data <- data.frame(Category, Pick_Rate, stringsAsFactors = TRUE) %>%
        mutate(Pick_Rate = round(Pick_Rate*100,2))
      colnames(data)[colnames(data) == 'Category'] <- 'Team_Comp'
      
      data$Team_Comp <- factor(data$Team_Comp, levels = levels(extra_cols$Team_Comp))
      data <- inner_join(x = data, y = extra_cols, by = c('Team_Comp' = 'Team_Comp'))
      
    }
    
    })
    
    data
    
  })
  
  ally_comp_agg <- reactive({
    
    if (!(input$measure == 'pickrate' & Pickrateadjusted_Comps() == 1)) {

    data <- ally_comp_pre_agg() %>%
      ungroup() %>%
      select(Game_Won, Team_Comp, Server_Type, Team_Roles) %>%
      group_by(Team_Comp, Team_Roles, Server_Type) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100, 2),
                Pick_Rate = round(n()/Champion_Pick_Size()*100,2),
                Sample_Size = n())
    
    } else {
      
      data <- ally_comp_pre_agg()
      
    }
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      data <- arrange(data, desc(Win_Rate)) %>%
        select(Team_Comp, Team_Roles, Server_Type, Win_Rate, Sample_Size)
      
    } else {
      
      data <- arrange(data, desc(Pick_Rate)) %>%
        select(Team_Comp, Team_Roles, Server_Type, Pick_Rate, Sample_Size)
      
    }
    
    data2 <- data %>%
      filter(Sample_Size >= 3)
    
    if (dim(data2)[1] < 10) {
      
      data2 <- data %>%
        filter(Sample_Size >= 2)
      
      if(dim(data2)[1] < 10) {
        
        data2 <- data
        
      }
      
    }

    data2
    
  })
  
  enemy_comp_pre_agg <- reactive({
    req(!is.null(filtered_data()))

    withProgress(message = 'Preliminary computations', value = 0, {
    
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won, Enemy_Comp, Server_Type, Enemy_Roles) %>%
      group_by(Game_ID, User_ID, Enemy_Comp, Server_Type, Enemy_Roles) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    
    if (input$measure == 'winrateadjusted') {
      
      data <- data %>%
        ungroup() %>%
        group_by(User_ID, Enemy_Comp, Server_Type, Enemy_Roles) %>%
        summarize(Game_Won = mean(Game_Won))
      
    }
    
    if (input$measure == 'pickrate' & Pickrateadjusted_Comps_Matchups() == 1) {
      
      data2 <- data %>%
        ungroup() %>%
        group_by(User_ID, Enemy_Comp) %>%
        summarize(Num_Picks = n())
      
      extra_cols <- data %>%
        ungroup() %>%
        group_by(Enemy_Comp, Server_Type, Enemy_Roles) %>%
        summarize(Sample_Size = n())
      
      data <- inner_join(x = data2, y = User_Games_Played(), by = c('User_ID' = 'User_ID'))
      
      #Too many categories for larger data sets. To reduce computation time will only take a subset for larger
      #data sets 
      
      if (dim(data)[1] > 50) {
        
        subset_comps <- data %>%
          ungroup() %>%
          group_by(Enemy_Comp) %>%
          summarize(Count = n()) %>%
          arrange(desc(Count))
        subset_comps <- head(subset_comps, 50)
        
        data <- filter(data, Enemy_Comp %in% subset_comps$Enemy_Comp)
        
      }
      
      categories <- unique(pull(data, Enemy_Comp))
      num_unique_users <- length(unique(data$User_ID))
      
      Category = c()
      Pick_Rate = c()
      Sample_Size = c()
      
      for (i in 1:length(categories)) {
        
        incProgress(1/(length(categories)), detail = paste0('Computing Pick_Rate for ', categories[i], ' (',
                                                            i, ' of ', length(categories)))
        
        data2 <- filter(data, Enemy_Comp == categories[i])
        user_pick_rates <- c()
        
        #Check if user has picked any other category and compute probability accordingly
        for (userid in data2$User_ID) {
          
          user_filtered <- filter(data2, User_ID == userid)
          user_pick_rate <- user_filtered$Num_Picks/user_filtered$User_Total_Games
          user_pick_rates <- c(user_pick_rates, user_pick_rate)
          
          
        }
        
        category_pick_rate <- sum(user_pick_rates)/num_unique_users
        sample_size <- length(user_pick_rates)
        
        Category <- c(Category, as.character(categories[i]))
        Pick_Rate <- c(Pick_Rate, category_pick_rate)
        Sample_Size <- c(Sample_Size, sample_size)
      }
      
      data <- data.frame(Category, Pick_Rate, stringsAsFactors = TRUE) %>%
        mutate(Pick_Rate = round(Pick_Rate*100,2))
      colnames(data)[colnames(data) == 'Category'] <- 'Enemy_Comp'
      
      data$Enemy_Comp <- factor(data$Enemy_Comp, levels = levels(extra_cols$Enemy_Comp))
      data <- inner_join(x = data, y = extra_cols, by = c('Enemy_Comp' = 'Enemy_Comp'))
      
    }
    
    })
    
    data
    
  })
  
  
  enemy_comp_agg <- reactive({
    req(!is.null(enemy_comp_pre_agg))

    if (!(input$measure == 'pickrate' & Pickrateadjusted_Comps_Matchups() == 1)) {
    
    data <- enemy_comp_pre_agg() %>%
      ungroup() %>%
      select(Game_Won, Enemy_Comp, Server_Type, Enemy_Roles) %>%
      group_by(Enemy_Comp, Enemy_Roles, Server_Type) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100, 2),
                Pick_Rate = round(n()/Champion_Pick_Size()*100,2),
                Sample_Size = n())
    
    } else {
      
      data <- enemy_comp_pre_agg()
      
    }
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      data <- arrange(data, desc(Win_Rate)) %>%
        select(Enemy_Comp, Server_Type, Enemy_Roles, Win_Rate, Sample_Size)
      
    } else {
      
      data <- arrange(data, desc(Pick_Rate)) %>%
        select(Enemy_Comp, Server_Type, Enemy_Roles, Pick_Rate, Sample_Size)
      
    }
    
    data2 <- data %>%
      filter(Sample_Size >= 3)
    
    if (dim(data2)[1] < 10) {
      
      data2 <- data %>%
        filter(Sample_Size >= 2)
      
      if(dim(data2)[1] < 10) {
        
        data2 <- data
        
      }
      
    }
    
    data2
    
  })
  
  
  output$BestCompsLabel <- renderUI({
    
    req(input$champion != 'None')
    
    dynamic_name <- create_dynamic_text(filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                        filtered_League(), filtered_Servertype(), filtered_Map(),
                                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                        filter_player(), player_selection(), barplot = FALSE)
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      initial_text <- 'Top 5 Best Comps'
      
      if (dynamic_name == '') {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, '</font></b></u>')
        
      } else {
        
       final_text <- paste0('<font size = "2"><b><u>', initial_text, ' While Filtering on: ',
                            dynamic_name, '</font></b></u>')

      }
      
    } else {
      
      initial_text <- 'Top 5 Most Played Comps'
      
      if (dynamic_name == '') {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, '</font></b></u>')
        
      } else {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, ' While Filtering on: ',
                             dynamic_name, '</font></b></u>') 
        
      }
    }
    
    HTML(final_text)
  })
  output$BestComps <- renderTable(head(ally_comp_agg(), 5)
    )
  
  output$WorstCompsLabel <- renderUI({
    
    req(input$champion != 'None')
    
    dynamic_name <- create_dynamic_text(filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                        filtered_League(), filtered_Servertype(), filtered_Map(),
                                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                        filter_player(), player_selection(), barplot = FALSE)
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      initial_text <- 'Top 5 Worst Comps'
      
      if (dynamic_name == '') {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, '</font></b></u>')
        
      } else {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, ' While Filtering on: ',
                             dynamic_name, '</font></b></u>')
        
      }
      
    } else {
      
      initial_text <- 'Top 5 Least Played Comps'
      
      if (dynamic_name == '') {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, '</font></b></u>')
        
      } else {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, ' While Filtering on: ',
                             dynamic_name, '</font></b></u>') 
        
      }
    }
    
    HTML(final_text)
      
  })
  
  output$WorstComps <- renderTable(tail(ally_comp_agg(), 5)
  )
  
  output$BestMatchupsLabel <- renderUI({
    
    req(input$champion != 'None')
    
    dynamic_name <- create_dynamic_text(filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                        filtered_League(), filtered_Servertype(), filtered_Map(),
                                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                        filter_player(), player_selection(), barplot = FALSE)
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      initial_text <- 'Top 5 Best Matchups'
      
      if (dynamic_name == '') {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, '</font></b></u>')
        
      } else {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, ' While Filtering on: ',
                             dynamic_name, '</font></b></u>')
        
      }
      
    } else {
      
      initial_text <- 'Top 5 Most Popular Matchups'
      
      if (dynamic_name == '') {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, '</font></b></u>')
        
      } else {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, ' While Filtering on: ',
                             dynamic_name, '</font></b></u>') 
        
      }
    }
    
    HTML(final_text)
    
  })
  
  output$BestMatchups <- renderTable(head(enemy_comp_agg(), 5))
  
  output$WorstMatchupsLabel <- renderUI({
    
    req(input$champion != 'None')
    
    dynamic_name <- create_dynamic_text(filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                        filtered_League(), filtered_Servertype(), filtered_Map(),
                                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                        filter_player(), player_selection(), barplot = FALSE)
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      initial_text <- 'Top 5 Worst Matchups'
      
      if (dynamic_name == '') {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, '</font></b></u>')
        
      } else {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, ' While Filtering on: ',
                             dynamic_name, '</font></b></u>')
        
      }
      
    } else {
      
      initial_text <- 'Top 5 Least Popular Matchups'
      
      if (dynamic_name == '') {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, '</font></b></u>')
        
      } else {
        
        final_text <- paste0('<font size = "2"><b><u>', initial_text, ' While Filtering on: ',
                             dynamic_name, '</font></b></u>') 
        
      }
    }
    
    HTML(final_text)
    
  })
  
  output$WorstMatchups <- renderTable(tail(enemy_comp_agg(), 5))
  
  ######################
  ###TEAM/ENEMY ROLES###
  ######################
  
  pre_agg_allyroles <- reactive({

      data <- filtered_data() %>%
        select(Game_ID, User_ID, Round_Won, Team_Roles, Enemy_Roles) %>%
        group_by(Game_ID, User_ID, Team_Roles, Enemy_Roles) %>%
        summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
      
      if (input$measure == 'winrateadjusted') {
        
        data <- data %>%
          ungroup() %>%
          group_by(User_ID, Team_Roles, Enemy_Roles) %>%
          summarize(Game_Won = mean(Game_Won))

      }
      
      if (input$measure == 'pickrate' & Pickrateadjusted_TeamRoles() == 1) {
        
        data2 <- data %>%
          ungroup() %>%
          group_by(User_ID, Team_Roles) %>%
          summarize(Num_Picks = n())
        
        extra_cols <- data %>%
          ungroup() %>%
          group_by(Team_Roles, Enemy_Roles) %>%
          summarize(Sample_Size = n())
        
        data <- inner_join(x = data2, y = User_Games_Played(), by = c('User_ID' = 'User_ID'))
        
        categories <- unique(pull(data, Team_Roles))
        num_unique_users <- length(unique(data$User_ID))
        
        Category = c()
        Pick_Rate = c()
        Sample_Size = c()
        
        for (category in categories) {
          
          data2 <- filter(data, Team_Roles == category)
          user_pick_rates <- c()
          
          #Check if user has picked any other category and compute probability accordingly
          for (userid in data2$User_ID) {
            
            user_filtered <- filter(data2, User_ID == userid)
            user_pick_rate <- user_filtered$Num_Picks/user_filtered$User_Total_Games
            user_pick_rates <- c(user_pick_rates, user_pick_rate)
            
            
          }
          
          category_pick_rate <- sum(user_pick_rates)/num_unique_users
          sample_size <- length(user_pick_rates)
          
          Category <- c(Category, category)
          Pick_Rate <- c(Pick_Rate, category_pick_rate)
          Sample_Size <- c(Sample_Size, sample_size)
        }
        
        data <- data.frame(Category, Pick_Rate, stringsAsFactors = TRUE) %>%
          mutate(Pick_Rate = round(Pick_Rate*100,2))
        colnames(data)[colnames(data) == 'Category'] <- 'Team_Roles'
        
        data$Team_Roles <- factor(data$Team_Roles, levels = levels(extra_cols$Team_Roles))
        data <- inner_join(x = data, y = extra_cols, by = c('Team_Roles' = 'Team_Roles'))
        data
        
      }
      
      
      
    
    data
    
    })
  
  Allyroles_df <- reactive({
    
    if (!(input$measure == 'pickrate' & Pickrateadjusted_TeamRoles() == 1)) {
    data <- pre_agg_allyroles() %>%
      ungroup() %>%
      select(Team_Roles, Game_Won) %>%
      group_by(Team_Roles) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100, 2),
                Pick_Rate = round(n()/Champion_Pick_Size()*100,2),
                Sample_Size = n())
    
    } else {
      
      data <- pre_agg_allyroles()
      
    }
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
    
      data <- arrange(data, desc(Win_Rate)) %>%
        select(Team_Roles, Win_Rate, Sample_Size)
      
    } else {
      
      data <- arrange(data, desc(Pick_Rate)) %>%
        select(Team_Roles, Pick_Rate, Sample_Size)
      
    }
    
    data2 <- data %>%
      filter(Sample_Size >= 3)
    
    if (dim(data2)[1] < 2) {
      
      data2 <- data %>%
        filter(Sample_Size >= 2)
      
      if(dim(data2)[1] < 2) {
        
        data2 <- data
        
      }
      
    }
    
    data2
    
  })
  
  Allyroles_best <- reactive({
    
    if (!(input$measure == 'pickrate' & Pickrateadjusted_TeamRoles() == 1)) {
    
    data <- pre_agg_allyroles() %>%
      ungroup() %>%
      select(Team_Roles, Enemy_Roles, Game_Won) %>%
      group_by(Team_Roles, Enemy_Roles) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100, 2),
                Pick_Rate = round(n()/Champion_Pick_Size()*100,2),
                Sample_Size = n())
    
    } else {
      
      data <- pre_agg_allyroles()
      
    }
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      data <- arrange(data, desc(Win_Rate)) %>%
        select(Team_Roles, Enemy_Roles, Win_Rate, Sample_Size)
      
    } else {
      
      data <- arrange(data, desc(Pick_Rate)) %>%
        select(Team_Roles, Enemy_Roles, Pick_Rate, Sample_Size)
      
    }
    
    data <- filter(data, Team_Roles == head(Allyroles_df(), 1)$Team_Roles)
    
    data2 <- data %>%
      filter(Sample_Size >= 3)
    
    if (dim(data2)[1] < 2) {
      
      data2 <- data %>%
        filter(Sample_Size >= 2)
      
      if(dim(data2)[1] < 2) {
        
        data2 <- data
        
      }
      
    }

    data2
    
  })
  
  Allyroles_worst <- reactive({
    
    if (!(input$measure == 'pickrate' & Pickrateadjusted_TeamRoles() == 1)) {
    
    data <- pre_agg_allyroles() %>%
      ungroup() %>%
      select(Team_Roles, Enemy_Roles, Game_Won) %>%
      group_by(Team_Roles, Enemy_Roles) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100, 2),
                Pick_Rate = round(n()/Champion_Pick_Size()*100,2),
                Sample_Size = n())
    
    } else {
      
      data <- pre_agg_allyroles()
      
    }
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      data <- arrange(data, desc(Win_Rate)) %>%
        select(Team_Roles, Enemy_Roles, Win_Rate, Sample_Size)
      
    } else {
      
      data <- arrange(data, desc(Pick_Rate)) %>%
        select(Team_Roles, Enemy_Roles, Pick_Rate, Sample_Size)
      
    }
    
      data <- filter(data, Team_Roles == tail(Allyroles_df(), 1)$Team_Roles)
    
    data2 <- data %>%
      filter(Sample_Size >= 3)
    
    if (dim(data2)[1] < 2) {
      
      data2 <- data %>%
        filter(Sample_Size >= 2)
      
      if(dim(data2)[1] < 2) {
        
        data2 <- data
        
      }
      
    }
    
    data2
    
  })
  
  output$BestAllyRoles <- renderUI({
    
    dynamic_name <- create_dynamic_text(filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                        filtered_League(), filtered_Servertype(), filtered_Map(),
                                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                        filter_player(), player_selection(), barplot = FALSE)
    
    bestoverall <- head(Allyroles_best(), 1)$Team_Roles
    bestoverall_count <- head(Allyroles_df(), 1)$Sample_Size
    bestmatchup <- head(Allyroles_best(), 1)$Enemy_Roles
    bestmatchup_count <- head(Allyroles_best(), 1)$Sample_Size
    worstmatchup <- tail(Allyroles_best(), 1)$Enemy_Roles
    worstmatchup_count <- tail(Allyroles_best(), 1)$Sample_Size
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {

    bestoverall_win <- head(Allyroles_df(), 1)$Win_Rate
    bestmatchup_win <- head(Allyroles_best(), 1)$Win_Rate
    worstmatchup_win <- tail(Allyroles_best(), 1)$Win_Rate
    
    if (dynamic_name == '') {
    
    output <- HTML(paste0('<font size = "2"> The best overall roles for ',
                input$champion,
                ' is ',
                bestoverall,
                ' with a win rate of ',
                bestoverall_win,
                ' percent with ',
                bestoverall_count,
                ' observations. The best matchup is against ',
                bestmatchup,
                ' with a win rate of ',
                bestmatchup_win,
                ' percent with ',
                bestmatchup_count,
                ' observations and the worst matchup against ',
                worstmatchup,
                ' with a win rate of ',
                worstmatchup_win,
                ' percent with ',
                worstmatchup_count,
                ' observations.</font></b></u>'))
    
    } else {
      
      output <- HTML(paste0('<font size = "2"> The best overall roles for ',
                            input$champion,
                            ' while filtering for ',
                            dynamic_name,
                            ' is ',
                            bestoverall,
                            ' with a win rate of ',
                            bestoverall_win,
                            ' percent with ',
                            bestoverall_count,
                            ' observations. The best matchup is against ',
                            bestmatchup,
                            ' with a win rate of ',
                            bestmatchup_win,
                            ' percent with ',
                            bestmatchup_count,
                            ' observations and the worst matchup against ',
                            worstmatchup,
                            ' with a win rate of ',
                            worstmatchup_win,
                            ' percent with ',
                            worstmatchup_count,
                            ' observations.</font></b></u>'))
      
      
    }
    } else {
      
      bestoverall_pick <- head(Allyroles_df(), 1)$Pick_Rate
      bestmatchup_pick <- head(Allyroles_best(), 1)$Pick_Rate
      worstmatchup_pick <- tail(Allyroles_best(), 1)$Pick_Rate
      
      if(dynamic_name == '') {
      
      output <- HTML(paste0('<font size = "2"> The most popular overall roles for ',
                            input$champion,
                            ' is ',
                            bestoverall,
                            ' with a pick rate of ',
                            bestoverall_pick,
                            ' percent with ',
                            bestoverall_count,
                            ' observations. The most popular matchup is against ',
                            bestmatchup,
                            ' with a pick rate of ',
                            bestmatchup_pick,
                            ' percent with ',
                            bestmatchup_count,
                            ' observations and the least popular matchup against ',
                            worstmatchup,
                            ' with a pick rate of ',
                            worstmatchup_pick,
                            ' percent with ',
                            worstmatchup_count,
                            ' observations.</font></b></u>'))
      } else {
        
        output <- HTML(paste0('<font size = "2"> The most popular overall roles for ',
                              input$champion,
                              ' while filtering for ',
                              dynamic_name,
                              ' is ',
                              bestoverall,
                              ' with a pick rate of ',
                              bestoverall_pick,
                              ' percent with ',
                              bestoverall_count,
                              ' observations. The most popular matchup is against ',
                              bestmatchup,
                              ' with a pick rate of ',
                              bestmatchup_pick,
                              ' percent with ',
                              bestmatchup_count,
                              ' observations and the least popular matchup against ',
                              worstmatchup,
                              ' with a pick rate of ',
                              worstmatchup_pick,
                              ' percent with ',
                              worstmatchup_count,
                              ' observations.</font></b></u>'))
        
        
      }
    }
    
    output
    
  })
  
  output$WorstAllyRoles <- renderUI({
    
    dynamic_name <- create_dynamic_text(filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                        filtered_League(), filtered_Servertype(), filtered_Map(),
                                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                        filter_player(), player_selection(), barplot = FALSE)
    
    worstoverall <- head(Allyroles_worst(), 1)$Team_Roles
    worstoverall_count <- tail(Allyroles_df(), 1)$Sample_Size
    bestmatchup <- head(Allyroles_worst(), 1)$Enemy_Roles
    bestmatchup_count <- head(Allyroles_worst(), 1)$Sample_Size
    worstmatchup <- tail(Allyroles_worst(), 1)$Enemy_Roles
    worstmatchup_count <- tail(Allyroles_worst(), 1)$Sample_Size
    
    
    if (input$measure == 'winrate' |
        input$measure == 'winrateadjusted') {
      
      worstoverall_win <- tail(Allyroles_df(), 1)$Win_Rate
      bestmatchup_win <- head(Allyroles_worst(), 1)$Win_Rate
      worstmatchup_win <- tail(Allyroles_worst(), 1)$Win_Rate
      
      if(dynamic_name == '') {
    
      output <- HTML(paste0('<font size = "2"> The worst overall roles for ',
                            input$champion,
                            ' is ',
                            worstoverall,
                            ' with a win rate of ',
                            worstoverall_win,
                            ' percent with ',
                            worstoverall_count,
                            ' observations. The best matchup is against ',
                            bestmatchup,
                            ' with a win rate of ',
                            bestmatchup_win,
                            ' percent with ',
                            bestmatchup_count,
                            ' observations and the worst matchup against ',
                            worstmatchup,
                            ' with a win rate of ',
                            worstmatchup_win,
                            ' percent with ',
                            worstmatchup_count,
                            ' observations.</font></b></u>'))
      
      } else {
        
        output <- HTML(paste0('<font size = "2"> The worst overall roles for ',
                              input$champion,
                              ' while filtering for ',
                              dynamic_name,
                              ' is ',
                              worstoverall,
                              ' with a win rate of ',
                              worstoverall_win,
                              ' percent with ',
                              worstoverall_count,
                              ' observations. The best matchup is against ',
                              bestmatchup,
                              ' with a win rate of ',
                              bestmatchup_win,
                              ' percent with ',
                              bestmatchup_count,
                              ' observations and the worst matchup against ',
                              worstmatchup,
                              ' with a win rate of ',
                              worstmatchup_win,
                              ' percent with ',
                              worstmatchup_count,
                              ' observations.</font></b></u>'))
        
      }
      
    } else {
      
      worstoverall_pick <- tail(Allyroles_df(), 1)$Pick_Rate
      bestmatchup_pick <- head(Allyroles_worst(), 1)$Pick_Rate
      worstmatchup_pick <- tail(Allyroles_worst(), 1)$Pick_Rate
      
      if (dynamic_name == '') {
      
      output <- HTML(paste0('<font size = "2"> The least popular roles for ',
                            input$champion,
                            ' is ',
                            worstoverall,
                            ' with a pick rate of ',
                            worstoverall_pick,
                            ' percent with ',
                            worstoverall_count,
                            ' observations. The most popular matchup is against ',
                            bestmatchup,
                            ' with a pick rate of ',
                            bestmatchup_pick,
                            ' percent with ',
                            bestmatchup_count,
                            ' observations and the least popular matchup against ',
                            worstmatchup,
                            ' with a pick rate of ',
                            worstmatchup_pick,
                            ' percent with ',
                            worstmatchup_count,
                            ' observations.</font></b></u>'))
      } else {
        
        output <- HTML(paste0('<font size = "2"> The least popular roles for ',
                              input$champion,
                              ' while filtering for ',
                              dynamic_name,
                              ' is ',
                              worstoverall,
                              ' with a pick rate of ',
                              worstoverall_pick,
                              ' percent with ',
                              worstoverall_count,
                              ' observations. The most popular matchup is against ',
                              bestmatchup,
                              ' with a pick rate of ',
                              bestmatchup_pick,
                              ' percent with ',
                              bestmatchup_count,
                              ' observations and the least popular matchup against ',
                              worstmatchup,
                              ' with a pick rate of ',
                              worstmatchup_pick,
                              ' percent with ',
                              worstmatchup_count,
                              ' observations.</font></b></u>'))
        
      }
 
    }
    
    output
    
  })
  
  
  #################
  ###BATTLERITES###
  #################
  
  
  create_battlerites_agg <- function(filtered_data,
                                     input_measure,
                                     filtered_battlerites,
                                     battlerites_df_prefilter,
                                     pickrateadjusted) {
 
    Battlerites <- c()
    Win_Rate <- c()
    Pick_Rate <- c()
    Sample_Size <- c()
    
    if (length(filtered_battlerites) == 5) {
      
      br_set <- filtered_data$Battlerites[1]
      
      if (input_measure == 'winrate' | input_measure == 'winrateadjusted') {
        
        value <- filter(battlerites_df_prefilter, Battlerites == br_set)$Win_Rate
        
      } else {
        
        value <- filter(battlerites_df_prefilter, Battlerites == br_set)$Pick_Rate
        
      }
      
      return(value)
      
    }
    
      if (!(input_measure == 'pickrate' & pickrateadjusted == 1)) {
      
    for (battlerite in c(names(filtered_data)[77:length(names(filtered_data))])) {
      
      
      
      pre_agg_battlerites1 <- pre_agg(filtered_data,
                                      input_measure,
                                      battlerite,
                                      FALSE,
                                      pickrateadjusted)
      
      var <- as.name(battlerite)
      
      br_filter <- filter(pre_agg_battlerites1, !!var == 1)
      
      battlerites1_df <- br_filter %>%
        ungroup() %>%
        select(!!var, Game_Won) %>%
        group_by(!!var) %>%
        summarize(Win_Rate = round(mean(Game_Won)*100, 2),
                  Pick_Rate = round(n()/Champion_Pick_Size()*100,2),
                  Sample_Size = n())
      
      Battlerites <- c(Battlerites, names(battlerites1_df)[1])
      Win_Rate <- c(Win_Rate, battlerites1_df$Win_Rate)
      Pick_Rate <- c(Pick_Rate, battlerites1_df$Pick_Rate)
      Sample_Size <- c(Sample_Size, battlerites1_df$Sample_Size)
      
    } 
        
        } else {

        data <- filtered_data %>%
          group_by(Game_ID, User_ID, Battlerites) %>%
          summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0)) %>%
          ungroup() %>%
          group_by(User_ID, Battlerites) %>%
          summarize(Num_Picks = n()) %>%
          arrange(User_ID, desc(Num_Picks)) %>%
          distinct(User_ID, .keep_all = TRUE) %>%
          separate(Battlerites, into = c('Battlerite 1', 'Battlerite 2', 'Battlerite 3', 'Battlerite 4', 'Battlerite 5'),
                   sep = ", ")
          
        Cdf <- data

        #Slice Cdf to include only Battlerite columns
        Cdf2 <- Cdf[, c('Battlerite 1', 'Battlerite 2', 'Battlerite 3', 'Battlerite 4', 'Battlerite 5')]
        
        #Convert categories to indicator variables
        Cdf3 <- dummy_cols(Cdf2)
        
        #Initial column names without Battlerite 1, Battlerite 2, etc
        init_colnames <- names(Cdf3)[6:length(names(Cdf3))]
        
        #Subset to not include Battlerite 1, Battlerite 2 columns
        Cdf3 <- Cdf3[, init_colnames]
        
        #Rename columns to remove prefix introduced by dummy_cols
        names(Cdf3) <- str_replace(init_colnames, paste0(names(Cdf2)[1] , '_'), replacement = '')
        names(Cdf3) <- str_replace(names(Cdf3), paste0(names(Cdf2)[2] , '_'), replacement = '')
        names(Cdf3) <- str_replace(names(Cdf3), paste0(names(Cdf2)[3] , '_'), replacement = '')
        names(Cdf3) <- str_replace(names(Cdf3), paste0(names(Cdf2)[4] , '_'), replacement = '')
        names(Cdf3) <- str_replace(names(Cdf3), paste0(names(Cdf2)[5] , '_'), replacement = '')
        
        if (length(Cdf3) != 5) {
        
        #Convert character variables to numeric to apply rowsums
        Cdf3 <- sapply(Cdf3, as.numeric)   
          
        #Group by same Battlerite name and have final unique set of battlerite choices as indicator variables
        Cdf3 <- t(rowsum(t(Cdf3), group = colnames(Cdf3)))
        
        }
        
        Cdf3 <- data.frame(Cdf3)
        names(Cdf3) <- str_replace(names(Cdf3), '\\.', replacement = ' ')
        
        #Remove the current Battlerite 1, Battlerite 2, etc columns
        Cdf <- Cdf[, !(names(Cdf) %in% c('Battlerite 1', 'Battlerite 2', 'Battlerite 3', 'Battlerite 4', 'Battlerite 5'))]
        
        #Attach new Battlerite columns (Some weird bug happening required to use for loop method to combine columns)
        for (i in 1:length(names(Cdf3))) {
          
          Cdf[, names(Cdf3)[i]] <- Cdf3[, i]
          
        }
      
        for (battlerite in c(names(Cdf)[3:length(names(Cdf))])) {
          
          if (battlerite %in% filtered_battlerites) {
            
            next
            
          }
          
          var <- as.name(battlerite)
          
          br_filter <- filter(Cdf, !!var == 1)
          
          battlerites1_df <- br_filter %>%
            group_by(!!var) %>%
            summarize(Pick_Rate = round(n()/length(unique(Cdf$User_ID))*100,2),
                      Sample_Size = n())
          
          Battlerites <- c(Battlerites, names(battlerites1_df)[1])
          Pick_Rate <- c(Pick_Rate, battlerites1_df$Pick_Rate)
          Sample_Size <- c(Sample_Size, battlerites1_df$Sample_Size)
        
        
        }
        
        }

      
    if (!(input_measure == 'pickrate' & pickrateadjusted == 1)) {
    
    br_winrates <- data.frame(Battlerites, Win_Rate, Pick_Rate, Sample_Size, stringsAsFactors = TRUE)
    
    } else {
      
      br_winrates <- data.frame(Battlerites, Pick_Rate, Sample_Size, stringsAsFactors = TRUE)
      
    }
    
    if (input_measure == 'winrate' | input_measure == 'winrateadjusted') {
      
      br_winrates <- mutate(br_winrates, Battlerites = fct_reorder(Battlerites, Win_Rate))
      
    } else {
      
      br_winrates <- mutate(br_winrates, Battlerites = fct_reorder(Battlerites, Pick_Rate))
      
    }
    
    
    br_crosswalk$Value <- factor(br_crosswalk$Value, levels = levels(br_winrates$Battlerites))
    
    if (!(input_measure == 'pickrate' & pickrateadjusted == 1)) {
    
    brtypes <- inner_join(x = br_winrates, y = br_crosswalk, by = c('Battlerites' = 'Value'))[-5]
    
    } else {
      
      brtypes <- inner_join(x = br_winrates, y = br_crosswalk, by = c('Battlerites' = 'Value'))[-4]
      
    }
    
    brtypes$`Battlerite Type` <- factor(brtypes$`Battlerite Type`, levels = c('Control', 'Defense',
                                                                              'Mobility', 'Offense',
                                                                              'Support', 'Utility', 'Mixed'))
    
    return(brtypes)

    
  }
  
  
  battlerites_agg1 <- reactive({
    
    req(input$champion != 'None')
    
    create_battlerites_agg(filtered_data(), input$measure, filtered_Battlerites$battlerites, 
                           Battlerites_df_prefilter(), Pickrateadjusted_Battlerites())
    
  })
  
  observeEvent(
    eventExpr = input$filterBattlerites,
    handlerExpr = {
      
      data <- battlerites_agg1()
      
      if (round(input$filterBattlerites$y) > length(levels(fct_drop(data[['Battlerites']])))) {
        
        filtered_Battlerites$battlerites <- c(filtered_Battlerites$battlerites, levels(fct_drop(data[['Battlerites']]))[length(levels(fct_drop(data[['Battlerites']])))])
        
      } else if (round(input$filterBattlerites$y) <= 1) {
        
        filtered_Battlerites$battlerites <- c(filtered_Battlerites$battlerites, levels(fct_drop(data[['Battlerites']]))[1])
        
      } else {
        
        filtered_Battlerites$battlerites <- c(filtered_Battlerites$battlerites, levels(data[['Battlerites']])[round(input$filterBattlerites$y)])
        
      }
    }
  )
  
  observeEvent(
    eventExpr = input$unfilterBattlerites,
    handlerExpr = {
      
      filtered_Battlerites$battlerites <- c()
      
    }
  )
  
  
  create_battlerites_barplot <- function(agg_df, measure, input_champion,
                                         filtered_totaltime, filtered_championtime,
                                         filtered_region, filtered_league, filtered_servertype,
                                         filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                         filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                         filtered_playertype, filtered_date, filtered_battlerites, filtered_ping,
                                         filter_player, player_selection) {
    
    if (measure == 'winrate') {
      
      initial_title <- paste0(input_champion, ' Win Rate Split by Battlerite Choice')
      
    } else if (measure == 'winrateadjusted') {
      
      initial_title <- paste0(input_champion, ' Win Rate (Adjusted) Split by Battlerite Choice')
      
    } else if (measure == 'pickrate') {
      
      initial_title <- paste0(input_champion, ' Battlerite Pick Rate')
      
    } else {
      
      initial_title <- paste0(input_champion, ' Battlerite Pick Rate (Adjusted)')
      
    }
    
    sub_title <- create_dynamic_text(filtered_totaltime, filtered_championtime,
                                        filtered_region, filtered_league, filtered_servertype,
                                        filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                        filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                        filtered_playertype, filtered_date, filtered_battlerites, filtered_ping,
                                        filter_player, player_selection, barplot = TRUE)
    
    Colors <- setNames(c('#FF4FCE', '#A8FF47', '#FFB800', '#FF2A1C', '#00FFE9', '#0095FF', '#BBFFFC'),
                       levels(agg_df$`Battlerite Type`))
    
    Colors_subset <- Colors[as.vector(agg_df$`Battlerite Type`)]
    
    if (measure == 'winrate' | measure == 'winrateadjusted') {
      
      bar <- ggplot(data = agg_df, aes(x = Battlerites, y = Win_Rate, fill = `Battlerite Type`)) +
        ylab('Win Rate (Percent)')
      
    } else {
      
      bar <- ggplot(data = agg_df, aes(x = Battlerites, y = Pick_Rate, fill = `Battlerite Type`)) +
        ylab('Pick Rate (Percent)')
      
    }
    
    bar <- bar + geom_bar(width = 1, stat = 'identity') +
      coord_flip() +
      scale_fill_manual(values = Colors_subset) +
      xlab('Battlerites') +
      theme(legend.position = 'none', plot.title = element_text(size = 16, face = 'bold'))
    
    if (sub_title == '') {
      
      bar <- bar + ggtitle(initial_title, subtitle = waiver())
      
    } else {
      
      final_title <- paste0(initial_title, ' Filtered on:')
      
      bar <- bar + ggtitle(final_title, subtitle = sub_title)
      
    }
    
    return(bar)
    
  }
    
    
  output$SelectedBattlerites <- renderUI({
    
    req(input$champion != 'None')
    if (!is.null(filtered_Battlerites$battlerites)) {
    
      if(length(filtered_Battlerites$battlerites) == 1) {
      
    HTML(paste0('<font size = "2">Current Battlerite Selection: <b>',
                filtered_Battlerites$battlerites,
                '.</font></b>'))
        
      } else if (length(filtered_Battlerites$battlerites) == 2) {
        
        HTML(paste0('<font size = "2">Current Battlerite Selection: <b>',
                    filtered_Battlerites$battlerites[1],
                    ', ',
                    filtered_Battlerites$battlerites[2],
                    '.</font></b>'))
        
        
      } else if (length(filtered_Battlerites$battlerites) == 3) {
        
        HTML(paste0('<font size = "2">Current Battlerite Selection: <b>',
                    filtered_Battlerites$battlerites[1],
                    ', ',
                    filtered_Battlerites$battlerites[2],
                    ', ',
                    filtered_Battlerites$battlerites[3],
                    '.</font></b>'))
        
      } else if (length(filtered_Battlerites$battlerites) == 4) {
        
        HTML(paste0('<font size = "2">Current Battlerite Selection: <b>',
                    filtered_Battlerites$battlerites[1],
                    ', ',
                    filtered_Battlerites$battlerites[2],
                    ', ',
                    filtered_Battlerites$battlerites[3],
                    ', ',
                    filtered_Battlerites$battlerites[4],
                    '.</font></b>'))
        
        
      } else {
        
        if (input$measure == 'winrate' | input$measure == 'winrateadjusted') {
        
        HTML(paste0('<font size = "2">Current Battlerite Selection: <b>',
                    filtered_Battlerites$battlerites[1],
                    ', ',
                    filtered_Battlerites$battlerites[2],
                    ', ',
                    filtered_Battlerites$battlerites[3],
                    ', ',
                    filtered_Battlerites$battlerites[4],
                    ', ',
                    filtered_Battlerites$battlerites[5],
                    ' </b>and the overall win rate is ',
                    battlerites_agg1(),
                    ' percent.</font>'))
        
        } else {
          
          HTML(paste0('<font size = "2">Current Battlerite Selection: <b>',
                      filtered_Battlerites$battlerites[1],
                      ', ',
                      filtered_Battlerites$battlerites[2],
                      ', ',
                      filtered_Battlerites$battlerites[3],
                      ', ',
                      filtered_Battlerites$battlerites[4],
                      ', ',
                      filtered_Battlerites$battlerites[5],
                      ' </b>and the overall pick rate is ',
                      battlerites_agg1(),
                      ' percent.</font>'))
          
        }
        
        
        }
      
    }
    
  })
    

  output$Battlerites <- renderPlot({
    
    req(input$champion != 'None')
    if(is.null(filtered_Battlerites$battlerites)) {
      
    create_battlerites_barplot(battlerites_agg1(), input$measure, input$champion,
                               filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                               filtered_League(), filtered_Servertype(), filtered_Map(),
                               filtered_Casual(), filtered_Mount(), filtered_Title(),
                               filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                               filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                               filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                               filter_player(), player_selection())
    
      } else if (length(filtered_Battlerites$battlerites) < 5) {
      
      create_battlerites_barplot(battlerites_agg1(), input$measure, input$champion,
                                 filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                 filtered_League(), filtered_Servertype(), filtered_Map(),
                                 filtered_Casual(), filtered_Mount(), filtered_Title(),
                                 filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                 filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                 filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                 filter_player(), player_selection())
      
    }

  })
  
  #Battlerites hover label
  output$Battlerites_tooltip <- renderUI({
    
    check <- TRUE
    
    if(!is.null(filtered_Battlerites$battlerites)) {
      
      if(length(filtered_Battlerites$battlerites) < 5) {
    
    info <- create_tooltip(input$hoverBattlerites, battlerites_agg1(), battlerites_agg1()$Battlerites,
                           input$measure, 40, 0)
    
      } else {
        
        check <- FALSE
        
      }
      
      
      } else {
      
        info <- create_tooltip(input$hoverBattlerites, battlerites_agg1(), battlerites_agg1()$Battlerites,
                               input$measure, 40, 0) 
        
      }
    
    if (check) {
    
      if (!is.null(info$Value)) {
        
        wellPanel(
          style = info$style,
          p(HTML(paste0("<font size = '1'>Value: ", info$Value, "<br/>Sample Size: ", info$Size, "</font>")))
        )
        
      }
      
    }
    
  })
  
  #Best overall set of 5 battlerites
  
  pre_agg_battlerites <- reactive({
    
    
    data <- pre_agg(filtered_data2(),
                    input$measure,
                    'Battlerites',
                    TRUE,
                    Pickrateadjusted_BestOverallBattlerites())
    
    })

  
  
  Battlerites_df_prefilter <- reactive({
    
    if (!(input$measure == 'pickrate' & Pickrateadjusted_BestOverallBattlerites() == 1)) {
      
      data <- pre_agg_battlerites() %>%
        ungroup() %>%
        select(Battlerites, Game_Won) %>%
        group_by(Battlerites) %>%
        summarize(Win_Rate = round(mean(Game_Won)*100, 2),
                  Pick_Rate = round(n()/Baseline_Champion_Pick_Size()*100, 2),
                  Sample_Size = n())
      
    } else {
      
      data <- pre_agg_battlerites()
      data
      
    }
    
    if (input$measure == 'winrate' | input$measure == 'winrateadjusted') {
      
      data <- arrange(data, desc(Win_Rate))
      
    } else {
      
      data <- arrange(data, desc(Pick_Rate))
      
    }
    
    data
    
  })
  
  Battlerites_df <- reactive ({
      
      data2 <- Battlerites_df_prefilter() %>%
        filter(Sample_Size >= 3)
      
      if (dim(data2)[1] == 0) {
        
        data2 <- Battlerites_df_prefilter() %>%
          filter(Sample_Size >= 2)
        
        if(dim(data2)[1] == 0) {
          
          data2 <- Battlerites_df_prefilter()
          
        }
      
      
      }
      
      data2

  })
      
  output$BestOverallBattlerites <- renderUI({
    
    dynamic_name <- create_dynamic_text(filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                        filtered_League(), filtered_Servertype(), filtered_Map(),
                                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                        filter_player(), player_selection(), barplot = FALSE)
    
    if (input$measure == 'winrate' | input$measure == 'winrateadjusted') {
      
      if (dynamic_name == '') {

    HTML(paste0('<font size = "2"> The best overall battlerites for ',
                input$champion,
                ' is <b>',
                head(Battlerites_df(), 1)$Battlerites,
                '</b> with a win rate of ',
                head(Battlerites_df(), 1)$Win_Rate,
                ' percent and has ',
                head(Battlerites_df(), 1)$Sample_Size,
                ' observations.</font>'))
        
      } else {
        
        HTML(paste0('<font size = "2"> The best overall battlerites for ',
                    input$champion,
                    ' while filtering for ',
                    dynamic_name,
                    ' is <b>',
                    head(Battlerites_df(), 1)$Battlerites,
                    '</b> with a win rate of ',
                    head(Battlerites_df(), 1)$Win_Rate,
                    ' percent and has ',
                    head(Battlerites_df(), 1)$Sample_Size,
                    ' observations.</font>')) 
        
        
      }
      
    } else {
      
      if (dynamic_name == '') {
      
      HTML(paste0('<font size = "2"> The most popular battlerites for ',
                  input$champion,
                  ' is <b>',
                  head(Battlerites_df(), 1)$Battlerites,
                  '</b> with a pick rate of ',
                  head(Battlerites_df(), 1)$Pick_Rate,
                  ' percent and has ',
                  head(Battlerites_df(), 1)$Sample_Size,
                  ' observations.</font>'))
      } else {
        
        HTML(paste0('<font size = "2"> The most popular battlerites for ',
                    input$champion,
                    ' while filtering for ',
                    dynamic_name,
                    ' is <b>',
                    head(Battlerites_df(), 1)$Battlerites,
                    '</b> with a pick rate of ',
                    head(Battlerites_df(), 1)$Pick_Rate,
                    ' percent and has ',
                    head(Battlerites_df(), 1)$Sample_Size,
                    ' observations.</font>'))
        
      }
      
    }
    
  })
  
#################  
###PLAYER DATA###
#################
  
  #Player Filter information (what is being filtered)
  output$Player_Filter_Info <- renderUI({
    
    dynamic_name <- create_dynamic_text(filtered_Totaltime(), filtered_Championtime(), filtered_Region(),
                                        filtered_League(), filtered_Servertype(), filtered_Map(),
                                        filtered_Casual(), filtered_Mount(), filtered_Title(),
                                        filtered_Avatar(), filtered_Outfit(), filtered_Attachment(),
                                        filtered_Pose(), filtered_RegionGroup(), filtered_PlayerType(),
                                        filtered_Date(), filtered_Battlerites$battlerites, filtered_Ping(),
                                        filter_player(), player_selection(), barplot = FALSE)
    
    req(dynamic_name != '')
    
    HTML(paste0('<font size = "2"><b>Players in the table directly below is filtered to ', dynamic_name, '</font></b>'))
    
  })
  
  
  
  #Player Table
  #Including User_ID and Name since there are some names with foreign characters that may not be parsed correctly
  player_agg <- reactive({
    
    req(!is.null(filtered_data()))
    
    data <- filtered_data() %>%
      group_by(Game_ID, User_ID, Name) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0)) %>%
      ungroup() %>%
      group_by(User_ID, Name) %>%
      summarize(Champion_Win_Rate = round(mean(Game_Won)*100, 2),
                Num_Games = n()) 
    
    data <- inner_join(x = data, y = User_Total_Games_df(), by = c('User_ID' = 'User_ID')) %>%
      mutate(Pick_Rate = round(Num_Games/Total_Games*100,2)) %>%
      select(-Num_Games)
    
    data

  })
  
  output$Players <- DT::renderDataTable(expr = {
    
    dat <- datatable(player_agg(), selection = list(mode = 'single', target = 'cell'),
                     options = list(searching = TRUE, paging = TRUE),
                     callback = JS(gsub("\n", "", paste0("table.on('click.dt', 'td', function() {
                                                         var row_=table.cell(this).index().row;
                                                         var col=table.cell(this).index().column;
                                                         var rnd= Math.random();
                                                         var data = [row_, col, rnd];
                                                         Shiny.onInputChange(", "'PlayerDataRow'", ",data);
  });"))))
    
    dat
    
  })
  
  #Drill down Player data observe event
  observeEvent(
    eventExpr = input$PlayerDataRow,
    handlerExpr = {
      
      row <- input$Players_cell_clicked
      selected_player <- player_agg()[row$row, 'User_ID']$User_ID
      player_selection(selected_player)
      
      game_selection(0)
      round_selection(-1)
      round_player_selection(0)
      
    }
  )
  
  #Filter data on selected player button
  observeEvent(
    eventExpr = input$filterPlayer,
    handlerExpr = {
      
      filter_player(1)
      
    }
  ) 
  
  #Unfilter button
  observeEvent(
    eventExpr = input$unfilterPlayer,
    handlerExpr = {
      
      filter_player(0)
      
    }
  )
  

  #Player Games Table
  player_games <- reactive({
    
    req(player_selection() != 0)
    
    data <- filter(filtered_data(), User_ID %in% player_selection()) %>%
      group_by(Game_ID, Server_Type, Ranking_Type, Map, Region, Date) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    data
    
  })
  
  output$Players_Game <- DT::renderDataTable(expr = {
    
    dat <- datatable(player_games(), selection = list(mode = 'single', target = 'cell'),
                     options = list(searching = FALSE, paging = TRUE),
                     callback = JS(gsub("\n", "", paste0("table.on('click.dt', 'td', function() {
                                                         var row_=table.cell(this).index().row;
                                                         var col=table.cell(this).index().column;
                                                         var rnd= Math.random();
                                                         var data = [row_, col, rnd];
                                                         Shiny.onInputChange(", "'PlayerGamesRow'", ",data);
  });"))))
    
    dat
    
})
  
  #Drill down Player Games observe event
  observeEvent(
    eventExpr = input$PlayerGamesRow,
    handlerExpr = {
      
      row <- input$Players_Game_cell_clicked
      selected_game <- player_games()[row$row, 'Game_ID']$Game_ID
      game_selection(selected_game)

      round_selection(-1)
      round_player_selection(0)
      
    }
  )
  
  
  #Player Game specific data round level
  
  filtered_game_df <- reactive({
    req(game_selection() != 0)
    
    data <- filter(df(), Game_ID %in% game_selection())
    data
    
  })
  
  player_rounds <- reactive({
    
    req(game_selection() != 0)
    
    data <- filtered_game_df() %>%
      select(Round, User_ID, Name, Champion, Round_Won, Highest_Score,
             Left_During_Round, User_Leave_Type, Forfeited,
             First_Orb, Queue_Time)
    data
    
  })
  
  output$Players_Round <- DT::renderDataTable(expr = {
    
    dat <- datatable(player_rounds(), selection = list(mode = 'single', target = 'cell'),
                     options = list(searching = FALSE, paging = FALSE),
                     callback = JS(gsub("\n", "", paste0("table.on('click.dt', 'td', function() {
                                                         var row_=table.cell(this).index().row;
                                                         var col=table.cell(this).index().column;
                                                         var rnd= Math.random();
                                                         var data = [row_, col, rnd];
                                                         Shiny.onInputChange(", "'PlayerRoundRow'", ",data);
  });")))) %>%
      
      formatStyle('User_ID', target = 'row',
                  backgroundColor = styleEqual(c(player_selection()),
                                               c('#D55E00')))
    
    
    dat
    
})
  
  #Drill down specific player within round observe event
  observeEvent(
    eventExpr = input$PlayerRoundRow,
    handlerExpr = {
      
      row <- input$Players_Round_cell_clicked
      selected_round <- player_rounds()[row$row, 'Round']$Round
      round_selection(selected_round)
      selected_player <- player_rounds()[row$row, 'User_ID']$User_ID
      round_player_selection(selected_player)
      
    }
  )
  
  #Selected player round stats
  round_player_df <- reactive({
    
    req(round_selection() != -1)
    
    data <- filter(filtered_game_df(), Round %in% round_selection()) %>%
      filter(User_ID %in% round_player_selection()) 
      
  })
  
  round_stats <- reactive({
    
    req(!is.null(round_player_df()))
    
    data <- round_player_df() %>%
      select(Kills, Deaths, Total_Score, Damage, Damage_Received, Protection, Protection_Received, Control,
             Control_Received, Energy_Gained, Energy_Used, Num_Energy_Used, Abilities_Used,
             Orb_Kills, Health_Shards, Energy_Shards, Time_Alive) %>%
      gather(key = 'Measure', value = 'Value')
    
  })
  
  output$Round_Stats<- DT::renderDataTable(expr = {
    
    dat <- datatable(round_stats(), selection = list(mode = 'single', target = 'cell'),
                     options = list(searching = FALSE, paging = FALSE))
    
    dat
    
})
  
  #Selected player information
  player_info <- reactive({
    
    req(!is.null(round_player_df()))
  
    data <- round_player_df() %>%
      select(League, Division, Division_Rating, Total_Time_Played, Champion_Time_Played, Battlerites,
             Title, Outfit, Attachment, Pose, Mount) %>%
      gather(key = 'Measure', value = 'Value')
    
  })
  
  output$Player_Information<- DT::renderDataTable(expr = {
    
    dat <- datatable(player_info(), selection = list(mode = 'single', target = 'cell'),
                     options = list(searching = FALSE, paging = FALSE))
    
    dat
    
  })
  
  
  }




shinyApp(ui = ui, server = server)




