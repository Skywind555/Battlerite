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

setwd('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects/Data/2019/July/10')

df <- read_csv('Jul-10-2019_1.csv')
region_coordinates <- read_excel('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects/Reference Files/region.xlsx')
#Remove the ally and enemy data other than comp/team roles
df <- df[, c(1:73, 112, 113)]

df$Champion <- factor(df$Champion)
df$Region <- factor(df$Region)
df$League <- factor(df$League, levels = rev(c('Grand Champion', 'Champion', 'Diamond', 'Platinum', 'Gold',
                                          'Silver', 'Bronze')), ordered = TRUE)
region_coordinates$Group <- factor(region_coordinates$Group)
df$`Battlerite 1` <- factor(df$`Battlerite 1`)
df$`Battlerite 2` <- factor(df$`Battlerite 2`)
df$`Battlerite 3` <- factor(df$`Battlerite 3`)
df$`Battlerite 4` <- factor(df$`Battlerite 4`)
df$`Battlerite 5` <- factor(df$`Battlerite 5`)

unique_champions <- unique(df$Champion)

#Convert seconds to hours and round up to use in input slider
df <- mutate(df, Total_Time_Played = ceiling(Total_Time_Played/3600),
                 Champion_Time_Played = ceiling(Champion_Time_Played/3600))

#Put Change Server_Type variable to show 2v2, 3v3, solo queue
df <- mutate(df, Server_Type = ifelse(Solo_Queue == 1 & Match_Type == 'LEAGUE3V3', 'Solo Queue', 
                                      ifelse(Server_Type == 'QUICK3V3', '3V3',
                                             ifelse(Server_Type == 'QUICK2V2', '2V2', Server_Type))))
df$Server_Type <- factor(df$Server_Type)
           

ui <- navbarPage('Navbar',
                 tabPanel('Champion',
                          fluidRow(
                            column(width = 4,
                                   selectInput(inputId = 'champion',
                                               label = 'Champion',
                                               multiple = FALSE,
                                               choices = c('None', levels(df$Champion)),
                                               selected = 'None')
                                   
                            ),
                            column(width = 4,
                                   uiOutput('Interactive_Slider1')
                            ),
                            column(width = 4,
                                   uiOutput('Interactive_Slider2')
                            )
                          ),
                          
                          fluidRow(
                            column(width = 12,
                                   radioButtons(inputId = 'measure',
                                                label = 'Measure',
                                                choices = c('Win Rate' = 'winrate',
                                                            'Win Rate(Adjusted)' = 'winrateadjusted',
                                                            'Pick Rate' = 'pickrate',
                                                            'Pick Rate(Adjusted)' = 'pickrateadjusted'))
                            )
                          ),
                          
                          fluidRow(
                            column(width = 12,
                                   htmlOutput('Chosen_Region'),
                                   leafletOutput(outputId = 'Region'),
                                   actionButton(inputId = 'unfilterRegion',
                                                label = 'Unfilter Region')
                            )
                          ),
                          
                          fluidRow(
                            column(width = 6,
                                   fluidRow(
                                     plotOutput(outputId = 'Battlerites',
                                                dblclick = 'filterBattlerites',
                                                click = 'unfilterBattlerites',
                                                hover = hoverOpts('hoverBattlerites'),
                                                height = '600px'),
                                     uiOutput('Battlerites_tooltip')
                                   )
                            ),
                            
                            column(width = 6,
                                   fluidRow(width = 6,
                                            plotOutput(outputId = 'League',
                                                       dblclick = 'filterLeague',
                                                       click = 'unfilterLeague',
                                                       hover = hoverOpts('hoverLeague'),
                                                       height = '300px'),
                                            uiOutput('League_tooltip')
                                   ),
                                   fluidRow(width = 6,
                                            plotOutput(outputId = 'ServerType',
                                                       dblclick = 'filterServerType',
                                                       click = 'unfilterServerType',
                                                       hover = hoverOpts('hoverServerType'),
                                                       height = '300px'),
                                            uiOutput('ServerType_tooltip')
                                   )
                            )
                          ),
                          
                          fluidRow(
                            column(width = 6,
                                   fluidRow(
                                     plotOutput(outputId = 'Map',
                                                dblclick = 'filterMap',
                                                click = 'unfilterMap',
                                                hover = hoverOpts('hoverMap'),
                                                height = '600px'),
                                     uiOutput('Map_tooltip')
                                   )
                            ),
                            
                            column(width = 6,
                                   fluidRow(width = 6,
                                            plotOutput(outputId = 'Casual',
                                                       dblclick = 'filterCasual',
                                                       click = 'unfilterCasual',
                                                       hover = hoverOpts('hoverCasual'),
                                                       height = '300px'),
                                            uiOutput('Casual_tooltip')
                                   ),
                                   fluidRow(width = 6,
                                            plotOutput(outputId = 'Ping',
                                                       hover = hoverOpts('hoverPing'),
                                                       height = '300px'),
                                            uiOutput('Ping_tooltip')
                                   )
                            )
                          ),
                          
                          fluidRow(
                            column(width = 6,
                                   plotOutput(outputId = 'Stats',
                                              hover = hoverOpts('hoverStats'),
                                              height = '300px'),
                                   uiOutput('Stats_tooltip')
                            ),
                            
                            column(width = 6,
                                   htmlOutput('BestAllyChampion2v2'),
                                   htmlOutput('BestAllyChampion3v3'),
                                   htmlOutput('WorstMatchups2v2'),
                                   htmlOutput('WorstMatchups3v3')
                            )
                          ),
                          
                          fluidRow(
                            column(width = 4,
                                   plotOutput(outputId = 'Mount',
                                              dblclick = 'filterMount',
                                              click = 'unfilterMount',
                                              hover = hoverOpts('hoverMount'),
                                              height = '600px'),
                                   uiOutput('Mount_tooltip')
                            ),
                            
                            column(width = 4,
                                   plotOutput(outputId = 'Title',
                                              dblclick = 'filterTitle',
                                              click = 'filterTitle',
                                              hover = hoverOpts('hoverTitle'),
                                              height = '600px'),
                                   uiOutput('Title_tooltip')
                            ),
                            
                            column(width = 4,
                                   plotOutput(outputId = 'Avatar',
                                              dblclick = 'filterAvatar',
                                              click = 'unfilterAvatar',
                                              hover = hoverOpts('hoverAvatar'),
                                              height = '600px'),
                                   uiOutput('Avatar_tooltip')
                            )
                          ),
                          
                          fluidRow(
                            column(width = 4,
                                   plotOutput(outputId = 'Outfit',
                                              dblclick = 'filterOutfit',
                                              click = 'unfilterOutfit',
                                              hover = hoverOpts('hoverOutfit'),
                                              height = '300px'),
                                   uiOutput('Outfit_tooltip')
                            ),
                            
                            column(width = 4,
                                   plotOutput(outputId = 'Attachment',
                                              dblclick = 'filterAttachment',
                                              click = 'unfilterAttachment',
                                              hover = hoverOpts('hoverAttachment'),
                                              height = '300px'),
                                   uiOutput('Attachment_tooltip')
                            ),
                            
                            column(width = 4,
                                   plotOutput(outputId = 'Pose',
                                              dblclick = 'filterPose',
                                              click = 'unfilterPose',
                                              hover = hoverOpts('hoverPose'),
                                              height = '300px')
                                   )
                          ),
                          
                          fluidRow(
                            column(width = 12,
                                   div(DT::dataTableOutput('Players'), style = 'font-size:75%')
                            )
                          )
                 )
)

server <- function(input, output) {
  
  ###############
  ###############
  ###FUNCTIONS###
  ###############
  ###############
  
  
  #Filter data on champion select first
  
  
  create_filtered_data <- function(champ_df, input_champion, input_total_time_played_min, input_total_time_played_max, 
                                   input_champ_played_min, input_champ_played_max,
                                   filtered_region, filtered_league, filtered_servertype,
                                   filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                   filtered_outfit, filtered_attachment, filtered_pose) {
    #Only include if have selected a champion
    req(input_champion != 'None')
    
    #Filter total time played
    data <- filter(champ_df, between(Total_Time_Played, input_total_time_played_min, input_total_time_played_max))
    
    #Filter champ time played
    data <- filter(data, between(Champion_Time_Played, input_champ_played_min, input_champ_played_max))
    
    #Filter on region
    if (filtered_region != 0) {
      data <- filter(data, Region %in% filtered_region)
    }
    
    #Filter on number of battlerites selected, up to five
    #if (sum(ifelse(filtered_battlerites == 0, 1, 0)) == 4) {
      
     # data <- filter(data, filtered_battlerites[1] == 1)
      
  #  } else if (sum(ifelse(filtered_battlerites == 0, 1, 0)) == 3) {
      
   #   data <- filter(data, filtered_battlerites[1] == 1) %>%
    #    filter(filtered_battlerites[2] == 1)
      
  #  } else if (sum(ifelse(filtered_battlerites == 0, 1, 0)) == 2) {
      
   #   data <- filter(data, filtered_battlerites[1] == 1) %>%
    #    filter(filtered_battlerites[2] == 1) %>%
     #   filter(filtered_battlerites[3] == 1)
      
    #} else if (sum(ifelse(filtered_battlerites == 0, 1, 0)) == 1) {
      
    #  data <- filter(data, filtered_battlerites[1] == 1) %>%
     #   filter(filtered_battlerites[2] == 1) %>%
      #  filter(filtered_battlerites[3] == 1) %>%
       # filter(filtered_battlerites[4] == 1)
      
   # } else if (sum(ifelse(filtered_battlerites == 0, 1, 0)) == 0) {
      
    #  data <- filter(data, filtered_battlerites[1] == 1) %>%
    #    filter(filtered_battlerites[2] == 1) %>%
    #    filter(filtered_battlerites[3] == 1) %>%
    #    filter(filtered_battlerites[4] == 1) %>%
    #    filter(filtered_battlerites[5] == 1)
      
 #   }
    
    #Filter on League
    if (filtered_league != 0) {
      data <- filter(data, League %in% filtered_league)
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
      
      filtered_variable(levels(data[[variable]])[round(input_filter_y)])
      
    }
    
  }
  

  
  ###Aggregates###
  
  pre_agg <- function(filtered_data, measure, variable) {
    
    var <- as.name(variable)
    
    if (measure == 'winrate') {
      
      data <- filtered_data %>%
        select(Game_ID, User_ID, Round_Won, !!var) %>%
        group_by(Game_ID, User_ID, !!var) %>%
        summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
      
      data
      
      return(data)
      
    }
    
    
  }
  
  region_agg <- function(pre_agg_data, measure, ref_regions) {
    
    if (measure == 'winrate') {
    
    data <- pre_agg_data %>%
      ungroup() %>%
      select(Region, Game_Won) %>%
      group_by(Region) %>%
      summarize(Win_Rate = mean(Game_Won)) %>%
      mutate(Win_Rate = round(Win_Rate*100, 2))

    }
    
    ref_regions$Region <- factor(ref_regions$Region, levels = levels(data$Region))
    
    #Add coordinates
    coord_data <- inner_join(x = data, y = ref_regions, by = c('Region' = 'Region'))
    
    return(coord_data)
    
  }
  
  common_agg <- function(pre_agg_data, measure, variable, sort_descending) {
    
    var <- as.name(variable)
    
    if (measure == 'winrate') {
      
      data <- pre_agg_data %>%
        ungroup() %>%
        select(!!var, Game_Won) %>%
        group_by(!!var) %>%
        summarize(Win_Rate = mean(Game_Won)) %>%
        mutate(Win_Rate = round(Win_Rate*100,2))
      
      if (sort_descending) {
        data <- mutate(data, !!var := fct_reorder(!!var, Win_Rate))
      }
    }
    
    return(data)
    
  }
  
  
  ###Plots###
  
  create_region_map <- function(region_df, input_champion) {
    
    req(input_champion != 'None')
    
    pal <- colorFactor(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"), 
                            levels(region_df$Group))
    
    m <- leaflet(data = region_df) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       stroke = FALSE,
                       layerId = ~Region,
                       color = ~pal(Group),
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
                                     ' | Win Rate: ', region_df$Win_Rate)
      )
    return(m)
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  #Group bars by 2v2, 3v3, solo queue etc stacked
  create_barplot <- function(agg_df, variable) {
    
    var <- as.name(variable)
    
    bar <- ggplot(data = agg_df, aes(x = !!var, y = Win_Rate)) +
      geom_bar(width = 1, stat = 'identity') +
      coord_flip() +
      ylab('Win Rate') +
      xlab(variable)
     
    
    return(bar)
    
  }

  
  ### Labels ###
  
  create_region_label <- function(filtered_region) {
    
    req(filtered_region != 0)
    
    string <- HTML(paste0('<font size = "5">You have selected ', filtered_region, '</font>'))
    
    return(string)
    
  }
  
  create_tooltip <- function(input_hover, count_aggregate_df, count_aggregate, left_adjust, top_adjust) {
    
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
    
    
    winrate <- count_aggregate_df[count_aggregate == Name,]$Win_Rate
    return(list("Name" = Name, "Win" = winrate, "style" = style))
    
    
  }
  
  
  
  ###############
  ###############
  ###CHAMPIONS### 
  ###############
  ###############
  
  #Set initial reactive values
  filtered_Region <- reactiveVal(0)
  filtered_Battlerites <- reactiveValues()
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
  
  #Filter on champion select first
  #Filter on initial champion select
  champ_df <- reactive({
    
    req(input$champion != 'None')
    Cdf <- filter(df, Champion == input$champion)
    
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
  
  ###Interactive sliders###
  
  output$Interactive_Slider1 <- renderUI({
    
    req(input$champion != 'None')
    
    sliderInput(inputId = 'total_played',
                label = 'Total Time Played (hours):',
                min = 0,
                max = max(champ_df()$Total_Time_Played, na.rm = TRUE),
                value = c(0, max(champ_df()$Total_Time_Played, na.rm = TRUE)))
    
 
  })
  
  output$Interactive_Slider2 <- renderUI({
    
    req(input$champion != 'None')
    
    sliderInput(inputId = 'champ_played',
                label = 'Total Time Champion Played (hours):',
                min = 0,
                max = max(champ_df()$`Champion_Time_Played`, na.rm = TRUE),
                value = c(0, max(champ_df()$`Champion_Time_Played`, na.rm = TRUE)))
    
    
  })
  
  
  #Filter starting data frame with any filters applied
  filtered_data <- reactive({create_filtered_data(champ_df(),
                                                  input$champion,
                                                  input$total_played[1],
                                                  input$total_played[2],
                                                  input$champ_played[1],
                                                  input$champ_played[2],
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
                                                  filtered_Pose()
                                                  )})
  

  ############
  ###REGION###
  ############

  
  #Create pre-aggregate data for region
  pre_agg_region <- reactive({pre_agg(filtered_data(),
                                  input$measure,
                                  'Region')})
  
  #Create aggregate region data
  Region_df <- reactive({region_agg(pre_agg_region(),
                                 input$measure,
                                 region_coordinates)})
  

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
  output$Region <- renderLeaflet({create_region_map(Region_df(), input$champion)
  })
  
  #Filtered region label
  output$Chosen_Region <- renderUI({create_region_label(filtered_Region())})
 
  ############
  ###LEAGUE###
  ############
  
  pre_agg_league <- reactive({pre_agg(filtered_data(),
                                      input$measure,
                                      'League')})
  
  League_df <- reactive({common_agg(pre_agg_league(),
                                    input$measure,
                                    'League',
                                    FALSE)})
  
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
    create_barplot(League_df(), 'League')})
  
  #League hover label
  output$League_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverLeague, League_df(), League_df()$League, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  ################
  ###SERVERTYPE###
  ################
  
  pre_agg_servertype <- reactive({pre_agg(filtered_data(),
                                      input$measure,
                                      'Server_Type')})
  
  Servertype_df <- reactive({common_agg(pre_agg_servertype(),
                                    input$measure,
                                    'Server_Type',
                                    FALSE)})
  
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
    
    create_barplot(Servertype_df(), 'Server_Type')
    
  })
  
  #Matchtype hover label
  output$ServerType_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverServerType, Servertype_df(), Servertype_df()$Server_Type, 40, 268)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  #########
  ###MAP###
  #########
  
  pre_agg_map <- reactive({pre_agg(filtered_data(),
                                          input$measure,
                                          'Map')})
  
  Map_df <- reactive({common_agg(pre_agg_map(),
                                        input$measure,
                                        'Map',
                                        TRUE)})
  
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
    
    create_barplot(Map_df(), 'Map')
    
  })
  
  #Matchtype hover label
  output$Map_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverMap, Map_df(), Map_df()$Map, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  
  #################
  ###BATTLERITES###
  #################
  
  
  
  
}



                        

shinyApp(ui = ui, server = server)



