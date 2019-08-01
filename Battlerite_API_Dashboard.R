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



df <- read_csv('Data/2019/July/10/Jul-10-2019_1.csv')
region_coordinates <- read_excel('Reference Files/region.xlsx')
#Remove the ally and enemy data other than comp/team roles
df <- df[, c(1:73, 112, 113)]

df$Champion <- factor(df$Champion)
df$Region <- factor(df$Region)
df$League <- factor(df$League, levels = rev(c('Grand Champion', 'Champion', 'Diamond', 'Platinum', 'Gold',
                                              'Silver', 'Bronze')), ordered = TRUE)
region_coordinates$`Region Group` <- factor(region_coordinates$`Region Group`)


#Attach Latitude/Longitude, City, Region Group
region_coordinates$Region <- factor(region_coordinates$Region, levels = levels(df$Region))
df <- inner_join(x = df, y = region_coordinates, by = c('Region' = 'Region'))
df$`Region Group` <- factor(df$`Region Group`)
df <- mutate(df, Region = Region_Clean)
df$Region <- factor(df$Region)


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

#Convert 'None' to 'Unranked' - there should only be ranked or unranked
df <- mutate(df, Ranking_Type = ifelse(Ranking_Type == 'NONE', 'UNRANKED', Ranking_Type))
df$Ranking_Type <- factor(df$Ranking_Type)

#Add bot player_type information. If outfit is a blank, is a bot (All default battlerites selected for all players
#With blank outfits for all champions) Or mount is blank, is a bot
df <- mutate(df, Player_Type = ifelse(is.na(Outfit) | is.na(Mount), 'BOT', 'PLAYER'))

#Bots use default outfits except for mounta, fill in blanks with defaults
#Bots can also have no mount recorded, fill in blank with RAM
df <- mutate(df, Outfit = ifelse(is.na(Outfit), 'DEFAULT OUTFIT', Outfit),
             Attachment = ifelse(is.na(Attachment), 'DEFAULT WEAPON', Attachment),
             Pose = ifelse(is.na(Pose), 'DEFAULT POSE', Pose),
             Mount = ifelse(is.na(Mount), 'RAM', Mount))



#Convert > 200 ping to 200 ping
df <- mutate(df, Ping = 10*round(Ping/10))
df <- mutate(df, Ping = ifelse(is.na(Ping), -1, ifelse(Ping > 200, 1000, Ping)))
df$Ping <- as.character(df$Ping)
df <- mutate(df, Ping = ifelse(Ping == '-1', 'Unknown', ifelse(Ping == 1000, 'Time out', Ping)))

df$Ping <- factor(df$Ping, levels = c('0', '10', '20', '30', '40', '50', '60', '70', '80', '90', '100',
                                      '110', '120', '130', '140', '150', '160', '170', '180', '190', '200',
                                      'Time out', 'Unknown'))

#Convert blank queue times to 0 (bots)
df <- mutate(df, Queue_Time = ifelse(is.na(Queue_Time), 0, Queue_Time))

#Round dates to days
df <- mutate(df, Date = substr(Date, 1, 10))


df$Player_Type <- factor(df$Player_Type)

#Count only each combination once in one specific order
df$Team_Roles <- sapply(df$Team_Roles, function(x) strsplit(x, ", "))
df$Team_Roles <- sapply(df$Team_Roles, function(x) paste(sort(x), collapse = ' '))
df$Enemy_Roles <- sapply(df$Enemy_Roles, function(x) strsplit(x, ", "))
df$Enemy_Roles <- sapply(df$Enemy_Roles, function(x) paste(sort(x), collapse = ' '))
df$Battlerites <- sapply(df$Battlerites, function(x) strsplit(x, ", "))
df$Battlerites <- names(sapply(df$Battlerites, function(x) paste(sort(x), collapse = ' ', sep = ', ')))

df$Battlerites <- factor(df$Battlerites)

#Group play time into buckets
df <- mutate(df, Total_Time_Played = ifelse(is.na(Total_Time_Played), 'Unknown', 
                                            ifelse(Total_Time_Played <= 10, 'Under 10',
                                                  ifelse(Total_Time_Played <= 50, '10-50',
                                                         ifelse(Total_Time_Played <= 100, '50-100',
                                                                ifelse(Total_Time_Played  <= 200, '100-200',
                                                                       ifelse(Total_Time_Played <= 500, '200-500', 'Over 500')))))),
             Champion_Time_Played = ifelse(is.na(Champion_Time_Played), 'Unknown', 
                                           ifelse(Champion_Time_Played <= 10, 'Under 10',
                                                 ifelse(Champion_Time_Played <= 50, '10-50',
                                                        ifelse(Champion_Time_Played <= 100, '50-100',
                                                               ifelse(Champion_Time_Played  <= 200, '100-200',
                                                                      ifelse(Champion_Time_Played <= 500, '200-500', 'Over 500'))))))
)

df$Total_Time_Played <- factor(df$Total_Time_Played, levels = c('Under 10', '10-50', '50-100', 
                                                                '100-200', '200-500', 'Over 500', 'Unknown'))
df$Champion_Time_Played <- factor(df$Champion_Time_Played, levels = c('Under 10', '10-50', '50-100', 
                                                                      '100-200', '200-500', 'Over 500', 'Unknown'))


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
                                   actionButton(inputId = 'ResetAll',
                                                label = 'Reset Everything'))
                          ),
                          
                          fluidRow(
                            column(width = 4,
                                   radioButtons(inputId = 'measure',
                                                label = 'Measure',
                                                choices = c('Win Rate' = 'winrate',
                                                            'Win Rate(Adjusted)' = 'winrateadjusted',
                                                            'Pick Rate' = 'pickrate',
                                                            'Pick Rate(Adjusted)' = 'pickrateadjusted'))
                            ),
                            
                            column(width = 4,
                                   uiOutput(outputId = 'ChampImage')),
                            
                            column(width = 4,
                                   htmlOutput('OverallWinRate'))
                          ),
                          
                          fluidRow(
                            column(width = 12,
                                   div(DT::dataTableOutput('RegionGroup'), style = "font-size:75%"),
                                   actionButton(inputId = 'unfilterRegionGroup',
                                                label = 'Unfilter Region Group')
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
                                   plotOutput(outputId = 'TotalTime',
                                              dblclick = 'filterTotalTime',
                                              click = 'unfilterTotalTime',
                                              hover = hoverOpts('hoverTotalTime'),
                                              height = '300px'),
                                   uiOutput('TotalTime_tooltip')
                            ),
                            column(width = 6,
                                   plotOutput(outputId = 'ChampionTime',
                                              dblclick = 'filterChampionTime',
                                              click = 'unfilterChampionTime',
                                              hover = hoverOpts('hoverChampionTime'),
                                              height = '300px'),
                                   uiOutput('ChampionTime_tooltip')
                            )
                          
                          ),
                          
                          fluidRow(
                            htmlOutput('BestOverallBattlerites')
                          ),
                          
                          fluidRow(
                            column(width = 6,
                                   fluidRow(
                                     htmlOutput('SelectedBattlerites'),
                                     actionButton(inputId = 'unfilterBattlerites',
                                                  label = 'Repick Battlerites'),
                                     plotOutput(outputId = 'Battlerites',
                                                dblclick = 'filterBattlerites',
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
                                   plotOutput(outputId = 'PlayerType',
                                              dblclick = 'filterPlayerType',
                                              click = 'unfilterPlayerType',
                                              hover = hoverOpts('hoverPlayerType'),
                                              height = '300px'),
                                   uiOutput('PlayerType_tooltip')),
                            column(width = 6,
                                   plotOutput(outputId = 'Date',
                                              dblclick = 'filterDate',
                                              click = 'unfilterDate',
                                              hover = hoverOpts('hoverDate'),
                                              height = '300px'),
                                   uiOutput('Date_tooltip'))
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
                            column(width = 3,
                                   htmlOutput('StatsWinLabel'),
                                   div(DT::dataTableOutput('StatsWin'), style = "font-size:70%")
                            ),
                            
                            column(width = 3,
                                   htmlOutput('StatsLoseLabel'),
                                   div(DT::dataTableOutput('StatsLose'), style = "font-size:70%")
                            ),
                            
                            column(width = 6,
                                   
                                   fluidRow(
                                     column(width = 3,
                                            htmlOutput('BestCompsLabel'),
                                            div(tableOutput('BestComps'), style = "font-size:80%")),
                                     column(width = 3, offset = 3,
                                            htmlOutput('WorstCompsLabel'),
                                            div(tableOutput('WorstComps'), style = "font-size:80%"))
                                   ),
                                   fluidRow(htmlOutput('BestAllyRoles')),
                                   
                                   fluidRow(
                                     column(width = 3,
                                            htmlOutput('BestMatchupsLabel'),
                                            div(tableOutput('BestMatchups'), style = "font-size:80%")),
                                     column(width = 3, offset = 3,
                                            htmlOutput('WorstMatchupsLabel'),
                                            div(tableOutput('WorstMatchups'), style = "font-size:80%"))
                                   ),
                                   fluidRow(htmlOutput('WorstAllyRoles'))
                                   
                          
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
                                              click = 'unfilterTitle',
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
                                              height = '300px'),
                                   uiOutput('Pose_tooltip')
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
  
  
  create_filtered_data <- function(champ_df, input_champion, filtered_totaltime, filtered_championtime,
                                   filtered_region, filtered_league, filtered_servertype,
                                   filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                   filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                   filtered_playertype, filtered_date, filtered_battlerites) {
    #Only include if have selected a champion
    req(input_champion != 'None')
    
    data <- champ_df
      
      print('filtered data')
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
      
      if (length(filtered_battlerites) > 0) {
        
        for (br in 1:length(filtered_battlerites)) {
          
          var <- as.name(filtered_battlerites[br])
          
          data <- filter(data, !!var == 1)
          
          #Adjust output battlerite to not include selected battlerite
          if (filtered_battlerites[br] %in% names(data)) {
            
            remove_col <- names(data) %in% filtered_battlerites[br]
            data <- data[, !remove_col]
            
          }
          
          }

          
        
        
      }
      
      
      
      
      #Check for columns with all 0's and remove
      remove_cols <- c()
      for (battlerite in c(names(data)[76:length(data)])) {
        
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
  
  pre_agg <- function(filtered_data, measure, variable) {
    
    req(!is.null(filtered_data))
    
    print(paste0('Pre agg', variable))
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
  
  region_agg <- function(filtered_data, measure) {
    req(!is.null(filtered_data))
    print('region_agg')
    if (measure == 'winrate') {
      
      data <- filtered_data %>%
        select(Game_ID, User_ID, Round_Won, City, Longitude, Latitude, Region, `Region Group`) %>%
        group_by(Game_ID, User_ID, City, Longitude, Latitude, Region, `Region Group`) %>%
        summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0)) %>%
        ungroup() %>%
        select(Region, `Region Group`, Longitude, Latitude, City, Game_Won) %>%
        group_by(Region, `Region Group`, Longitude, Latitude, City) %>%
        summarize(Win_Rate = round(mean(Game_Won)*100,2))
      
    }
    
    return(data)
    
  }
  
  region_table_agg <- function(region_agg) {
    req(!is.null(region_agg))
    print('region table agg')
    data <- region_agg %>%
      ungroup() %>%
      select(`Region Group`, Win_Rate) %>%
      group_by(`Region Group`) %>%
      summarize(Win_Rate = round(mean(Win_Rate), 2)) %>%
      arrange(desc(Win_Rate))
    
    return(data)
    
  }
  
  common_agg <- function(pre_agg_data, measure, variable, sort_descending) {
    req(!is.null(pre_agg_data))
    print(paste0('agg', variable))
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
  
  create_region_map <- function(region_df, input_champion, region_ref) {
    req(!is.null(region_df))
    req(input_champion != 'None')
    print('region map')
    pal <- colorFactor(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"), 
                       levels(region_ref$`Region Group`))
    
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
                                     ' | Win Rate: ', region_df$Win_Rate)
      )
    return(m)
    
  }
  
  
  create_region_table <- function(data, row_name) {
    req(!is.null(data))
    print('region data table')
    dat <- datatable(data, selection = list(mode = 'single', target = 'cell'), options = list(searching = FALSE,
                                                                                              paging = FALSE),
                     callback = JS(gsub("\n", "", paste0("table.on('click.dt', 'td', function() {
                                                         var row_=table.cell(this).index().row;
                                                         var col=table.cell(this).index().column;
                                                         var rnd= Math.random();
                                                         var data = [row_, col, rnd];
                                                         Shiny.onInputChange(", row_name, ",data);
  });"))) ) %>%
      formatStyle('Region Group', target = 'row',
                  backgroundColor = styleEqual(c('USA', 'Europe', 'OCE', 'Asia', 'South America', 'Other'),
                                               c("#D55E00", "#56B4E9", "#009E73", "#E69F00", "#0072B2", "#F0E442")))
    
    #c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
    return(dat)
    
    
}
  
  
  
  
  
  
  
  
  #Group bars by 2v2, 3v3, solo queue etc stacked
  create_barplot <- function(agg_df, variable) {
    req(!is.null(agg_df))
    print(paste0('bar plot', variable))
    var <- as.name(variable)
    var2 <- var
    
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
        
        pal = colorRampPalette(c('green', 'yellow', 'red'))
        Colors1 = setNames(pal(21), levels(agg_df$Ping)[1:21])
        Colors2 = setNames(c('#6D756B', '#6D756B'), c('Time out', 'Unknown'))
        Colors = c(Colors1, Colors2)
        
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
      
      
      bar <- ggplot(data = agg_df, aes(x = !!var, y = Win_Rate, fill = !!var2)) +
        geom_bar(width = 1, stat = 'identity') +
        ylab('Win Rate') +
        xlab(variable) +
        scale_fill_manual(values = Colors_subset) +
        theme(legend.position = 'none')
      
      
    } else {
      
      if (variable %in% c('Date', 'Title', 'Avatar')) {
        
        Color <- "#E69F00"
        bar <- ggplot(data = agg_df, aes(x = !!var, y = Win_Rate)) +
          geom_bar(width = 1, stat = 'identity', fill = Color) +
          ylab('Win Rate') +
          xlab(variable)
        
      }
      
    }
    
    if (variable != 'Ping') {
      
      bar <- bar + coord_flip()
      
    }
    
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
  
  #Tooltip for ping graph uses hover$x instead of hover$y
  create_tooltip2 <- function(input_hover, count_aggregate_df, count_aggregate, left_adjust, top_adjust) {
    
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
  
  #Filter on champion select first
  #Filter on initial champion select
  champ_df <- reactive({
    
    req(input$champion != 'None')
    Cdf <- filter(df, Champion == input$champion)
    print('champ df')
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
                                                  filtered_Battlerites$battlerites
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
      
      
    }
  )
  
  ####################
  ###CHAMPION IMAGE###
  ####################
  
  selected_champ <- reactive({
    
    req(input$champion != 'None')
    
    print('champ')
    data <- filter(champion_crosswalk, Value == input$champion)
    filename <- data$`Icon File Name`
    imagelink <- paste0('https://github.com/Skywind555/Personal-Projects/blob/master/ImagesConverted/', 
                        filename, '.png?raw=true')
    

    
  })
  
  output$ChampImage <- renderUI({
    
    req(input$champion != 'None')
  print('champ image')
    tags$img(src = selected_champ())
    
  })
  
  #####################
  ###OVERALL WINRATE###
  #####################
  
  #Overall winrate
  overallwinrate <- reactive({
    
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won) %>%
      group_by(Game_ID, User_ID) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    
    winrate <- round(mean(data$Game_Won)*100,2)
    
    
  })
  
  output$OverallWinRate <- renderUI({
    
    HTML(paste0('<font size = "3"><b> Overall winrate: ',
                overallwinrate(),
                ' percent.</font></b>'))
    
  })
  
  
  
  ############
  ###REGION###
  ############
  
  
  #Create aggregate region data
  Region_df <- reactive({region_agg(filtered_data(),
                                    input$measure)})
  
  
  
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
  output$Region <- renderLeaflet({create_region_map(Region_df(), input$champion, region_coordinates)
  })
  
  #Filtered region label
  output$Chosen_Region <- renderUI({create_region_label(filtered_Region())})
  
  ##################
  ###REGION GROUP###
  ##################
  
  #Pre agg
  pre_agg_regiongroup <- reactive({pre_agg(filtered_data(),
                                           input$measure,
                                           'Region Group')})
  
  RegionGroup_df <- reactive({common_agg(pre_agg_regiongroup(),
                                         input$measure,
                                         'Region Group',
                                         FALSE)})
  
  
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
    
    create_region_table(RegionGroup_df(), "'RegionGroupRow'")
    
  })
  
  #######################
  ###TOTAL TIME PLAYED###
  #######################
  
  pre_agg_totaltime <- reactive({pre_agg(filtered_data(),
                                    input$measure,
                                    'Total_Time_Played')})
  
  Totaltime_df <- reactive({common_agg(pre_agg_totaltime(),
                                  input$measure,
                                  'Total_Time_Played',
                                  FALSE)})
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
    
    create_barplot(Totaltime_df(), 'Total_Time_Played')
    
  })
  
  #Totaltime hover label
  output$TotalTime_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverTotalTime, Totaltime_df(), Totaltime_df()$Total_Time_Played, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  ##########################
  ###CHAMPION TIME PLAYED###
  ##########################
  
  pre_agg_championtime <- reactive({pre_agg(filtered_data(),
                                         input$measure,
                                         'Champion_Time_Played')})
  
  Championtime_df <- reactive({common_agg(pre_agg_championtime(),
                                       input$measure,
                                       'Champion_Time_Played',
                                       FALSE)})
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
    
    create_barplot(Championtime_df(), 'Champion_Time_Played')
    
  })
  
  #Totaltime hover label
  output$ChampionTime_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverChampionTime, Championtime_df(), Championtime_df()$Champion_Time_Played, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  
  
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
  
  #################
  ###PLAYER TYPE###
  #################
  
  pre_agg_playertype <- reactive({pre_agg(filtered_data(),
                                          input$measure,
                                          'Player_Type')})
  
  PlayerType_df <- reactive({common_agg(pre_agg_playertype(),
                                        input$measure,
                                        'Player_Type',
                                        FALSE)})
  
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
    
    create_barplot(PlayerType_df(), 'Player_Type')
    
  })
  
  #PlayerType hover label
  output$PlayerType_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverPlayerType, PlayerType_df(), PlayerType_df()$Player_Type, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  
  ##########
  ###DATE###
  ##########
  
  pre_agg_date <- reactive({pre_agg(filtered_data(),
                                          input$measure,
                                          'Date')})
  
  date_df <- reactive({common_agg(pre_agg_date(),
                                        input$measure,
                                        'Date',
                                        FALSE)})
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
    
    create_barplot(date_df(), 'Date')
    
  })
  
  #Date hover label
  output$Date_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverDate, date_df(), date_df()$Date, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  ##########
  ###PING###
  ##########
  
  agg_ping <- reactive({
    
    if (input$measure == 'winrate') {
      
      test <- 1
      
    
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won, Ping) %>%
      group_by(Game_ID, User_ID, Ping) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0)) %>%
      ungroup() %>%
      group_by(Ping) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100,2))
    
    }
    
  })
  
  output$Ping <- renderPlot({

    create_barplot(agg_ping(), 'Ping') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$Ping_tooltip <- renderUI({
    
      info <- create_tooltip2(input$hoverPing, agg_ping(), agg_ping()$Ping, -25, 210)
      
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
  
  Map_df <- reactive({
    
    data <- common_agg(pre_agg_map(),
                       input$measure,
                       'Map',
                       TRUE) %>%
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
  
  ##################
  ###RANKING TYPE###
  ##################
  
  pre_agg_casual <- reactive({pre_agg(filtered_data(),
                                      input$measure,
                                      'Ranking_Type')})
  
  Casual_df <- reactive({common_agg(pre_agg_casual(),
                                    input$measure,
                                    'Ranking_Type',
                                    FALSE)})
  
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
    
    create_barplot(Casual_df(), 'Ranking_Type')
    
  })
  
  #Ranking_Type hover label
  output$Casual_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverCasual, Casual_df(), Casual_df()$Ranking_Type, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  ############
  ###AVATAR###
  ############
  
  pre_agg_avatar <- reactive({pre_agg(filtered_data(),
                                      input$measure,
                                      'Avatar')})
  
  Avatar_df <- reactive({
    
            common_agg(pre_agg_avatar(),
                       input$measure,
                       'Avatar',
                       TRUE)
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
    
    create_barplot(Avatar_df(), 'Avatar')
    
  })
  
  #Avatar hover label
  output$Avatar_tooltip <- renderUI({
    
    req(!is.null(input$hoverAvatar))
    
    info <- create_tooltip(input$hoverAvatar, Avatar_df(), Avatar_df()$Avatar, 40, -30)
    selected_avatar <- info$Name
    filename <- filter(avatar_crosswalk, Value == selected_avatar)$`Avatar File Name`
    imagelink <- paste0('https://github.com/Skywind555/Personal-Projects/blob/master/ImagesConverted/', 
                        filename, '.png?raw=true')
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>"))),
        tags$img(src = imagelink)
      )
      
    }
    
  })
  
  ###########
  ###TITLE###
  ###########
  
  pre_agg_title <- reactive({pre_agg(filtered_data(),
                                     input$measure,
                                     'Title')})
  
  Title_df <- reactive({common_agg(pre_agg_title(),
                                   input$measure,
                                   'Title',
                                   TRUE)})
  
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
    
    create_barplot(Title_df(), 'Title')
    
  })
  
  #Title hover label
  output$Title_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverTitle, Title_df(), Title_df()$Title, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  
  
  ############
  ###OUTFIT###
  ############
  
  pre_agg_outfit <- reactive({pre_agg(filtered_data(),
                                      input$measure,
                                      'Outfit')})
  
  Outfit_df <- reactive({
    
    data <- common_agg(pre_agg_outfit(),
                       input$measure,
                       'Outfit',
                       TRUE)
    
    outfit_crosswalk$Value <- factor(outfit_crosswalk$Value, levels = levels(data$Outfit))
    data <- inner_join(x = data, y = outfit_crosswalk, by = c('Outfit' = 'Value'))[-3]
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
    
    create_barplot(Outfit_df(), 'Outfit')
    
  })
  
  #Outfit hover label
  output$Outfit_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverOutfit, Outfit_df(), Outfit_df()$Outfit, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  
  ################
  ###ATTACHMENT###
  ################
  
  pre_agg_attachment <- reactive({pre_agg(filtered_data(),
                                          input$measure,
                                          'Attachment')})
  
  Attachment_df <- reactive({
    
    data <- common_agg(pre_agg_attachment(),
                       input$measure,
                       'Attachment',
                       TRUE)
    
    attachment_crosswalk$Value <- factor(attachment_crosswalk$Value, levels = levels(data$Attachment))
    data <- inner_join(x = data, y = attachment_crosswalk, by = c('Attachment' = 'Value'))[-3]
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
    
    create_barplot(Attachment_df(), 'Attachment')
    
  })
  
  #Attachment hover label
  output$Attachment_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverAttachment, Attachment_df(), Attachment_df()$Attachment, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  ###########
  ###MOUNT###
  ###########
  
  pre_agg_mount <- reactive({pre_agg(filtered_data(),
                                     input$measure,
                                     'Mount')})
  
  Mount_df <- reactive({
    data <- common_agg(pre_agg_mount(),
                       input$measure,
                       'Mount',
                       TRUE)
    
    mount_crosswalk$Value <- factor(mount_crosswalk$Value, levels = levels(data$Mount))
    data <- inner_join(x = data, y = mount_crosswalk, by = c('Mount' = 'Value'))[-3]
    data$Rarity <- factor(data$Rarity, levels = c('Common', 'Rare', 'Epic', 'Legendary'))
    data <- data[!duplicated(data$Mount),]
    
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
    
    create_barplot(Mount_df(), 'Mount')
    
  })
  
  #Attachment hover label
  output$Mount_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverMount, Mount_df(), Mount_df()$Mount, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  
  
  ##########
  ###POSE###
  ##########
  
  pre_agg_pose <- reactive({pre_agg(filtered_data(),
                                    input$measure,
                                    'Pose')})
  
  Pose_df <- reactive({
    
    data <- common_agg(pre_agg_pose(),
                       input$measure,
                       'Pose',
                       TRUE)

    pose_crosswalk$Value <- factor(pose_crosswalk$Value, levels = levels(data$Pose))
    data <- inner_join(x = data, y = pose_crosswalk, by = c('Pose' = 'Value'))[-3]
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
    
    create_barplot(Pose_df(), 'Pose')
    
  })
  
  #Pose hover label
  output$Pose_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverPose, Pose_df(), Pose_df()$Pose, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  ###################
  ###AVERAGE STATS###
  ###################
  
  stats_pre_agg <- reactive({
    
    if (input$measure == 'winrate') {
    print('stats pre agg')
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won, Kills, Deaths, Assists, Damage, Damage_Received, Protection, 
             Protection_Received, Control, Control_Received, Energy_Used, Energy_Gained, Abilities_Used,
             Num_Energy_Used, Orb_Kills, First_Orb, MVP, Round_Length, Total_Score, Highest_Score,
             Health_Shards, Energy_Shards, Time_Alive, Queue_Time) %>%
      mutate(Survived = ifelse(is.negative(Time_Alive), 1, 0)) %>%
      select(Game_ID, User_ID, Round_Won, Kills, Deaths, Assists, Damage, Damage_Received, Protection, 
             Protection_Received, Control, Control_Received, Energy_Used, Energy_Gained, Abilities_Used,
             Num_Energy_Used, Orb_Kills, First_Orb, MVP, Round_Length, Total_Score, Highest_Score,
             Health_Shards, Energy_Shards, Survived, Queue_Time) %>%
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
    
    }
    
  })
  
  stats_agg <- reactive({
    print('stats agg')
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

  })
  
  stats_agg_lose <- reactive({
    
    data <- filter(stats_agg(), Game_Won == 0)
    
  })
  
  stats_agg_win <- reactive({
    
    data <- filter(stats_agg(), Game_Won == 1)
    
  })
  
  stats_win_df <- reactive({
    
    #Remove Game_Won column
    data <- stats_agg_win()[,2:length(stats_agg_win())]
    data <- gather(data, key = 'Measure', value = 'Value')
    data$Value <- round(data$Value, 2)
    data


    
  })
  
  stats_lose_df <- reactive({
    
    #Remove Game_Won column
    data <- stats_agg_lose()[,2:length(stats_agg_lose())]
    data <- gather(data, key = 'Measure', value = 'Value')
    data$Value <- round(data$Value, 2)
    data

    
  })
  
  output$StatsWinLabel <- renderUI({
    
    HTML('<font size = "2"><b><u>Average Stats Per Round For Game Wins</font></b></u>')
    
  })
  
  output$StatsLoseLabel <- renderUI({
    
    HTML('<font size = "2"><b><u>Average Stats Per Round For Game Losses</font></b></u>')
    
  })
  
  
  output$StatsWin <- DT::renderDataTable(expr = {
    print('stats win data table')
    datatable(stats_win_df(), options = list(searching = FALSE,
                                             paging = FALSE))
    
  })
  
  output$StatsLose <- DT::renderDataTable(expr = {
    print('stats lose data table')
    datatable(stats_lose_df(), options = list(searching = FALSE,
                                              paging = FALSE))
    
  })
  
  ##############
  ###MATCHUPS###
  ##############
  
  ally_comp_pre_agg <- reactive({
    
    if (input$measure == 'winrate') {
    print('ally comp pre agg')
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won, Team_Comp, Server_Type, Team_Roles) %>%
      group_by(Game_ID, User_ID, Team_Comp, Server_Type, Team_Roles) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    }
    
  })
  
  ally_comp_agg <- reactive({
    print('ally comp agg')
    data <- ally_comp_pre_agg() %>%
      ungroup() %>%
      select(Game_Won, Team_Comp, Server_Type, Team_Roles) %>%
      group_by(Team_Comp, Team_Roles, Server_Type) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100, 2)) %>%
      arrange(desc(Win_Rate))
    
  })
  
  enemy_comp_pre_agg <- reactive({
    req(!is.null(filtered_data()))
    print('enemy comp pre agg')
    if (input$measure == 'winrate') {
    
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won, Enemy_Comp, Server_Type, Enemy_Roles) %>%
      group_by(Game_ID, User_ID, Enemy_Comp, Server_Type, Enemy_Roles) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    
    }
    
  })
  
  
  enemy_comp_agg <- reactive({
    req(!is.null(enemy_comp_pre_agg))
    print('enemy comp agg')
    data <- enemy_comp_pre_agg() %>%
      ungroup() %>%
      select(Game_Won, Enemy_Comp, Server_Type, Enemy_Roles) %>%
      group_by(Enemy_Comp, Enemy_Roles, Server_Type) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100, 2)) %>%
      arrange(desc(Win_Rate))
    
  })
  
  
  output$BestCompsLabel <- renderUI({
    
    HTML('<font size = "2"><b><u>Top 5 Best Comps</font></b></u>')
    
  })
  output$BestComps <- renderTable(head(ally_comp_agg(), 5)
    )
  
  output$WorstCompsLabel <- renderUI({
    
    HTML('<font size = "2"><b><u>Top 5 Worst Comps</font></b></u>')
    
  })
  
  output$WorstComps <- renderTable(tail(ally_comp_agg(), 5)
  )
  
  output$BestMatchupsLabel <- renderUI({
    
    HTML('<font size = "2"><b><u>Top 5 Best Matchups</font></b></u>')
    
  })
  
  output$BestMatchups <- renderTable(head(enemy_comp_agg(), 5))
  
  output$WorstMatchupsLabel <- renderUI({
    
    HTML('<font size = "2"><b><u>Top 5 Worst Matchups</font></b></u>')
    
  })
  
  output$WorstMatchups <- renderTable(tail(enemy_comp_agg(), 5))
  
  ######################
  ###TEAM/ENEMY ROLES###
  ######################
  
  pre_agg_allyroles <- reactive({
    print('ally role pre agg')
    if (input$measure == 'winrate') {
      
      data <- filtered_data() %>%
        select(Game_ID, User_ID, Round_Won, Team_Roles, Enemy_Roles) %>%
        group_by(Game_ID, User_ID, Team_Roles, Enemy_Roles) %>%
        summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    }
    
    })
  
  Allyroles_df <- reactive({
    print('ally role agg')
    data <- pre_agg_allyroles() %>%
      ungroup() %>%
      select(Team_Roles, Game_Won) %>%
      group_by(Team_Roles) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100,2)) %>%
      arrange(desc(Win_Rate))
    data
    
  })
  
  Allyroles_best <- reactive({
    
    data <- pre_agg_allyroles() %>%
      ungroup() %>%
      select(Team_Roles, Enemy_Roles, Game_Won) %>%
      group_by(Team_Roles, Enemy_Roles) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100,2)) %>%
      arrange(desc(Win_Rate)) %>%
      filter(Team_Roles == head(Allyroles_df(), 1)$Team_Roles)
    
  })
  
  Allyroles_worst <- reactive({
    
    data <- pre_agg_allyroles() %>%
      ungroup() %>%
      select(Team_Roles, Enemy_Roles, Game_Won) %>%
      group_by(Team_Roles, Enemy_Roles) %>%
      summarize(Win_Rate = round(mean(Game_Won)*100,2)) %>%
      arrange(desc(Win_Rate)) %>%
      filter(Team_Roles == tail(Allyroles_df(), 1)$Team_Roles)
    
  })
  
  output$BestAllyRoles <- renderUI({
    
    bestoverall <- head(Allyroles_best(), 1)$Team_Roles
    bestmatchup <- head(Allyroles_best(), 1)$Enemy_Roles
    worstmatchup <- tail(Allyroles_best(), 1)$Enemy_Roles
    
    HTML(paste0('<font size = "2"> The best overall roles for ',
                input$champion,
                ' is ',
                bestoverall,
                ' with the best matchup against ',
                bestmatchup,
                ' and the worst matchup against ',
                worstmatchup,
                '.</font></b></u>'))
    
  })
  
  output$WorstAllyRoles <- renderUI({
    
    worstoverall <- head(Allyroles_worst(), 1)$Team_Roles
    bestmatchup <- head(Allyroles_worst(), 1)$Enemy_Roles
    worstmatchup <- tail(Allyroles_worst(), 1)$Enemy_Roles
    
    HTML(paste0('<font size = "2"> The worst overall roles for ',
                input$champion,
                ' is ',
                worstoverall,
                ' with the best matchup against ',
                bestmatchup,
                ' and the worst matchup against ',
                worstmatchup,
                '.</font></b></u>'))
    
  })
  
  
  #################
  ###BATTLERITES###
  #################
  



  
  
  create_battlerites_agg <- function(filtered_data,
                                     input_measure,
                                     filtered_battlerites) {
    print('battlerites agg')
    Battlerites = c()
    Win_Rate = c()
    
    if (length(names(filtered_data)) >= 76) {
    
    for (battlerite in c(names(filtered_data)[76:length(names(filtered_data))])) {
      
      pre_agg_battlerites1 <- pre_agg(filtered_data,
                                      input_measure,
                                      battlerite)
      
      var <- as.name(battlerite)
      
      br_filter <- filter(pre_agg_battlerites1, !!var == 1)
      
      battlerites1_df <- br_filter %>%
        ungroup() %>%
        select(!!var, Game_Won) %>%
        group_by(!!var) %>%
        summarize(Win_Rate = round(mean(Game_Won)*100, 2))
      
      Battlerites <- c(Battlerites, names(battlerites1_df)[1])
      Win_Rate <- c(Win_Rate, battlerites1_df$Win_Rate)
      
    }
    
    br_winrates <- data.frame(Battlerites, Win_Rate, stringsAsFactors = TRUE)
    br_winrates <- br_winrates %>%
      mutate(Battlerites = fct_reorder(Battlerites, Win_Rate))
    br_crosswalk$Value <- factor(br_crosswalk$Value, levels = levels(br_winrates$Battlerites))
    
    brtypes <- inner_join(x = br_winrates, y = br_crosswalk, by = c('Battlerites' = 'Value'))[-3]
    brtypes$`Battlerite Type` <- factor(brtypes$`Battlerite Type`, levels = c('Control', 'Defense',
                                                                              'Mobility', 'Offense',
                                                                              'Support', 'Utility', 'Mixed'))
    
    return(brtypes)
    
    } else {
      
      pre_agg_df <- filtered_data %>%
        select(Game_ID, User_ID, Round_Won) %>%
        group_by(Game_ID, User_ID) %>%
        summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
      
      win <- round(mean(pre_agg_df$Game_Won)*100, 2)
      
      return(win)
      

    }


    
  }
  
  
  battlerites_agg1 <- reactive({
    
    req(input$champion != 'None')
    
    create_battlerites_agg(filtered_data(), input$measure, filtered_Battlerites$battlerites)
    
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
  
  
  create_battlerites_barplot <- function(agg_df) {
    print('battlerites barplot')
    Colors <- setNames(c('#FF4FCE', '#A8FF47', '#FFB800', '#FF2A1C', '#00FFE9', '#0095FF', '#BBFFFC'),
                       levels(agg_df$`Battlerite Type`))
    
    Colors_subset <- Colors[as.vector(agg_df$`Battlerite Type`)]
    
    bar <- ggplot(data = agg_df, aes(x = Battlerites, y = Win_Rate, fill = `Battlerite Type`)) +
      geom_bar(width = 1, stat = 'identity') +
      coord_flip() +
      scale_fill_manual(values = Colors_subset) +
      ylab('Win Rate') +
      xlab('Battlerites') +
      theme(legend.position = 'none')
    
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
                    ' </b>and the win rate is ',
                    battlerites_agg1(),
                    ' percent.</font>'))
        
      }
      
    }
    
  })
    

    
    
  
  output$Battlerites <- renderPlot({
    
    req(input$champion != 'None')
    if(is.null(filtered_Battlerites$battlerites)) {
      
    create_battlerites_barplot(battlerites_agg1())
    
      } else if (length(filtered_Battlerites$battlerites) < 5) {
      
      create_battlerites_barplot(battlerites_agg1())
      
    }

  })
  
  #Battlerites hover label
  output$Battlerites_tooltip <- renderUI({
    
    info <- create_tooltip(input$hoverBattlerites, battlerites_agg1(), battlerites_agg1()$Battlerites, 40, 0)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
  })
  
  #Best overall set of 5 battlerites
  
  pre_agg_battlerites <- reactive({pre_agg(filtered_data(),
                                           input$measure,
                                           'Battlerites')})
  
  Battlerites_df <- reactive({
    
    data <- common_agg(pre_agg_battlerites(),
                       input$measure,
                       'Battlerites',
                       FALSE) %>%
      arrange(desc(Win_Rate))
    
    data
    
    
    
    })

  output$BestOverallBattlerites <- renderUI({


    HTML(paste0('<font size = "2"> The best overall battlerites for ',
                input$champion,
                ' is <b>',
                head(Battlerites_df(), 1)$Battlerites,
                '</b> with a win rate of ',
                head(Battlerites_df(), 1)$Win_Rate,
                ' percent.</font>'))
    
  })
  
  }




shinyApp(ui = ui, server = server)




