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

#Get crosswalk reference files
br_crosswalk <- read_csv('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects/Reference Files/Battlerite_Crosswalk.csv')
pose_crosswalk <- read_csv('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects/Reference Files/Pose_Crosswalk.csv')
outfit_crosswalk <- read_csv('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects/Reference Files/Outfit_Crosswalk.csv')
attachment_crosswalk <- read_csv('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects/Reference Files/Attachment_Crosswalk.csv')
mount_crosswalk <- read_csv('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects/Reference Files/Mount_Crosswalk.csv')
champion_crosswalk <- read_csv('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects/Reference Files/Champion_Crosswalk.csv')
avatar_crosswalk <- read_csv('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects/Reference Files/Avatar_Crosswalk.csv')



df <- read_csv('Jul-10-2019_1.csv')
region_coordinates <- read_excel('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects/Reference Files/region.xlsx')
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
#With blank outfits for all champions)
df <- mutate(df, Player_Type = ifelse(is.na(Outfit), 'BOT', 'PLAYER'))

#Bots use default outfits except for mounts, fill in blanks with defaults
df <- mutate(df, Outfit = ifelse(is.na(Outfit), 'DEFAULT OUTFIT', Outfit),
             Attachment = ifelse(is.na(Attachment), 'DEFAULT WEAPON', Attachment),
             Pose = ifelse(is.na(Pose), 'DEFAULT POSE', Pose))

#Convert > 200 ping to 200 ping
df <- mutate(df, Ping = ifelse(Ping > 200, 200, Ping))
df <- mutate(df, Ping = 10*round(Ping/10))
df$Ping <- factor(df$Ping, levels = c('0', '10', '20', '30', '40', '50', '60', '70', '80', '90', '100',
                                      '110', '120', '130', '140', '150', '160', '170', '180', '190', '200'))

#Round dates to days
df <- mutate(df, Date = substr(Date, 1, 10))


df$Player_Type <- factor(df$Player_Type)


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
                            conditionalPanel(
                              condition = "input.champion != 'None'",
                              column(width = 2,
                                     uiOutput('BattleriteInteractive1')
                              ),
                              column(width = 2,
                                     uiOutput('BattleriteInteractive2')
                              ),
                              column(width = 2,
                                     uiOutput('BattleriteInteractive3')
                              ),
                              column(width = 2,
                                     uiOutput('BattleriteInteractive4')
                              ),
                              column(width = 2,
                                     uiOutput('BattleriteInteractive5')
                              )
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
                                   div(DT::dataTableOutput('StatsWin'), style = "font-size:75%")
                            ),
                            
                            column(width = 3,
                                   div(DT::dataTableOutput('StatsLose'), style = "font-size:75%")
                            ),
                            
                            column(width = 6,
                                   
                                   fluidRow(
                                     column(width = 3,
                                            htmlOutput('BestCompsLabel'),
                                            div(tableOutput('BestComps'), style = "font-size:75%")),
                                     column(width = 3, offset = 3,
                                            htmlOutput('WorstCompsLabel'),
                                            div(tableOutput('WorstComps'), style = "font-size:75%"))
                                   ),
                                   
                                   fluidRow(
                                     column(width = 3,
                                            htmlOutput('BestMatchupsLabel'),
                                            div(tableOutput('BestMatchups'), style = "font-size:75%")),
                                     column(width = 3, offset = 3,
                                            htmlOutput('WorstMatchupsLabel'),
                                            div(tableOutput('WorstMatchups'), style = "font-size:75%"))
                                   )
                                   
                          
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
  
  
  create_filtered_data <- function(champ_df, input_champion, input_total_time_played_min, input_total_time_played_max, 
                                   input_champ_played_min, input_champ_played_max,
                                   filtered_region, filtered_league, filtered_servertype,
                                   filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                   filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                   filtered_playertype, filtered_date, input_selectbattlerite1,
                                   input_selectbattlerite2, input_selectbattlerite3, input_selectbattlerite4) {
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
    
    
    if (filtered_regiongroup != 0) {
      data <- filter(data, `Region Group` %in% filtered_regiongroup)
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
    
    if (!is.null(input_selectbattlerite1)) {
      
      if (input_selectbattlerite1 == 'None') {
        
        data <- data
        
      } else if (input_selectbattlerite1 != 'None') {
        
        data <- filter(data, !!as.name(input_selectbattlerite1) == 1)
      }
      
      if (!is.null(input_selectbattlerite2)) {
        
        if (input_selectbattlerite2 != 'None') {
          
          data <- filter(data, !!as.name(input_selectbattlerite2) == 1)
          
        }
        
        if (!is.null(input_selectbattlerite3)) {
          
          if (input_selectbattlerite3 != 'None') {
            
            data <- filter(data, !!as.name(input_selectbattlerite3) == 1)
          }
          
          if (!is.null(input_selectbattlerite4)) {
            
            if (input_selectbattlerite4 != 'None') {
              
              data <- filter(data, !!as.name(input_selectbattlerite4) == 1)
            }
            
          }}}}
    
    return(data)
    
  }
  
  
  create_filtered_data2 <- function(champ_df, input_champion, input_total_time_played_min, input_total_time_played_max, 
                                   input_champ_played_min, input_champ_played_max,
                                   filtered_region, filtered_league, filtered_servertype,
                                   filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                   filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                   filtered_playertype, filtered_date) {
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
    
    if (filtered_regiongroup != 0) {
      data <- filter(data, `Region Group` %in% filtered_regiongroup)
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
  
  region_agg <- function(filtered_data, measure) {
    
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
    
    data <- region_agg %>%
      ungroup() %>%
      select(`Region Group`, Win_Rate) %>%
      group_by(`Region Group`) %>%
      summarize(Win_Rate = round(mean(Win_Rate), 2)) %>%
      arrange(desc(Win_Rate))
    
    return(data)
    
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
  
  create_region_map <- function(region_df, input_champion, region_ref) {
    
    req(input_champion != 'None')
    
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
    
    dat <- datatable(data, selection = list(mode = 'single', target = 'cell'),
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
    
    var <- as.name(variable)
    var2 <- var
    
    if (variable %in% c('League', 'Server_Type', 'Player_Type', 'Map', 'Ranking_Type', 'Pose',
                        'Mount', 'Outfit', 'Attachment', 'Ping')) {
    
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
        Colors = setNames(pal(21), levels(agg_df$Ping))
        
        Colors_subset <- Colors[as.vector(agg_df$Ping)]
        
        
        
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
  filtered_RegionGroup <- reactiveVal(0)
  filtered_PlayerType <- reactiveVal(0)
  filtered_Date <- reactiveVal(0)
  
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
                                                  filtered_Pose(),
                                                  filtered_RegionGroup(),
                                                  filtered_PlayerType(),
                                                  filtered_Date(),
                                                  input$SelectBattlerite1b,
                                                  input$SelectBattlerite2b,
                                                  input$SelectBattlerite3b,
                                                  input$SelectBattlerite4b
  )})
  
  
  #Filtered data for battlerites
  filtered_data2 <- reactive({create_filtered_data2(champ_df(),
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
                                                    filtered_Pose(),
                                                    filtered_RegionGroup(),
                                                    filtered_PlayerType(),
                                                    filtered_Date()
  )})
  
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
    
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won, Ping) %>%
      group_by(Game_ID, User_ID, Ping) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0)) %>%
      ungroup() %>%
      group_by(Ping) %>%
      summarize(Win_Rate = mean(Game_Won))
    
    }
    
  })
  
  output$Ping <- renderPlot({

    create_barplot(agg_ping(), 'Ping')
    
  })
  
  output$Ping_tooltip <- renderUI({
    
      info <- create_tooltip(input$hoverPing, agg_ping(), agg_ping()$Ping, -25, 210)
      
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
  
  Avatar_df <- reactive({common_agg(pre_agg_avatar(),
                                    input$measure,
                                    'Avatar',
                                    TRUE)})
  
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
    
    info <- create_tooltip(input$hoverAvatar, Avatar_df(), Avatar_df()$Avatar, 40, -30)
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
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
    
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won, Kills, Deaths, Assists, Damage, Damage_Received, Protection, 
             Protection_Received, Control, Control_Received, Energy_Used, Energy_Gained, Abilities_Used,
             Num_Energy_Used, Orb_Kills, First_Orb, MVP, Round_Length, Total_Score, Highest_Score,
             Health_Shards, Energy_Shards, Time_Alive, Queue_Time) %>%
      group_by(Game_ID, User_ID) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0),
                Kills = mean(Kills),
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
                Time_Alive = mean(Time_Alive),
                Queue_Time = mean(Queue_Time))
    
    }
    
  })
  
  stats_agg <- reactive({
    
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
    data <- gather(data, key = 'Measure(Wins)', value = 'Value')
    data$Value <- round(data$Value, 2)
    data


    
  })
  
  stats_lose_df <- reactive({
    
    #Remove Game_Won column
    data <- stats_agg_lose()[,2:length(stats_agg_lose())]
    data <- gather(data, key = 'Measure(Losses)', value = 'Value')
    data$Value <- round(data$Value, 2)
    data

    
  })
  
  
  output$StatsWin <- DT::renderDataTable(expr = {
    
    datatable(stats_win_df())
    
  })
  
  output$StatsLose <- DT::renderDataTable(expr = {
    
    datatable(stats_lose_df())
    
  })
  
  ##############
  ###MATCHUPS###
  ##############
  
  ally_comp_pre_agg <- reactive({
    
    if (input$measure == 'winrate') {
    
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won, Team_Comp, Server_Type, Team_Roles) %>%
      group_by(Game_ID, User_ID, Team_Comp, Server_Type, Team_Roles) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    }
    
  })
  
  ally_comp_agg <- reactive({
    
    data <- ally_comp_pre_agg() %>%
      ungroup() %>%
      select(Game_Won, Team_Comp, Server_Type, Team_Roles) %>%
      group_by(Team_Comp, Team_Roles, Server_Type) %>%
      summarize(Game_Won = round(mean(Game_Won)*100, 2)) %>%
      arrange(desc(Game_Won))
    
  })
  
  enemy_comp_pre_agg <- reactive({
    
    if (input$measure == 'winrate') {
    
    data <- filtered_data() %>%
      select(Game_ID, User_ID, Round_Won, Enemy_Comp, Server_Type, Enemy_Roles) %>%
      group_by(Game_ID, User_ID, Enemy_Comp, Server_Type, Enemy_Roles) %>%
      summarize(Game_Won = ifelse(sum(Round_Won) == 3, 1, 0))
    
    }
    
  })
  
  
  enemy_comp_agg <- reactive({
    
    data <- enemy_comp_pre_agg() %>%
      ungroup() %>%
      select(Game_Won, Enemy_Comp, Server_Type, Enemy_Roles) %>%
      group_by(Enemy_Comp, Enemy_Roles, Server_Type) %>%
      summarize(Game_Won = round(mean(Game_Won)*100, 2)) %>%
      arrange(desc(Game_Won))
    
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
  
  #################
  ###BATTLERITES###
  #################
  
  #Create battlerite selection pool based on champion
  output$BattleriteInteractive1 <- renderUI({
    
    if (!is.null(input$SelectBattlerite1)) {
      
      if (!is.null(input$SelectBattlerite1b)) {
      
      selectInput(inputId = 'SelectBattlerite1b',
                  label = 'Choose Battlerite 1: ',
                  choices = c('None', names(filtered_data2())[76:length(names(filtered_data2()))]),
                  multiple = FALSE,
                  selected = input$SelectBattlerite1b
      )
    
      } else {
        
  
        selectInput(inputId = 'SelectBattlerite1b',
                    label = 'Choose Battlerite 1: ',
                    choices = c('None', names(filtered_data2())[76:length(names(filtered_data2()))]),
                    multiple = FALSE,
                    selected = input$SelectBattlerite1)
        
      }
        
    } else {
      
      selectInput(inputId = 'SelectBattlerite1',
                  label = 'Choose Battlerite 1: ',
                  choices = c('None', names(filtered_data2())[76:length(names(filtered_data2()))]),
                  multiple = FALSE,
                  selected = 'None')
      
    }
  })
  
  output$BattleriteInteractive2 <- renderUI({
    
    if(input$SelectBattlerite1b != 'None' & length(adjusted_champ_df1()) > 76) {
      
      if (!is.null(input$SelectBattlerite2)) {
        
        if (!is.null(input$SelectBattlerite2b)) {
          
          selectInput(inputId = 'SelectBattlerite2b',
                      label = 'Choose Battlerite 2: ',
                      choices = c('None', names(adjusted_champ_df1())[76:length(names(adjusted_champ_df1()))]),
                      multiple = FALSE,
                      selected = input$SelectBattlerite2b)
          
        } else {
          
          selectInput(inputId = 'SelectBattlerite2b',
                      label = 'Choose Battlerite 2: ',
                      choices = c('None', names(adjusted_champ_df1())[76:length(names(adjusted_champ_df1()))]),
                      multiple = FALSE,
                      selected = input$SelectBattlerite2)
          
        }
        
      } else {
        
        selectInput(inputId = 'SelectBattlerite2',
                    label = 'Choose Battlerite 2: ',
                    choices = c('None', names(adjusted_champ_df1())[76:length(names(adjusted_champ_df1()))]),
                    multiple = FALSE,
                    selected = 'None')
        
      }
      
    } 
    
  })
  
  output$BattleriteInteractive3 <- renderUI({
    
    if(input$SelectBattlerite2b != 'None' & length(adjusted_champ_df2()) > 76) {
      
      if (!is.null(input$SelectBattlerite3)) {
        
        if (!is.null(input$SelectBattlerite3b)) {
          
          selectInput(inputId = 'SelectBattlerite3b',
                      label = 'Choose Battlerite 3: ',
                      choices = c('None', names(adjusted_champ_df2())[76:length(names(adjusted_champ_df2()))]),
                      multiple = FALSE,
                      selected = input$SelectBattlerite3b)
          
        } else {
          
          selectInput(inputId = 'SelectBattlerite3b',
                      label = 'Choose Battlerite 3: ',
                      choices = c('None', names(adjusted_champ_df2())[76:length(names(adjusted_champ_df2()))]),
                      multiple = FALSE,
                      selected = input$SelectBattlerite3)
          
        }
        
      } else {
        
        selectInput(inputId = 'SelectBattlerite3',
                    label = 'Choose Battlerite 3: ',
                    choices = c('None', names(adjusted_champ_df2())[76:length(names(adjusted_champ_df2()))]),
                    multiple = FALSE,
                    selected = 'None')
        
      }
      
    } 
    
  })
  
  output$BattleriteInteractive4 <- renderUI({
    
    if(input$SelectBattlerite3b != 'None' & length(adjusted_champ_df3()) > 76) {
      
      if (!is.null(input$SelectBattlerite4)) {
        
        if (!is.null(input$SelectBattlerite4b)) {
          
          selectInput(inputId = 'SelectBattlerite4b',
                      label = 'Choose Battlerite 4: ',
                      choices = c('None', names(adjusted_champ_df3())[76:length(names(adjusted_champ_df3()))]),
                      multiple = FALSE,
                      selected = input$SelectBattlerite4b)
          
        } else {
          
          selectInput(inputId = 'SelectBattlerite4b',
                      label = 'Choose Battlerite 4: ',
                      choices = c('None', names(adjusted_champ_df3())[76:length(names(adjusted_champ_df3()))]),
                      multiple = FALSE,
                      selected = input$SelectBattlerite4)
          
        }
        
      } else {
        
        selectInput(inputId = 'SelectBattlerite4',
                    label = 'Choose Battlerite 4: ',
                    choices = c('None', names(adjusted_champ_df3())[76:length(names(adjusted_champ_df3()))]),
                    multiple = FALSE,
                    selected = 'None')
        
      }
      
    } 
    
  })
  
  output$BattleriteInteractive5 <- renderUI({
    
    if(input$SelectBattlerite4b != 'None' & length(adjusted_champ_df4()) > 76) {
      
      if (!is.null(input$SelectBattlerite5)) {
        
        if (!is.null(input$SelectBattlerite5b)) {
          
          selectInput(inputId = 'SelectBattlerite5b',
                      label = 'Choose Battlerite 5: ',
                      choices = c('None', names(adjusted_champ_df4())[76:length(names(adjusted_champ_df4()))]),
                      multiple = FALSE,
                      selected = input$SelectBattlerite5b)
          
        } else {
          
          selectInput(inputId = 'SelectBattlerite5b',
                      label = 'Choose Battlerite 5: ',
                      choices = c('None', names(adjusted_champ_df4())[76:length(names(adjusted_champ_df4()))]),
                      multiple = FALSE,
                      selected = input$SelectBattlerite5)
          
        }
        
      } else {
        
        selectInput(inputId = 'SelectBattlerite5',
                    label = 'Choose Battlerite 5: ',
                    choices = c('None', names(adjusted_champ_df4())[76:length(names(adjusted_champ_df4()))]),
                    multiple = FALSE,
                    selected = 'None')
        
      }
      
    } 
    
  })
  
  create_adjusted_champ <- function(input_selectbattlerite, champ_df, input_total_time_played_min, input_total_time_played_max, 
                                    input_champ_played_min, input_champ_played_max,
                                    filtered_region, filtered_league, filtered_servertype,
                                    filtered_map, filtered_casual, filtered_mount, filtered_title, filtered_avatar,
                                    filtered_outfit, filtered_attachment, filtered_pose, filtered_regiongroup,
                                    filtered_playertype, filtered_date) {

    req(input_selectbattlerite != 'None') 
    
    data <- filter(champ_df, !!as.name(input_selectbattlerite) == 1)
    
    #Adjust output battlerite to not include selected battlerite
    remove_cols <- names(data) %in% input_selectbattlerite
    data <- data[, !remove_cols]
    
    
    #Filter total time played
    data <- filter(data, between(Total_Time_Played, input_total_time_played_min, input_total_time_played_max))
    
    #Filter champ time played
    data <- filter(data, between(Champion_Time_Played, input_champ_played_min, input_champ_played_max))
    
    #Filter on region
    if (filtered_region != 0) {
      data <- filter(data, Region %in% filtered_region)
    }
    
    
    if (filtered_regiongroup != 0) {
      data <- filter(data, `Region Group` %in% filtered_regiongroup)
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
    
    remove_cols <- c()
    
    #Check for columns with all 0's and remove
    for (battlerite in c(names(data)[76:length(data)])) {
      
      column <- names(data) %in% battlerite
      col_sum <- sum(data[, column])
      
      if (col_sum == 0) {
        
        remove_cols <- c(remove_cols, battlerite)
        
      }
      
    }
    
    remove_cols <- names(data) %in% remove_cols
    
    data <- data[, !remove_cols]
    data
    return(data)
    
  }
  
  #Adjusted champ df
  adjusted_champ_df1 <- reactive({
    
    create_adjusted_champ(input$SelectBattlerite1b, 
                          filtered_data2(),
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
                          filtered_Pose(),
                          filtered_RegionGroup(),
                          filtered_PlayerType(),
                          filtered_Date())
    
  })
  
  
  #Adjusted champ df
  adjusted_champ_df2 <- reactive({
    
  
    create_adjusted_champ(input$SelectBattlerite2b, 
                          adjusted_champ_df1(),
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
                          filtered_Pose(),
                          filtered_RegionGroup(),
                          filtered_PlayerType(),
                          filtered_Date())

  })
  
  
  #Adjusted champ df
  adjusted_champ_df3 <- reactive({
    
    create_adjusted_champ(input$SelectBattlerite3b, 
                          adjusted_champ_df2(),
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
                          filtered_Pose(),
                          filtered_RegionGroup(),
                          filtered_PlayerType(),
                          filtered_Date())
    
  })
  
  #Adjusted champ df
  adjusted_champ_df4 <- reactive({
    
    create_adjusted_champ(input$SelectBattlerite4b, 
                          adjusted_champ_df3(),
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
                          filtered_Pose(),
                          filtered_RegionGroup(),
                          filtered_PlayerType(),
                          filtered_Date())
    
  })
  
  
  create_battlerites_agg <- function(input_selectbattlerite, champ_df,
                                     input_measure) {
    
    Battlerites = c()
    Win_Rate = c()
    
    for (battlerite in c(names(champ_df)[76:length(names(champ_df))])) {
      
      pre_agg_battlerites1 <- pre_agg(champ_df,
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
    brtypes

    return(brtypes)
    
  }
  
  
  battlerites_agg1 <- reactive({
    
    req(input$champion != 'None')
    req(!is.null(input$SelectBattlerite1))
    req(input$SelectBattlerite1b == 'None')
    
    create_battlerites_agg(input$SelectBattlerite1b, filtered_data2(), input$measure)
    
  })
  
  battlerites_agg2 <- reactive({
    
    req(input$champion != 'None')
    req(!is.null(input$SelectBattlerite1))
    req(input$SelectBattlerite1b != 'None')
    
    create_battlerites_agg(input$SelectBattlerite1b, adjusted_champ_df1(), input$measure)
    
    
  })
  
  battlerites_agg3 <- reactive({
    
    req(input$champion != 'None')
    req(input$SelectBattlerite2b != 'None')
    
    create_battlerites_agg(input$SelectBattlerite2b, adjusted_champ_df2(), input$measure)
    
  })
  
  battlerites_agg4 <- reactive({
    
    req(input$champion != 'None')
    req(!is.null(input$SelectBattlerite3))
    req(input$SelectBattlerite3b != 'None')
    
    create_battlerites_agg(input$SelectBattlerite3b, adjusted_champ_df3(), input$measure)
    
  })
  
  battlerites_agg5 <- reactive({
    
    req(input$champion != 'None')
    req(!is.null(input$SelectBattlerite4))
    req(input$SelectBattlerite4b != 'None')
    
    create_battlerites_agg(input$SelectBattlerite4b, adjusted_champ_df4(), input$measure)
    
  })
  
  create_battlerites_barplot <- function(agg_df) {
    
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
    
    
  
  
  output$Battlerites <- renderPlot({
    
    req(input$champion != 'None')
    req(!is.null(input$SelectBattlerite1))

    if (input$SelectBattlerite1b == 'None') {

      p <- create_battlerites_barplot(battlerites_agg1())
      
    }  
    
    if (input$SelectBattlerite1b != 'None') {

      p <- create_battlerites_barplot(battlerites_agg2())
      
    } 
    
    if(!is.null(input$SelectBattlerite2b)){
      
      if (input$SelectBattlerite2b != 'None') {
        
        p <- create_battlerites_barplot(battlerites_agg3())
        
      } 
      
      if(!is.null(input$SelectBattlerite3b)) {
        
        if (input$SelectBattlerite3b != 'None') {
          
          p <- create_battlerites_barplot(battlerites_agg4())
          
        } 
        
        if(!is.null(input$SelectBattlerite4b)) {
          
          if (input$SelectBattlerite4b != 'None') {
            
            p <- create_battlerites_barplot(battlerites_agg5())
            
          }}}}
    
    p
    
  })
  
  #Battlerites hover label
  output$Battlerites_tooltip <- renderUI({
    
    req(input$champion != 'None')
    req(!is.null(input$SelectBattlerite1))
    
    if (input$SelectBattlerite1b == 'None') {
      
      info <- create_tooltip(input$hoverBattlerites, battlerites_agg1(), battlerites_agg1()$Battlerites, 40, -30)
      
    }
    
    if (input$SelectBattlerite1b != 'None') {
      
      info <- create_tooltip(input$hoverBattlerites, battlerites_agg2(), battlerites_agg2()$Battlerites, 40, -30)
      
    }
    
    if(!is.null(input$SelectBattlerite2b)){
      
      if (input$SelectBattlerite2b != 'None') {
        
        info <- create_tooltip(input$hoverBattlerites, battlerites_agg3(), battlerites_agg3()$Battlerites, 40, -30)
        
      }
      
      if(!is.null(input$SelectBattlerite3b)) {
        
        if (input$SelectBattlerite3b != 'None') {
          
          info <- create_tooltip(input$hoverBattlerites, battlerites_agg4(), battlerites_agg4()$Battlerites, 40, -30)
          
        }
        
        if(!is.null(input$SelectBattlerite4b)) {
          
          if (input$SelectBattlerite4b != 'None') {
            
            info <- create_tooltip(input$hoverBattlerites, battlerites_agg5(), battlerites_agg5()$Battlerites, 40, -30)
            
          }
          
        }}}
    
    if (!is.null(info$Win)) {
      
      wellPanel(
        style = info$style,
        p(HTML(paste0("<font size = '1'>", info$Win, "</font>")))
      )
      
    }
    
    
  })
  
  

  
  }





shinyApp(ui = ui, server = server)




