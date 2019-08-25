library('dplyr')
library('readr')
library('forcats')
library('tidyr')
library('stringr')
library('readxl')
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

for (day in 10:17) {
  
  files <- list.files(path = 'Data', pattern = paste0('^Jul-', day), recursive = TRUE)
  
  for (file in files) {
    
    rawfilename <- substr(file, 14, 35)
    df <- read_csv(paste0('Data/', file))[, c(1:73, 112, 113)]
    
    #Process data
    df$Map <- factor(df$Map)
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
    df$Team_Comp <- factor(df$Team_Comp)
    df$Enemy_Comp <- factor(df$Enemy_Comp)
    
    df <- mutate(df, Title = ifelse(is.na(Title),'Unknown',Title))
    df$Title <- factor(df$Title)
    
    df$Avatar <- factor(df$Avatar)
    
    #Round dates to days
    df <- mutate(df, Date = substr(Date, 1, 10))
    df$Date <- factor(df$Date)
    
    df$User_ID <- as.character(df$User_ID)
    
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
    df$Player_Type <- factor(df$Player_Type)
    
    #Bots use default outfits except for mount, fill in blanks with defaults
    #Bots can also have no mount recorded, fill in blank with RAM
    df <- mutate(df, Outfit = ifelse(is.na(Outfit), 'DEFAULT OUTFIT', Outfit),
                 Attachment = ifelse(is.na(Attachment), 'DEFAULT WEAPON', Attachment),
                 Pose = ifelse(is.na(Pose), 'DEFAULT POSE', Pose),
                 Mount = ifelse(is.na(Mount), 'RAM', Mount))
    
    df$Outfit <- factor(df$Outfit)
    df$Mount <- factor(df$Mount)
    df$Attachment <- factor(df$Attachment)
    df$Pose <- factor(df$Pose)
    
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
    
    #Count only each combination once in one specific order
    df$Team_Roles <- sapply(df$Team_Roles, function(x) strsplit(x, ", "))
    df$Team_Roles <- sapply(df$Team_Roles, function(x) paste(sort(x), collapse = ' '))
    df$Team_Roles <- factor(df$Team_Roles)
    
    df$Enemy_Roles <- sapply(df$Enemy_Roles, function(x) strsplit(x, ", "))
    df$Enemy_Roles <- sapply(df$Enemy_Roles, function(x) paste(sort(x), collapse = ' '))
    df$Enemy_Roles <- factor(df$Enemy_Roles)
    
    df$Battlerites <- sapply(df$Battlerites, function(x) strsplit(x, ", "))
    df$Battlerites <- names(sapply(df$Battlerites, function(x) paste(sort(x), collapse = ' ', sep = ', ')))
    
    df$Battlerites <- factor(df$Battlerites)
    
    #Convert seconds to hours and round up
    df <- mutate(df, Total_Time_Played = ceiling(Total_Time_Played/3600),
                 Champion_Time_Played = ceiling(Champion_Time_Played/3600))
    
    
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
    
    
    
    write.csv(df, paste0('C:/Users/skywi/Desktop/Battlerite Project/Personal-Projects/Dashboard Data/2019/July/', day, '/', rawfilename))
    
    
  }
  
  
}
