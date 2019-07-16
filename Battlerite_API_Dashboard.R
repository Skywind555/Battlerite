library('shiny')
library('dplyr')
library('readr')
library('ggplot2')
library('forcats')
library('DT')
library('tidyr')
library('stringr')
library('fastDummies')

setwd('C:/Users/skywi/Desktop/Battlerite')

df = read_csv('df.csv')

df$Champion <- factor(df$Champion)
df$`Battlerite 1` <- factor(df$`Battlerite 1`)
df$`Battlerite 2` <- factor(df$`Battlerite 2`)
df$`Battlerite 3` <- factor(df$`Battlerite 3`)
df$`Battlerite 4` <- factor(df$`Battlerite 4`)
df$`Battlerite 5` <- factor(df$`Battlerite 5`)

unique_champions <- unique(df$Champion)


#Filter to champion
Cdf <- filter(df, Champion %in% unique_champions[1])

#################
###BATTLERITES###
#################

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


ui <- navbarPage('Navbar',
                 tabPanel('Champion',
                          fluidRow(
                            column(width = 4,
                                   selectInput(inputId = 'champion',
                                               label = 'Champion',
                                               multiple = FALSE,
                                               choices = c(NULL, levels(df$Champion)),
                                               selected = NULL)
                                   
                            ),
                            column(width = 4,
                                   sliderInput(inputId = 'total_played',
                                               label = 'Total Time Played:',
                                               min = 0,
                                               max = max(df$`Total Time Played`, na.rm = TRUE),
                                               value = c(0, max(df$`Total Time Played`, na.rm = TRUE)))
                            ),
                            column(width = 4,
                                   sliderInput(inputId = 'champ_played',
                                               label = 'Total Time Champion Played:',
                                               min = 0,
                                               max = max(df$`Character Time Played`),
                                               value = c(0, max(df$`Character Time Played`)))
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
                                            plotOutput(outputId = 'MatchType',
                                                       dblclick = 'filterMatchType',
                                                       click = 'unfilterMatchType',
                                                       hover = hoverOpts('hoverMatchType'),
                                                       height = '300px'),
                                            uiOutput('MatchType_tooltip')
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
                        





