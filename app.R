# Activate thes libraries in R (this assumes you already have them installed through install.packages)
library(shiny)
library(DT)
library(shinythemes)
library(rvest)
library(expss)
library(dplyr)
library(tidyr)
library(stringr)
library(sqldf)
library(xlsx)
library(scales)

# List of schools and class: https://nsaa-static.s3.amazonaws.com/textfile/bask/bbbclass.pdf

# Manually create Class A names and enrollment dataframe
School <-
  c(
    'Omaha South',
    'Omaha Central',
    'Grand Island',
    'Millard North',
    'Millard South',
    'Millard West',
    'Lincoln East',
    'Lincoln High',
    'North Star',
    'Creighton Prep',
    'Omaha North',
    'Lincoln Southeast',
    'Burke',
    'Lincoln Southwest',
    'Bryan',
    'Omaha Westside',
    'Papillion-LaVista South',
    'Papillion-LaVista',
    'Lincoln Northeast',
    'Bellevue West',
    'Omaha Northwest',
    'Kearney',
    'Fremont',
    'Bellevue East',
    'Benson',
    'Gretna',
    'Elkhorn',
    'Elkhorn South',
    'Norfolk',
    'Columbus',
    'North Platte',
    'Pius X',
    'South Sioux City'
  )

Enrollment <-
  c(
    2166,
    2051,
    1982,
    1920,
    1881,
    1783,
    1695,
    1692,
    1571,
    1548,
    1522,
    1515,
    1514,
    1501,
    1480,
    1452,
    1442,
    1368,
    1280,
    1240,
    1236,
    1188,
    1113,
    1099,
    1062,
    1050,
    1026,
    1008,
    1005,
    971,
    905,
    897,
    860
  )

classA <- data.frame(School, Enrollment)

# create game scores table -- I added game dates manually
maxprep_baseURL <- 
  "https://www.maxpreps.com/list/schedules_scores.aspx?date="

maxprep_paramURL <- 
  "&gendersport=boys,basketball&state=ne&statedivisionid=85757869-a232-41b9-a6b3-727edb24825e"

game_dates <- c(
  "12/5/2019",
  "12/6/2019",
  "12/7/2019",
  "12/9/2019",
  "12/10/2019",
  "12/12/2019",
  "12/13/2019",
  "12/14/2019",
  "12/16/2019",
  "12/17/2019",
  "12/19/2019",
  "12/20/2019",
  "12/21/2019",
  "12/27/2019",
  "12/28/2019",
  "12/30/2019",
  "12/31/2019",
  "1/2/2020",
  "1/3/2020",
  "1/4/2020",
  "1/7/2020",
  "1/9/2020",
  "1/10/2020",
  "1/11/2020",
  "1/14/2020",
  "1/16/2020",
  "1/17/2020",
  "1/18/2020",
  "1/21/2020",
  "1/23/2020",
  "1/24/2020",
  "1/25/2020",
  "1/28/2020",
  "1/30/2020",
  "1/31/2020",
  "2/1/2020",
  "2/3/2020",
  "2/4/2020",
  "2/7/2020",
  "2/8/2020",
  "2/11/2020",
  "2/13/2020",
  "2/14/2020",
  "2/15/2020",
  "2/18/2020",
  "2/20/2020",
  "2/21/2020",
  "2/22/2020",
  "2/28/2020"
)

maxprep_page_list <- 
  as.list(paste0(maxprep_baseURL, game_dates, maxprep_paramURL))

maxprep_html <- lapply(maxprep_page_list, FUN=function(URLLink){
  read_html(URLLink) %>% html_nodes("[data-contest-state='boxscore']") %>% html_text()
})

# Unlist since above returns a list of lists which is difficult to work with
scores <-
  unlist(maxprep_html)

# Clean up and separate table into multiple columns
scores <- 
  gsub("Final","", scores)

scores <- 
  grep("#", scores, invert = TRUE, value = TRUE)

scores <- 
  data.frame(scores)

colnames(scores) <- 
  c("V1")

scores <- 
  scores %>%
  mutate(V1 = gsub("(\\d+)", ";\\1;", V1)) %>%
  separate(V1, c(NA, "No1", "Let1", "No2", "Let2"), sep = " *; *")

colnames(scores) <- 
  c("Away_Score", "Away_Team", "Home_Score", "Home_Team")

# Create new variables: Winner, Loser, Home_W, Home_L, Away_W, Away_L based on existing columns
scores$Winner <- 
  if_else(scores$Away_Score > scores$Home_Score, scores$Away_Team, scores$Home_Team)

scores$Loser <- 
  if_else(scores$Away_Score < scores$Home_Score, scores$Away_Team, scores$Home_Team)

scores$Home_W <- 
  if_else(scores$Winner==scores$Home_Team, scores$Home_Team, "NA")

scores$Home_L <- 
  if_else(scores$Loser==scores$Home_Team, scores$Home_Team, "NA")

scores$Away_W <- 
  if_else(scores$Winner==scores$Away_Team, scores$Away_Team, "NA")

scores$Away_L <- 
  if_else(scores$Loser==scores$Away_Team, scores$Away_Team, "NA")

scores <- 
  scores %>% mutate_all(~gsub('\r|\n', '', .))

scores$Winner <- str_trim(scores$Winner, side = "both")
scores$Loser <- str_trim(scores$Loser, side = "both")
scores$Away_Team <- str_trim(scores$Away_Team, side = "both")
scores$Home_Team <- str_trim(scores$Home_Team, side = "both")

scores$Home_Score <- as.numeric(scores$Home_Score)
scores$Away_Score <- as.numeric(scores$Away_Score)

# Create new variable with amount won (or lost) by
scores$Won_By <- abs(scores$Home_Score - scores$Away_Score)

# Create new variable telling whether or not the home and away teams are in the list of Class A names
scores$Home_Class <- scores$Home_Team %in% classA$School
scores$Away_Class <- scores$Away_Team %in% classA$School

# Now create new variable in ClassA dataframe based on scores data frame data
classA <- 
  merge(classA, stack(table(factor(scores$Winner, levels = classA$School))), 
        by.x = 'School', by.y = "ind")
names(classA)[names(classA) == 'values'] <- 'Wins'

classA <- 
  merge(classA, stack(table(factor(scores$Loser, levels = classA$School))), 
        by.x = 'School', by.y = "ind")
names(classA)[names(classA) == 'values'] <- 'Losses'

# Create new Win_Pct and Games_Played variables
classA$Win_Pct <- 
  round(classA$Wins / (classA$Wins + classA$Losses), digits = 2)

classA$Games_Played <- 
  classA$Wins + classA$Losses

# Create new variables Home_Wins, Home_Losses, Away_Wins, Away_Losses
varHW <- sqldf("select scores.Home_Team, count(scores.Home_W)
               from scores
               where scores.Home_Class==TRUE AND scores.Home_Team==scores.Winner
               group by scores.Home_Team",
               stringsAsFactors=FALSE)
names(varHW)[2] <- "Home_Wins"
classA <- sqldf("select classA.*, varHW.Home_Wins 
                from classA 
                left join varHW on classA.School = varHW.Home_Team", 
                stringsAsFactors = FALSE)

varHL <- sqldf("select scores.Home_Team, count(scores.Home_L)
               from scores
               where scores.Home_Class==TRUE AND scores.Home_Team==scores.Loser
               group by scores.Home_Team",
               stringsAsFactors=FALSE)
names(varHL)[2] <- "Home_Losses"
classA <- sqldf("select classA.*, varHL.Home_Losses 
                from classA 
                left join varHL on classA.School = varHL.Home_Team", 
                stringsAsFactors = FALSE)

varAW <- sqldf("select scores.Away_Team, count(scores.Away_W)
               from scores
               where scores.Away_Class==TRUE AND scores.Away_Team==scores.Winner
               group by scores.Away_Team",
               stringsAsFactors=FALSE)
names(varAW)[2] <- "Away_Wins"
classA <- sqldf("select classA.*, varAW.Away_Wins 
                from classA 
                left join varAW on classA.School = varAW.Away_Team", 
                stringsAsFactors = FALSE)

varAL <- sqldf("select scores.Away_Team, count(scores.Away_L)
               from scores
               where scores.Away_Class==TRUE AND scores.Away_Team==scores.Loser
               group by scores.Away_Team",
               stringsAsFactors=FALSE)
names(varAL)[2] <- "Away_Losses"
classA <- sqldf("select classA.*, varAL.Away_Losses 
                from classA 
                left join varAL on classA.School = varAL.Away_Team", 
                stringsAsFactors = FALSE)

classA[is.na(classA)] <- 0

scores$Away_Score <- as.numeric(scores$Away_Score)
scores$Home_Score <- as.numeric(scores$Home_Score)
classA$School <- as.character(classA$School)

# Create Away_PPG, Home_PPG, Home_dPPG, Away_dPPG variables
classA <- 
  scores %>% 
  group_by(Away_Team) %>% 
  summarise(Away_PPG = mean(Away_Score, na.rm = TRUE)) %>% 
  right_join(classA, by = c(Away_Team = 'School'))
names(classA)[names(classA) == 'Away_Team'] <- 'School'

classA <- 
  scores %>% 
  group_by(Home_Team) %>% 
  summarise(Home_PPG = mean(Home_Score, na.rm = TRUE)) %>% 
  right_join(classA, by = c(Home_Team = 'School'))
names(classA)[names(classA) == 'Home_Team'] <- 'School'

classA <- 
  scores %>% 
  group_by(Home_Team) %>% 
  summarise(Home_dPPG = mean(Away_Score, na.rm = TRUE)) %>% 
  right_join(classA, by = c(Home_Team = 'School'))
names(classA)[names(classA) == 'Home_Team'] <- 'School'

classA <- 
  scores %>% 
  group_by(Away_Team) %>% 
  summarise(Away_dPPG = mean(Home_Score, na.rm = TRUE)) %>% 
  right_join(classA, by = c(Away_Team = 'School'))
names(classA)[names(classA) == 'Away_Team'] <- 'School'

classA$Away_PPG <- round(classA$Away_PPG, digits = 0)
classA$Home_PPG <- round(classA$Home_PPG, digits = 0)
classA$Home_dPPG <- round(classA$Home_dPPG, digits = 0)
classA$Away_dPPG <- round(classA$Away_dPPG, digits = 0)

# Create PPG differential variables
classA$Home_PPG_Diff <- round(classA$Home_PPG - classA$Home_dPPG, digits = 0)
classA$Away_PPG_Diff <- round(classA$Away_PPG - classA$Away_dPPG, digits = 0)

# Create Home and Away Points and Points Allowed variables
classA <- 
  scores %>% 
  group_by(Away_Team) %>% 
  summarise(Away_Total_Points = sum(Away_Score, na.rm = TRUE)) %>% 
  right_join(classA, by = c(Away_Team = 'School'))
names(classA)[names(classA) == 'Away_Team'] <- 'School'

classA <- 
  scores %>% 
  group_by(Home_Team) %>% 
  summarise(Home_Total_Points = sum(Home_Score, na.rm = TRUE)) %>% 
  right_join(classA, by = c(Home_Team = 'School'))
names(classA)[names(classA) == 'Home_Team'] <- 'School'

classA <- 
  scores %>% 
  group_by(Home_Team) %>% 
  summarise(Home_Points_Allowed = sum(Away_Score, na.rm = TRUE)) %>% 
  right_join(classA, by = c(Home_Team = 'School'))
names(classA)[names(classA) == 'Home_Team'] <- 'School'

classA <- 
  scores %>% 
  group_by(Away_Team) %>% 
  summarise(Away_Points_Allowed = sum(Home_Score, na.rm = TRUE)) %>% 
  right_join(classA, by = c(Away_Team = 'School'))
names(classA)[names(classA) == 'Away_Team'] <- 'School'

# Combine above variables to get totals and subtract for differntial
classA$Total_Points <- classA$Home_Total_Points + classA$Away_Total_Points
classA$PPG <- round(classA$Total_Points / classA$Games_Played, digits = 0)

classA$Points_Allowed <- classA$Home_Points_Allowed + classA$Away_Points_Allowed
classA$dPPG <- round(classA$Points_Allowed / classA$Games_Played, digits = 0)

classA$Total_Points_Diff <- classA$Total_Points - classA$Points_Allowed
classA$PPG_Diff <- classA$PPG - classA$dPPG

# Create variables showing how often teams play against other Class A teams
againstAhome <- sqldf("select scores.Home_Team, count(scores.Away_Class)
                      from scores
                      where scores.Away_Class==TRUE AND scores.Home_Class=TRUE
                      group by scores.Home_Team",
                      stringsAsFactors=FALSE)
names(againstAhome)[2] <- "Home_A_Schedule"
classA <- sqldf("select classA.*, againstAhome.Home_A_Schedule 
                from classA 
                left join againstAhome on classA.School = againstAhome.Home_Team", 
                stringsAsFactors = FALSE)

againstAaway <- sqldf("select scores.Away_Team, count(scores.Home_Class)
                      from scores
                      where scores.Home_Class==TRUE AND scores.Away_Class=TRUE
                      group by scores.Away_Team",
                      stringsAsFactors=FALSE)
names(againstAaway)[2] <- "Away_A_Schedule"
classA <- sqldf("select classA.*, againstAaway.Away_A_Schedule 
                from classA 
                left join againstAaway on classA.School = againstAaway.Away_Team", 
                stringsAsFactors = FALSE)

classA$A_Schedule <- round((classA$Home_A_Schedule + classA$Away_A_Schedule) / classA$Games_Played, digits = 2)

# Create a cleaned up table for publishing online or other uses
classA_clean <- 
  data.frame(classA$School,
             classA$Performance_Points,
             classA$Wins, 
             classA$Losses, 
             classA$Win_Pct, 
             classA$Home_Wins, 
             classA$Home_Losses, 
             classA$Away_Wins, 
             classA$Away_Losses, 
             classA$PPG,
             classA$dPPG,
             classA$Home_PPG_Diff, 
             classA$Away_PPG_Diff,
             classA$A_Schedule,
             classA$SOS)

names(classA_clean) <- 
  c("School",
    "Performance Points",
    "Wins", 
    "Losses", 
    "Win %", 
    "Home Wins", 
    "Home Losses", 
    "Away Wins", 
    "Away Losses", 
    "PPG",
    "Def PPG",
    "Home PPG Diff", 
    "Away PPG Diff", 
    "Class A Schedule",
    "SOS")

classA_clean <- classA_clean[, c(16, 1:15)]
classA_clean$SOS <- round(classA_clean$SOS, digits = 2)
classA_clean$`Win %` <- percent(classA_clean$`Win %`, scale = 100, suffix = "%")
classA_clean$`Class A Schedule` <- percent(classA_clean$`Class A Schedule`, scale = 100, suffix = "%")

# write.xlsx2(classA, "classabbb.xlsx")


# Define UI for application that displays a table
ui <- fluidPage(
  theme = "sandstone",
              
                br(),
                h2("2019/20 Nebraska High School Boys Basketball Computer Rankings", style = "color: DarkGoldenRod"),
                br(),
                DT::DTOutput("mytable")
)

server <- function(input, output) {
  output$mytable = DT::renderDT({
    DT::datatable(
      classA_clean,
      rownames = FALSE,
      options = list(
        paging = FALSE, 
        searching = FALSE))
  })
}

shinyApp(ui = ui, server = server)
