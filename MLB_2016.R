library(dplyr)

#parse.retrosheet.pbp written by Max Marchi and Jim Albert in "Analyzing Baseball Data with R"
#parse.retrosheet.pbp takes a completed MLB season as input and creates files in download.folder/unzipped
#The files are called allXXXX.csv and rosterXXXX.csv, with XXXX being the season taken as input
#all is all events in the season
#roster matches batter id with player and team

parse.retrosheet.pbp <- function(season){
  # download, unzip, append retrosheet data
  # assume current directory has a folder download.folder
  # download.folder has two subfolders unzipped and zipped
  # program cwevent.exe is in unzipped folder (for windows)
  
  download.retrosheet <- function(season){
    # get zip file from retrosheet website
    download.file(
      url=paste("http://www.retrosheet.org/events/", season, "eve.zip", sep="")
      , destfile=paste("download.folder", "/zipped/", season, "eve.zip", sep="")
    )
  }
  unzip.retrosheet <- function(season){
    #unzip retrosheet files
    unzip(paste("download.folder", "/zipped/", season, "eve.zip", sep=""), 
          exdir=paste("download.folder", "/unzipped", sep=""))
  }
  create.csv.file=function(year){
    # http://chadwick.sourceforge.net/doc/cwevent.html#cwtools-cwevent
    # shell("cwevent -y 2000 2000TOR.EVA > 2000TOR.bev")
    wd = getwd()
    setwd("download.folder/unzipped")
    shell(paste(paste("cwevent -y", year, "-f 0-96"), 
          paste(year,"*.EV*",sep=""),
          paste("> all", year, ".csv", sep="")))              
    setwd(wd)
  }
  create.csv.roster = function(year){
    # creates a csv file of the rosters
    filenames <- list.files(path = "download.folder/unzipped/")
    filenames.roster = 
      subset(filenames, substr(filenames, 4, 11)==paste(year,".ROS",sep=""))
    read.csv2 = function(file)
      read.csv(paste("download.folder/unzipped/", file, sep=""),header=FALSE)
    R = do.call("rbind", lapply(filenames.roster, read.csv2))
    names(R)[1:6] = c("Player.ID", "Last.Name", "First.Name", 
                      "Bats", "Pitches", "Team")
    wd = getwd()
    setwd("download.folder/unzipped")
    write.csv(R, file=paste("roster", year, ".csv", sep=""))
    setwd(wd)
  }
  cleanup = function(){
    # removes retrosheet files not needed
    wd = getwd()
    setwd("download.folder/unzipped")
    shell("del *.EVN")
    shell("del *.EVA")
    shell("del *.ROS")
    shell("del TEAM*")
    setwd(wd)
  }
  download.retrosheet(season)
  unzip.retrosheet(season)
  create.csv.file(season)
  create.csv.roster(season)
  cleanup()
}

readinteger <- function(prompt)
{ 
  n <- readline(prompt)
  if(!grepl("^[0-9]+$",n))
  {
    print("Try Again")
    return(readinteger(prompt))
  }
  
  return(as.integer(n))
}

retrosheet.clean <- function(chadwick_table) {
  Clean_events <- chadwick_table %>% 
    select(V5, V11, V27, V28, V29, V30, V44) %>% 
    rename(outs = "V5", batter = "V11", b1 = "V27", b2 = "V28", b3 = "V29", event = "V30", rbi_play = "V44") %>% 
    mutate(b1 = ifelse(b1=="", 0, 1), 
           b2 = ifelse(b2=="", 0, 1),
           b3 = ifelse(b3=="", 0, 1),
           b1 = as.numeric(b1),
           b2 = as.numeric(b2),
           b3 = as.numeric(b3),
           outs = as.numeric(outs),
           situation = case_when(
             b1==0 & b2==0 & b3==0 & outs==0 ~ 1, 
             b1==0 & b2==0 & b3==0 & outs==1 ~ 2,
             b1==0 & b2==0 & b3==0 & outs==2 ~ 3,
             b1==1 & b2==0 & b3==0 & outs==0 ~ 4,
             b1==1 & b2==0 & b3==0 & outs==1 ~ 5,
             b1==1 & b2==0 & b3==0 & outs==2 ~ 6,
             b1==0 & b2==1 & b3==0 & outs==0 ~ 7,
             b1==0 & b2==1 & b3==0 & outs==1 ~ 8,
             b1==0 & b2==1 & b3==0 & outs==2 ~ 9,
             b1==1 & b2==1 & b3==0 & outs==0 ~ 10,
             b1==1 & b2==1 & b3==0 & outs==1 ~ 11,
             b1==1 & b2==1 & b3==0 & outs==2 ~ 12,
             b1==0 & b2==0 & b3==1 & outs==0 ~ 13,
             b1==0 & b2==0 & b3==1 & outs==1 ~ 14,
             b1==0 & b2==0 & b3==1 & outs==2 ~ 15,
             b1==1 & b2==0 & b3==1 & outs==0 ~ 16,
             b1==1 & b2==0 & b3==1 & outs==1 ~ 17,
             b1==1 & b2==0 & b3==1 & outs==2 ~ 18,
             b1==0 & b2==1 & b3==1 & outs==0 ~ 19,
             b1==0 & b2==1 & b3==1 & outs==1 ~ 20,
             b1==0 & b2==1 & b3==1 & outs==2 ~ 21,
             b1==1 & b2==1 & b3==1 & outs==0 ~ 22,
             b1==1 & b2==1 & b3==1 & outs==1 ~ 23,
             b1==1 & b2==1 & b3==1 & outs==2 ~ 24),
           cRBI = ifelse(grepl("HR", event), rbi_play - 1, rbi_play),
           ibb_flag = ifelse(grepl("IW", event), 1, 0)) %>% 
    filter(ibb_flag == 0 | (ibb_flag == 1 & situation >= 22)) %>% 
    select(batter, situation, cRBI)
  return(Clean_events)
}

#-------------------------------------------------------------------------

setwd("C:/Users/Sam/Desktop/STAT184_Final")

start_season <- readinteger("Enter first season to create RE Matrix: ")
2012
end_season   <- readinteger("Enter final season to create RE Matrix: ")
2016

Events_years <- list()

for (i in start_season:end_season) {
  parse.retrosheet.pbp(i)
  setwd("download.folder/unzipped")
  name <- paste("Events", i, sep="_") 
  assign(name, (read.csv(paste("all", i, ".csv", sep=""), header = FALSE)))
  Events_years
  if (i != end_season) {
    setwd("C:/Users/Sam/Desktop/STAT184_Final")
  }
}

All_Events <- rbind(Events_2012, Events_2013, Events_2014, Events_2015, Events_2016)

Cleaned <- retrosheet.clean(All_Events)

Run_matrix <- Cleaned %>% 
  filter(batter != lead(batter)) %>% 
  group_by(situation) %>% 
  summarise(n_situation=n(), total=sum(cRBI)) %>% 
  mutate(ecRBI = (total / n_situation))
 
setwd("C:/Users/Sam/Desktop/STAT184_Final")
parse.retrosheet.pbp(2017)
setwd("download.folder/unzipped")
Events_2017 <- read.csv("all2017.csv", header = FALSE)
 
Season_Analysis <- retrosheet.clean(Events_2017)

Player_RBI <- Season_Analysis %>% 
  left_join(Run_matrix, by="situation") %>% 
  select(batter, cRBI, ecRBI) %>% 
  group_by(batter) %>% 
  summarise(pa = n(), total_cRBI = sum(cRBI), total_ecRBI = sum(ecRBI))

Players_2017 <- read.csv("roster2017.csv")
 
Final <- Player_RBI %>% 
  left_join(Players_2017, by=c("batter"="Player.ID")) %>% 
  select(batter, total_cRBI, total_ecRBI, pa, Last.Name, First.Name, V7) %>% 
  rename(position = "V7") %>% 
  filter(pa >= 100)
 
Final_unduped <- unique(Final[,])
 
Final_unduped <- Final_unduped %>% 
  mutate(stat = 100 + (((total_cRBI - total_ecRBI)/total_ecRBI)*100)) %>% 
  select(Last.Name, First.Name, stat, pa, total_ecRBI, total_cRBI, position) %>% 
  arrange(desc(stat))

Last <- Final_unduped %>% 
  arrange(desc(stat))