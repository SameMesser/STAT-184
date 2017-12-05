---
title: "Final Project Rough Draft"
author: "Sam Messer"
date: "11/26/17"
output: 
  html_document:
    fig_height: 3
    fig_width: 5
---
<!-- Don't edit in between this line and the one below -->
```{r include=FALSE}
# Don't delete this chunk if you are using the DataComputing package
library(DataComputing)
```
*Source file* 
```{r, results='asis', echo=FALSE}
includeSourceDocuments()
```
<!-- Don't edit the material above this line -->

My goal here is to create a run expectancy matrix based on the 2016 MLB season and compare individual batters' preformances to the matrix in order to find out who is the best hitter with runners on base. 

I used data from retrosheet (a free online database with play-by-play files for most MLB games dating back to the 1920's). The function I used to make the data from retrosheet usable is not mine, it came from a book called "Analyzing Baseball Data with R" (Note that the only part of the book used here is the parse.retrosheet.pbp function, the rest of the code is mine.)

```{r}
parse.retrosheet.pbp = function(season){
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
```

The above function serves to pull the data down from retrosheet.org and turn it into usable csv files. From here, I read in the data set and select only the variables I need for my analysis. These include batter id, outs, runners on each base, the number of runs batted in on the play, and the actual event that happened. I then processed the data to get the necessary information from each observation.

```{r}
Events_2016 <- read.csv("all2016.csv",header=FALSE)

data <- Events_2016 %>% 
  select(V5, V11, V27, V28, V29, V30, V44) %>% 
  rename(outs = "V5", batter = "V11", b1 = "V27", b2 = "V28", b3 = "V29", event = "V30", rbi_play = "V44")

data_edit <- data %>%
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
         cRBI = ifelse(grepl("HR", event), rbi_play - 1, rbi_play)) %>% 
  select(batter, situation, cRBI) 
```

The "situation" variable is determined by where the runners are on the basepaths and how many outs there are. We are only interested in how well the batter drives in runners on base, so if the batter hits a homerun (determined with use of a regex on the event text), we subtract 1 from the RBI total, leaving only RBI of runners on base. 

From here, we just sum up the RBI for each situation, taking care not to count an AB twice in the case that there is more than one event during the AB.

```{r}
Run_matrix <- data_edit %>% 
  filter(batter != lead(batter)) %>% 
  group_by(situation) %>% 
  summarise(n_situation=n(), total=sum(cRBI)) %>% 
  mutate(ecRBI = (total / n_situation))
```
And here is the final run matrix, the expected number of runs batted in in each situation.

```{r}
Run_matrix
```

From here, I will compare players to this expectancy and see how they rank and do some visualization. I was thinking the most "clutch" hitters, the least "clutch" hitters, and stacking up baseballs biggest names against the eRBI. 

