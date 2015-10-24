# R implementation of Logistic Regression/Markov Chain (LRMC) model originally developed 
# by Joel Sokol and Paul Kvam at Georgia Tech (see http://www2.isye.gatech.edu/~jsokol/lrmc/about_lrmc.html). 
# This script builds off a previous version written by Richard W. Sharp 

###################

# XML package for inputing tables
library(XML)

# Make list of division I teams ranked by Rating Percentage Index (RPI)

# Get top teams
url = "http://www.cbssports.com/collegebasketball/rankings/rpi/index1"
tables = readHTMLTable(url)
# get teams 1-34
teams1 = tables[[4]]
# get teams 35-68
teams2 = tables[[5]]

# Get remaining teams
url = "http://www.cbssports.com/collegebasketball/rankings/rpi/index2"
tables = readHTMLTable(url)
teams3 = tables[[4]]
teams4 = tables[[5]]

teams = rbind(teams1, teams2, teams3, teams4)

# make all lower case and replace spaces with '-'
for(i in 1:length(teams$School)) {
  name = teams$School[i]
  url.name = gsub("&", "", gsub(" ", "-", tolower(as.character(name))))
  teams$url.name[i] = url.name
  url = paste("http://www.sports-reference.com/cbb/schools/",url.name, sep="")
}

# function to manually correct problematic names 
transformName1 = function(name) {
  pairs = c(
    "Miami (Fla.)","miami-fl",
    "St. Mary's","saint-marys-ca",
    "Southern Miss","southern-mississippi",
    "Ole Miss","mississippi",
    "Stephen F. Austin","stephen-f-austin",
    "Detroit","detroit-mercy",
    "Saint Joseph's","saint-josephs",
    "Mount St. Mary's","mount-st-marys",
    "St. John's","st-johns-ny",
    "Loyola-Maryland","loyola-md",
    "St. Bonaventure","st-bonaventure",
    "Albany","albany-ny",
    "Bryant University","bryant",
    "LIU-Brooklyn","long-island-university",
    "St. Francis (N.Y.)","st-francis-ny",
    "NC-Asheville","north-carolina-asheville",
    "Loyola-Chicago","loyola-il",
    "Northridge","cal-state-northridge",
    "Santa Barbara","california-santa-barbara",
    "Bowling Green","bowling-green-state",
    "William & Mary","william-mary",
    "Miami (Ohio)","miami-oh",
    "E. Tennessee State","east-tennessee-state",
    "Texas State-San Marcos","texas-state",
    "St. Peter's","saint-peters",
    "New Jersey Tech","njit",
    "NC-Wilmington","north-carolina-wilmington",
    "SIU-Edwardsville","southern-illinois-edwardsville",
    "Wisconsin-Milwaukee","milwaukee",
    "St. Francis (Pa.)","saint-francis-pa",
    "Prairie View A&M","prairie-view",
    "NC-Greensboro","north-carolina-greensboro",
    "The Citadel","citadel",
    "NC State", "north-carolina-state",
    "UC Davis", "california-davis",
    "UC Irvine", "california-irvine",
    "UL Lafayette", "louisiana-lafayette",
    "UT Martin", "tennessee-martin",
    "UMass Lowell", "massachusetts-lowell",
    "VCU", "virginia-commonwealth",
    "SMU", "southern-methodist",
    "UMKC", "missouri-kansas-city")
  
  n = length(pairs)
  full.name = pairs[seq(1,n,2)]
  url.name  = pairs[seq(2,n,2)]
  
  idx = which(full.name == name)
  if(length(idx) > 0) {
    return(url.name[idx])
  } else {
    return("")
  }
}

# another function to manually correct problematic names 
transformName2 = function(name) {
  
  pairs = c(   
    "Albany (NY)", "albany-ny",
    "Alabama-Birmingham", "uab",         
    "Nevada-Las Vegas", "unlv",           
    "Miami (FL)", "miami-fl",                  
    "Loyola (IL)", "loyola-il",                
    "Lafayette", "lafayette",            
    "Texas A&M", "texas-am",                  
    "Louisiana State", "lsu",     
    "West Virginia", "west-virginia",    
    "Texas Christian", "tcu",
    "St. John's (NY)", "st-johns-ny",            
    "Alabama A&M", "alabama-am",              
    "Miami (OH)", "miami-oh",  
    "Central Florida", "central-florida",    
    "Loyola (MD)", "loyola-md",            
    "Saint Mary's (CA)", "saint-marys-ca", 
    "North Carolina A&T", "north-carolina-at",
    "Saint Francis (PA)", "saint-francis-pa",
    "Saint Peter's", "saint-peters",
    "Grambling", "grambling",
    "Virginia Military Institute", "vmi",
    "Texas A&M-Corpus Christi", "texas-am-corpus-christi",   
    "St. Francis (NY)", "st-francis-ny",
    "Florida A&M", "florida-am",
    "William & Mary","william-mary",
    "Stephen F. Austin","stephen-f-austin",
    "St. Bonaventure","st-bonaventure",
    "Saint Joseph's","saint-josephs",
    "Mount St. Mary's","mount-st-marys",
    "Central Florida", "ucf"
    )
  
  n = length(pairs)
  full.name = pairs[seq(1,n,2)]
  url.name  = pairs[seq(2,n,2)]
  
  idx = which(full.name == name)
  if(length(idx) > 0) {
    return(url.name[idx])
  } else {
    return("")
  }
}


n = nrow(teams)

for(i in 1:n) {
  name = teams$School[i]
  url.name = transformName1(name)
  if(url.name != "") {
    teams$url.name[i] = url.name
  }
}

t = matrix(0,n,n)
teams$ngames = array(0, n)

# loop through teams 
bad.teams = c()
bad.home = c()
for(teams.idx in 1:n) {
  
  # input game results from website
  team1.name = teams$url.name[teams.idx]
  url = paste("http://www.sports-reference.com/cbb/schools/",team1.name, sep="")
  url = paste(url, "/2015-schedule.html", sep="")
  tables = readHTMLTable(url)
  
  outcomes = tables[[length(tables)]]
  if(ncol(outcomes) != 15) {
    break
  }
  
  for(games.idx in 1:nrow(outcomes)) {

    # skip non D1 games
    if(outcomes$Conf[games.idx] == "") {
      next
    }
    
    team2 = outcomes$Opponent[games.idx]
    
    # skip header rows inserted midway through the table
    if(team2 == "Opponent") {
      next
    }
    
    team2.name = transformName2(team2)
      
    if(team2.name == "") {      
      team2.name = gsub(" ", "-", tolower(as.character(team2)))
    }  
    
    team.index = which(teams$url.name == team1.name)
    opponent.index = which(teams$url.name == team2.name)
    if(length(opponent.index) == 0) {
      bad.teams = c(bad.teams, as.character(team2))
      bad.home = c(bad.home, as.character(team1.name))
    }
    
    if(team2 == "EMPTY") {
      stop("team 2 error")
    }
    
  }
}

unique(bad.teams)

ncaa.team1  = c()
ncaa.team2  = c()
ncaa.winner = c()

# constants and function for computing P(teamX is better than teamY) based on point spread
tau <- 4.26
sig <- 11 
h   <- 4
a <- 2*tau^2/(sig*sqrt((sig^2+2*tau^2)*(sig^2+4*tau^2)))
b <- 2*tau^2*h/(sig*sqrt((sig^2+2*tau^2)*(sig^2+4*tau^2)))
x <- 20
pnorm(a*x-b) 


# loop through all teams on the teams list
for(teams.idx in 1:n) {
  
  # read in team game results
  team1.name = teams$url.name[teams.idx]
  url = paste("http://www.sports-reference.com/cbb/schools/",team1.name, sep="")
  url = paste(url, "/2015-schedule.html", sep="")
  tables = readHTMLTable(url)
  
  outcomes = tables[[length(tables)]]
  if(ncol(outcomes) != 15) {
    break
  }
  outcomes$Tm = as.numeric(as.character(outcomes$Tm))
  outcomes$Opp = as.numeric(as.character(outcomes$Opp))  
  
  for(games.idx in 1:nrow(outcomes)) {
    
    # skip non D1 games
    if(outcomes$Conf[games.idx] == "") {
      next
    }
    
    # skip games scheduled, but not yet played (i.e., no result available)
    if(is.na(outcomes$Tm[games.idx]) || is.na(outcomes$Opp[games.idx])) {
      next
    }

    team2 = outcomes$Opponent[games.idx]
    
    # skip header rows inserted midway through the table
    if(team2 == "Opponent") {
      next
    }
    
    team2.name = transformName2(team2)
    if(team2.name == "") {
      team2.name = gsub(" ", "-", tolower(as.character(team2)))
    }  
    
    team.index = which(teams$url.name == team1.name)
    opponent.index = which(teams$url.name == team2.name)

    if(outcomes$Type[games.idx] == "NCAA") {
      # record outcome of NCAA tournament games
      if(length(opponent.index) > 0) {
        
        if(team.index < opponent.index) {
          
          ncaa.team1 = c(ncaa.team1, as.character(teams$url.name[teams.idx]))
          ncaa.team2 = c(ncaa.team2, as.character(team2.name))

          if(outcomes$Tm[games.idx] > outcomes$Opp[games.idx]) {
            ncaa.winner = c(ncaa.winner, 1)
          } else {
            ncaa.winner = c(ncaa.winner, 2)
          }
        }
      }
    
      # don't use tournament games in rankings calculation
      next
    }
    
    if(length(opponent.index) > 0) {

      # don't count the same game twice (team1 v. team2 and team2 v. team1)
      if(opponent.index < team.index) {
        next
      }
      
      # get margin of victory for team playing on home court
      if(as.character(outcomes[games.idx,4]) == "") {
        team.home = team1.name
        team.away = team2.name
        i = team.index
        j = opponent.index
        if(outcomes$OT[games.idx] == "OT") {
          spread = 0
        } else {
          spread = outcomes$Tm[games.idx] - outcomes$Opp[games.idx]
        }
      } else if(as.character(outcomes[games.idx,4]) == "@") {
        team.home = team2.name
        team.away = team1.name
        i = opponent.index
        j = team.index
        if(outcomes$OT[games.idx] == "OT") {
          spread = 0
        } else {
          spread = outcomes$Opp[games.idx] - outcomes$Tm[games.idx]
        }
      } else {
        # for games on neutral court, treat as if team1 played on home court
        team.home = team1.name
        team.away = team2.name
        i = team.index
        j = opponent.index
        if(outcomes$OT[games.idx] == "OT") {
          spread = h
        } else {
          spread = outcomes$Tm[games.idx] - outcomes$Opp[games.idx] + h
        }
      }
    
      if(is.na(spread)) {
        next # if game is listed with no score (e.g. hasn't been played yet)
      }
      
      teams$ngames[i] = teams$ngames[i] + 1 # increment number of home games for team i
      teams$ngames[j] = teams$ngames[j] + 1 # increment number of away games for team j
  
      # update the probability matrix: 
      r = pnorm(a*spread-b)  # probability that home team is better than away team
      # update teams' rankings in probability matrix:  t[i,j] = P(team i is better than team  j)
      t[i,j] = t[i,j] + (1-r)
      t[j,i] = t[j,i] + r
      t[i,i] = t[i,i] + r
      t[j,j] = t[j,j] + (1-r)
  
      } 
  }
}

# normalize
for(i in 1:n) {
  if(teams$ngames[i]>0)
  t[i,] = t[i,]/teams$ngames[i]
}

#initialize ranking procedure
p = matrix(1/n, 1, n)

for(i in 1:n) {
  p[i] = n-i+1
}

p = p/sum(p)

# run ranking procedure
for(i in 1:1000) {
  p.next = p %*% t
  p = p.next
}

# add LRMC score to table and sort to get ranking
teams$LRMC.score = t(p)
teams = teams[order(teams$LRMC.score, decreasing=TRUE),]
teams.alpha = teams[order(teams$School, decreasing=FALSE),]

#######################
# NCAACompare dataframe to compare performance of LRMC and other models
NCAACompare=data.frame(t(rbind(ncaa.team1, ncaa.team2, ncaa.winner)))
NCAACompare$LRMC.diff=NA
NCAACompare$LRMC.Pred=0
NCAACompare$SOS.diff=NA
NCAACompare$SOS.Pred=0
NCAACompare$RPI.diff=NA
NCAACompare$RPI.Pred=0
#######################

# for each matchup in the NCAA Tournement, compute the number of
# times the LRMC, SOS and RPI models predicted the winner.
# correct = 0
# correctSOS = 0 
# correctRPI = 0
total = length(ncaa.team1)
for(i in 1:total) {

  #LRMC model
  score1 = teams$LRMC.score[teams$url.name == ncaa.team1[i]]
  score2 = teams$LRMC.score[teams$url.name == ncaa.team2[i]]
  NCAACompare$LRMC.diff[i] = score1 - score2
  
  if(score1 > score2) {
    NCAACompare$LRMC.Pred[i]=1
  }
  
  if(score2 > score1) {
    NCAACompare$LRMC.Pred[i]=2
  }
  
  # SOS model
  score1SOS = score1*as.numeric(as.character(teams$SOS[teams$url.name == ncaa.team1[i]]))
  score2SOS = score2*as.numeric(as.character(teams$SOS[teams$url.name == ncaa.team2[i]]))
  NCAACompare$SOS.diff[i] = score1SOS - score2SOS 
  
  if(score1SOS > score2SOS) {
    NCAACompare$SOS.Pred[i]=1
  }
  
  if(score2SOS > score1SOS) {
    NCAACompare$SOS.Pred[i]=2
  }
  
  # RPI model
  score1RPI = as.numeric(as.character(teams$RPI[teams$url.name == ncaa.team1[i]]))
  score2RPI = as.numeric(as.character(teams$RPI[teams$url.name == ncaa.team2[i]]))
  NCAACompare$RPI.diff[i] = score1RPI - score2RPI 
  
  if(score1RPI > score2RPI) {
    NCAACompare$RPI.Pred[i]=1 
  }
  
  if(score2RPI > score1RPI) {
    NCAACompare$RPI.Pred[i]=2
  }
}

# write NCAACompare to csv if you would like to work with this data again later
write.csv(NCAACompare, file = "NCAACompare.csv", row.names = TRUE)

##############################################################################

# read in saved NCAACompare data frame for comparisons and additional modeling
NCAACompare = read.csv("NCAACompare.csv") 

# NCAACompare holds the teams and the winner of each tournament game, 
# the difference of the teams' scores according to LRMC, SOS, and RPI models, 
# and the predicted winner according to each model.

# normalize the score differentials for each model and between each pair of teams, and add to NCAACompare
NCAACompare$LRMC.dNorm=NCAACompare$LRMC.diff/(max(NCAACompare$LRMC.diff)-min(NCAACompare$LRMC.diff))
NCAACompare$SOS.dNorm=NCAACompare$SOS.diff/(max(NCAACompare$SOS.diff)-min(NCAACompare$SOS.diff))
NCAACompare$RPI.dNorm=NCAACompare$RPI.diff/(max(NCAACompare$RPI.diff)-min(NCAACompare$RPI.diff))

# Inspect qqplots of comparing normalized score differentials of the models
qqplot(NCAACompare$LRMC.dNorm,NCAACompare$SOS.dNorm)
abline(0,1)
qqplot(NCAACompare$LRMC.dNorm,NCAACompare$RPI.dNorm)
abline(0,1)
qqplot(NCAACompare$SOS.dNorm,NCAACompare$RPI.dNorm)
abline(0,1)

# Use logistical regression to combine the models into a single, equally weighted model
NCAACompare$winner=as.numeric(NCAACompare$ncaa.winner)-1 # fast and dirty binarization of winner data
mylogit = glm(winner ~ LRMC.dNorm + SOS.dNorm + RPI.dNorm, NCAACompare ,  family = "binomial")
NCAACompare$GLM.prob=fitted.values(mylogit)
NCAACompare$GLM.Pred=round(fitted.values(mylogit))+1

# compare accuracy of LRMC, SOS, and combined LOGIT models to predict winners 
print(nrow(NCAACompare[NCAACompare$LRMC.Pred==NCAACompare$ncaa.winner,])/nrow(NCAACompare))
print(nrow(NCAACompare[NCAACompare$SOS.Pred==NCAACompare$ncaa.winner,])/nrow(NCAACompare))
print(nrow(NCAACompare[NCAACompare$RPI.Pred==NCAACompare$ncaa.winner,])/nrow(NCAACompare))
print(nrow(NCAACompare[NCAACompare$GLM.Pred==NCAACompare$ncaa.winner,])/nrow(NCAACompare))

# For this 2015 data, LRMC and SOS models performed equally well, and RPI model performed slightly worse.
# The combined model performed the best
# It us unclear how well these results generalize but it can be assessed by appylying these models to previous seasons.
