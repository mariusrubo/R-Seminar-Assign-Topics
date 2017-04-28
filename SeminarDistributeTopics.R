# This script is originally used to assign dates and topics for a seminar in which students have to hold a presentation about some topic
# at some date, and topics are not linked to dates. 
# Simply let the students insert their preferred dates and topics in "SelectedTopics.csv" and this script will try to find a solution that 
# suits most students. 
# I'm sure there are better optimization algorithms if you need to find "the" best solution, but this script's results seem ok to me. 

rm(list=ls())
dates <- 1:10 # list of dates
topics <- 1:14 # list of topics
datapath <- paste(getwd(), '/SelectedTopics.csv', sep="")
writepath <- paste(getwd(), '/AssignedTopics.csv', sep="")


# No need to change anything beyond this point

data <- read.table(datapath,skip=1,dec=",", sep=";",na.strings="NA")
names(data) <- c("Name","Email","Date1","Date2","Date3","Topic1","Topic2","Topic3","AssignedDate","AssignedTopic", "GotSelectedDate", "GotSelectedTopic")

startingtime <- Sys.time()
library(pracma) # for randomization
StudentN <- 1:nrow(data) # there are as many students as row in the datasheet

resultsDate <- numeric() # alway note down selected combination of dates ...
bestDate <- numeric() # to find best combination
resultsTopic <- numeric()
bestTopic <- numeric()

# STEP 1: assign dates
bestResultdates <- max(StudentN) # to compare various results and remember which results in fewest "No"s
for (perm in 1:1000){
  data$GotSelectedDate[StudentN] <- "Yes" # first set all results to yes
  order <- randperm(StudentN) # order students randomly, go from first to last
for (i in 1:length(order)){ # i <- 4
if (!data$Date1[order[i]] %in% data$AssignedDate){data$AssignedDate[order[i]] <- data$Date1[order[i]] # if possible, give first priority
} else if (!data$Date2[order[i]] %in% data$AssignedDate){data$AssignedDate[order[i]] <- data$Date2[order[i]] # otherwise try to give second...
} else if (!data$Date3[order[i]] %in% data$AssignedDate){data$AssignedDate[order[i]] <- data$Date3[order[i]] # or third priority
} else { 
  data$GotSelectedDate[order[i]] <- "No" # if none are still available, note this and randomly draw one
  StillAvailable <- dates[!dates%in%data$AssignedDate]
  if (length(StillAvailable) > 1){StillAvailable <- randperm(StillAvailable)} # shuffle only if length is greater than 1, otherwise randperm shuffles all values up to this number
  data$AssignedDate[order[i]] <- StillAvailable[1]
  }
}

if (length(grep("No", data$GotSelectedDate)) < bestResultdates) { # if combination in this loop outperforms best one so far
  bestResultdates <- length(grep("No", data$GotSelectedDate)) # note new record
  bestDate <- data$AssignedDate # remember combination
  resultsDate <- data$GotSelectedDate # including list of who got selected date
}

}
data$AssignedDate <- bestDate # after entire loop, note best result
data$GotSelectedDate <- resultsDate


# STEP 1: assign topics
bestResulttopics <- max(StudentN)
for (perm in 1:1000){
  data$GotSelectedTopic[StudentN] <- "Yes"
  order <- randperm(StudentN) 
for (i in 1:length(order)){ # i <- 4
  if (!data$Topic1[order[i]] %in% data$AssignedTopic){data$AssignedTopic[order[i]] <- data$Topic1[order[i]] # if possible, give first priority
  } else if (!data$Topic2[order[i]] %in% data$AssignedTopic){data$AssignedTopic[order[i]] <- data$Topic2[order[i]] # otherwise try to give second...
  } else if (!data$Topic3[order[i]] %in% data$AssignedTopic){data$AssignedTopic[order[i]] <- data$Topic3[order[i]] # or third priority
  } else { 
    data$GotSelectedTopic[order[i]] <- "No" # if none are still available, note this and randomly draw one
    StillAvailable <- topics[!topics%in%data$AssignedTopic]
    if (length(StillAvailable) > 1){StillAvailable <- randperm(StillAvailable)} 
    data$AssignedTopic[order[i]] <- StillAvailable[1]
  }
}

if (length(grep("No", data$GotSelectedTopic)) < bestResulttopics) { 
  bestResulttopics <- length(grep("No", data$GotSelectedTopic))
  bestTopic <- data$AssignedTopic
  resultsTopic <- data$GotSelectedTopic
}

}
data$AssignedTopic <- bestTopic
data$GotSelectedTopic <- resultsTopic

# STEP 3: Write best results in csv file
write.table(data, file = writepath, sep=";",row.names=F, col.names=T,qmethod = c("escape", "double"))
print(bestResultdates)
print(bestResulttopics)
print(startingtime) # show how long script took
print(Sys.time())
