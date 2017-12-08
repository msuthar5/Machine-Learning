# Manish Suthar
# CS 422 
# Illinois Institute of Technology

rm(list=ls())

collegeData <- data.frame(read.csv("/Users/manishsuthar/Desktop/fall2017/422/labs/lab1/College.csv"))

rownames(collegeData)<-collegeData[,1]

#Eliminate the first name from the data so it is not treated as actual data
collegeData <- collegeData[,-1]

library(summarytools)

#Provide a brief summary of each attribute of the data
summary(collegeData)

#Scatter plot for first 10 attributes
pairs(collegeData[,1:10])


#Plot of the private School tuition vs public school tuition
plot(collegeData$Private, collegeData$Outstate, axis(labels = TRUE))
mtext("Private School or Not", side = 1)
mtext("Tuition", side = 2)
title(" Figure A: Plot of Private School/Public School vs Tuition Rate")

#Create variable of elite with the number or rows from collegeData
#and each row will have "No"
Elite <- rep("No", nrow(collegeData))
#Change value of Elite class label for certian schools 
Elite[collegeData$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)

#Create new DF by combining collegeData and the Elite column
collegeData <- data.frame(collegeData, Elite)
summary(Elite)

#Box Plot
plot(collegeData$Elite, collegeData$Outstate, colo)
title("Figure B: Elite vs Outstate")
mtext("Elite", side=1)
mtext("Tuition", side=2)

#Using Regex to find all rows of private schools
privateDf <- collegeData[grep("Yes", collegeData$Private, ignore.case = T),]

#Using Regex to find all rows of non-private schools
nonPrivateDf <- collegeData[grep("No", collegeData$Private, ignore.case = T), ]

hist(collegeData$Top10perc, 
     xlim = c(0,100), 
     breaks = 10,
     xlab = "# top 10% students enrolled", 
     ylab = "# Schools",
     main = "Figure C: Frequency of Schools with Top 10% highschool Students",
     border = "blue",
     col = "green",
     ylim = c(0,300))

hist(collegeData$Accept,
     xlim = c(0,20000), 
     breaks = 30,
     xlab = "# Applications Accepted",
     ylab = "# Schools",
     main = "Figure D: Frequency of Applications Accepted",
     border= "black",
     col = "red")

hist(privateDf$Expend,
     breaks = seq(0,60000,5000),
     ylim = c(0,400),
     xlab = "Instructional Expenditure ($)",
     ylab = "# Schools",
     main = "Figure E: Frequency of Instructional Expenditure",
     border = "black",
     col = "salmon1")

hist(collegeData$perc.alumni,
     ylim = c(0,130),
     xlim = c(0,70),
     breaks = 10,
     main = "Figure F: Frequency of Percent of Alumni Donating",
     xlab = "Alumni Donating (%)",
     ylab = "# Schools",
     border = "black",
     col = "yellow")

summary(privateDf$PhD)
summary(nonPrivateDf$PhD)


#Plot of Non-Private Schools Apps Received vs Accepted
plot(nonPrivateDf$Apps, nonPrivateDf$Accept,
     main = "Figure G: Non-Private School Apps Received vs Accepted",
     xlab = "Apps Received",
     ylab = "Apps Accepted")
#best fit line
lines(lowess(nonPrivateDf$Apps, nonPrivateDf$Accept), col="blue")

#Plot of Non-Private Schools Apps Received vs Accepted
plot(privateDf$Apps, privateDf$Accept,
     main = "Figure H: Private School Apps Received vs Accepted",
     xlab = "Apps Received",
     ylab = "Apps Accepted")
#best fit line
lines(lowess(privateDf$Apps, privateDf$Accept), col="blue")

nrow(privateDf)
nrow(nonPrivateDf)

sprintf("As shown by the graphs private schools have a more linear relationship of applications received vs accepted than non-private school. However private schools have %f times the records than the non-private schools. This could be a potential cause for the more linear fit line", nrow(privateDf)/nrow(nonPrivateDf))

# The following 2 plots Show the scatter plots
# of the the S.F Ratio vs the # of top 10% of new students
# from highschools attending a given school

"As can Be Seen, scatter plots do not capture this relationship
so we turn to the histagrams"

plot(privateDf$S.F.Ratio,privateDf$Top10perc,
     xlab = "Student/Faculty Ratio",
     ylab = "Top 10% Highschools attending a Schools",
     main = "Figure I: Private School: S.F Ratio vs Top 10% Incoming Freshman")

plot(nonPrivateDf$S.F.Ratio,nonPrivateDf$Top10perc,
     xlab = "Student/Faculty Ratio",
     ylab = "Top 10% Highschools attending a Schools",
     main = "Figure J: Non-Private School: S.F Ratio vs Top 10% Incoming Freshman")

# Need to Find the right parameters for the plot
private_SF <- summary(privateDf$S.F.Ratio)
nonPrivate_SF <- summary(nonPrivateDf$S.F.Ratio)

hist(privateDf$S.F.Ratio,
     xlab = "S.F. Ratio",
     main = "Figure K: Frequency of S.F. Ratio at Private Schools",
     xlim = c(0,30),
     ylim = c(0,500),
     col = "red")

hist(nonPrivateDf$S.F.Ratio,
     xlab = "S.F. Ratio",
     main = "Figure L: Frequency of S.F. Ratio at Non-Private Schools",
     ylim = c(0,60),
     breaks = seq(0,40,5),
     col = "red")

#Testing Relationship between Apps # Accepted vs # Students Enrolled 

plot(privateDf$Accept, privateDf$Enroll, 
     xlab = "# Apps Accepted", 
     ylab = "#Students Enrolled",
     main = "Figure M: Private: #Apps Accepted vs #Students Enrolled")
lines(lowess(privateDf$Accept, privateDf$Enroll), col="blue")

plot(nonPrivateDf$Accept, nonPrivateDf$Enroll, 
     xlab = "# Apps Accepted", 
     ylab = "#Students Enrolled",
     main = "Figure N: Non-Private: #Apps Accepted vs #Students Enrolled")
lines(lowess(nonPrivateDf$Accept, nonPrivateDf$Enroll), col="blue")
