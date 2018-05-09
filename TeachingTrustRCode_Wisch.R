#The purpose of this code is to
# Calculate the percentage of students whose STAAR reading scores are:
#   Approaching grade level 
# Overall
# By school type (elementary (E), middle (M), and high school (S))
# By gender
# By race
# Meets grade level 
# Overall
# By school type (elementary (E), middle (M), and high school (S))
# By gender
# By race
# Create at least one graph that illustrates the difference in the percentage of students approaching vs. meets reading scores by school type
library(dplyr)
library(ggplot2)

df <- read.csv(file="C:/Users/julie.wisch/Documents/TeachingTrust/Data.csv", header=TRUE, sep=",", colClasses = "character")

#converting the data frame from a bunch of character strings to a bunch of numeric values
df_hold<-df
df[] <- lapply(df, function(x) as.numeric(as.character(x)))
df$GRDTYPE<-df_hold$GRDTYPE


#Getting rid of the .'s and negative values, setting all of these to NA
#Assuming that a negative value or a decimal value means that there were no students associated with that group
df[df == "."] <- NA
df[df < 0] <- NA



#creating dataframes that only contain the information I need
#df.approaching_num contains the numerator for all student groups approaching grade readiness
df.approaching_num<-select(df, CAMPUS, GRDTYPE, CB00AR01S17N, CA00AR01S17N, CI00AR01S17N, CR00AR01S17N,
                           C300AR01S17N, CL00AR01S17N, CE00AR01S17N, CF00AR01S17N, CH00AR01S17N,
                           CM00AR01S17N, C400AR01S17N, CS00AR01S17N, C200AR01S17N, CW00AR01S17N)
colnames(df.approaching_num)<-c("Campus", "Type", "AfAm", "All", "AmIn", "AtRisk", "Asian", "ELL", "EconDis",
                                "Female", "Hisp", "Male", "PacIs", "Sped", "Biracial", "White")
#df.approaching_den contains the denominator for all student groups approaching grade readiness
df.approaching_den<-select(df, CAMPUS, GRDTYPE, CB00AR01017D, CA00AR01017D, CI00AR01017D, CR00AR01017D,
                           C300AR01017D, CL00AR01017D, CE00AR01017D, CF00AR01017D, CH00AR01017D,
                           CM00AR01017D, C400AR01017D, CS00AR01017D, C200AR01017D, CW00AR01017D)
colnames(df.approaching_den)<-c("Campus", "Type", "AfAm", "All", "AmIn", "AtRisk", "Asian", "ELL", "EconDis",
                                "Female", "Hisp", "Male", "PacIs", "Sped", "Biracial", "White")
#df.ready_num contains the numerator for all student groups achieving grade readiness
df.ready_num<-select(df, CAMPUS, GRDTYPE, CB00AR04217N, CA00AR04217N, CI00AR04217N, CR00AR04217N,
                     C300AR04217N, CL00AR04217N, CE00AR04217N, CF00AR04217N, CH00AR04217N,
                     CM00AR04217N, C400AR04217N, CS00AR04217N, C200AR04217N, CW00AR04217N)
colnames(df.ready_num)<-c("Campus", "Type", "AfAm", "All", "AmIn", "AtRisk", "Asian", "ELL", "EconDis",
                          "Female", "Hisp", "Male", "PacIs", "Sped", "Biracial", "White")
#df.ready_den contains the denominator for all student groups approaching grade readiness
df.ready_den<-select(df, CAMPUS, GRDTYPE, CB00AR04217D, CA00AR04217D, CI00AR04217D, CR00AR04217D,
                     C300AR04217D, CL00AR04217D, CE00AR04217D, CF00AR04217D, CH00AR04217D,
                     CM00AR04217D, C400AR04217D, CS00AR04217D, C200AR04217D, CW00AR04217D)
colnames(df.ready_den)<-c("Campus", "Type", "AfAm", "All", "AmIn", "AtRisk", "Asian", "ELL", "EconDis",
                          "Female", "Hisp", "Male", "PacIs", "Sped", "Biracial", "White")

#replacing na's with 0's
df.approaching_num[is.na(df.approaching_num)] <- 0
df.approaching_den[is.na(df.approaching_den)] <- 0
df.ready_num[is.na(df.ready_num)] <- 0
df.ready_den[is.na(df.ready_den)] <- 0

#Checking to make sure that the numerator is smaller than the denominator for each set of students
#creating a list of any campus names where that is not the case
k<-0
ListtoLookInto_Approaching<-matrix(nrow = 10, ncol = 2)
for (i in 1:length(df.approaching_den$Campus)){
  for (j in 3:16){
  if (df.approaching_den[i, j] < df.approaching_num[i, j]){
    k<- (k + 1)
    df.approaching_den[i, j]<- 0
    df.approaching_num[i, j]<-0
    ListtoLookInto_Approaching[k, 1]<-df.approaching_den[i, 1]
    ListtoLookInto_Approaching[k, 2]<-colnames(df.approaching_den[j])
  }
  }}

k<-0
ListtoLookInto_Ready<-matrix(nrow = 10, ncol = 2)
for (i in 1:length(df.ready_den$Campus)){
  for (j in 3:16){
    if (df.ready_den[i, j] < df.ready_num[i, j]){
      k<- (k + 1)
      df.ready_den[i, j]<- 0
      df.ready_num[i, j]<-0
      ListtoLookInto_Ready[k, 1]<-df.ready_den[i, 1]
      ListtoLookInto_Ready[k, 2]<-colnames(df.ready_den[j])
    }
  }}
#Checks reveal that numerator is always smaller than denominator.  That's good.


# Proportion_Approaching_All<-sum(df.approaching_num$All)/sum(df.approaching_den$All)
# Proportion_Ready_All<-sum(df.ready_num$All)/sum(df.ready_den$All)
#creating vectors to store data
Proportion_Approaching<-rep(0, 13)
Proportion_Ready<-rep(0, 13)

for (i in 3:16){
  Proportion_Approaching[i]<-sum(df.approaching_num[,i])/sum(df.approaching_den[,i])
  Proportion_Ready[i]<-sum(df.ready_num[,i])/sum(df.ready_den[,i])
}
Proportion_Approaching<-t(as.data.frame(Proportion_Approaching))
Proportion_Ready<-t(as.data.frame(Proportion_Ready))
colnames(Proportion_Approaching)<-c("Campus", "Type", "AfAm", "All", "AmIn", "AtRisk", "Asian", "ELL", "EconDis",
                          "Female", "Hisp", "Male", "PacIs", "Sped", "Biracial", "White")

colnames(Proportion_Ready)<-c("Campus", "Type", "AfAm", "All", "AmIn", "AtRisk", "Asian", "ELL", "EconDis",
                                    "Female", "Hisp", "Male", "PacIs", "Sped", "Biracial", "White")

#Breaking the dataframes out by school type
#Type 4 is S, Type 3 is M, Type 2 is B, and Type 1 is E
df.approaching_num_e <- subset(df.approaching_num, Type == "E")
df.approaching_den_e <- subset(df.approaching_den, Type == "E")
df.approaching_num_m <- subset(df.approaching_num, Type == "M")
df.approaching_den_m <- subset(df.approaching_den, Type == "M")
df.approaching_num_s <- subset(df.approaching_num, Type == "S")
df.approaching_den_s <- subset(df.approaching_den, Type == "S")

df.ready_num_e <- subset(df.ready_num, Type == "E")
df.ready_den_e <- subset(df.ready_den, Type == "E")
df.ready_num_m <- subset(df.ready_num, Type == "M")
df.ready_den_m <- subset(df.ready_den, Type == "M")
df.ready_num_s <- subset(df.ready_num, Type == "S")
df.ready_den_s <- subset(df.ready_den, Type == "S")

Proportion_Approaching_byType<-(c(sum(df.approaching_num_e$All)/sum(df.approaching_den_e$All), 
                            sum(df.approaching_num_m$All)/sum(df.approaching_den_m$All),
                            sum(df.approaching_num_s$All)/sum(df.approaching_den_s$All)))

Proportion_Ready_byType<-(c(sum(df.ready_num_e$All)/sum(df.ready_den_e$All), 
                            sum(df.ready_num_m$All)/sum(df.ready_den_m$All),
                            sum(df.ready_num_s$All)/sum(df.ready_den_s$All)))
                          
plotting<-matrix(nrow = 6,  ncol = 3)
plotting[,1]<- rbind(Proportion_Approaching_byType[1], Proportion_Approaching_byType[2], Proportion_Approaching_byType[3],
      Proportion_Ready_byType[1], Proportion_Ready_byType[2], Proportion_Ready_byType[3])
plotting[,2]<-c("Approaching", "Approaching", "Approaching", "Ready", "Ready", "Ready")
plotting[,3]<-c("Elementary", "Middle", "Secondary", "Elementary", "Middle", "Secondary")
colnames(plotting)<-c("Proportion", "Status", "Type")
plotting<-as.data.frame(plotting)
plotting$Proportion<-as.numeric(as.character(plotting$Proportion))
plotting$Status<-as.factor(plotting$Status)
plotting$Type<-as.factor(plotting$Type)

# Plot compares the proportion of students that test at either Approaching Grade Level or Ready
#Based on their school classification
ggplot(data=plotting, aes(factor(Type), Proportion, fill=Status)) +
  geom_bar(stat="identity", position="dodge")+theme_classic() + xlab("School Classification")
  + ylab("Proportion")


