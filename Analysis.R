#------------------------------------------------------------------------------#
# script that analyze RSA
#------------------------------------------------------------------------------#

#import files

files<-list.files(pattern = ".csv")

all_files<-vector()
for (f in 1:length(files)){
  
  curr_f<-read.csv(files[f])
  
  # assign participant
  curr_f$part<-1:3
  
# assign the region
  stop<-regexpr("LH", files[f], fixed = TRUE)
  
  curr_f$ROI_name<-substr(files[f], 1, stop-2)
 
  # append
  all_files<-rbind(all_files, curr_f)
  
}

library(reshape2)
all_files_long<-melt(all_files, id = c("part", "ROI_name"), variable_name = "within_between")

library(lme4)
library(lmerTest)
library(car)
# model it
model<-lmer(value~variable*ROI_name+(1|part), data = all_files_long)
summary(model)

Anova(model)

model2<-lmer(value~variable+(1|part), data = all_files_long[all_files_long$ROI_name== "inferior-temporal",])
summary(model2)

