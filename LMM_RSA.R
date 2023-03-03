# ANALYZE RSA
rm(list=ls())
library(ggplot2)
library(dplyr)
# get the parent folder
#------------------------------------------------------------------------------#
# get the script directory
path<-rstudioapi::getSourceEditorContext()$path

# split the string into the names of the folders
names<-unlist(strsplit(path, split="/"))

# get number of carachters last name (file name)
charfile<-nchar(tail(names,1))

# subtract that to the path
path<-substr(path, 1,nchar(path) - charfile)
#------------------------------------------------------------------------------#

# get the labels
labels<-read.csv("labels.csv", sep = ";")
labels<-names(labels)

# get the subject specific RDMs
RDMs<-list.files("by_sub_rdm")

# get subjects
subname<-unique(substr(RDMs, 1, 6))

# create a list object to contain all the data
all_list<-list()

# set a progress bar
pb<-txtProgressBar(min=0, max=64*64*5*length(subname), style =3)

# initialize a counter for the whole df
counter2<-1

# loop over the subject
for (sub in 1:length(subname)){
  
  # get the files of one sub
  curr_sub<-subname[sub]
  
  # select the files of this subject
  curr_files<-RDMs[grep(curr_sub, RDMs)]
  
  # create a long dataframe
  long_df<-as.data.frame(matrix(NA, nrow = 64^2*length(curr_files), ncol = 5))
  
  names(long_df)<-c("dissimilarity", "same_categ", "object_categ", "ROI", "sub")
  
  # initialize a counter for the subject dataframe
  counter<-1
  
  # loop over the files of this subject
  for (file in 1:length(curr_files)){
    
    # select the ROI for that participant
    c_file<-curr_files[file]
    
    # import the RDM
    c_RDM<-read.csv(paste0("by_sub_rdm/", c_file), header = F)
    
    # get the name of this ROI
    split<-strsplit(c_file, "_condition-think_dissim")
    
    # select the name after the subject
    ROI<-substr(split[[1]][1], 8, nchar(split[[1]][1]))
    
    # from dataframe to matrix
    c_RDM_matrix<-as.matrix(c_RDM, nrow = 64, ncol = 64)
    
    # select only below the diagonal of the matrix
    sel<-upper.tri(c_RDM_matrix) # select the upper. 
    
    # assign NA to the upper
    c_RDM_matrix[sel]<-NA
    
    # assign NA to the diagonal
    diag(c_RDM_matrix)<-NA
    
    # set it in a dataframe
    c_RDM_df<-as.data.frame(c_RDM_matrix)
    
    # now get row and column names
    colnames(c_RDM_df)<-labels
    rownames(c_RDM_df)<-labels
  
    
    # loop through the columns
    for (col in 1:ncol(c_RDM_df)){
      
      # loop through the rows
    for (row in 1:nrow(c_RDM_df)){
      
      # get the dissimilarity
      long_df$dissimilarity[counter]<-c_RDM_df[row, col]
      
      # get the colname of that case
      colname<-colnames(c_RDM_df)[col]
      
      # if a character is contained in a string
      if (grepl(".",colname, fixed = T)){
      # select the name before the dot
      colname<-substr(colname, 1, regexpr(".", colname, fixed = TRUE)-1)
      }
      
      # same for rowname
      rowname<-rownames(c_RDM_df)[row]
      
      # if a character is contained in a string
      if (grepl(".",rowname, fixed = T)){
        # select the name before the dot
        rowname<-substr(rowname, 1, regexpr(".", rowname, fixed = TRUE)-1)
      }
      
      # create the "same_categ"index
      long_df$same_categ[counter]<- ifelse(colname == rowname, 1, 0)
      
      # object category
      long_df$object_categ[counter]<- colname
      
      # ROI
      long_df$ROI[counter]<-ROI
      
      # subject
      long_df$sub[counter]<-sub
      
      counter<-counter+1
      counter2<-counter2+1
      # set the progress bar
      setTxtProgressBar(pb,counter2 ) 

    } # end of the loop through rows
      
    } # end of the loop through columns
    
  } # end of the loop through ROIs
  
  # add the df to the object linst
  all_list[[sub]]<-long_df

}

# bind all the RDMs
all_df<-do.call(rbind, all_list)

# count the cases
a<-all_df %>%
  group_by( ROI, object_categ, same_categ) %>%
  tally()

# get rid of the NAs
all_df<-all_df[!is.na(all_df$dissimilarity),]


#------------------------------------------------------------------------------#
all_df$same_categ<-ifelse(all_df$same_categ==1, "within_categ", "between_categ")

# now plot 
ggplot(all_df %>%
         group_by(sub,same_categ, object_categ, ROI) %>%
         summarise(dissim = mean(dissimilarity, na.rm=T)) , 
                    aes(x = same_categ, y = dissim))+
  geom_boxplot(alpha=0)+

  geom_point(aes(group = sub, color = same_categ),
             cex = 3.25,
             
             position = position_dodge(0.1),
             show.legend = FALSE,
             stat="summary")+
  geom_line(data = all_df %>%
              group_by(sub,same_categ, object_categ, ROI) %>%
              summarise(dissim = mean(dissimilarity, na.rm=T)) , 
             aes(same_categ, dissim,group = sub), 
            size=1, alpha=0.2, stat="summary")+
  theme_classic() +
  facet_wrap(ROI~object_categ)

ggplot(all_df, 
       aes(x = same_categ, y = dissimilarity))+
  geom_boxplot(alpha=0)+
  geom_point

