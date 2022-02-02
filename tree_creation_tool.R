#Loading necessary libraries
library(tidyverse)
library(data.tree)
library(openxlsx)
library(magrittr)
library(dplyr)
#loading functions into workspace
source("parent_line.R")
source("parent_line_path_string.R")
#Importing raw data (parent- child relationship) 
# stringsAsFactors=FALSE will allow the data to be coerced as character and prevent from premature conversion to factor type variable

data1<-read.csv("air_vehicle_data_new.csv", stringsAsFactors=FALSE)
# Display orphans
data1[(is.na(data1$NEXT.ASSEMBLY) | data1$COMPONENT==""), ]
#removing orphans
data2<-data1[!(is.na(data1$NEXT.ASSEMBLY) | data1$COMPONENT==""), ]
# Displaying and Removing repeated parent child relationships to retain only unique relation ships
data2 [duplicated(data2[c(1,2)]),]
data3<-data2 [!duplicated(data2[c(1,2)]),]
#Displaying parent = child relationships
data3[1:2][apply(data3[1:2],1, function(x) length(unique(x)) == 1),]
#Removing 
data4<-data3[1:2][apply(data3[1:2],1, function(x) length(unique(x)) != 1),]
#list of repetitions
#duplicates<-duplicated(data1[c(1,2)])
#dcates<-data1[duplicates,]
# To take care of the same child type of multiple parents for e.g. bolt, nut etc. we create different identity for all such items hiphened by serial number
data5<-transform(data4,COMPONENT.new = ifelse(duplicated(data4$COMPONENT) | duplicated(data4$COMPONENT, fromLast=TRUE),paste(data4$COMPONENT, ave(data4$COMPONENT, data4$COMPONENT, FUN=seq_along), sep='_'),data4$COMPONENT))
# calling the function 'parent_line.R' to establish line of parents of the given child
data6<-data5
#replacing component names containing '/' with '@' so as to not coincide pathstring seperator  '/'
data6$COMPONENT.new<- gsub('/', '@', data5$COMPONENT.new)
data6$parent_line <- lapply(data6$COMPONENT.new,parent_line)
# determines the level of each component in the BOM
data6$level  <- lengths(data6$parent_line)
#  parent_line_path_string.R works same as above except that it stores the parent line with seperator as "/", as required by data.tree package when calling as.node function
data6$pathString <- lapply(data6$COMPONENT.new,parent_line_path_string)

#NOTE: the format of column name 'pathString' is suggested by data.tree package so as to detect path string.

# changing the format of data5 (in table or data frame) to tree (leaf and node structure) with the help of pathString created above.
finalassembly_data <- as.Node(data6)


print(finalassembly_data,"NEXT.ASSEMBLY","COMPONENT", "pathString","level","parent_line2", "level2")

# Convert back the tree structure type  to data frame type
BOM_Green_vehicle <- ToDataFrameTree(finalassembly_data, "NEXT.ASSEMBLY", "COMPONENT","COMPONENT.new","parent_line","pathString" ,"level")

write.xlsx(BOM_Green_vehicle, "BOM_air_vehicle_new.xlsx")
