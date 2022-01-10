library(rJava)
library(tabulizer)
library(openxlsx)
library(dplyr)
pagelist<-c(15,19,23,28,31,38,39,44,49,54,57,62,66,69,73,77,83,87,93,97)
N=length(pagelist)
table <- vector("list", N)
for (i in 1:N)
{
  table[[i]]<-data.frame(extract_tables(file ='Sikorsky_S76D_IPC/S76D_IPC_Chapter_25.pdf', pages=pagelist[i],guess= FALSE,method="lattice",columns=list(c(63.36,111.6,208.08,454.32,519.12,565.92))))
  table[[i]]$page_No <-pagelist[i]
  table[[i]] <- table[[i]][2:(dim(table[[i]])[1]-5),]
}


table<-bind_rows(table)
h <- table[1:3,]
header<-apply(h, 2, paste0, collapse = " ")
table<-table[-c(1:3),]
names(table)<-header
colnames(table)[7] <- "Page.No"
write.xlsx(table, "Tilak/IPC_25.xlsx")

#table_wm_rm<-data.frame(extract_tables(file ='S76D_IPC_Chapter_21_pg31_watermarkremoved.pdf', pages=31, guess=FALSE, method="lattice"))