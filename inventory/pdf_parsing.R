# ================================================================================
# The Future of Military Engines
# By Greg Sanders
# --------------------------------------------------------------------------------
# Testing Rand data import
# ================================================================================

# cleaning and transformation ====================================================
# load packages ------------------------------------------------------------------
library(pdftools)
library(readr)
library(dplyr)
library(tidyverse)

install.packages("rJava")
library(rJava) # load and attach 'rJava' now
# install.packages("devtools")
# library(devtools)
# devtools::install_github("ropensci/tabulizer", args="--no-multiarch")

# Setwd is only necessary because the engines project filel is broken. We should fix tht.
# setwd("C:\\Users\\gsand\\Repositories\\Engines")

#Read in the PDF
list.files("inventory\\raw_data")
specs<-pdf_text(pdf="inventory\\raw_data\\MR1596.pdf")

#Select the pg
specs<-specs[81:86]
t62page1<-unlist(strsplit(specs[1],"\n"))
t62page2<-unlist(strsplit(specs[2],"\n"))
t62page3<-unlist(strsplit(specs[3],"\n"))
t63page1<-unlist(strsplit(specs[4],"\n"))
t63page2<-unlist(strsplit(specs[5],"\n"))
t63page3<-unlist(strsplit(specs[6],"\n"))

write.csv(file="inventory\\raw_data\\table_6_2_page1.txt",
          t62page1,row.names = FALSE)

write.csv(file="inventory\\raw_data\\table_6_2_page2.txt",
          t62page2,row.names = FALSE)

write.csv(file="inventory\\raw_data\\table_6_2_page3.txt",
          t62page3,row.names = FALSE)


write.csv(file="inventory\\raw_data\\table_6_3_page1.txt",
          t63page1,row.names = FALSE)

write.csv(file="inventory\\raw_data\\table_6_3_page2.txt",
          t63page2,row.names = FALSE)

write.csv(file="inventory\\raw_data\\table_6_3_page3.txt",
          t63page3,row.names = FALSE)


#I then used excel fixed with import tools to import.

# write.csv(file="inventory\\raw_data\\table_6_3_note.text",
#           t63page2[20:21],row.names = FALSE)

engines62<-read_csv(file="inventory\\data\\MR1596_engine_table_6_2.csv",na="N/A")
engines63<-read_csv(file="inventory\\data\\MR1596_engine_table_6_3.csv",na="N/A")

engine<-dplyr::full_join(engines62,engines63)
engine$EngineSeries<-str_split_fixed(engine$Engine,"-",2)[,1]
write.csv(engine,file="inventory\\data\\engine_specs_combined.csv",row.names = FALSE)

colnames(engine)<-make.names(colnames(engine))
save(engine,file="inventory\\data\\engine_specs_combined.rda")

View(engine %>% arrange(Engine) %>% filter(is.na(`Thrust (at Intermediate Rating Point) (kg/second)`)|is.na(`NAVAIR Technical Scale`)))



