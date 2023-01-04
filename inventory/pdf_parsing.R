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

install.packages("rJava")
library(rJava) # load and attach 'rJava' now
# install.packages("devtools")
# library(devtools)
# devtools::install_github("ropensci/tabulizer", args="--no-multiarch")

# Setwd is only necessary because the engines project filel is broken. We should fix tht.
setwd("C:\\Users\\gsand\\Repositories\\Engines")

#Read in the PDF
list.files("inventory\\raw_data")
specs<-pdf_text(pdf="inventory\\raw_data\\MR1596.pdf")

#Select the pg
specs<-specs[85:86]
page1<-unlist(strsplit(specs[1],"\n"))
page2<-unlist(strsplit(specs[2],"\n"))

write.csv(file="inventory\\raw_data\\table_6_3_page1.txt",
          page1,row.names = FALSE)

write.csv(file="inventory\\raw_data\\table_6_3_page2.txt",
          page2,row.names = FALSE)

#I then used excel fixed with import tools to import.

write.csv(file="inventory\\raw_data\\table_6_3_note.text",
          page2[20:21],row.names = FALSE)

engines<-read_csv(file="inventory\\data\\MR1596_engine_table.csv",na="N/A")

