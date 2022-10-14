# ================================================================================
# The Future of Military Engines
# By Gabriel Coll
# --------------------------------------------------------------------------------
# engine-related contract numbers from the Federal Procurement Data System
# ================================================================================

# ================================================================================
# cleaning and transformation 
# --------------------------------------------------------------------------------
# load packages 

library(tidyverse)
library(Cairo)
library(ggthemes)
library(extrafont)
library(extrafontdb)
library(svglite)
library(scales)
library(csis360)
library(readr)
# --------------------------------------------------------------------------------
# add theme

source("contracts/theme/chart_theme.R")
source("contracts/theme/money_labels.R")

# --------------------------------------------------------------------------------
# Read engine contract data ####

engine_contracts <-
  read_delim("contracts/data/Project.SP_EngineAllVendorHistoryCompetitionFundingMechanismVendorSizeProdServAreaSubCustomer.txt",
             na =c("NA","NULL"),delim="\t", guess_max = 900000)

# engine_contracts[nrow(engine_contracts),1]
while(engine_contracts[nrow(engine_contracts),1] %in% c("0","Return Value","0\r" ,"Return Value\r"))
  engine_contracts<-engine_contracts[1:nrow(engine_contracts)-1,]

engine_contracts<-apply_standard_lookups(engine_contracts)


# --------------------------------------------------------------------------------
# read topline contract data

# topline_contracts <- read.delim("contracts/data/Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.txt",
#                                      na.strings=c("NA","NULL"),sep="\t")
topline_contracts <- read.delim("contracts/data/Federal_Location.SP_ProdServPlatformAgencyPlaceOriginVendor.txt",
                                     na.strings=c("NA","NULL"),sep="\t")
while(topline_contracts[nrow(topline_contracts),1] %in% c("0","Return Value","0\r" ,"Return Value\r",
                                                                    "An error occurred while executing batch. Error message is: One or more errors occurred."))
  topline_contracts<-topline_contracts[1:nrow(topline_contracts)-1,]

if(colnames(topline_contracts)[1]=="productorservicecode"&colnames(topline_contracts)[5]=="ProductOrServiceCode")
  topline_contracts<-topline_contracts[,2:ncol(topline_contracts)]
topline_contracts<-standardize_variable_names(topline_contracts)

topline_contracts<-topline_contracts%>% select(Fiscal_Year, Contracting_Agency_ID, ProductOrServiceCode, ProjectID,PlatformPortfolio, Action_Obligation) %>%
  group_by(Fiscal_Year, Contracting_Agency_ID, ProductOrServiceCode, ProjectID,PlatformPortfolio) %>%
  dplyr::summarise(Action_Obligation = sum(Action_Obligation, na.rm = TRUE))

topline_contracts<-apply_standard_lookups(topline_contracts)
topline_contracts<-topline_contracts%>% filter(Customer=="Defense")
# if ("SubCustomer.platform" %in% names(engine_contracts) & "ProjectName" %in% names(topline_contracts)){
#   topline_contracts$SubCustomer.JPO<-as.character(topline_contracts$SubCustomer.platform)
#   topline_contracts$SubCustomer.JPO[topline_contracts$ProjectName %in% c("JSF (F-35) ","JSF (F-35)") & !is.na(topline_contracts$ProjectName)&engine_contracts$SubCustomer.platform=="Navy"]<-"F-35 JPO"
#   topline_contracts$SubCustomer.JPO<-factor(topline_contracts$SubCustomer.JPO)
# }

# --------------------------------------------------------------------------------
# clean and summarize data
# engine_contracts$Fiscal_Year<-text_to_number(engine_contracts$Fiscal_Year)
#   
#   engine_contracts <- engine_contracts %>%
#     dplyr::rename(
#     #   Fiscal_Year = fiscal_year,
#     amount = Action_Obligation
#     #   SimpleArea = Simple,
#     #   platform_portfolio = PlatformPortfolio,
#     #   customer_2 = Customer,
#     #   Customer = SubCustomer,
#     #   CompetitionClassification = ClassifyNumberOfOffers,
#     #   PricingMechanism = typeofcontractpricingtext,
#     #   VendorSize = VendorSize,
#     #   ParentID = ParentID,
#     #   ProjectName = ProjectName
#     ) %>%
#     # left_join(deflate.year, by = "Fiscal_Year") %>%
#     # mutate(amount_19 = amount / deflator) %>% #This was where the deflator mistake was made.
#     group_by(Fiscal_Year,
#              Customer,
#              SimpleArea,
#              ParentID,
#              ProjectName,
#              CompetitionClassification,
#              PricingMechanism,
#              VendorSize) %>%
#     dplyr::filter(Fiscal_Year <= 2021 & Fiscal_Year >= 2000) #%>%
#     # dplyr::summarise(amount = sum(amount_19, na.rm = TRUE))
# 
# engine_contracts<-csis360::deflate(data=engine_contracts,
#                                           money_var= "amount",
#                                           fy_var="Fiscal_Year",
#                                           deflator_var="OMB23_GDP21"
# )
# 
# # colnames(engine_contracts)[colnames(engine_contracts)=="Action_Obligation_OMB_2019"]
# 
# topline_contracts <- topline_contracts %>%
#   group_by(Fiscal_Year, Customer, ProductServiceOrRnDarea) %>%
#   dplyr::summarise(amount = sum(Action_Obligation, na.rm = TRUE)) %>%
#   dplyr::filter(Fiscal_Year <= 2018 & Fiscal_Year >= 2000)
# 
# topline_contracts<-csis360::deflate(data=topline_contracts,
#                                    money_var= "amount",
#                                    fy_var="Fiscal_Year",
#                                    deflator_var="OMB23_GDP21"
# )
# 
# # --------------------------------------------------------------------------------
# # reclassify Vendor Size
# #Simplify Vendor Size Classifications
# engine_contracts<-csis360::read_and_join(engine_contracts,
#                                   "LOOKUP_Contractor_Size.csv",
#                                   by="VendorSize",
#                                   add_var="VendorSize.sum"
# ) %>% select(-VendorSize) 
# colnames(engine_contracts)[colnames(engine_contracts)=="VendorSize.sum"]<-"VendorSize"
# engine_contracts$VendorSize<-factor(engine_contracts$VendorSize,c("Large (Big 5)" ,"Large","Medium",        "Small",         "Unlabeled" ))
# 
# # --------------------------------------------------------------------------------
# #Simplify Competition Classifications
# engine_contracts<-read_and_join(engine_contracts,"Lookup_SQL_CompetitionClassification.csv",
#                       by=c("CompetitionClassification",
#                            "ClassifyNumberOfOffers"),
#                       add_var="Competition.multisum") %>% select(-ClassifyNumberOfOffers,-CompetitionClassification) 
# 
# 
# 
# # --------------------------------------------------------------------------------
# # reclassify contract type
# 
# engine_contracts<-read_and_join(engine_contracts,
#                       "LOOKUP_Pricing_Mechanism.csv",
#                       by=c("PricingMechanism"),
#                       replace_na_var="PricingMechanism",
#                       add_var="PricingMechanism.sum"
# ) %>% select(-PricingMechanism) 
# colnames(engine_contracts)[colnames(engine_contracts)=="PricingMechanism.sum"]<-"PricingMechanism"
# 
# 
# # --------------------------------------------------------------------------------
# # reclassify product or service area
# 
# topline_contracts<-read_and_join_experiment(topline_contracts,
#                       "LOOKUP_Buckets.csv",
#                       by=c("ProductServiceOrRnDarea"),
#                       replace_na_var="ProductServiceOrRnDarea",
#                       add_var="ServicesCategory.sum")  %>% dplyr::select(-ProductServiceOrRnDarea) 
# colnames(topline_contracts)[colnames(topline_contracts)=="ServicesCategory.sum"]<-"SimpleArea"
# topline_contracts$SimpleArea<-
#   factor(topline_contracts$SimpleArea,
#          levels=c( "Products (All)" , 
#                    "R&D"            ,
#                    "Services (Non-R&D)",
#                    "Unlabeled"),
#          labels=c( "Products" , 
#                    "R&D"            ,
#                    "Services",
#                    "Unlabeled"))
#   



# --------------------------------------------------------------------------------
# write a new dataset for powerbi

summary(factor(engine_contracts$Action_Obligation_OMB23_GDP21))

biz_engine_contracts<-engine_contracts %>% dplyr::rename(
  fy = Fiscal_Year,
  amount = Action_Obligation_OMB23_GDP21,
  category = SimpleArea,
  platform_portfolio = PlatformPortfolio,
  customer_2 = Customer,
  customer = SubCustomer,
  competition = Competition.multisum,
  contract_type = PricingMechanism,
  vendor_size = VendorSize,
  # parent = ParentID,
  AllContractor = AllContractor,
  project = Project.Name
)
if ("SubCustomer.platform" %in% names(engine_contracts) & "Project.Name" %in% names(engine_contracts)){
  engine_contracts$SubCustomer.JPO<-as.character(engine_contracts$SubCustomer.platform)
  engine_contracts$SubCustomer.JPO[engine_contracts$Project.Name %in% c("JSF (F-35) ","JSF (F-35)") & !is.na(engine_contracts$Project.Name)&engine_contracts$SubCustomer.platform=="Navy"]<-"F-35 JPO"
  engine_contracts$SubCustomer.JPO<-factor(engine_contracts$SubCustomer.JPO)
}
write.csv(biz_engine_contracts, "contracts/app/power_bi.csv")

# 
# engine_contracts<-csis360::read_and_join_experiment(engine_contracts,
#                                                     "ProductOrServiceCodes.csv",
#                                                     by=c("ProductOrServiceCodeText"="ProductOrServiceCodeText"),
#                                                     add_var=c("ProductServiceOrRnDarea"),
#                                                     path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
#                                                     # skip_check_var = c("CrisisProductOrServiceArea","Simple"),
#                                                     dir=""
# )

engine_contracts<-csis360::read_and_join_experiment(engine_contracts,
                                               "ProductOrServiceCodes.csv",
                                               by=c("ProductOrServiceCode"="ProductOrServiceCode"),
                                               add_var=c("ProductServiceOrRnDarea",
                                                         "ProductOrServiceCodeText"),
                                               path="https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/",
                                               # skip_check_var = c("CrisisProductOrServiceArea","Simple"),
                                               dir=""
)

servicelist<-c('J028','J029','H128','H129','H228','H229','H328','H329','H928','H929','K028','K029','L028','L029','N028','N029','W028','W029')
nrow(engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                           ProductOrServiceArea == "Engines & Power Plants"))))
#294,956
nrow(engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                           (ProductOrServiceArea == "Engines & Power Plants" |
                                                         ClaimantProgramCode == 'A1B')))))
#630349
nrow(engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                           (ProductOrServiceArea == "Engines & Power Plants" |
                                                              ClaimantProgramCode == 'A1B'|
                                                              ProductOrServiceCode %in% servicelist)))))
#634148
nrow(engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                           (ProductOrServiceArea == "Engines & Power Plants" |
                                                              ClaimantProgramCode == 'A1B'|
                                                              ProductOrServiceCode %in% servicelist)))|
                                                          (ProductOrServiceCode %in% servicelist &
                                                           ClaimantProgramCode %in% c('C9E','S1','S10'))))
#643885


nrow(engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                           (ProductOrServiceArea == "Engines & Power Plants" |
                                                              ClaimantProgramCode == 'A1B'|
                                                              ProductOrServiceCode %in% servicelist)))|
                                   (ProductOrServiceCode %in% servicelist &
                                      ClaimantProgramCode %in% c('C9E','S1','S10')&
                                      (#fundingrequestingofficeid %in% c('F4FDAN','N00019','N61340','N68335') |
                                         SubCustomer=="Air Force"))))

engine_contracts<-engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                      (ProductOrServiceArea == "Engines & Power Plants" |
                                                         ClaimantProgramCode == 'A1B'|
                                                         ProductOrServiceCode %in% servicelist)))|
                              (ProductOrServiceCode %in% servicelist &
                                 ClaimantProgramCode %in% c('C9E','S1','S10')&
                                 (#fundingrequestingofficeid %in% c('F4FDAN','N00019','N61340','N68335') |
                                   SubCustomer=="Air Force")))

engine_contracts$SimpleArea.AETP<-engine_contracts$SimpleArea
engine_contracts$SimpleArea.AETP[engine_contracts$ProjectID==2260 | engine_contracts$SimpleArea=="R&D"]<-"R&D and AETP"
summary(factor(engine_contracts$SimpleArea.AETP))

topline_contracts$SimpleArea.AETP<-as.character(topline_contracts$SimpleArea)
topline_contracts$SimpleArea.AETP[topline_contracts$ProjectID==2260 | topline_contracts$SimpleArea=="R&D"]<-"R&D and AETP"
summary(factor(topline_contracts$SimpleArea.AETP))


save(topline_contracts ,engine_contracts,file="contracts/app/engine_contract.Rdata")

nrow(engine_contracts %>% filter(Customer!="Defense"))

save(engine_contracts,file="contracts/data/just_engine_contract.Rdata")

# load(file="contracts/app/engine_contract.Rdata")




#-----------------------------------------------------------------------
# Top 10 Data processing
##############################################################
# Get Top 10 rows of aggregated Amount and # of action ####
##############################################################
library(dplyr)
library(tidyr)
library(csis360)
library(readr)


# engine_vendor <-   read_delim("contracts/data/Project.SP_EngineAllVendorHistoryCompetitionFundingMechanismVendorSizeProdServAreaSubCustomer.txt",
                              # na =c("NA","NULL"),delim="\t")
  # read.delim("contracts/data/Project.SP_EngineAllVendorHistoryCompetitionFundingMechanismVendorSizeProdServAreaSubCustomer.txt",
  #                                    na.strings=c("NA","NULL"),sep="\t")

# engine_vendor<-remove_bom(engine_vendor)
# engine_vendor<-standardize_variable_names(engine_vendor)
engine_vendor<-engine_contracts

engine_vendor<-engine_vendor%>% 
  group_by(Fiscal_Year,AllContractor) %>% # group_by(Fiscal_Year,IsDefense,PlatformPortfolioUAV) %>%
  summarise(NumberOfActions=sum(NumberOfActions,na.rm=TRUE),
            Action_Obligation_OMB23_GDP21=sum(Action_Obligation_OMB23_GDP21,na.rm=TRUE)) %>%
  group_by(Fiscal_Year) %>%
  dplyr::mutate(pos=rank(-Action_Obligation_OMB23_GDP21))%>%
  arrange(Fiscal_Year,pos)

engine_vendor_overall<-engine_vendor%>% 
  group_by(AllContractor) %>% # group_by(Fiscal_Year,IsDefense,PlatformPortfolioUAV) %>%
  summarise(NumberOfActions=sum(NumberOfActions,na.rm=TRUE),
            Action_Obligation_OMB23_GDP21=sum(Action_Obligation_OMB23_GDP21,na.rm=TRUE),
            MinOfFiscalYear=min(Fiscal_Year),
            MaxOfFiscalYear=max(Fiscal_Year),
            ) %>%
  group_by() %>%
  dplyr::mutate(pos=rank(-Action_Obligation_OMB23_GDP21))%>%
  arrange(pos)

write.csv(engine_vendor_overall, file="contracts/data/TopEngineVendors.csv")

save(file="contracts/data/engine_vendor.rda",engine_vendor)
colnames(engine_vendor)




# OTA ####


label_engine<-function(x,col="Description_of_Requirement"){
  x<-as.data.frame(x)
  # grep("UAS|Unmanned|Uninhabited|Uncrewed|Remotely Crewed",x$Description_of_Requirement)
  # x$UAS[grep("RPA",x[,col],ignore.case = TRUE)]<-TRUE Doesn't seem to be any.
  x$Engine<-NA
  x$Engine[grep("engine[^e]",x[,col],ignore.case = TRUE)]<-TRUE
  sum(x$Engine,na.rm=TRUE)
  
  # x$Engine[grep("QUASAR",x[,col])]<-NA
  # sum(x$Engine,na.rm=TRUE)
  # x$Engine[grep("UAV",x[,col])]<-TRUE
  # sum(x$Engine,na.rm=TRUE)
  # x$Engine[grep("Unmanned",x[,col],ignore.case = TRUE)]<-TRUE
  # sum(x$Engine,na.rm=TRUE)
  # 
  # x$Engine[grep("Uncrewed",x[,col],ignore.case = TRUE)]<-TRUE
  # sum(x$Engine,na.rm=TRUE)
  # 
  # # Excluding Counter UAS
  # x$CUAS[grep("Counter Unmanned",x[,col],ignore.case = TRUE)]<-TRUE
  # sum(x$CUAS,na.rm=TRUE)
  # x$CUAS[grep("C-UAS",x[,col],ignore.case = TRUE)]<-TRUE
  # x$CUAS[grep("CSUAS",x[,col],ignore.case = TRUE)]<-TRUE
  # 
  # x$CUAS[grep("Counter-Unmanned",x[,col],ignore.case = TRUE)]<-TRUE
  # sum(x$CUAS,na.rm=TRUE)
  # 
  # x$UGS[grep("Unmanned Ground",x[,col],ignore.case = TRUE)]<-TRUE
  # 
  # x$Maritime[grep("Unmanned Surface",x[,col],ignore.case = TRUE)]<-TRUE
  # x$Maritime[grep("Unmanned Maritime",x[,col],ignore.case = TRUE)]<-TRUE
  # 
  # x$Maritime[grep("UNDERWATER UNMANNED",x[,col],ignore.case = TRUE)]<-TRUE
  # x$Maritime[grep("UNMANNED UNDERWATER",x[,col],ignore.case = TRUE)]<-TRUE
  # 
  # x$Maritime[grep("UNMANNED UNDERSEA",x[,col],ignore.case = TRUE)]<-TRUE
  # sum(x$CUAS,na.rm=TRUE)
  # 
  # 
  # x$Engine[x$CUAS==TRUE]<-NA
  # 
  # x$Engine[x$Maritime==TRUE]<-NA
  # x$Engine[x$UGS==TRUE]<-NA
  # sum(x$Engine,na.rm=TRUE)
  
  
  # 
  # x$mq[grep("MQ-",x[,col],ignore.case = TRUE)]<-TRUE
  # # x$mq[grep("MQT",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("MQSA",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("MESSAGE QUEUE",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("WEBSPHERE MQ",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("CMQA",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("NMQ",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("MQU",x[,col],ignore.case = TRUE)]<-NA #Covers most proper words
  # # x$mq[grep("OFMQ",x[,col],ignore.case = TRUE)]<-NA
  # sum(x$mq,na.rm = TRUE)
  # x$rq[grep("RQ-",x[,col],ignore.case = TRUE)]<-TRUE
  # # x$rq[grep("RQU",x[,col],ignore.case = TRUE)]<-NA #Covers most proper words
  # # x$rq[grep("RQMT",x[,col],ignore.case = TRUE)]<-NA
  # x$rq[grep("AN/SRQ-",x[,col],ignore.case = TRUE)]<-NA
  # x$rq[grep("AN/ARQ-",x[,col],ignore.case = TRUE)]<-NA
  # # x$rq[grep("TRQ",x[,col],ignore.case = TRUE)]<-NA
  # # x$rq[grep("SRQ",x[,col],ignore.case = TRUE)]<-NA
  # # x$rq[grep("AHRQ",x[,col],ignore.case = TRUE)]<-NA
  # sum(x$rq,na.rm=TRUE)
  # 
  # x$any_uas<-NA
  # x$any_uas<-x$Engine|x$mq|x$rq
  x
}



OTA_data_current <- read_delim(
  "contracts//data//OTA_All_Fields.csv",delim = ",",
  col_names = TRUE, guess_max = 500000,na=c("NA","NULL"),skip = 2)

OTA_data_current<-apply_standard_lookups(OTA_data_current)

# debug(label_engine)
OTA_data_current<-label_engine(OTA_data_current,
                               col="Description_of_Requirement")
sum(OTA_data_current$Engine,na.rm=TRUE)
summary(factor(OTA_data_current$Engine))
OTA_engine<-OTA_data_current %>% filter(ProductServiceOrRnDarea == "Engines & Power Plants"  |
                                          Engine==TRUE|
  ProductOrServiceCode %in% c(
  'J028','J029','H128','H129','H228','H229','H328','H329','H928','H929',
  'K028','K029','L028','L029','N028','N029','W028','W029'))
write_csv(file="contracts//data//OTA_engine.csv",OTA_engine)