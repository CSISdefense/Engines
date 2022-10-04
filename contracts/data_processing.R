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

# --------------------------------------------------------------------------------
# add theme

source("contracts/theme/chart_theme.R")
source("contracts/theme/money_labels.R")

# --------------------------------------------------------------------------------
# read engine contract data

engine_contracts <-
  read_delim("contracts/data/Project.SP_EngineAllVendorHistoryCompetitionFundingMechanismVendorSizeProdServAreaSubCustomer.txt",
             na =c("NA","NULL"),delim="\t")


while(engine_contracts[nrow(engine_contracts),1] %in% c("0","Return Value"))
  engine_contracts<-engine_contracts[1:nrow(engine_contracts)-1,]

engine_contracts<-apply_standard_lookups(engine_contracts)

# --------------------------------------------------------------------------------
# read topline contract data

read_topline_contracts <- read.delim("contracts/data/Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.txt",
                                     na.strings=c("NA","NULL"),sep="\t")
topline_contracts<-apply_standard_lookups(read_topline_contracts)

# --------------------------------------------------------------------------------

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
  project = ProjectName
)

write.csv(biz_engine_contracts, "contracts/app/power_bi.csv")
save(topline_contracts ,engine_contracts,file="contracts/app/engine_contract.Rdata")


#-----------------------------------------------------------------------
# Top 10 Data processing
###############################################################
# Get Top 10 rows of aggregated Amount and # of action
################################################################
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




