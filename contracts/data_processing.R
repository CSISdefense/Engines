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

read_engine_contracts <-
  read.csv("contracts/data/Project.SP_EngineAllVendorHistoryCompetitionFundingMechanismVendorSizeProdServAreaSubCustomer.csv",
           na.strings=c("NA","NULL"))
engine_contracts<-csis360::standardize_variable_names(read_engine_contracts)

# --------------------------------------------------------------------------------
# read topline contract data

read_topline_contracts <- read.csv("contracts/data/topline_contracts.csv",
                                   na.strings=c("NA","NULL"))
topline_contracts<-csis360::standardize_variable_names(read_topline_contracts)
# --------------------------------------------------------------------------------
# deflate data

# deflator <-
#   c(
#     0.6981148243,
#     0.7148243359,
#     0.7263924593,
#     0.7402742074,
#     0.7586118252,
#     0.7824335904,
#     0.8078834619,
#     0.8298200514,
#     0.8470437018,
#     0.8568980291,
#     0.8644387318,
#     0.8820051414,
#     0.8981148243,
#     0.9132819195,
#     0.9299057412,
#     0.9411311054,
#     0.9520137104,
#     0.9683804627,
#     0.9835475578,
#     1,
#     1.018508997
#   )
# 
# Fiscal.Year <- c(2000:2020)
# 
# deflate.year <- as.data.frame(cbind(Fiscal.Year, deflator))

# --------------------------------------------------------------------------------
# clean and summarize data


engine_contracts <- engine_contracts %>%
  dplyr::rename(
  #   Fiscal.Year = fiscal_year,
  amount = Action_Obligation
  #   SimpleArea = Simple,
  #   platform_portfolio = PlatformPortfolio,
  #   customer_2 = Customer,
  #   Customer = SubCustomer,
  #   CompetitionClassification = ClassifyNumberOfOffers,
  #   Pricing.Mechanism = typeofcontractpricingtext,
  #   Vendor.Size = VendorSize,
  #   ParentID = ParentID,
  #   ProjectName = ProjectName
  ) %>%
  # left_join(deflate.year, by = "Fiscal.Year") %>%
  # mutate(amount_19 = amount / deflator) %>% #This was where the deflator mistake was made.
  group_by(Fiscal.Year,
           Customer,
           SimpleArea,
           ParentID,
           ProjectName,
           CompetitionClassification,
           Pricing.Mechanism,
           Vendor.Size) %>%
  filter(Fiscal.Year <= 2017) #%>%
  # dplyr::summarise(amount = sum(amount_19, na.rm = TRUE))

engine_contracts<-csis360::deflate(data=engine_contracts,
                                          money_var= "amount",
                                          fy_var="Fiscal.Year",
                                          deflator_var="OMB19_19"
)

# colnames(engine_contracts)[colnames(engine_contracts)=="Action_Obligation_OMB_2019"]

topline_contracts <- topline_contracts %>%
  dplyr::rename(
    ProductOrServiceArea=PS,
  #   Fiscal.Year = Fiscal.Year,
    amount = Amount
  #   SimpleArea = SimpleArea,
  #   platform_portfolio = Portfolio,
  #   Customer = Customer,
  #   Vendor.Size = VendorSize
  ) %>%
  # left_join(deflate.year, by = "Fiscal.Year") %>%
  # mutate(amount_19 = amount * deflator) %>%
  group_by(Fiscal.Year, Customer, ProductOrServiceArea) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

topline_contracts<-csis360::deflate(data=topline_contracts,
                                   money_var= "amount",
                                   fy_var="Fiscal.Year",
                                   deflator_var="OMB19_19"
)

# --------------------------------------------------------------------------------
# reclassify Vendor Size
# 
# engine_contracts$Vendor.Size <-
#   as.character(engine_contracts$Vendor.Size)
# engine_contracts$Vendor.Size[engine_contracts$Vendor.Size == "Medium <1B"] <-
#   "Medium"
# engine_contracts$Vendor.Size[engine_contracts$Vendor.Size == "Medium >1B"] <-
#   "Medium"
# engine_contracts$Vendor.Size[engine_contracts$Vendor.Size == "Small"] <-
#   "Small"
# engine_contracts$Vendor.Size[engine_contracts$Vendor.Size == "Large: Big 6"] <-
#   "Big Five"
# engine_contracts$Vendor.Size[engine_contracts$Vendor.Size == "Large: Big 6 JV"] <-
#   "Big Five"


#Simplify Vendor Size Classifications
engine_contracts<-csis360::read_and_join(engine_contracts,
                                  "LOOKUP_Contractor_Size.csv",
                                  by="Vendor.Size",
                                  add_var="Vendor.Size.sum"
) %>% select(-Vendor.Size) 
colnames(engine_contracts)[colnames(engine_contracts)=="Vendor.Size.sum"]<-"Vendor.Size"

# --------------------------------------------------------------------------------
# reclassify CompetitionClassification

# engine_contracts$CompetitionClassification <-
#   as.character(engine_contracts$CompetitionClassification)
# 
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == ""] <-
#   "Unlabeled"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "NULL"] <-
#   "Unlabeled"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Unlabeled: Blank Extent Competed"] <-
#   "Unlabeled"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Unlabeled: Blank Fair Opportunity"] <-
#   "Unlabeled"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Unlabeled: CompetitionClassification; Zero Offers"] <-
#   "Unlabeled"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Unlabeled: No CompetitionClassification; multiple offers"] <-
#   "Unlabeled"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Unlabeled: No CompetitionClassification; multiple offers; Overrode blank Fair Opportunity)"] <-
#   "Unlabeled"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "10-24 Offers"] <-
#   "Effective CompetitionClassification"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "100+ Offers"] <-
#   "Effective CompetitionClassification"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "25-99 Offers"] <-
#   "Effective CompetitionClassification"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "3-4 Offers"] <-
#   "Effective CompetitionClassification"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "5-9 Offers"] <-
#   "Effective CompetitionClassification"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Two Offers"] <-
#   "Effective CompetitionClassification"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "No CompetitionClassification; Overrode blank Fair Opportunity)"] <-
#   "No CompetitionClassification"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "One Offer"] <-
#   "CompetitionClassification with single offer"
# engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Effective CompetitionClassification"] <-
#   "Effective CompetitionClassification"

#Simplify Vendor Size Classifications
engine_contracts<-read_and_join(engine_contracts,"Lookup_SQL_CompetitionClassification.csv",
                      by=c("CompetitionClassification",
                           "ClassifyNumberOfOffers"),
                      add_var="Competition.multisum") %>% select(-ClassifyNumberOfOffers,-CompetitionClassification) 



# --------------------------------------------------------------------------------
# reclassify contract type

# engine_contracts$Pricing.Mechanism <-
#   as.character(engine_contracts$Pricing.Mechanism)
# 
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Combination (two or more)"] <-
#   "Combination"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Cost No Fee"] <-
#   "Cost Reimbursement"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Cost Plus Award Fee"] <-
#   "Cost Reimbursement"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Cost Plus Fixed Fee"] <-
#   "Cost Reimbursement"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Cost Plus Incentive"] <-
#   "Cost Reimbursement"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Cost Sharing"] <-
#   "Cost Reimbursement"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Firm Fixed Price"] <-
#   "Fixed Price"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Fixed Price Award Fee"] <-
#   "Fixed Price"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Fixed Price Incentive"] <-
#   "Fixed Price"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Fixed Price Level of Effort"] <-
#   "Fixed Price"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Fixed Price Redetermination"] <-
#   "Fixed Price"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Fixed Price with Economic Price Adjustment"] <-
#   "Fixed Price"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Labor Hours"] <-
#   "Time and Materials"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Not Reported"] <-
#   "Unlabeled"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Order Dependent (IDV only)"] <-
#   "Other"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Other (none of the above)"] <-
#   "Other"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Time and Materials"] <-
#   "Time and Materials"
# engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "NULL"] <-
#   "Unlabeled"

engine_contracts<-read_and_join(engine_contracts,
                      "LOOKUP_Pricing_Mechanism.csv",
                      by=c("Pricing.Mechanism"),
                      replace_na_var="Pricing.Mechanism",
                      add_var="Pricing.Mechanism.sum"
) %>% select(-Pricing.Mechanism) 
colnames(engine_contracts)[colnames(engine_contracts)=="Pricing.Mechanism.sum"]<-"Pricing.Mechanism"


# --------------------------------------------------------------------------------
# reclassify product or service area


colnames(topline_contracts)[colnames(topline_contracts)=="ProductOrServiceArea"]<-"ProductServiceOrRnDarea"
topline_contracts<-read_and_join(topline_contracts,
                      "LOOKUP_Buckets.csv",
                      by="ProductServiceOrRnDarea",
                      replace_na_var="ProductServiceOrRnDarea",
                      add_var="ServicesCategory.sum")  %>% select(-ProductServiceOrRnDarea) 
colnames(topline_contracts)[colnames(topline_contracts)=="ServicesCategory.sum"]<-"SimpleArea"
topline_contracts$SimpleArea<-
  factor(topline_contracts$SimpleArea,
         levels=c( "Products (All)" , 
                   "R&D"            ,
                   "Services (Non-R&D)",
                   "Unlabeled"),
         labels=c( "Products" , 
                   "R&D"            ,
                   "Services",
                   "Unlabeled"))
  



# --------------------------------------------------------------------------------
# write a new dataset for powerbi

engine_contracts$Pricing.Mechanism

biz_engine_contracts<-engine_contracts %>% dplyr::rename(
  fy = Fiscal.Year,
  amount = amount_OMB_2019,
  category = SimpleArea,
  platform_portfolio = PlatformPortfolio,
  customer_2 = Customer,
  customer = SubCustomer,
  competition = Competition.multisum,
  contract_type = Pricing.Mechanism,
  vendor_size = Vendor.Size,
  parent = ParentID,
  project = ProjectName
)

write.csv(biz_engine_contracts, "contracts/app/power_bi.csv")
save(topline_contracts ,engine_contracts,file="contracts/app/engine_contract.Rdata")
