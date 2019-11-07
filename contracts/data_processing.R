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
  read.delim("contracts/data/Project.SP_EngineAllVendorHistoryCompetitionFundingMechanismVendorSizeProdServAreaSubCustomer.txt",
           na.strings=c("NA","NULL"),sep="\t")
engine_contracts<-csis360::standardize_variable_names(read_engine_contracts)

# --------------------------------------------------------------------------------
# read topline contract data

read_topline_contracts <- read.delim("contracts/data/Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.txt",
                                     na.strings=c("NA","NULL"),sep="\t")
topline_contracts<-csis360::standardize_variable_names(read_topline_contracts)
colnames(topline_contracts)[colnames(topline_contracts)=="FY"]<-"Fiscal.Year"
# --------------------------------------------------------------------------------

# --------------------------------------------------------------------------------
# clean and summarize data
engine_contracts$Fiscal.Year<-text_to_number(engine_contracts$Fiscal.Year)
  
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
    dplyr::filter(Fiscal.Year <= 2018 & Fiscal.Year >= 2000) #%>%
    # dplyr::summarise(amount = sum(amount_19, na.rm = TRUE))

engine_contracts<-csis360::deflate(data=engine_contracts,
                                          money_var= "amount",
                                          fy_var="Fiscal.Year",
                                          deflator_var="OMB19_19"
)

# colnames(engine_contracts)[colnames(engine_contracts)=="Action_Obligation_OMB_2019"]

topline_contracts <- topline_contracts %>%
  group_by(Fiscal.Year, Customer, ProductServiceOrRnDarea) %>%
  dplyr::summarise(amount = sum(Action_Obligation, na.rm = TRUE)) %>%
  dplyr::filter(Fiscal.Year <= 2018 & Fiscal.Year >= 2000)

topline_contracts<-csis360::deflate(data=topline_contracts,
                                   money_var= "amount",
                                   fy_var="Fiscal.Year",
                                   deflator_var="OMB19_19"
)

# --------------------------------------------------------------------------------
# reclassify Vendor Size
#Simplify Vendor Size Classifications
engine_contracts<-csis360::read_and_join(engine_contracts,
                                  "LOOKUP_Contractor_Size.csv",
                                  by="Vendor.Size",
                                  add_var="Vendor.Size.sum"
) %>% select(-Vendor.Size) 
colnames(engine_contracts)[colnames(engine_contracts)=="Vendor.Size.sum"]<-"Vendor.Size"
engine_contracts$Vendor.Size<-factor(engine_contracts$Vendor.Size,c("Large (Big 5)" ,"Large","Medium",        "Small",         "Unlabeled" ))

# --------------------------------------------------------------------------------
#Simplify Competition Classifications
engine_contracts<-read_and_join(engine_contracts,"Lookup_SQL_CompetitionClassification.csv",
                      by=c("CompetitionClassification",
                           "ClassifyNumberOfOffers"),
                      add_var="Competition.multisum") %>% select(-ClassifyNumberOfOffers,-CompetitionClassification) 



# --------------------------------------------------------------------------------
# reclassify contract type

engine_contracts<-read_and_join(engine_contracts,
                      "LOOKUP_Pricing_Mechanism.csv",
                      by=c("Pricing.Mechanism"),
                      replace_na_var="Pricing.Mechanism",
                      add_var="Pricing.Mechanism.sum"
) %>% select(-Pricing.Mechanism) 
colnames(engine_contracts)[colnames(engine_contracts)=="Pricing.Mechanism.sum"]<-"Pricing.Mechanism"


# --------------------------------------------------------------------------------
# reclassify product or service area

topline_contracts<-read_and_join_experiment(topline_contracts,
                      "LOOKUP_Buckets.csv",
                      by=c("ProductServiceOrRnDarea"),
                      replace_na_var="ProductServiceOrRnDarea",
                      add_var="ServicesCategory.sum")  %>% dplyr::select(-ProductServiceOrRnDarea) 
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

summary(factor(engine_contracts$Pricing.Mechanism))

biz_engine_contracts<-engine_contracts %>% dplyr::rename(
  fy = Fiscal.Year,
  amount = amount_OMB19_19,
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
