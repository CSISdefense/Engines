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
  read.csv("contracts/data/engine_contracts.csv", na.strings=c("NA","NULL"))
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
  amount = Action.Obligation
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
                                          deflator_var="OMB.2019"
)

# colnames(engine_contracts)[colnames(engine_contracts)=="Action.Obligation.OMB.2019"]

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
                                   deflator_var="OMB.2019"
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
  amount = amount.OMB.2019,
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
# load(file="contracts/app/engine_contract.Rdata")
# ================================================================================
# charting engines 
# --------------------------------------------------------------------------------
# engine contracts 

(
  total <- engine_contracts %>%
    group_by(Fiscal.Year) %>%
    dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount.OMB.2019, x = Fiscal.Year), alpha = .9 , stat = "identity") +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    scale_y_continuous(labels = money_labels) +
    chart_theme +
    ggtitle("DoD Aircraft Engine Contract Obligations") +
    xlab("Fiscal Year") +
    ylab("Constant 2019 $")
)

ggsave(
  "contracts/charts/amount_total.svg",
  total,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# --------------------------------------------------------------------------------
# engine contracts by Vendor Size 
engine_contracts$Vendor.Size<-factor(engine_contracts$Vendor.Size,c("Large (Big 5)" ,"Large","Medium",        "Small",         "Unlabeled" ))
(
  Vendor.Size <- engine_contracts %>%
    filter(Vendor.Size != "Unlabeled") %>%
    group_by(Fiscal.Year, Vendor.Size) %>%
    dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount.OMB.2019, x = Fiscal.Year), alpha = .9, stat = "identity") +
    facet_wrap(~ Vendor.Size, nrow = 2) +
    chart_theme +
    ggtitle("DoD Aircraft Engine Contract Obligations by Vendor Size") +
    xlab("Fiscal Year") +
    ylab("Constant 2019 $") +
    scale_y_continuous(labels = money_labels) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "contracts/charts/amount_vendor_size.svg",
  Vendor.Size,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# --------------------------------------------------------------------------------
# engine contracts by CompetitionClassification  
(
  Competiton <- engine_contracts %>%
    filter(Competition.multisum != "Unlabeled") %>%
    group_by(Fiscal.Year, Competition.multisum) %>%
    dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
    mutate(Competiton = factor(
      Competition.multisum,
      levels=c("2+ Offers" ,
               "1 Offer"   ,
               "No Comp."),
      labels = c(
        "Effective Competition",
        "Competition with single offer",
        "No Competition"
      )
    )) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    geom_area(aes(y = amount.OMB.2019, x = Fiscal.Year), alpha = .9, stat = "identity") +
    facet_wrap(~ Competiton, nrow = 1) +
    chart_theme +
    ggtitle("DoD Aircraft Engine Contract Obligations by Extent of Competition") +
    xlab("Fiscal Year") +
    ylab("Constant 2019 $") +
    scale_y_continuous(labels = money_labels) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "contracts/charts/amount_competition.svg",
  Competiton,
  device = "svg",
  width = 8,
  height = 4,
  units = "in"
)

# --------------------------------------------------------------------------------
# engine contracts by Contract Type 
levels(factor(engine_contracts$Pricing.Mechanism))
(
  Pricing.Mechanism <- engine_contracts %>%
    filter(Pricing.Mechanism != "Other") %>%
    group_by(Fiscal.Year, Pricing.Mechanism) %>%
    dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
    mutate(Pricing.Mechanism = factor(
      Pricing.Mechanism,
      levels = c(
        "Cost+",
        "Combination",
        "Time/Mat.",
        "Fixed",
        "Unlabeled"
      ),
      labels = c(
        "Cost Reimbursement",
        "Combination",
        "Time and Materials",
        "Fixed Price",
        "Unlabeled"
      )
    )) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    geom_area(aes(y = amount.OMB.2019, x = Fiscal.Year), alpha = .9, stat = "identity") +
    facet_wrap(~ Pricing.Mechanism, nrow = 1) +
    chart_theme +
    ggtitle("DoD Aircraft Engine Contract Obligations by Contract Type") +
    xlab("Fiscal Year") +
    ylab("Constant 2019 $") +
    scale_y_continuous(labels = money_labels) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "contracts/charts/amount_contract_type.svg",
  Pricing.Mechanism,
  device = "svg",
  width = 12,
  height = 4,
  units = "in"
)

# --------------------------------------------------------------------------------
# super facet: engine contracts by service and SimpleArea

#This could be done in fewer steps but on skimming it seems to check out.

total <- engine_contracts %>%
  group_by(Fiscal.Year) %>%
  dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  as.data.frame(.)

total_category <- engine_contracts %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
  as.data.frame(.)

total <- total %>%
  rbind(total_category) %>%
  mutate(SubCustomer = "Total")

army <- engine_contracts %>%
  filter(SubCustomer == "Army") %>%
  group_by(Fiscal.Year) %>%
  dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  as.data.frame(.)

army_category <- engine_contracts %>%
  filter(SubCustomer == "Army") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
  as.data.frame(.)

army <- army %>%
  rbind(army_category) %>%
  mutate(SubCustomer = "Army")

navy <- engine_contracts %>%
  group_by(Fiscal.Year) %>%
  filter(SubCustomer == "Navy") %>%
  dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

navy_category <- engine_contracts %>%
  filter(SubCustomer == "Navy") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
  as.data.frame(.)

navy <- navy %>%
  rbind(navy_category) %>%
  mutate(SubCustomer = "Navy")

air_force <- engine_contracts %>%
  group_by(Fiscal.Year) %>%
  filter(SubCustomer == "Air Force") %>%
  dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

air_force_category <- engine_contracts %>%
  filter(SubCustomer == "Air Force") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
  as.data.frame(.)

air_force <- air_force %>%
  rbind(air_force_category) %>%
  mutate(SubCustomer = "Air Force")

dla <- engine_contracts %>%
  group_by(Fiscal.Year) %>%
  filter(SubCustomer == "DLA") %>%
  dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

dla_category <- engine_contracts %>%
  filter(SubCustomer == "DLA") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
  as.data.frame(.)

dla <- dla %>%
  rbind(dla_category) %>%
  mutate(SubCustomer = "DLA")

(
  super_facet <- total %>%
    rbind(army, navy, air_force, dla) %>%
    mutate(
      SubCustomer = factor(SubCustomer, levels = c("Army",
                                             "Navy",
                                             "Air Force",
                                             "DLA",
                                             "Total")),
      SimpleArea = factor(SimpleArea, levels = c("Products",
                                             "Services",
                                             "R&D",
                                             "Total"))
    ) %>%
    
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    geom_area(aes(x = Fiscal.Year, y = amount.OMB.2019), alpha = .9) +
    facet_grid(SubCustomer ~ SimpleArea) +
    chart_theme +
    xlab("Fiscal Year") +
    ylab("Constant 2019 $") +
    ggtitle("DoD Aircraft Engine Contract Obligations by service and SimpleArea") +
    scale_y_continuous(labels = money_labels) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )+
    labs(caption="Obligations for the remainder of DoD and with an unlabeled customer or area are excluded from the graph.")
)


ggsave(
  "contracts/charts/amount_customer_category.svg",
  super_facet,
  device = "svg",
  width = 10,
  height = 12,
  units = "in"
)

# ================================================================================
# charting engines and topline 
# --------------------------------------------------------------------------------
# join engines and topline 

engine_contracts <- engine_contracts %>%
  mutate(type = "Engines")

topline_contracts <- topline_contracts %>%
  mutate(type = "Topline")
colnames(topline_contracts)[colnames(topline_contracts)=="Customer"]<-"SubCustomer"
# engine_contracts <- engine_contracts %>%
#   select(Fiscal.Year, SubCustomer, SimpleArea, amount.OMB.2019, type)

comparison_contracts <- engine_contracts %>%
  select(Fiscal.Year, SubCustomer, SimpleArea, amount.OMB.2019, type) %>%
  rbind(topline_contracts %>%
          select(Fiscal.Year, SubCustomer, SimpleArea, amount.OMB.2019, type)) %>%
  group_by(Fiscal.Year, SubCustomer, SimpleArea, type) %>%
  dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE))

dyear <- comparison_contracts %>%
  group_by() %>%
  dplyr::rename(amount.OMB.2019.lagged = amount.OMB.2019) %>%
  mutate(Fiscal.Year = Fiscal.Year + 1) # %>%
# select(-fyb)

# comparison_contracts <- comparison_contracts %>% filter(Fiscal.Year >= 2001)
#Add lagged year
sumcheck<-sum(comparison_contracts$amount.OMB.2019,na.rm=TRUE)
comparison_contracts <- comparison_contracts %>%
  left_join(dyear, by = c("Fiscal.Year", "SubCustomer", "SimpleArea", "type"))# %>%
if(sumcheck!=sum(comparison_contracts$amount.OMB.2019,na.rm=TRUE)) stop("Sum Check Error")


dyear <- comparison_contracts %>%
  filter(Fiscal.Year==2000) %>%
  dplyr::rename(amount.OMB.2019.2000 = amount.OMB.2019) %>%
  group_by()%>%
  select(-amount.OMB.2019.lagged,-Fiscal.Year)

#Add 2000
sumcheck<-sum(comparison_contracts$amount.OMB.2019,na.rm=TRUE)
comparison_contracts <- comparison_contracts %>%
  left_join(dyear, by = c("SubCustomer", "SimpleArea", "type")) %>%
  mutate(amount_change = amount.OMB.2019 - amount.OMB.2019.lagged,
         baseline_change = amount.OMB.2019 - amount.OMB.2019.2000) #%>% 
if(sumcheck!=sum(comparison_contracts$amount.OMB.2019,na.rm=TRUE)) stop("Sum Check Error")



  
  
rm(dyear)

(
  comparison_contracts_overall <- comparison_contracts %>%
    group_by(Fiscal.Year, type) %>%
    dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE),
                     amount.OMB.2019.lagged = sum(amount.OMB.2019.lagged, na.rm = TRUE),
                     amount_change = sum(amount_change, na.rm = TRUE),
                     amount.OMB.2019.2000 = sum(amount.OMB.2019.2000, na.rm = TRUE),
                     baseline_change = sum(baseline_change, na.rm = TRUE)) %>%
    mutate(amount_percent_change = (amount_change) / amount.OMB.2019.lagged,
           baseline_percent_change = (baseline_change) / amount.OMB.2019.2000) #%>%
    # dplyr::rename(amount.OMB.2019 = amount.OMB.2019) %>%
    # select(Fiscal.Year, amount.OMB.2019, amount_change, amount_percent_change, type) #%>%
    # This has already been done, which is good, as I don't htink it makes sense to sum percent changes like that.
    # group_by(Fiscal.Year, type) %>%
    # dplyr::summarise(
    #   amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE),
    #   amount_change = sum(amount_change, na.rm = TRUE),
    #   amount_percent_change = sum(amount_percent_change, na.rm = TRUE)
    # )
)


# --------------------------------------------------------------------------------
# percent change comparison 
# 
# base_year <- engine_contracts %>%
#   as.tibble(.) %>%
#   group_by(Fiscal.Year, type) %>%
#   dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE))
# 
# dyear <- base_year %>%
#   filter(Fiscal.Year == 2000) %>%
#   select(amount.OMB.2019, type)
# 
# base_year.eng <- base_year %>%
#   left_join(dyear, by = "type") %>%
#   select(-Fiscal.Year.y) %>%
#   mutate(amount_change = amount.OMB.2019 - amount.OMB.2019.lagged) %>%
#   mutate(amount_percent_change = (amount_change) / amount.OMB.2019.lagged * 100) %>%
#   mutate(base = 0) %>%
#   mutate(amount.OMB.2019 = base + amount_percent_change) %>%
#   dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
#   group_by(Fiscal.Year) %>%
#   dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
#   mutate(type = "engine") %>%
#   mutate(amount.OMB.2019 = amount.OMB.2019 / 100)
# 
# base_year <- topline_contracts %>%
#   group_by(Fiscal.Year, type) %>%
#   dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE))
# 
# dyear <- base_year %>%
#   filter(Fiscal.Year == 2000) %>%
#   select(amount.OMB.2019, type)
# 
# (
#   base_year.top <- base_year %>%
#     left_join(dyear, by = "type") %>%
#     select(-Fiscal.Year.y) %>%
#     mutate(amount_change = amount.OMB.2019 - amount.OMB.2019.lagged) %>%
#     mutate(amount_percent_change = (amount_change) / amount.OMB.2019.lagged * 100) %>%
#     mutate(base = 0) %>%
#     mutate(amount.OMB.2019 = base + amount_percent_change) %>%
#     dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
#     group_by(Fiscal.Year) %>%
#     dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
#     mutate(type = "topline") %>%
#     mutate(amount.OMB.2019 = amount.OMB.2019 / 100)
# )

# eng.topline <- rbind(base_year.eng, base_year.top)

(
  graph_contracts_overall <- comparison_contracts_overall %>%
    mutate(type = factor(type, levels = c("Topline", "Engines"))) %>%
    ggplot() +
    geom_line(
      aes(x = Fiscal.Year, y = baseline_percent_change),
      alpha = .9,
      color = "#554449",
      size = 1
    ) +
    geom_hline(
      yintercept = 0,
      alpha = .5,
      color = "#554449",
      size = .5,
      linetype = "dotted"
    ) +
    facet_wrap(~ type) +
    chart_theme +
    ggtitle("Change in Aircraft Engine Contract Obligations") +
    xlab("Fiscal Year") +
    ylab("Cumulative Percent Change, Inflation Adjusted (2000=100%)") +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "contracts/charts/percent_change_total.svg",
  graph_contracts_overall,
  width = 9,
  height = 6,
  units = "in",
  device = "svg"
)

# --------------------------------------------------------------------------------
# percent change comparison by SubCustomer


# 
# 
# base_year <- engine_contracts %>%
#   as.tibble(.) %>%
#   group_by(Fiscal.Year, type, SubCustomer) %>%
#   dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE))
# 
# dyear <- base_year %>%
#   filter(Fiscal.Year == 2000) %>%
#   select(amount.OMB.2019, type, SubCustomer)
# 
# base_year.eng <- base_year %>%
#   left_join(dyear, by = c("type", "SubCustomer")) %>%
#   filter(SubCustomer != "Other DoD") %>%
#   select(-Fiscal.Year.y) %>%
#   mutate(amount_change = amount.OMB.2019 - amount.OMB.2019.lagged) %>%
#   mutate(amount_percent_change = (amount_change) / amount.OMB.2019.lagged * 100) %>%
#   mutate(base = 0) %>%
#   mutate(amount.OMB.2019 = base + amount_percent_change) %>%
#   dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
#   group_by(Fiscal.Year, SubCustomer) %>%
#   dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
#   mutate(data_type = "engine") %>%
#   mutate(amount.OMB.2019 = amount.OMB.2019 / 100)
# 
# base_year <- topline_contracts %>%
#   group_by(Fiscal.Year, type, SubCustomer) %>%
#   dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE))
# 
# dyear <- base_year %>%
#   filter(Fiscal.Year == 2000) %>%
#   select(amount.OMB.2019, type, SubCustomer)
# 
# base_year.top <- base_year %>%
#   left_join(dyear, by = c("type", "SubCustomer")) %>%
#   filter(SubCustomer != "Other DoD") %>%
#   select(-Fiscal.Year.y) %>%
#   mutate(amount_change = amount.OMB.2019 - amount.OMB.2019.lagged) %>%
#   mutate(amount_percent_change = (amount_change) / amount.OMB.2019.lagged * 100) %>%
#   mutate(base = 0) %>%
#   mutate(amount.OMB.2019 = base + amount_percent_change) %>%
#   dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
#   group_by(Fiscal.Year, SubCustomer) %>%
#   dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
#   mutate(data_type = "topline") %>%
#   mutate(amount.OMB.2019 = amount.OMB.2019 / 100)
# 
# eng.topline <- rbind(base_year.eng, base_year.top)



(
  comparison_contracts_subcustomer <- comparison_contracts %>%
    group_by(Fiscal.Year, SubCustomer, type) %>%
    dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE),
                     amount.OMB.2019.lagged = sum(amount.OMB.2019.lagged, na.rm = TRUE),
                     amount_change = sum(amount_change, na.rm = TRUE),
                     amount.OMB.2019.2000 = sum(amount.OMB.2019.2000, na.rm = TRUE),
                     baseline_change = sum(baseline_change, na.rm = TRUE)) %>%
    mutate(amount_percent_change = (amount_change) / amount.OMB.2019.lagged,
           baseline_percent_change = (baseline_change) / amount.OMB.2019.2000) #%>%
)

levels(factor(comparison_contracts_subcustomer$type))
max((comparison_contracts_subcustomer %>% 
       filter(SubCustomer=="Other DoD" &
                type =="Engines"))$amount.OMB.2019
    )

(
  graph_contracts_subcustomer <- comparison_contracts_subcustomer %>%
    filter(!is.na(SubCustomer) & SubCustomer %in% c("Total",
                                                     "Army",
                                                     "Navy",
                                                     "Air Force",
                                                     "DLA")) %>%
    group_by() %>%
    mutate(SubCustomer = factor(
      SubCustomer, levels = c("Total",
                           "Army",
                           "Navy",
                           "Air Force",
                           "DLA"
                           )
    )) %>%
    mutate(type = factor(type, levels = c("Topline", "Engines"))) %>%
    ggplot() +
    geom_line(
      aes(x = Fiscal.Year, y = baseline_percent_change),
      alpha = .9,
      color = "#554449",
      size = 1
    ) +
    geom_hline(
      yintercept = 0,
      alpha = .5,
      color = "#554449",
      size = .5,
      linetype = "dotted"
    ) +
    
    facet_grid(type ~ SubCustomer) +
    chart_theme +
    #scale_x_continuous(breaks = seq(2000, 2020, by = 2),
    #labels = function(x) {substring(as.character(x), 3, 4)})+
    #scale_y_continuous(limits = c(0:750)) +
    ggtitle("Change in Aircraft Engine Contract Obligations") +
    xlab("Fiscal Year") +
    ylab("Cumulative Percent Change, Inflation Adjusted (2000=100%)") +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )+
    labs(caption="Obligations for the remainder of DoD and with an unlabeled customer are excluded from the graph.")
)

ggsave(
  "contracts/charts/percent_change_customer.svg",
  graph_contracts_subcustomer,
  width = 9,
  height = 6,
  units = "in",
  device = "svg"
)

# --------------------------------------------------------------------------------
# percent change comparison by SimpleArea 
# 
# base_year <- engine_contracts %>%
#   as.tibble(.) %>%
#   group_by(Fiscal.Year, type, SimpleArea) %>%
#   dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE))
# 
# dyear <- base_year %>%
#   filter(Fiscal.Year == 2000) %>%
#   select(amount.OMB.2019, type, SimpleArea)
# 
# base_year.eng <- base_year %>%
#   left_join(dyear, by = c("type", "SimpleArea")) %>%
#   filter(SimpleArea != "Services") %>%
#   select(-Fiscal.Year.y) %>%
#   mutate(amount_change = amount.OMB.2019 - amount.OMB.2019.lagged) %>%
#   mutate(amount_percent_change = (amount_change) / amount.OMB.2019.lagged * 100) %>%
#   mutate(base = 0) %>%
#   mutate(amount.OMB.2019 = base + amount_percent_change) %>%
#   dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
#   group_by(Fiscal.Year, SimpleArea) %>%
#   dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
#   mutate(type = "engine") %>%
#   mutate(amount.OMB.2019 = amount.OMB.2019 / 100)
# 
# base_year <- topline_contracts %>%
#   group_by(Fiscal.Year, type, SimpleArea) %>%
#   dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE))
# 
# dyear <- base_year %>%
#   filter(Fiscal.Year == 2000) %>%
#   select(amount.OMB.2019, type, SimpleArea)
# 
# base_year.top <- base_year %>%
#   left_join(dyear, by = c("type", "SimpleArea")) %>%
#   filter(SimpleArea != "Services") %>%
#   select(-Fiscal.Year.y) %>%
#   mutate(amount_change = amount.OMB.2019 - amount.OMB.2019.lagged) %>%
#   mutate(amount_percent_change = (amount_change) / amount.OMB.2019.lagged * 100) %>%
#   mutate(base = 0) %>%
#   mutate(amount.OMB.2019 = base + amount_percent_change) %>%
#   dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
#   group_by(Fiscal.Year, SimpleArea) %>%
#   dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE)) %>%
#   mutate(type = "topline") %>%
#   mutate(amount.OMB.2019 = amount.OMB.2019 / 100)
# 
# eng.topline <- rbind(base_year.eng, base_year.top)


(
  comparison_contracts_area <- comparison_contracts %>%
    group_by(Fiscal.Year, SimpleArea, type) %>%
    dplyr::summarise(amount.OMB.2019 = sum(amount.OMB.2019, na.rm = TRUE),
                     amount.OMB.2019.lagged = sum(amount.OMB.2019.lagged, na.rm = TRUE),
                     amount_change = sum(amount_change, na.rm = TRUE),
                     amount.OMB.2019.2000 = sum(amount.OMB.2019.2000, na.rm = TRUE),
                     baseline_change = sum(baseline_change, na.rm = TRUE)) %>%
    mutate(amount_percent_change = (amount_change) / amount.OMB.2019.lagged,
           baseline_percent_change = (baseline_change) / amount.OMB.2019.2000) #%>%
)

(
  graph_contracts_area <- comparison_contracts_area %>%
    filter(!is.na(SimpleArea) & !SimpleArea %in% c("Services","Unlabeled")) %>%
    # mutate(SimpleArea = factor(SimpleArea, levels = c("Products",
    #                                               # "Services",
    #                                               "R&D"))) %>%
    mutate(type = factor(type, levels = c("Topline", "Engines"))) %>%
    ggplot() +
    geom_line(
      aes(x = Fiscal.Year, y = baseline_percent_change),
      alpha = .9,
      color = "#554449",
      size = 1
    ) +
    geom_hline(
      yintercept = 0,
      alpha = .5,
      color = "#554449",
      size = .5,
      linetype = "dotted"
    ) +
    facet_grid(type ~ SimpleArea) +
    chart_theme +
    ggtitle("Change in Aircraft Engine Contract Obligations") +
    xlab("Fiscal Year") +
    ylab("Cumulative Percent Change, Inflation Adjusted (2000=100%)") +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )+
    labs(caption="Unlabeled and services obligations are not shown.")
)



ggsave(
  "contracts/charts/percent_change_category.svg",
  graph_contracts_area,
  width = 9,
  height = 6,
  units = "in",
  device = "svg"
)


