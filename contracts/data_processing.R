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
  read.csv("contracts/data/engine_contracts.csv")

# --------------------------------------------------------------------------------
# read topline contract data

read_topline_contracts <- read.csv("contracts/data/topline_contracts.csv")
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
read_engine_contracts<-remove_bom(read_engine_contracts)
engine_contracts<-csis360::standardize_variable_names(read_engine_contracts)


engine_contracts <- engine_contracts %>%
  # dplyr::rename(
  #   Fiscal.Year = fiscal_year,
  #   amount = SumOfobligatedAmount,
  #   SimpleArea = Simple,
  #   platform_portfolio = PlatformPortfolio,
  #   customer_2 = Customer,
  #   Customer = SubCustomer,
  #   CompetitionClassification = ClassifyNumberOfOffers,
  #   Pricing.Mechanism = typeofcontractpricingtext,
  #   Vendor.Size = VendorSize,
  #   ParentID = ParentID,
  #   ProjectName = ProjectName
  # ) %>%
  # left_join(deflate.year, by = "Fiscal.Year") %>%
  # mutate(amount_19 = amount * deflator) %>%
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
                                          money_var= "Action.Obligation",
                                          fy_var="Fiscal.Year",
                                          deflator_var="OMB.2019"
)

# colnames(engine_contracts)[colnames(engine_contracts)=="Action.Obligation.OMB.2019"]

topline_contracts <- topline_contracts %>%
  # dplyr::rename(
  #   Fiscal.Year = Fiscal.Year,
  #   amount = Amount,
  #   SimpleArea = SimpleArea,
  #   platform_portfolio = Portfolio,
  #   Customer = Customer,
  #   Vendor.Size = VendorSize
  # ) %>%
  # left_join(deflate.year, by = "Fiscal.Year") %>%
  # mutate(amount_19 = amount * deflator) %>%
  group_by(Fiscal.Year, Customer, SimpleArea) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

topline_contracts<-csis360::deflate(data=topline_contracts,
                                   money_var= "Action.Obligation",
                                   fy_var="Fiscal.Year",
                                   deflator_var="OMB.2019"
)

# --------------------------------------------------------------------------------
# reclassify vendor size

engine_contracts$Vendor.Size <-
  as.character(engine_contracts$Vendor.Size)
engine_contracts$Vendor.Size[engine_contracts$Vendor.Size == "Medium <1B"] <-
  "Medium"
engine_contracts$Vendor.Size[engine_contracts$Vendor.Size == "Medium >1B"] <-
  "Medium"
engine_contracts$Vendor.Size[engine_contracts$Vendor.Size == "Small"] <-
  "Small"
engine_contracts$Vendor.Size[engine_contracts$Vendor.Size == "Large: Big 6"] <-
  "Big Five"
engine_contracts$Vendor.Size[engine_contracts$Vendor.Size == "Large: Big 6 JV"] <-
  "Big Five"


#Classify Product or Service Codes
engine_contracts<-csis360::read_and_join(engine_contracts,
                                  "LOOKUP_Contractor_Size.csv",
                                  # by="ProductOrServiceArea",
                                  by="Vendor.Size",
                                  # replace_na_var="ProductServiceOrRnDarea",
                                  add_var="Vendor.Size.sum",
                                  # path="https://raw.githubusercontent.com/CSISdefense/R-scripts-and-data/master/",
                                  # dir="Lookups/"
)


# --------------------------------------------------------------------------------
# reclassify CompetitionClassification

engine_contracts$CompetitionClassification <-
  as.character(engine_contracts$CompetitionClassification)

engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == ""] <-
  "Unlabeled"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "NULL"] <-
  "Unlabeled"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Unlabeled: Blank Extent Competed"] <-
  "Unlabeled"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Unlabeled: Blank Fair Opportunity"] <-
  "Unlabeled"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Unlabeled: CompetitionClassification; Zero Offers"] <-
  "Unlabeled"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Unlabeled: No CompetitionClassification; multiple offers"] <-
  "Unlabeled"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Unlabeled: No CompetitionClassification; multiple offers; Overrode blank Fair Opportunity)"] <-
  "Unlabeled"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "10-24 Offers"] <-
  "Effective CompetitionClassification"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "100+ Offers"] <-
  "Effective CompetitionClassification"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "25-99 Offers"] <-
  "Effective CompetitionClassification"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "3-4 Offers"] <-
  "Effective CompetitionClassification"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "5-9 Offers"] <-
  "Effective CompetitionClassification"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Two Offers"] <-
  "Effective CompetitionClassification"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "No CompetitionClassification; Overrode blank Fair Opportunity)"] <-
  "No CompetitionClassification"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "One Offer"] <-
  "CompetitionClassification with single offer"
engine_contracts$CompetitionClassification[engine_contracts$CompetitionClassification == "Effective CompetitionClassification"] <-
  "Effective CompetitionClassification"

# --------------------------------------------------------------------------------
# reclassify contract type

engine_contracts$Pricing.Mechanism <-
  as.character(engine_contracts$Pricing.Mechanism)

engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Combination (two or more)"] <-
  "Combination"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Cost No Fee"] <-
  "Cost Reimbursement"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Cost Plus Award Fee"] <-
  "Cost Reimbursement"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Cost Plus Fixed Fee"] <-
  "Cost Reimbursement"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Cost Plus Incentive"] <-
  "Cost Reimbursement"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Cost Sharing"] <-
  "Cost Reimbursement"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Firm Fixed Price"] <-
  "Fixed Price"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Fixed Price Award Fee"] <-
  "Fixed Price"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Fixed Price Incentive"] <-
  "Fixed Price"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Fixed Price Level of Effort"] <-
  "Fixed Price"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Fixed Price Redetermination"] <-
  "Fixed Price"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Fixed Price with Economic Price Adjustment"] <-
  "Fixed Price"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Labor Hours"] <-
  "Time and Materials"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Not Reported"] <-
  "Unlabeled"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Order Dependent (IDV only)"] <-
  "Other"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Other (none of the above)"] <-
  "Other"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "Time and Materials"] <-
  "Time and Materials"
engine_contracts$Pricing.Mechanism[engine_contracts$Pricing.Mechanism == "NULL"] <-
  "Unlabeled"

# --------------------------------------------------------------------------------
# write a new dataset for powerbi

write.csv(engine_contracts, "app/power_bi.csv")

# ================================================================================
# charting engines 
# --------------------------------------------------------------------------------
# engine contracts 

(
  total <- engine_contracts %>%
    group_by(Fiscal.Year) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = Fiscal.Year), alpha = .9 , stat = "identity") +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    scale_y_continuous(labels = money_labels) +
    chart_theme +
    ggtitle("DoD aircraft engine contract obligations") +
    xlab("fiscal year") +
    ylab("constant fy19 dollars")
)

ggsave(
  "charts/amount_total.svg",
  total,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# --------------------------------------------------------------------------------
# engine contracts by vendor size 

(
  Vendor.Size <- engine_contracts %>%
    filter(Vendor.Size != "Unlabeled") %>%
    group_by(Fiscal.Year, Vendor.Size) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = Fiscal.Year), alpha = .9, stat = "identity") +
    facet_wrap(~ Vendor.Size, nrow = 2) +
    chart_theme +
    ggtitle("DoD aircraft engine contract obligations by vendor size") +
    xlab("fiscal year") +
    ylab("constant fy19 dollars") +
    scale_y_continuous(labels = money_labels) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "charts/amount_vendor_size.svg",
  Vendor.Size,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# --------------------------------------------------------------------------------
# engine contracts by CompetitionClassification  

(
  CompetitionClassification <- engine_contracts %>%
    filter(CompetitionClassification != "Unlabeled") %>%
    group_by(Fiscal.Year, CompetitionClassification) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    mutate(CompetitionClassification = factor(
      CompetitionClassification,
      levels = c(
        "Effective CompetitionClassification",
        "CompetitionClassification with single offer",
        "No CompetitionClassification"
      )
    )) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    geom_area(aes(y = amount, x = Fiscal.Year), alpha = .9, stat = "identity") +
    facet_wrap(~ CompetitionClassification, nrow = 1) +
    chart_theme +
    ggtitle("DoD aircraft engine contract obligations by CompetitionClassification") +
    xlab("fiscal year") +
    ylab("constant fy19 dollars") +
    scale_y_continuous(labels = money_labels) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "charts/amount_competition.svg",
  CompetitionClassification,
  device = "svg",
  width = 8,
  height = 4,
  units = "in"
)

# --------------------------------------------------------------------------------
# engine contracts by contract type 

(
  Pricing.Mechanism <- engine_contracts %>%
    filter(Pricing.Mechanism != "Other") %>%
    group_by(Fiscal.Year, Pricing.Mechanism) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    mutate(Pricing.Mechanism = factor(
      Pricing.Mechanism,
      levels = c(
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
    geom_area(aes(y = amount, x = Fiscal.Year), alpha = .9, stat = "identity") +
    facet_wrap(~ Pricing.Mechanism, nrow = 1) +
    chart_theme +
    ggtitle("DoD aircraft engine contract obligations by contract type") +
    xlab("fiscal year") +
    ylab("constant fy19 dollars") +
    scale_y_continuous(labels = money_labels) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "charts/amount_contract_type.svg",
  Pricing.Mechanism,
  device = "svg",
  width = 12,
  height = 4,
  units = "in"
)

# --------------------------------------------------------------------------------
# super facet: engine contracts by service and SimpleArea

total <- engine_contracts %>%
  group_by(Fiscal.Year) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  as.data.frame(.)

total_category <- engine_contracts %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)

total <- total %>%
  rbind(total_category) %>%
  mutate(Customer = "Total")

army <- engine_contracts %>%
  filter(Customer == "Army") %>%
  group_by(Fiscal.Year) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  as.data.frame(.)

army_category <- engine_contracts %>%
  filter(Customer == "Army") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)

army <- army %>%
  rbind(army_category) %>%
  mutate(Customer = "Army")

navy <- engine_contracts %>%
  group_by(Fiscal.Year) %>%
  filter(Customer == "Navy") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

navy_category <- engine_contracts %>%
  filter(Customer == "Navy") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)

navy <- navy %>%
  rbind(navy_category) %>%
  mutate(Customer = "Navy")

air_force <- engine_contracts %>%
  group_by(Fiscal.Year) %>%
  filter(Customer == "Air Force") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

air_force_category <- engine_contracts %>%
  filter(Customer == "Air Force") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)

air_force <- air_force %>%
  rbind(air_force_category) %>%
  mutate(Customer = "Air Force")

dla <- engine_contracts %>%
  group_by(Fiscal.Year) %>%
  filter(Customer == "DLA") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

dla_category <- engine_contracts %>%
  filter(Customer == "DLA") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)

dla <- dla %>%
  rbind(dla_category) %>%
  mutate(Customer = "DLA")

(
  super_facet <- total %>%
    rbind(army, navy, air_force, dla) %>%
    mutate(
      Customer = factor(Customer, levels = c("Army",
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
    geom_area(aes(x = Fiscal.Year, y = amount), alpha = .9) +
    facet_grid(Customer ~ SimpleArea) +
    chart_theme +
    xlab("fiscal year") +
    ylab("constant fy19 dollars") +
    ggtitle("DoD Aircraft Engine Contract Obligations by service and SimpleArea") +
    scale_y_continuous(labels = money_labels) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "charts/amount_customer_category.svg",
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

engine_contracts <- engine_contracts %>%
  select(Fiscal.Year, Customer, SimpleArea, amount, type)

comparison_contracts_og <- engine_contracts %>%
  rbind(topline_contracts) %>%
  group_by(Fiscal.Year, type) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- comparison_contracts_og %>%
  dplyr::rename(fyb = Fiscal.Year) %>%
  mutate(Fiscal.Year = fyb + 1) %>%
  select(-fyb)

(
  comparison_contracts <- comparison_contracts_og %>%
    left_join(dyear, by = c("Fiscal.Year", "type")) %>%
    filter(Fiscal.Year >= 2001) %>%
    select(Fiscal.Year, amount.x, amount.y, type) %>%
    mutate(amount_change = amount.x - amount.y) %>%
    mutate(amount_percent_change = (amount_change) / amount.y * 100) %>%
    dplyr::rename(amount = amount.x) %>%
    select(Fiscal.Year, amount, amount_change, amount_percent_change, type) %>%
    group_by(Fiscal.Year, type) %>%
    dplyr::summarise(
      amount = sum(amount, na.rm = TRUE),
      amount_change = sum(amount_change, na.rm = TRUE),
      amount_percent_change = sum(amount_percent_change, na.rm = TRUE)
    )
)

# --------------------------------------------------------------------------------
# percent change comparison by Customer

base_year <- engine_contracts %>%
  as.tibble(.) %>%
  group_by(Fiscal.Year, type, Customer) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(Fiscal.Year == 2000) %>%
  select(amount, type, Customer)

base_year.eng <- base_year %>%
  left_join(dyear, by = c("type", "Customer")) %>%
  filter(Customer != "Other DoD") %>%
  select(-Fiscal.Year.y) %>%
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>%
  mutate(base = 0) %>%
  mutate(amount = base + amount_percent_change) %>%
  dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
  group_by(Fiscal.Year, Customer) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(data_type = "engine") %>%
  mutate(amount = amount / 100)

base_year <- topline_contracts %>%
  group_by(Fiscal.Year, type, Customer) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(Fiscal.Year == 2000) %>%
  select(amount, type, Customer)

base_year.top <- base_year %>%
  left_join(dyear, by = c("type", "Customer")) %>%
  filter(Customer != "Other DoD") %>%
  select(-Fiscal.Year.y) %>%
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>%
  mutate(base = 0) %>%
  mutate(amount = base + amount_percent_change) %>%
  dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
  group_by(Fiscal.Year, Customer) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(data_type = "topline") %>%
  mutate(amount = amount / 100)

eng.topline <- rbind(base_year.eng, base_year.top)

(
  eng.topline1 <- eng.topline %>%
    filter(Customer != "NULL" & Customer != "MDA") %>%
    mutate(Customer = factor(
      Customer, levels = c("Total",
                           "Army",
                           "Navy",
                           "Air Force",
                           "DLA")
    )) %>%
    mutate(data_type = factor(data_type, levels = c("topline", "engine"))) %>%
    ggplot() +
    geom_line(
      aes(x = Fiscal.Year, y = amount),
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
    
    facet_grid(data_type ~ Customer) +
    chart_theme +
    #scale_x_continuous(breaks = seq(2000, 2020, by = 2),
    #labels = function(x) {substring(as.character(x), 3, 4)})+
    #scale_y_continuous(limits = c(0:750)) +
    ggtitle("Change in aircraft engine contract obligations") +
    xlab("fiscal year") +
    ylab(NULL) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "charts/percent_change_customer.svg",
  eng.topline1,
  width = 9,
  height = 6,
  units = "in",
  device = "svg"
)

# --------------------------------------------------------------------------------
# percent change comparison by SimpleArea 

base_year <- engine_contracts %>%
  as.tibble(.) %>%
  group_by(Fiscal.Year, type, SimpleArea) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(Fiscal.Year == 2000) %>%
  select(amount, type, SimpleArea)

base_year.eng <- base_year %>%
  left_join(dyear, by = c("type", "SimpleArea")) %>%
  filter(SimpleArea != "Services") %>%
  select(-Fiscal.Year.y) %>%
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>%
  mutate(base = 0) %>%
  mutate(amount = base + amount_percent_change) %>%
  dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(data_type = "engine") %>%
  mutate(amount = amount / 100)

base_year <- topline_contracts %>%
  group_by(Fiscal.Year, type, SimpleArea) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(Fiscal.Year == 2000) %>%
  select(amount, type, SimpleArea)

base_year.top <- base_year %>%
  left_join(dyear, by = c("type", "SimpleArea")) %>%
  filter(SimpleArea != "Services") %>%
  select(-Fiscal.Year.y) %>%
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>%
  mutate(base = 0) %>%
  mutate(amount = base + amount_percent_change) %>%
  dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(data_type = "topline") %>%
  mutate(amount = amount / 100)

eng.topline <- rbind(base_year.eng, base_year.top)

(
  eng.topline1 <- eng.topline %>%
    filter(SimpleArea != "NULL" & SimpleArea != "MDA") %>%
    mutate(SimpleArea = factor(SimpleArea, levels = c("Products",
                                                  # "Services",
                                                  "R&D"))) %>%
    mutate(data_type = factor(data_type, levels = c("topline", "engine"))) %>%
    ggplot() +
    geom_line(
      aes(x = Fiscal.Year, y = amount),
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
    facet_grid(data_type ~ SimpleArea) +
    chart_theme +
    ggtitle("Change in aircraft engine contract obligations") +
    xlab("fiscal year") +
    ylab(NULL) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "charts/percent_change_category.svg",
  eng.topline1,
  width = 9,
  height = 6,
  units = "in",
  device = "svg"
)

# --------------------------------------------------------------------------------
# percent change comparison 

base_year <- engine_contracts %>%
  as.tibble(.) %>%
  group_by(Fiscal.Year, type) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(Fiscal.Year == 2000) %>%
  select(amount, type)

base_year.eng <- base_year %>%
  left_join(dyear, by = "type") %>%
  select(-Fiscal.Year.y) %>%
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>%
  mutate(base = 0) %>%
  mutate(amount = base + amount_percent_change) %>%
  dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
  group_by(Fiscal.Year) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(data_type = "engine") %>%
  mutate(amount = amount / 100)

base_year <- topline_contracts %>%
  group_by(Fiscal.Year, type) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(Fiscal.Year == 2000) %>%
  select(amount, type)

(
  base_year.top <- base_year %>%
    left_join(dyear, by = "type") %>%
    select(-Fiscal.Year.y) %>%
    mutate(amount_change = amount.x - amount.y) %>%
    mutate(amount_percent_change = (amount_change) / amount.y * 100) %>%
    mutate(base = 0) %>%
    mutate(amount = base + amount_percent_change) %>%
    dplyr::rename(Fiscal.Year = Fiscal.Year.x) %>%
    group_by(Fiscal.Year) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    mutate(data_type = "topline") %>%
    mutate(amount = amount / 100)
)

eng.topline <- rbind(base_year.eng, base_year.top)

(
  eng.topline1 <- eng.topline %>%
    mutate(data_type = factor(data_type, levels = c("topline", "engine"))) %>%
    ggplot() +
    geom_line(
      aes(x = Fiscal.Year, y = amount),
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
    facet_wrap(~ data_type) +
    chart_theme +
    ggtitle("Change in aircraft engine contract obligations") +
    xlab("fiscal year") +
    ylab(NULL) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "charts/percent_change_total.svg",
  eng.topline1,
  width = 9,
  height = 6,
  units = "in",
  device = "svg"
)

