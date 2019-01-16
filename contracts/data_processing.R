# ================================================================================
# The Future of Military Engines 
# By Gabriel Coll
# --------------------------------------------------------------------------------
# engine-related contract numbers from the Federal Procurement Data System 
# ================================================================================

# cleaning and transformation ====================================================
# load packages ------------------------------------------------------------------

library(tidyverse)
library(Cairo)
library(ggthemes)
library(extrafont)
library(extrafontdb)
library(svglite)
library(scales)

# theme --------------------------------------------------------------------------

source("chart_theme.R")

# money labels--------------------------------------------------------------------

source("money_labels.R")

# read data ----------------------------------------------------------------------

read_engine_contracts <-
  read.csv(
    "Project_SP_EngineAllVendorHistorycompetitionFundingMechanismVendorSizeProdServAreaSubcustomer - Copy.csv"
  )
read_topline_contracts <- read.csv("FPDS_data.csv")

# summarise contract data --------------------------------------------------------

deflator <- c(0.7097926468, 0.7267816693, 0.7385433002,0.7526572574, 0.7713016205, 
              0.7955218679, 0.821397456, 0.8437009932, 0.8612127548, 0.8712319219,  
              0.8788987629,  0.8967590173,  0.9131381774, 0.9285589824,  0.9454608817,  
              0.9568740199,  0.9679386653, 0.984579195, 1,  1.016727653, 1.035546262)
fy <- c(2000:2020)
deflate.year <- as.data.frame(cbind(fy, deflator))

engine_contracts <- read_engine_contracts %>%
  dplyr::rename(
    fy = "ï..fiscal_year",
    amount = SumOfobligatedAmount,
    category = Simple,
    platform_portfolio = PlatformPortfolio,
    customer_2 = Customer,
    customer = SubCustomer,
    competition = ClassifyNumberOfOffers,
    contract_type = typeofcontractpricingtext,
    vendor_size = VendorSize,
    parent = ParentID, 
    project = ProjectName
  ) %>%
  # mutate(amount_millions = amount/1000000) %>% 
    left_join(deflate.year, by = "fy") %>% 
  # mutate(amount_millions = amount/1000000) %>%
  mutate(amount_18 = amount * deflator) %>% 
  group_by(fy, customer, category, parent, project, competition, contract_type, vendor_size) %>%
  dplyr::summarise(amount = sum(amount_18, na.rm = TRUE))

topline_contracts <- read_topline_contracts %>%
  dplyr::rename(
    fy = FY,
    amount = Amount,
    category = Category,
    platform_portfolio = Portfolio,
    customer = Customer,
    vendor_size = VendorSize
  ) %>%
  # mutate(amount_millions = amount/1000000) %>%
  left_join(deflate.year, by = "fy") %>% 
  mutate(amount_18 = amount * deflator) %>% 
  group_by(fy, customer, category) %>%
  dplyr::summarise(amount = sum(amount_18, na.rm = TRUE))

# reclassify vendor size ---------------------------------------------------------

engine_contracts$vendor_size <-
  as.character(engine_contracts$vendor_size)
engine_contracts$vendor_size[engine_contracts$vendor_size == "Medium <1B"] <-
  "Medium"
engine_contracts$vendor_size[engine_contracts$vendor_size == "Medium >1B"] <-
  "Medium"
engine_contracts$vendor_size[engine_contracts$vendor_size == "Small"] <-
  "Small"
engine_contracts$vendor_size[engine_contracts$vendor_size == "Large: Big 6"] <-
  "Big Five"
engine_contracts$vendor_size[engine_contracts$vendor_size == "Large: Big 6 JV"] <-
  "Big Five"

# reclassify competition ---------------------------------------------------------

engine_contracts$competition <- as.character(engine_contracts$competition)

engine_contracts$competition[engine_contracts$competition == ""] <-
  "Unlabeled"
engine_contracts$competition[engine_contracts$competition == "NULL"] <-
  "Unlabeled"
engine_contracts$competition[engine_contracts$competition == "Unlabeled: Blank Extent Competed"] <-
  "Unlabeled"
engine_contracts$competition[engine_contracts$competition == "Unlabeled: Blank Fair Opportunity"] <-
  "Unlabeled"
engine_contracts$competition[engine_contracts$competition == "Unlabeled: Competition; Zero Offers"] <-
  "Unlabeled"
engine_contracts$competition[engine_contracts$competition == "Unlabeled: No competition; multiple offers"] <-
  "Unlabeled"
engine_contracts$competition[engine_contracts$competition == "Unlabeled: No competition; multiple offers; Overrode blank Fair Opportunity)"] <-
  "Unlabeled"
engine_contracts$competition[engine_contracts$competition == "10-24 Offers"] <-
  "Effective competition"
engine_contracts$competition[engine_contracts$competition == "100+ Offers"] <-
  "Effective competition"
engine_contracts$competition[engine_contracts$competition == "25-99 Offers"] <-
  "Effective competition"
engine_contracts$competition[engine_contracts$competition == "3-4 Offers"] <-
  "Effective competition"
engine_contracts$competition[engine_contracts$competition == "5-9 Offers"] <-
  "Effective competition"
engine_contracts$competition[engine_contracts$competition == "Two Offers"] <-
  "Effective competition"
engine_contracts$competition[engine_contracts$competition == "No competition; Overrode blank Fair Opportunity)"] <-
  "No competition"
engine_contracts$competition[engine_contracts$competition == "One Offer"] <-
  "Competition with single offer"
engine_contracts$competition[engine_contracts$competition == "Effective Competition"] <-
  "Effective competition"

# reclassify contract type--------------------------------------------------------

engine_contracts$contract_type <-
  as.character(engine_contracts$contract_type)

engine_contracts$contract_type[engine_contracts$contract_type == "Combination (two or more)"] <-
  "Combination"
engine_contracts$contract_type[engine_contracts$contract_type == "Cost No Fee"] <-
  "Cost Reimbursement"
engine_contracts$contract_type[engine_contracts$contract_type == "Cost Plus Award Fee"] <-
  "Cost Reimbursement"
engine_contracts$contract_type[engine_contracts$contract_type == "Cost Plus Fixed Fee"] <-
  "Cost Reimbursement"
engine_contracts$contract_type[engine_contracts$contract_type == "Cost Plus Incentive"] <-
  "Cost Reimbursement"
engine_contracts$contract_type[engine_contracts$contract_type == "Cost Sharing"] <-
  "Cost Reimbursement"
engine_contracts$contract_type[engine_contracts$contract_type == "Firm Fixed Price"] <-
  "Fixed Price"
engine_contracts$contract_type[engine_contracts$contract_type == "Fixed Price Award Fee"] <-
  "Fixed Price"
engine_contracts$contract_type[engine_contracts$contract_type == "Fixed Price Incentive"] <-
  "Fixed Price"
engine_contracts$contract_type[engine_contracts$contract_type == "Fixed Price Level of Effort"] <-
  "Fixed Price"
engine_contracts$contract_type[engine_contracts$contract_type == "Fixed Price Redetermination"] <-
  "Fixed Price"
engine_contracts$contract_type[engine_contracts$contract_type == "Fixed Price with Economic Price Adjustment"] <-
  "Fixed Price"
engine_contracts$contract_type[engine_contracts$contract_type == "Labor Hours"] <-
  "Time and Materials"
engine_contracts$contract_type[engine_contracts$contract_type == "Not Reported"] <-
  "Unlabeled"
engine_contracts$contract_type[engine_contracts$contract_type == "Order Dependent (IDV only)"] <-
  "Other"
engine_contracts$contract_type[engine_contracts$contract_type == "Other (none of the above)"] <-
  "Other"
engine_contracts$contract_type[engine_contracts$contract_type == "Time and Materials"] <-
  "Time and Materials"
engine_contracts$contract_type[engine_contracts$contract_type == "NULL"] <-
  "Unlabeled"

# fonts --------------------------------------------------------------------------
# m notes-------------------------------------------------------------------------

# fonts()

# I managed to make this work, but you need to make sure that you download the font
# Open Sans, then import it into R and load it into the usable font table (you can 
# check if it is added by checking the file that results from the function fonttable())
# After downloading Open Sans, run the following:
# font_import()
# y
# loadfonts()
# extrafont:::fonttable()

# then you can use the below ggsave function to save any ggplot 
# into pdf.

# charting (part 1) ==============================================================
# total --------------------------------------------------------------------------

(
  total <- engine_contracts %>%
    group_by(fy) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), alpha = .9 , stat = "identity") +
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
    ylab("constant fy18 dollars")
)

ggsave("charts/total_engine_related.svg", total,
       device = "svg", width = 8, height = 6, units = "in")

(
  topline <- topline_contracts %>%
    group_by(fy) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    chart_theme + 
    ggtitle("DoD contract obligations") +
    xlab("fiscal year")
)

# ggsave("charts/DoD contract obligations.svg", total, 
#        device = "svg", width = 8, height = 6, units = "in")

# army ---------------------------------------------------------------------------

(
  army <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "Army") %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    chart_theme + 
    ggtitle("Army contract obligations related to aircraft engines") +
    xlab("fiscal year")
)

# ggsave("charts/Army contract obligations related to aircraft engines.svg", army, 
#        device = "svg", width = 8, height = 6, units = "in")

# navy ---------------------------------------------------------------------------

(
  navy <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "Navy") %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    chart_theme + 
    ggtitle("Navy contract obligations related to aircraft engines") +
    xlab("fiscal year")
)

(
  navy <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "Navy") %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE))
)

# ggsave("charts/Navy contract obligations related to aircraft engines.svg", navy, 
#        device = "svg", width = 8, height = 6, units = "in")

# air force ----------------------------------------------------------------------

(
  air_force <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "Air Force") %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    chart_theme + 
    ggtitle("Air Force contract obligations related to aircraft engines") +
    xlab("fiscal year")
)

# ggsave("charts/Air Force contract obligations related to aircraft engines.svg", air_force, 
#        device = "svg", width = 8, height = 6, units = "in")

(
  air_force <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "Air Force") %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE))
)

# service facet ------------------------------------------------------------------

(
  service <- engine_contracts %>%
    group_by(fy, customer) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    filter(customer == "Army" |
             customer == "Navy" | customer == "Air Force") %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap( ~ customer, nrow = 1) +
    chart_theme + 
    ggtitle("DoD contract obligations related to aircraft engines by service") +
    xlab("fiscal year")
)

# ggsave("charts/DoD contract obligations related to aircraft engines by service.svg", service, 
#        device = "svg", width = 8, height = 6, units = "in")

# category facet -----------------------------------------------------------------

(
  category <- engine_contracts %>%
    group_by(fy, category) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap( ~ category, nrow = 1) +
    chart_theme + 
    ggtitle("DoD contract obligations related to aircraft engines by category") +
    xlab("fiscal year")
)

# ggsave("charts/DoD contract obligations related to aircraft engines by category.svg", category, 
#        device = "svg", width = 8, height = 6, units = "in")

# vendor size facet --------------------------------------------------------------

(
  vendor_size <- engine_contracts %>%
    filter(vendor_size != "Unlabeled") %>% 
    group_by(fy, vendor_size) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), alpha = .9, stat = "identity") +
    facet_wrap( ~ vendor_size, nrow = 2) +
    chart_theme + 
    ggtitle("DoD aircraft engine contract obligations by vendor size") +
    xlab("fiscal year") + 
    ylab("constant fy18 dollars") +
    scale_y_continuous(labels = money_labels) + 
    scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                       labels = function(x) {substring(as.character(x), 3, 4)})
)

ggsave("charts/vendor_size.svg", vendor_size, 
       device = "svg", width = 8, height = 6, units = "in")

# competition facet --------------------------------------------------------------

(
  competition <- engine_contracts %>%
    filter(competition != "Unlabeled") %>% 
    group_by(fy, competition) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    mutate(competition = factor(competition, levels = c("Effective competition",
                                                        "Competition with single offer",
                                                        "No competition"))) %>% 
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(y = amount, x = fy), alpha = .9, stat = "identity") +
    facet_wrap( ~ competition, nrow = 1) +
    chart_theme + 
    ggtitle("DoD aircraft engine contract obligations by competition") +
    xlab("fiscal year") +
    ylab("constant fy18 dollars") +
    scale_y_continuous(labels = money_labels) + 
    scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                       labels = function(x) {substring(as.character(x), 3, 4)})
)

ggsave("charts/competition.svg", competition, 
       device = "svg", width = 8, height = 4, units = "in")

# contract type facet ------------------------------------------------------------

(
  contract_type <- engine_contracts %>%
    filter(contract_type != "Other") %>% 
    group_by(fy, contract_type) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    mutate(contract_type = factor(contract_type, levels = c("Cost Reimbursement",
                                                        "Combination",
                                                        "Time and Materials",
                                                        "Fixed Price",
                                                        "Unlabeled"))) %>% 
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(y = amount, x = fy), alpha = .9, stat = "identity") +
    facet_wrap( ~ contract_type, nrow = 1) +
    chart_theme + 
    ggtitle("DoD aircraft engine contract obligations by contract type") +
    xlab("fiscal year") +
    ylab("constant fy18 dollars") +
    scale_y_continuous(labels = money_labels) + 
    scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                       labels = function(x) {substring(as.character(x), 3, 4)})
)

ggsave("charts/contract_type.svg", contract_type, 
       device = "svg", width = 12, height = 4, units = "in")

# competition facet by service ---------------------------------------------------

(
  competition <- engine_contracts %>%
    filter(competition != "Unlabeled") %>% 
    group_by(fy, competition, customer) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_grid(customer ~ competition) +
    chart_theme + 
    ggtitle("DoD aircraft engine contract obligations by competition") +
    xlab("fiscal year") +
    ylab("constant fy18 dollars") +
    scale_y_continuous(labels = money_labels) + 
    scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                       labels = function(x) {substring(as.character(x), 3, 4)})
)

# ggsave("charts/DoD contract obligations by competition and customer.svg", competition, 
#        device = "svg", width = 8, height = 6, units = "in")

# army / category facet ----------------------------------------------------------

(
  army_category <- engine_contracts %>%
    filter(customer == "Army") %>%
    group_by(fy, customer, category) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap( ~ category, nrow = 1) +
    chart_theme + 
    ggtitle("Army contract obligations related to aircraft engines by category") +
    xlab("fiscal year")
)

# ggsave("charts/Army contract obligations related to aircraft engines by category.svg", army_category, 
#        device = "svg", width = 8, height = 6, units = "in")

# navy / category facet ----------------------------------------------------------

(
  navy_category <- engine_contracts %>%
    filter(customer == "Navy") %>%
    group_by(fy, customer, category) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap( ~ category, nrow = 1) +
    chart_theme + 
    ggtitle("Navy contract obligations related to aircraft engines by category") +
    xlab("fiscal year")
)

# ggsave("charts/Navy contract obligations related to aircraft engines by category.svg", navy_category, 
#        device = "svg", width = 8, height = 6, units = "in")

# air force / category facet -----------------------------------------------------

(
  air_force_category <- engine_contracts %>%
    filter(customer == "Air Force") %>%
    group_by(fy, customer, category) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap(~ category, nrow = 1) +
    chart_theme +
    ggtitle(
      "Air Force contract obligations related to aircraft engines by category"
    ) +
    xlab("fiscal year")
)

# ggsave("charts/Air Force contract obligations related to aircraft engines by category.svg", air_force_category,
#        device = "svg", width = 8, height = 6, units = "in")

# super facet --------------------------------------------------------------------

total <- engine_contracts %>%
  group_by(fy) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(category = "Total") %>%
  as.data.frame(.)

total_category <- engine_contracts %>%
  group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)

total <- total %>%
  rbind(total_category) %>%
  mutate(customer = "Total")


army <- engine_contracts %>%
  filter(customer == "Army") %>%
  group_by(fy) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(category = "Total") %>%
  as.data.frame(.)

army_category <- engine_contracts %>%
  filter(customer == "Army") %>%
  group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)
# # select(fy, amount, customer, category) %>%

army <- army %>%
  rbind(army_category) %>%
  mutate(customer = "Army")

navy <- engine_contracts %>%
  group_by(fy) %>%
  filter(customer == "Navy") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  # mutate(customer = "Navy") %>%
  mutate(category = "Total") %>%
  # mutate(customer = as.factor(customer)) %>%
  mutate(category = as.factor(category)) %>%
  as.data.frame(.)

navy_category <- engine_contracts %>%
  filter(customer == "Navy") %>%
  group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)
# # select(fy, amount, customer, category) %>%

navy <- navy %>%
  rbind(navy_category) %>%
  mutate(customer = "Navy")

air_force <- engine_contracts %>%
  group_by(fy) %>%
  filter(customer == "Air Force") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  # mutate(customer = "Air Force") %>%
  mutate(category = "Total") %>%
  # mutate(customer = as.factor(customer)) %>%
  mutate(category = as.factor(category)) %>%
  as.data.frame(.)

air_force_category <- engine_contracts %>%
  filter(customer == "Air Force") %>%
  group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)
# # select(fy, amount, customer, category) %>%

air_force <- air_force %>%
  rbind(air_force_category) %>%
  mutate(customer = "Air Force")

dla <- engine_contracts %>%
  group_by(fy) %>%
  filter(customer == "DLA") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  # mutate(customer = "DLA") %>%
  mutate(category = "Total") %>%
  # mutate(customer = as.factor(customer)) %>%
  mutate(category = as.factor(category)) %>%
  as.data.frame(.)

dla_category <- engine_contracts %>%
  filter(customer == "DLA") %>%
  group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)
# # select(fy, amount, customer, category) %>%

dla <- dla %>%
  rbind(dla_category) %>%
  mutate(customer = "DLA")

(super_facet <- total %>%
    rbind(army, navy, air_force, dla) %>%
    mutate(customer = factor(customer, levels = c("Army",
                                                  "Navy",
                                                  "Air Force",
                                                  "DLA", 
                                                  "Total")),
           category = factor(category, levels = c("Products",
                                                  "Services",
                                                  "R&D", 
                                                  "Total"))) %>%
    
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) + 
    geom_area(aes(x = fy, y = amount), alpha = .9) +
    facet_grid(category ~ customer) +
    chart_theme + 
  xlab("fiscal year") + 
  ylab("constant fy18 dollars") +
  ggtitle("DoD Aircraft Engine Contract Obligations") +
  scale_y_continuous(labels = money_labels) + 
  scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                     labels = function(x) {substring(as.character(x), 3, 4)}))

ggsave("charts/super_facet.svg", super_facet,
       device = "svg", width = 12, height = 10, units = "in")

# parent (top 12) ----------------------------------------------------------------

parent_rank <- engine_contracts %>% 
  group_by(parent) %>%
  summarise(amount = sum(amount, na.rm = TRUE)) %>% 
  arrange(desc(amount))

parent_rank_12 <- parent_rank[c(1:12), ]
parent_rank_other <- parent_rank[-c(1:12), -1]

parent_rank_other <- parent_rank_other %>% 
  summarise(amount = sum(amount, na.rm = TRUE)) %>% 
  mutate(parent = "NULL") %>% 
  select(parent, amount)

parent_rank <- parent_rank_12 %>% 
  rbind(parent_rank_other) %>% 
  group_by(parent) %>% 
  summarise(amount = sum(amount, na.rm = TRUE)) %>% 
  select(parent) %>% 
  left_join(engine_contracts)

parent_rank$parent <- as.character(parent_rank$parent)

parent_rank$parent[parent_rank$parent == "GE ROLLS-ROYCE FIGHTER ENGINE TEAM [GE/ROLLS ROYCE]"] <-
  "GE/ROLLS ROYCE JV"

parent_rank$parent[parent_rank$parent == "CFM INTERNATIONAL"] <-
  "GE/SAFRAN JV"

parent_rank$parent[parent_rank$parent == "KELLY AVIATION CENTER [Joint Venture Lockheed Martin/Rolls Royce]"] <-
  "LOCKHEED/ROLLS ROYCE JV"

parent_rank$parent[parent_rank$parent == "NULL"] <-
  "OTHER"

parent_rank$parent[parent_rank$parent == "UNITED TECH"] <-
  "UNITED TECHNOLOGIES"

(
  parent <- parent_rank %>%
    group_by(fy, parent) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    mutate(parent = factor(parent, levels = c(
      "UNITED TECHNOLOGIES",
      "GENERAL ELECTRIC",
      "ROLLS ROYCE",
      "HONEYWELL",
      "TEXTRON",
      "GE/ROLLS ROYCE JV",
      "LOCKHEED/ROLLS ROYCE JV",
      "GE/SAFRAN JV",
      "BOEING",
      "L3 COMMUNICATIONS",
      "NORTHROP GRUMMAN", 
      "OTHER"))) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), alpha = .9, stat = "identity") +
    facet_wrap(~ parent) +
    chart_theme + 
    ggtitle("Top aircraft engine contractors") +
    xlab("fiscal year") + 
    ylab("constant fy18 dollars") +
    scale_y_continuous(labels = money_labels) + 
    scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                       labels = function(x) {substring(as.character(x), 3, 4)})
)

ggsave("charts/top_12.svg", parent, 
       device = "svg", width = 12, height = 10, units = "in")

# GE vs Pratt --------------------------------------------------------------------

(
  parent <- engine_contracts %>%
    filter(parent %in% c("UNITED TECH",
                         "GENERAL ELECTRIC")) %>%
    group_by(fy, parent, customer) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_grid(customer ~ parent) +
    chart_theme + 
    ggtitle("DoD contract obligations - GE vs Pratt") +
    xlab("fiscal year")
)

# ggsave("charts/DoD aircraft engine contract obligations - GE vs Pratt.svg", parent, 
#        device = "svg", width = 8, height = 6, units = "in")

# project (top 17) ---------------------------------------------------------------

(project <- engine_contracts %>% 
   filter(project %in% c("NULL",
                         "JSF (F-35) ",
                         "F/A-22 ",
                         "F-100 Engine",
                         "H-1 UPGRADE ",
                         "V22 ",
                         "F/A-18 E/F ",
                         "BJN",
                         "C-17A ",
                         "C-17A CARGO TRANSPORT",
                         "F-110",
                         "F414-GE-400",
                         "C130-J ",
                         "BLACK HAWK (UH-60A/L) ",
                         "UH-60 BLACKHAWK UTTAS",
                         "BSJ",
                         "F-18 HORNET"
   )) %>% 
   group_by(fy, project) %>% 
   dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
   ggplot() +
   geom_area(aes(y = amount, x = fy), stat = "identity") +
   facet_wrap(~ project) +
   chart_theme + 
   ggtitle("DoD aircraft engine contract obligations - top 17 engine projects") +
   xlab("fiscal year")
)

# ggsave("charts/DoD aircraft engine contract obligations - top 17 engine projects.svg", project, 
#        device = "svg", width = 8, height = 6, units = "in")

# --------------------------------------------------------------------------------
# jsf 
# --------------------------------------------------------------------------------

(project <- engine_contracts %>% 
   filter(project %in% c(
     "JSF (F-35) "
   )) %>% 
   group_by(fy, project) %>% 
   dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
   ggplot() +
   geom_area(aes(y = amount, x = fy), stat = "identity") +
   facet_wrap(~ project) +
   chart_theme + 
   ggtitle("DoD aircraft engine contract obligations - JSF") +
   xlab("fiscal year")
)

# ggsave("charts/DoD aircraft engine contract obligations - JSF.svg", project, 
#        device = "svg", width = 8, height = 6, units = "in")

# comparison to the topline ======================================================
# join engines and topline -------------------------------------------------------

engine_contracts <- engine_contracts %>%
  mutate(type = "Engines") 

topline_contracts <- topline_contracts %>%
  mutate(type = "Topline")

engine_contracts <- engine_contracts %>% 
  select(fy, customer, category, amount, type)

comparison_contracts_og <- engine_contracts %>%
  rbind(topline_contracts) %>%
  group_by(fy, type) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- comparison_contracts_og %>%
  dplyr::rename(fyb = fy) %>%
  mutate(fy = fyb + 1) %>%
  select(-fyb)

(comparison_contracts <- comparison_contracts_og %>%
  left_join(dyear, by = c("fy", "type")) %>%
  filter(fy >= 2001) %>%
  select(fy, amount.x, amount.y, type) %>%
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>%
  dplyr::rename(amount = amount.x) %>%
  select(fy, amount, amount_change, amount_percent_change, type) %>%
  group_by(fy, type) %>%
  dplyr::summarise(
    amount = sum(amount, na.rm = TRUE),
    amount_change = sum(amount_change, na.rm = TRUE),
    amount_percent_change = sum(amount_percent_change, na.rm = TRUE)
  ))

# difference in amount -----------------------------------------------------------

(diff.amount <- ggplot(data = comparison_contracts) +
  geom_line(aes(
    x = fy,
    y = amount,
    group = type,
    color = type
  )) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  xlab("fiscal year") +   
  ggtitle("difference in amount") +
  theme(plot.title = element_text(hjust = 0.5)))

# M: I added x-axis label, I named it so I could refer to it, and 
# I centered the title

# ggsave("charts/difference in amount.svg", diff.amount, 
#        device = "svg", width = 8, height = 6, units = "in")

# difference in year-to-year change ----------------------------------------------

diff.year <- ggplot(data = comparison_contracts) +
  geom_line(aes(
    x = fy,
    y = amount_change,
    group = type,
    color = type
  )) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  xlab("Fiscal Year") +
  ylab("Amount") +
  ggtitle("difference in year-to-year change")+
  theme(plot.title = element_text(hjust = 0.5))

# M: I added x-axis label, I named it so I could refer to it, and 
# I centered the title

# ggsave("charts/difference in year-to-year change.svg", diff.year, 
#        device = "svg", width = 8, height = 6, units = "in")


# --------------------------------------------------------------------------------
# difference in year-to-year % change 

# filter(fy <= 2017) %>%
# mutate(fy = as.factor(as.character(fy)))
diff.percent <- ggplot(data = comparison_contracts) +
  geom_line(aes(
    x = fy,
    y = amount_percent_change,
    group = type,
    color = type
  )) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  xlab("Fiscal Year") +
  ylab("Amount") +
  ggtitle("difference in year-to-year % change") +
  theme(plot.title = element_text(hjust = 0.5))

# M: I added x-axis label, I named it so I could refer to it, and 
# I centered the title

# ggsave("charts/difference in year-to-year percent change.svg", diff.percent, 
#        device = "svg", width = 8, height = 6, units = "in")



# --------------------------------------------------------------------------------
# percent change
# --------------------------------------------------------------------------------

base_year <- engine_contracts %>% 
  group_by(fy, type) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>% 
  select(amount, type) 
  # rename(fyb = fy) %>%
  # mutate(fy = fyb + 1) %>%
  # select(-fyb)

base_year <- base_year %>%
  left_join(dyear, by = "type") %>% 
  select(- fy.y) %>% 
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>% 
  mutate(base = 100) %>% 
  mutate(amount = base + amount_percent_change)

# --------------------------------------------------------------------------------
# percent change, by customer 
# --------------------------------------------------------------------------------

base_year <- engine_contracts %>% 
  group_by(fy, type, customer) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>% 
  select(amount, type, customer)

(base_year <- base_year %>%
    left_join(dyear, by = c("type", "customer")) %>% 
    filter(customer != "Other DoD") %>% 
    select(- fy.y) %>% 
    mutate(amount_change = amount.x - amount.y) %>%
    mutate(amount_percent_change = (amount_change) / amount.y * 100) %>% 
    mutate(base = 0) %>% 
    mutate(amount = base + amount_percent_change) %>% 
    dplyr::rename(fy = fy.x) %>% 
    group_by(fy, customer) %>% 
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    mutate(customer = factor(customer, levels = c("Army",
                                                  "Navy",
                                                  "Air Force",
                                                  "DLA"))) %>% 
    mutate(amount = amount / 100)
)

(plot_base <- ggplot(base_year) + 
  geom_line(aes(x = fy, y = amount), alpha = .9, color = "#554449", size = 1) + 
  geom_hline(yintercept = 0, alpha = .5, color = "#554449", size = .5, linetype = "dotted") + 
  facet_wrap( ~ customer) +
  chart_theme +
    ggtitle("Change in aircraft engine contract obligations from 2000") +
    xlab("fiscal year") + 
    ylab(NULL) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                       labels = function(x) {substring(as.character(x), 3, 4)})
  )

ggsave("charts/amount_change.svg", plot_base,
       device = "svg", width = 8, height = 6, units = "in")
  
# percent change comparison -- ---------------------------------------------------

base_year <- engine_contracts %>% 
  as.tibble(.) %>% 
  group_by(fy, type, customer) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>% 
  select(amount, type, customer) 
# rename(fyb = fy) %>%
# mutate(fy = fyb + 1) %>%
# select(-fyb)

base_year.eng <- base_year %>%
  left_join(dyear, by = c("type", "customer")) %>% 
  filter(customer != "Other DoD") %>% 
  select(- fy.y) %>% 
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>% 
  mutate(base = 0) %>% 
  mutate(amount = base + amount_percent_change) %>% 
  dplyr::rename(fy = fy.x) %>% 
  group_by(fy, customer) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
  mutate(data_type = "engine") %>% 
  mutate(amount = amount / 100)

base_year <- topline_contracts %>% 
  group_by(fy, type, customer) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>% 
  select(amount, type, customer) 
# rename(fyb = fy) %>%
# mutate(fy = fyb + 1) %>%
# select(-fyb)

base_year.top <- base_year %>%
  left_join(dyear, by = c("type", "customer")) %>% 
  filter(customer != "Other DoD") %>% 
  select(- fy.y) %>% 
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>% 
  mutate(base = 0) %>% 
  mutate(amount = base + amount_percent_change) %>% 
  dplyr::rename(fy = fy.x) %>% 
  group_by(fy, customer) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
  mutate(data_type = "topline") %>% 
  mutate(amount = amount / 100)

eng.topline <- rbind(base_year.eng, base_year.top)

(eng.topline1 <- eng.topline %>% 
  filter(customer != "NULL" & customer != "MDA") %>% 
    mutate(customer = factor(customer, levels = c("Total",
                                                  "Army",
                                                  "Navy",
                                                  "Air Force",
                                                  "DLA"))) %>% 
    mutate(data_type = factor(data_type, levels = c("topline", "engine"))) %>% 
  ggplot() + 
  geom_line(aes(x = fy, y = amount), alpha = .9, color = "#554449", size = 1) + 
    geom_hline(yintercept = 0, alpha = .5, color = "#554449", size = .5, linetype = "dotted") + 
  
  facet_grid(data_type ~ customer) +
  chart_theme +
  #scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                     #labels = function(x) {substring(as.character(x), 3, 4)})+
  #scale_y_continuous(limits = c(0:750)) +
    ggtitle("Change in aircraft engine contract obligations from 2000") +
    xlab("fiscal year") + 
    ylab(NULL) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                       labels = function(x) {substring(as.character(x), 3, 4)})
  )

ggsave("charts/amount_change_customer.svg", eng.topline1, width = 9, height = 6, units = "in",
       device = "svg")

# percent change comparison by category ------------------------------------------

base_year <- engine_contracts %>% 
  as.tibble(.) %>% 
  group_by(fy, type, category) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>% 
  select(amount, type, category) 
# rename(fyb = fy) %>%
# mutate(fy = fyb + 1) %>%
# select(-fyb)

base_year.eng <- base_year %>%
  left_join(dyear, by = c("type", "category")) %>% 
  # filter(customer != "Other DoD") %>% 
  select(- fy.y) %>% 
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>% 
  mutate(base = 0) %>% 
  mutate(amount = base + amount_percent_change) %>% 
  dplyr::rename(fy = fy.x) %>% 
  group_by(fy, category) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
  mutate(data_type = "engine") %>% 
  mutate(amount = amount / 100)

base_year <- topline_contracts %>% 
  group_by(fy, type, category) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>% 
  select(amount, type, category) 
# rename(fyb = fy) %>%
# mutate(fy = fyb + 1) %>%
# select(-fyb)

base_year.top <- base_year %>%
  left_join(dyear, by = c("type", "category")) %>% 
  # filter(customer != "Other DoD") %>% 
  select(- fy.y) %>% 
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>% 
  mutate(base = 0) %>% 
  mutate(amount = base + amount_percent_change) %>% 
  dplyr::rename(fy = fy.x) %>% 
  group_by(fy, category) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
  mutate(data_type = "topline") %>% 
  mutate(amount = amount / 100)

eng.topline <- rbind(base_year.eng, base_year.top)

(eng.topline1 <- eng.topline %>% 
    filter(category != "NULL" & category != "MDA") %>% 
    mutate(category = factor(category, levels = c("Products",
                                                  "Services",
                                                  "R&D"))) %>%
    mutate(data_type = factor(data_type, levels = c("topline", "engine"))) %>% 
    ggplot() + 
    geom_line(aes(x = fy, y = amount), alpha = .9, color = "#554449", size = 1) + 
    geom_hline(yintercept = 0, alpha = .5, color = "#554449", size = .5, linetype = "dotted") + 
    facet_grid(data_type ~ category) +
    chart_theme +
    #scale_x_continuous(breaks = seq(2000, 2020, by = 2),
    #labels = function(x) {substring(as.character(x), 3, 4)})+
    #scale_y_continuous(limits = c(0:750)) +
    ggtitle("Change in aircraft engine contract obligations from 2000") +
    xlab("fiscal year") + 
    ylab(NULL) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                       labels = function(x) {substring(as.character(x), 3, 4)})
)

ggsave("charts/amount_change_category.svg", eng.topline1, width = 9, height = 6, units = "in",
       device = "svg")

# percent change comparison ------------------------------------------------------
 
base_year <- engine_contracts %>% 
  as.tibble(.) %>% 
  group_by(fy, type) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>% 
  select(amount, type) 
# rename(fyb = fy) %>%
# mutate(fy = fyb + 1) %>%
# select(-fyb)

base_year.eng <- base_year %>%
  left_join(dyear, by = "type") %>% 
  # filter(customer != "Other DoD") %>% 
  select(- fy.y) %>% 
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>% 
  mutate(base = 0) %>% 
  mutate(amount = base + amount_percent_change) %>% 
  dplyr::rename(fy = fy.x) %>% 
  group_by(fy) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
  mutate(data_type = "engine") %>% 
  mutate(amount = amount / 100)

base_year <- topline_contracts %>% 
  group_by(fy, type) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>% 
  select(amount, type) 
# rename(fyb = fy) %>%
# mutate(fy = fyb + 1) %>%
# select(-fyb)

(base_year.top <- base_year %>%
  left_join(dyear, by = "type") %>% 
  # filter(customer != "Other DoD") %>% 
  select(- fy.y) %>% 
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>% 
  mutate(base = 0) %>% 
  mutate(amount = base + amount_percent_change) %>% 
  dplyr::rename(fy = fy.x) %>% 
  group_by(fy) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
  mutate(data_type = "topline") %>% 
  mutate(amount = amount / 100))

eng.topline <- rbind(base_year.eng, base_year.top)

(eng.topline1 <- eng.topline %>% 
    # filter(category != "NULL" & category != "MDA") %>% 
    # mutate(category = factor(category, levels = c("Product",
    #                                               "Services",
    #                                               "R&D"))) %>%
    mutate(data_type = factor(data_type, levels = c("topline", "engine"))) %>% 
    ggplot() + 
    geom_line(aes(x = fy, y = amount), alpha = .9, color = "#554449", size = 1) + 
    geom_hline(yintercept = 0, alpha = .5, color = "#554449", size = .5, linetype = "dotted") + 
    facet_wrap( ~ data_type) +
    chart_theme +
    #scale_x_continuous(breaks = seq(2000, 2020, by = 2),
    #labels = function(x) {substring(as.character(x), 3, 4)})+
    #scale_y_continuous(limits = c(0:750)) +
    ggtitle("Change in aircraft engine contract obligations from 2000") +
    xlab("fiscal year") + 
    ylab(NULL) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                       labels = function(x) {substring(as.character(x), 3, 4)})
)

ggsave("charts/amount_change.svg", eng.topline1, width = 9, height = 6, units = "in",
       device = "svg")

-----------------------------------------------
####super-facet customer/category####
  
###engine
total <- engine_contracts %>%
      group_by(fy) %>%
      dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
      mutate(category = "Total") %>% 
      as.data.frame(.)

total_category <- engine_contracts %>%
    group_by(fy, category) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    as.data.frame(.)

total <- total %>% 
  rbind(total_category) %>% 
  mutate(customer = "Total")


army <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "Army") %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    mutate(category = "Total") %>% 
    as.data.frame(.)

army_category <- engine_contracts %>%
    filter(customer == "Army") %>%
    group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    as.data.frame(.)
  # # select(fy, amount, customer, category) %>% 

army <- army %>% 
  rbind(army_category) %>% 
  mutate(customer = "Army")

navy <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "Navy") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    # mutate(customer = "Navy") %>%
    mutate(category = "Total") %>% 
    # mutate(customer = as.factor(customer)) %>%
    mutate(category = as.factor(category)) %>% 
    as.data.frame(.)

navy_category <- engine_contracts %>%
    filter(customer == "Navy") %>%
    group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    as.data.frame(.)
  # # select(fy, amount, customer, category) %>% 

navy <- navy %>% 
  rbind(navy_category) %>% 
  mutate(customer = "Navy")

air_force <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "Air Force") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    # mutate(customer = "Air Force") %>%
    mutate(category = "Total") %>% 
    # mutate(customer = as.factor(customer)) %>%
    mutate(category = as.factor(category)) %>% 
    as.data.frame(.)

air_force_category <- engine_contracts %>%
    filter(customer == "Air Force") %>%
    group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    as.data.frame(.)
  # # select(fy, amount, customer, category) %>% 

air_force <- air_force %>% 
  rbind(air_force_category) %>% 
  mutate(customer = "Air Force")

dla <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "DLA") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    # mutate(customer = "DLA") %>%
    mutate(category = "Total") %>% 
    # mutate(customer = as.factor(customer)) %>%
    mutate(category = as.factor(category)) %>% 
    as.data.frame(.)

dla_category <- engine_contracts %>%
    filter(customer == "DLA") %>%
    group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    as.data.frame(.)
  # # select(fy, amount, customer, category) %>% 

dla <- dla %>% 
  rbind(dla_category) %>% 
  mutate(customer = "DLA")

engine <- total %>% 
    rbind(army, navy, air_force, dla) %>%
    mutate(customer = factor(customer, levels = c("Total",
                                                  "Army",
                                                  "Navy",
                                                  "Air Force",
                                                  "DLA")), 
           category = factor(category, levels = c("Total",
                                                  "Products", 
                                                  "Services",
                                                  "R&D")))  
  
base_year <- engine %>% 
  dplyr::group_by(fy, category, customer) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>% 
  select(fy, amount, category, customer) 
# rename(fyb = fy) %>%
# mutate(fy = fyb + 1) %>%
# select(-fyb)


base_year.eng <- base_year %>%
  left_join(dyear, by = c("category", "customer")) %>% 
  select(- fy.y) %>% 
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change / amount.y)* 100) %>% 
  dplyr::rename(fy = fy.x) %>% 
  group_by(fy, category, customer) %>% 
  dplyr::summarise(amount = sum(amount_percent_change, na.rm = TRUE)) ##%>% 
  ##mutate(data_type = "engine")

#plot####
(catbycust <- base_year.eng %>% 
    mutate(customer = factor(customer, levels = c("Total",
                                                  "Army",
                                                  "Navy",
                                                  "Air Force",
                                                  "DLA")), 
    category = factor(category, levels = c("Total",
                                           "Products", 
                                           "Services",
                                           "R&D"))) %>% 
    ggplot() + 
    geom_line(aes(x = fy, y = amount), size = 1) + 
    geom_hline(yintercept = 0, color = "#554449") + 
    
    facet_grid(category ~ customer) +
    chart_theme +
    #scale_x_continuous(breaks = seq(2000, 2020, by = 2),
    #labels = function(x) {substring(as.character(x), 3, 4)})+
    #scale_y_continuous(limits = c(0:750)) +
    xlab("Fiscal Year") +
   ylab("Cumulative Percent Change"))

# ggsave("charts/amount percent change, contract obligation by customer and category.svg", catbycust, width = 9, height = 6, units = "in",
#        device = "svg")

-----##with 100 base
  
base_year.eng <- base_year %>%
  left_join(dyear, by = c("category", "customer")) %>% 
  select(- fy.y) %>% 
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change / amount.y)* 100) %>% 
  mutate(base = 100) %>% 
  mutate(amount = base + amount_percent_change) %>% 
  dplyr::rename(fy = fy.x) %>% 
  group_by(fy, category, customer) %>% 
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) ##%>% 
##mutate(data_type = "engine")

  (catbycust <- base_year.eng %>% 
     mutate(customer = factor(customer, levels = c("Total",
                                                   "Army",
                                                   "Navy",
                                                   "Air Force",
                                                   "DLA")), 
            category = factor(category, levels = c("Total",
                                                   "Products", 
                                                   "Services",
                                                   "R&D"))) %>% 
     ggplot() + 
     geom_line(aes(x = fy, y = amount), size = 1) + 
     geom_hline(yintercept = 100, color = "#554449") +
     facet_grid(category ~ customer) +
     chart_theme +
     xlab("Fiscal Year") +
     ylab("Cumulative Percent Change"))

# ggsave("charts/amount percent change, contract obligation by customer and category, base 100.svg", catbycust, width = 9, height = 6, units = "in",
#        device = "svg")  

