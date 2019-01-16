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

# --------------------------------------------------------------------------------
# add theme

source("theme/chart_theme.R")
source("theme/money_labels.R")

# --------------------------------------------------------------------------------
# read engine contract data

read_engine_contracts <-
  read.csv("data/engine_contracts.csv")

# --------------------------------------------------------------------------------
# read topline contract data

read_topline_contracts <- read.csv("data/topline_contracts.csv")

# --------------------------------------------------------------------------------
# deflate data

deflator <-
  c(
    0.6981148243,
    0.7148243359,
    0.7263924593,
    0.7402742074,
    0.7586118252,
    0.7824335904,
    0.8078834619,
    0.8298200514,
    0.8470437018,
    0.8568980291,
    0.8644387318,
    0.8820051414,
    0.8981148243,
    0.9132819195,
    0.9299057412,
    0.9411311054,
    0.9520137104,
    0.9683804627,
    0.9835475578,
    1,
    1.018508997
  )

fy <- c(2000:2020)

deflate.year <- as.data.frame(cbind(fy, deflator))

# --------------------------------------------------------------------------------
# clean and summarize data

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
  left_join(deflate.year, by = "fy") %>%
  mutate(amount_19 = amount * deflator) %>%
  group_by(fy,
           customer,
           category,
           parent,
           project,
           competition,
           contract_type,
           vendor_size) %>%
  filter(fy <= 2017) %>%
  dplyr::summarise(amount = sum(amount_19, na.rm = TRUE))

topline_contracts <- read_topline_contracts %>%
  dplyr::rename(
    fy = FY,
    amount = Amount,
    category = Category,
    platform_portfolio = Portfolio,
    customer = Customer,
    vendor_size = VendorSize
  ) %>%
  left_join(deflate.year, by = "fy") %>%
  mutate(amount_19 = amount * deflator) %>%
  group_by(fy, customer, category) %>%
  dplyr::summarise(amount = sum(amount_19, na.rm = TRUE))

# --------------------------------------------------------------------------------
# reclassify vendor size

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

# --------------------------------------------------------------------------------
# reclassify competition

engine_contracts$competition <-
  as.character(engine_contracts$competition)

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

# --------------------------------------------------------------------------------
# reclassify contract type

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

# --------------------------------------------------------------------------------
# write a new dataset for powerbi

write.csv(engine_contracts, "app/power_bi.csv")

# ================================================================================
# charting engines 
# --------------------------------------------------------------------------------
# engine contracts 

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
  vendor_size <- engine_contracts %>%
    filter(vendor_size != "Unlabeled") %>%
    group_by(fy, vendor_size) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), alpha = .9, stat = "identity") +
    facet_wrap(~ vendor_size, nrow = 2) +
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
  vendor_size,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# --------------------------------------------------------------------------------
# engine contracts by competition  

(
  competition <- engine_contracts %>%
    filter(competition != "Unlabeled") %>%
    group_by(fy, competition) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    mutate(competition = factor(
      competition,
      levels = c(
        "Effective competition",
        "Competition with single offer",
        "No competition"
      )
    )) %>%
    ggplot() +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    geom_area(aes(y = amount, x = fy), alpha = .9, stat = "identity") +
    facet_wrap(~ competition, nrow = 1) +
    chart_theme +
    ggtitle("DoD aircraft engine contract obligations by competition") +
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
  competition,
  device = "svg",
  width = 8,
  height = 4,
  units = "in"
)

# --------------------------------------------------------------------------------
# engine contracts by contract type 

(
  contract_type <- engine_contracts %>%
    filter(contract_type != "Other") %>%
    group_by(fy, contract_type) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
    mutate(contract_type = factor(
      contract_type,
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
    geom_area(aes(y = amount, x = fy), alpha = .9, stat = "identity") +
    facet_wrap(~ contract_type, nrow = 1) +
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
  contract_type,
  device = "svg",
  width = 12,
  height = 4,
  units = "in"
)

# --------------------------------------------------------------------------------
# super facet: engine contracts by service and category

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

army <- army %>%
  rbind(army_category) %>%
  mutate(customer = "Army")

navy <- engine_contracts %>%
  group_by(fy) %>%
  filter(customer == "Navy") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(category = "Total") %>%
  mutate(category = as.factor(category)) %>%
  as.data.frame(.)

navy_category <- engine_contracts %>%
  filter(customer == "Navy") %>%
  group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)

navy <- navy %>%
  rbind(navy_category) %>%
  mutate(customer = "Navy")

air_force <- engine_contracts %>%
  group_by(fy) %>%
  filter(customer == "Air Force") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(category = "Total") %>%
  mutate(category = as.factor(category)) %>%
  as.data.frame(.)

air_force_category <- engine_contracts %>%
  filter(customer == "Air Force") %>%
  group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)

air_force <- air_force %>%
  rbind(air_force_category) %>%
  mutate(customer = "Air Force")

dla <- engine_contracts %>%
  group_by(fy) %>%
  filter(customer == "DLA") %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  mutate(category = "Total") %>%
  mutate(category = as.factor(category)) %>%
  as.data.frame(.)

dla_category <- engine_contracts %>%
  filter(customer == "DLA") %>%
  group_by(fy, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  as.data.frame(.)

dla <- dla %>%
  rbind(dla_category) %>%
  mutate(customer = "DLA")

(
  super_facet <- total %>%
    rbind(army, navy, air_force, dla) %>%
    mutate(
      customer = factor(customer, levels = c("Army",
                                             "Navy",
                                             "Air Force",
                                             "DLA",
                                             "Total")),
      category = factor(category, levels = c("Products",
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
    geom_area(aes(x = fy, y = amount), alpha = .9) +
    facet_grid(customer ~ category) +
    chart_theme +
    xlab("fiscal year") +
    ylab("constant fy19 dollars") +
    ggtitle("DoD Aircraft Engine Contract Obligations by service and category") +
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
  select(fy, customer, category, amount, type)

comparison_contracts_og <- engine_contracts %>%
  rbind(topline_contracts) %>%
  group_by(fy, type) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- comparison_contracts_og %>%
  dplyr::rename(fyb = fy) %>%
  mutate(fy = fyb + 1) %>%
  select(-fyb)

(
  comparison_contracts <- comparison_contracts_og %>%
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
    )
)

# --------------------------------------------------------------------------------
# percent change comparison by customer

base_year <- engine_contracts %>%
  as.tibble(.) %>%
  group_by(fy, type, customer) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>%
  select(amount, type, customer)

base_year.eng <- base_year %>%
  left_join(dyear, by = c("type", "customer")) %>%
  filter(customer != "Other DoD") %>%
  select(-fy.y) %>%
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

base_year.top <- base_year %>%
  left_join(dyear, by = c("type", "customer")) %>%
  filter(customer != "Other DoD") %>%
  select(-fy.y) %>%
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

(
  eng.topline1 <- eng.topline %>%
    filter(customer != "NULL" & customer != "MDA") %>%
    mutate(customer = factor(
      customer, levels = c("Total",
                           "Army",
                           "Navy",
                           "Air Force",
                           "DLA")
    )) %>%
    mutate(data_type = factor(data_type, levels = c("topline", "engine"))) %>%
    ggplot() +
    geom_line(
      aes(x = fy, y = amount),
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
    
    facet_grid(data_type ~ customer) +
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
# percent change comparison by category 

base_year <- engine_contracts %>%
  as.tibble(.) %>%
  group_by(fy, type, category) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>%
  select(amount, type, category)

base_year.eng <- base_year %>%
  left_join(dyear, by = c("type", "category")) %>%
  filter(category != "Services") %>%
  select(-fy.y) %>%
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

base_year.top <- base_year %>%
  left_join(dyear, by = c("type", "category")) %>%
  filter(category != "Services") %>%
  select(-fy.y) %>%
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

(
  eng.topline1 <- eng.topline %>%
    filter(category != "NULL" & category != "MDA") %>%
    mutate(category = factor(category, levels = c("Products",
                                                  # "Services",
                                                  "R&D"))) %>%
    mutate(data_type = factor(data_type, levels = c("topline", "engine"))) %>%
    ggplot() +
    geom_line(
      aes(x = fy, y = amount),
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
    facet_grid(data_type ~ category) +
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
  group_by(fy, type) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

dyear <- base_year %>%
  filter(fy == 2000) %>%
  select(amount, type)

base_year.eng <- base_year %>%
  left_join(dyear, by = "type") %>%
  select(-fy.y) %>%
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

(
  base_year.top <- base_year %>%
    left_join(dyear, by = "type") %>%
    select(-fy.y) %>%
    mutate(amount_change = amount.x - amount.y) %>%
    mutate(amount_percent_change = (amount_change) / amount.y * 100) %>%
    mutate(base = 0) %>%
    mutate(amount = base + amount_percent_change) %>%
    dplyr::rename(fy = fy.x) %>%
    group_by(fy) %>%
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
      aes(x = fy, y = amount),
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

