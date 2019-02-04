# ================================================================================
# The Future of Military Engines
# By Gabriel Coll
# --------------------------------------------------------------------------------
# engine-related budget numbers from the Future Years Defense Program
# ================================================================================

# ================================================================================
# cleaning and transformation
# --------------------------------------------------------------------------------
# load packages

library(tidyverse)
library(Cairo)
library(ggthemes)
library(car)
library(extrafont)
library(csis360)
# --------------------------------------------------------------------------------
# add theme

source("budget/theme/chart_theme.R")
source("budget/theme/money_labels.R")

# --------------------------------------------------------------------------------
# read data (each President's budget has a separate data table)

d19 <- read.csv("budget/data/19.csv")
d18 <- read.csv("budget/data/18.csv")
d17 <- read.csv("budget/data/17.csv")
d16 <- read.csv("budget/data/16.csv")
d15 <- read.csv("budget/data/15.csv")
d14 <- read.csv("budget/data/14.csv")
d13 <- read.csv("budget/data/13.csv")
d12 <- read.csv("budget/data/12.csv")
d11 <- read.csv("budget/data/11.csv")
d10 <- read.csv("budget/data/10.csv")
d09 <- read.csv("budget/data/09.csv")
d08 <- read.csv("budget/data/08.csv")
d07 <- read.csv("budget/data/07.csv")
d06 <- read.csv("budget/data/06.csv")
d05 <- read.csv("budget/data/05.csv")
d04 <- read.csv("budget/data/04.csv")
d03 <- read.csv("budget/data/03.csv")
d02 <- read.csv("budget/data/02.csv")
d01 <- read.csv("budget/data/01.csv")
d00 <- read.csv("budget/data/00.csv")
d99 <- read.csv("budget/data/99.csv")


#Renaming the columns is really about removing byte order  marks (bom)
d13<-remove_bom(d13)
d14<-remove_bom(d14)
d15<-remove_bom(d15)
d16<-remove_bom(d16)
d17<-remove_bom(d17)
d18<-remove_bom(d18)
d19<-remove_bom(d19)

# names(d19)[1] <- "FYDP.Year"
# names(d18)[1] <- "FYDP.Year"
# names(d17)[1] <- "FYDP.Year"
# names(d16)[1] <- "FYDP.Year"
# names(d15)[1] <- "FYDP.Year"
# names(d14)[1] <- "FYDP.Year"
# names(d13)[1] <- "FYDP.Year"

stages <- read.csv("budget/data/stages_join.csv")

# --------------------------------------------------------------------------------
# make the data long

d19 <- d19 %>%
  gather(`X2017`:`X2023`, key = "FY", value = "Amount")
d18 <- d18 %>%
  gather(`X2016`:`X2022`, key = "FY", value = "Amount")
d17 <- d17 %>%
  gather(`X2015`:`X2021`, key = "FY", value = "Amount")
d16 <- d16 %>%
  gather(`X2014`:`X2020`, key = "FY", value = "Amount")
d15 <- d15 %>%
  gather(`X2013`:`X2019`, key = "FY", value = "Amount")
d14 <- d14 %>%
  gather(`X2012`:`X2018`, key = "FY", value = "Amount")
d13 <- d13 %>%
  gather(`X2011`:`X2017`, key = "FY", value = "Amount")
d12 <- d12 %>%
  gather(`X2010`:`X2016`, key = "FY", value = "Amount")
d11 <- d11 %>%
  gather(`X2009`:`X2015`, key = "FY", value = "Amount")
d10 <- d10 %>%
  gather(`X2008`:`X2014`, key = "FY", value = "Amount")
d09 <- d09 %>%
  gather(`X2007`:`X2013`, key = "FY", value = "Amount")
d08 <- d08 %>%
  gather(`X2006`:`X2013`, key = "FY", value = "Amount")
d07 <- d07 %>%
  gather(`X2005`:`X2011`, key = "FY", value = "Amount")
d06 <- d06 %>%
  gather(`X2004`:`X2011`, key = "FY", value = "Amount")
d05 <- d05 %>%
  gather(`X2003`:`X2009`, key = "FY", value = "Amount")
d04 <- d04 %>%
  gather(`X2002`:`X2009`, key = "FY", value = "Amount")
d03 <- d03 %>%
  gather(`X2001`:`X2007`, key = "FY", value = "Amount")
d02 <- d02 %>%
  gather(`X2000`:`X2007`, key = "FY", value = "Amount")
d01 <- d01 %>%
  gather(`X1999`:`X2005`, key = "FY", value = "Amount")
d00 <- d00 %>%
  gather(`X1998`:`X2005`, key = "FY", value = "Amount")
d99 <- d99 %>%
  gather(`X1997`:`X2003`, key = "FY", value = "Amount")

# --------------------------------------------------------------------------------
# combine data 
#   (notes: the data tables for each President's budget have to be combined)

engine_budget <- rbind(
  d99,
  d00,
  d01,
  d02,
  d03,
  d04,
  d05,
  d06,
  d07,
  d08,
  d09,
  d10,
  d11,
  d12,
  d13,
  d14,
  d15,
  d16,
  d17,
  d18,
  d19
)

# --------------------------------------------------------------------------------
# add F135 
#   (note: separate because F135 funding is embeded into the broader F-35 line)

service_ratio <- read_csv("budget/data/service_ratio.csv")
service_ratio$checksum<-service_ratio$navy_ratio+service_ratio$air_force_ratio

f135 <- read_csv("budget/data/f135_spending.csv")
f135_amount<-sum(f135$Amount)

if(any(duplicated(service_ratio$FY))) stop("Duplicate Fiscal Years in service ratio")
f135 <- f135 %>%
  left_join(service_ratio,by="FY")

f135$FY <- str_c("X", f135$FY)

sum(f135$Amount)

f135_navy <- f135 %>%
  mutate(Amount = Amount * navy_ratio) %>%
  mutate(Force = "Navy") %>%
  select(
    "FYDP.Year",
    "Type",
    "Force",
    "Program.Number",
    "Program.Name",
    "Project.Number",
    "Project.Name",
    "FY",
    "Amount"
  ) %>%
  mutate(Project.Number = "2261")

f135_navy$Program.Number <- str_c(f135_navy$Program.Number, "N")

f135_air_force <- f135 %>%
  mutate(Amount = Amount * air_force_ratio) %>%
  mutate(Force = "Air Force") %>%
  select(
    "FYDP.Year",
    "Type",
    "Force",
    "Program.Number",
    "Program.Name",
    "Project.Number",
    "Project.Name",
    "FY",
    "Amount"
  ) %>%
  mutate(Project.Number = "653831")

f135_air_force$Program.Number <-
  str_c(f135_air_force$Program.Number, "F")

f135 <- rbind(f135_navy, f135_air_force)
if(sum(f135$Amount,na.rm=TRUE)!= f135_amount) warning("F135 Checksum failure")

f135_original <- read_csv("budget/data/f135_spending.csv")
f135_original$FY <- str_c("X", f135_original$FY)
f135_annual<- f135_original %>% group_by(FY) %>% summarize(Original_Amount=sum(Amount,na.rm = TRUE)) %>% left_join(
  f135 %>% group_by(FY) %>%  summarize(Split_Amount=sum(Amount,na.rm = TRUE)))
f135_annual %>% filter(Original_Amount!=Split_Amount)

# --------------------------------------------------------------------------------
# add F136 
#   (note: separate because F135 funding is embeded into the broader F-35 line)

service_ratio <- read_csv("budget/data/service_ratio.csv")


f136 <- read_csv("budget/data/f136_spending.csv")
f136_amount<-sum(f136$Amount)
if(any(duplicated(service_ratio$FY))) stop("Duplicate Fiscal Years in service ratio")
f136 <- f136 %>%
  left_join(service_ratio)

f136$FY <- str_c("X", f136$FY)

f136_navy <- f136 %>%
  mutate(Amount = Amount * navy_ratio) %>%
  mutate(Force = "Navy") %>%
  select(
    "FYDP.Year",
    "Type",
    "Force",
    "Program.Number",
    "Program.Name",
    "Project.Number",
    "Project.Name",
    "FY",
    "Amount"
  ) %>%
  mutate(Project.Number = "2261")

f136_navy$Program.Number <- str_c(f136_navy$Program.Number, "N")

f136_air_force <- f136 %>%
  mutate(Amount = Amount * air_force_ratio) %>%
  mutate(Force = "Air Force") %>%
  select(
    "FYDP.Year",
    "Type",
    "Force",
    "Program.Number",
    "Program.Name",
    "Project.Number",
    "Project.Name",
    "FY",
    "Amount"
  ) %>%
  mutate(Project.Number = "653831")

f136_air_force$Program.Number <-
  str_c(f136_air_force$Program.Number, "F")

f136 <- rbind(f136_navy, f136_air_force)

if(sum(f136$Amount,na.rm=TRUE)!= f136_amount) stop("F136 Checksum failure")


# --------------------------------------------------------------------------------
# combine engine_budget with F135 and F136

f135_f136 <- rbind(f135, f136)

sum(f135_f136$Amount,na.rm=TRUE)
summary(f135_f136)
?substring


if(nrow(engine_budget %>% filter(Program.Name %in% c("F135","F136") |
                                 substring(Program.Number,1,6) %in% substring(unique(f135_f136$Program.Number),1,6)|
                                 Project.Number %in% unique(f135_f136$Project.Number)))>0){
  stop("Adding redundant data!")
} else {
  engine_budget <- engine_budget %>%
    rbind(f135_f136)
}
# --------------------------------------------------------------------------------
# deflate data
#   (note: used Table 10.1 - GDP and deflators used in OMB historical tables:
#   ...1940 - 2023, FY19 = 1)

deflator <-
  c(
    0.68389032,
    0.69811482,
    0.71482434,
    0.72639246,
    0.74027421,
    0.75861183,
    0.78243359,
    0.80788346,
    0.82982005,
    0.84704370,
    0.85689803,
    0.86443873,
    0.88200514,
    0.89811482,
    0.91328192,
    0.92990574,
    0.94113111,
    0.95201371,
    0.96838046,
    0.98354756,
    1.00000000,
    1.01850900,
    1.03864610,
    1.05946872,
    1.08080548
  )

fy <- c(1999:2023)

deflate_year <- as.data.frame(cbind(fy, deflator))
engine_budget_alternate<- engine_budget %>%
  separate(FY, into = c("X", "FY"), sep = 1) 
engine_budget_alternate<-csis360::deflate(data=engine_budget_alternate,
                 money_var= "Amount",
                 fy_var="FY",
                 deflator_var="OMB.2019"
                 )

# --------------------------------------------------------------------------------
# clean and summarize data
#   (note: the funding in the last project was moved from PE 0602203F to 
#   ... Project 3048 starting in FY 2010 to more accurately align efforts with 
#   ... organizational structure.)

engine_budget <- engine_budget %>%
  separate(FY, into = c("X", "FY"), sep = 1) %>%
  dplyr::rename(
    fydp_year = FYDP.Year,
    account = Type,
    organization = Force,
    program_number = Program.Number,
    program_name = Program.Name,
    project_number = Project.Number,
    project_name = Project.Name,
    amount = Amount,
    fy = FY
  ) %>%
  mutate(fy = as.numeric(fy)) %>%
  dplyr::left_join(deflate_year, by = "fy") %>%
  mutate(amount_19 = amount / deflator) %>%
  dplyr::mutate(fydp_year = as.factor(fydp_year)) %>%
  dplyr::mutate(fydp = "FYDP") %>%
  unite(fydp_year, fydp_year, fydp, sep = " ") %>%
  select(fydp_year:project_name, fy, amount, amount_19) %>%
  dplyr::mutate(
    project_name = recode(
      project_name,
      "'Acft Demo Engines' = 'ACFT Demo Engines';
      'ACFT Demo Engines ' = 'ACFT Demo Engines';
      'Aircraft Demonstration Engine' = 'ACFT Demo Engines';
      'Aircraft Demonstration Engines' = 'ACFT Demo Engines';
      'Adv Propulsion Rsch' = 'Advanced Propulsion Research';
      'Adv Propulsion' = 'Advanced Propulsion Research';
      'Aerospace Fuels and Atmospheric Propulsion' = 'Aerospace Fuels';
      'Aircraft Engine Component Improvement Program (CIP) (USN)' = 'Aircraft Engine Component Improvement Program (USN)';
      'Aircraft Engine Component Improvement Program (CIP) (USA)' = 'Aircraft Engine Component Improvement Program (USA)';
      'Aircraft Engine Component Improvement Program' = 'Aircraft Engine Component Improvement Program (USAF)';
      'Aircraft Engine CIP' = 'Aircraft Engine Component Improvement Program (USN)';
      'Aircraft Component Improvement Program (CIP)' = 'Aircraft Engine Component Improvement Program (USA)';
      'A/C Eng Comp Imp (CIP)' = 'Aircraft Engine Component Improvement Program (USN)';
      'Acft Engines Comp Imp Prog' = 'Aircraft Engine Component Improvement Program (USN)';
      'F-35' = 'F135 Aircraft Engine Component Improvement Program';
      'Aircraft Propulsion Subsystem Integration' = 'Aircraft Propulsion Subsystems Int';
      'Vehicle Propulsion and Structures Technology' = 'Veh Prop & Struct Tech';
      'Propulsion and Power Component Improvement Program' = 'Aircraft Engine Component Improvement Program (USN)';
      'Aircraft Engine Component Improvement Program (CIP)' = 'Aircraft Engine Component Improvement Program (USA)';
      'A/C Compon Improv Prog' = 'Aircraft Engine Component Improvement Program (USA)';
      'F135 Aircraft Engine Component Improvement Program' = 'Aircraft Engine Component Improvement Program (F135)';
      'Materials for Structures, Propulsion and Subsystems' = 'Materials for Structures, Propulsion, and Subsystems';
      'Fuels and Lubrication' = 'Combustion and Mechanical Systems';
      'Propulsion' = 'Advanced Aerospace Propulsion';
      'Aerospace Fuel Technology' = 'Combustion and Mechanical Systems'"
    )
    )
if(sum(engine_budget$amount,na.rm=TRUE) != sum(engine_budget_alternate$Amount.Then.Year,na.rm=TRUE) |
   sum(engine_budget$amount_19,na.rm=TRUE) != sum(engine_budget_alternate$Amount.OMB.2019,na.rm=TRUE)) stop("Checksum failure")

# --------------------------------------------------------------------------------
# join stages 
#   (note: these are the stages of R&D, i.e. basic, applied research)

stages<-csis360::remove_bom(stages)

stages <- stages %>%
  dplyr::rename(stage = "Stage") %>% #remove-bom covers this
  dplyr::rename(project_name = "Project.Name") %>% 
  dplyr::mutate(
    stage = recode(
      stage,
      "'Basic Research' = '(6.1) Basic Research';
      'Applied Research' = '(6.2) Applied Research';
      'Advanced Technology Development' = '(6.3) Advanced Technology Development';
      'Advanced Component Development & Prototypes' = '(6.4) Advanced Component Development & Prototypes';
      'System Development & Demonstration' = '(6.5) System Development & Demonstration';
      'Operational Systems Development' = '(6.7) Operational Systems Development'"
    )
    )

if(any(duplicated(stages$project_name))) stop("Duplicate Project Name")
engine_budget <- engine_budget %>%
  left_join(stages, by = "project_name") %>%
  mutate(amount = amount * 1000000)

# --------------------------------------------------------------------------------

engine_budget_wide <-
  spread(engine_budget, key = "fy", value = "amount") # to view discrepancies

engine_budget %>% group_by(project_name) %>% dplyr::summarise(amount=sum(amount,na.rm=TRUE))

#I'm not sure why you're filtering these.
engine_budget <- engine_budget %>%
  filter(
    project_name %in% c(
      "ACFT Demo Engines",
      # "Adv Propulsion",
      "Advanced Aerospace Propulsion",
      "Advanced Propulsion Research",
      "Advanced Propulsion Technology",
      "Advanced Turbine Engine Gas Generator",
      # "Aerospace Fuel Technology",
      "Aerospace Fuels",
      "Aircraft Engine Component Improvement Program (F135)",
      "Aircraft Engine Component Improvement Program (USA)",
      "Aircraft Engine Component Improvement Program (USAF)",
      "Aircraft Engine Component Improvement Program (USN)",
      "Aircraft Propulsion Subsystems Int",
      "AV-8B",
      # "Aviation Advanced Technology Initiatives",
      "Combustion and Mechanical Systems",
      # "Fuels and Lubrication",
      "Improved Turbine Engine Program",
      # "Materials",
      "Materials for Structures, Propulsion, and Subsystems",
      # "Materials Technology for Sustainment",
      # "Materials Transition",
      # "Propulsion",
      # "Propulsion ",
      "Turbine Engine Technology",
      # "Vectored Thrust Ducted Propeller (CA)",
      "Veh Prop & Struct Tech",
      "F135",
      "F136",
      "Advanced Engine Development/Transition Prioritization"
    )
  ) %>%
  filter(fydp_year != "1999 FYDP")

engine_budget_wide <-
  spread(engine_budget, key = "fy", value = "amount") # to view discrepancies

# ================================================================================
# charting engines - future  
# --------------------------------------------------------------------------------
# engine spending (by FYDP year)

engine_budget_future <- engine_budget %>%
  mutate(fy = as.numeric(fy)) %>%
  group_by(fydp_year, fy) %>%
  filter(amount != 0) %>%
  dplyr::summarize(amount = sum(amount, na.rm = TRUE),
                   amount_19 = sum(amount_19, na.rm = TRUE))

#Quick examination of variance in number of years tracked by each budget
engine_budget_future_wide <-
  spread(engine_budget_future, key = "fy", value = "amount")
View(engine_budget_future)
engine_budget_future %>% group_by(fydp_year) %>%
  dplyr::summarise(fy_count=length(fy),
                   min_fy=min(fy),
                   max_fy=max(fy))


(
  plot_ebf <- ggplot(engine_budget_future) +
    geom_line(aes(
      x = fy,
      y = amount_19,
      group = fydp_year,
      alpha = fydp_year
    ), color = "#554449", size = 1) +
    ggtitle("DoD RDT&E Aircraft Engine Spending Projections") +
    chart_theme +
    scale_y_continuous(labels = scales::comma) +

    xlab("Fiscal Year") + 
    ylab("Constant 2019 $ Millions") 
)

ggsave(
  "budget/charts/amount_total.svg",
  plot_ebf,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# --------------------------------------------------------------------------------
# engine spending by Service

engine_budget_organization_future <- engine_budget %>%
  mutate(fy = as.numeric(fy)) %>%
  group_by(fydp_year, fy, organization) %>%
  filter(amount != 0) %>%
  dplyr::summarize(amount = sum(amount, na.rm = TRUE),
                   amount_19 = sum(amount_19, na.rm = TRUE))

engine_budget_organization_future_wide <-
  spread(engine_budget_organization_future, key = "fy", value = "amount")

(
  plot_ebf <- ggplot(engine_budget_organization_future) +
    geom_line(
      aes(
        x = fy,
        y = amount_19,
        group = fydp_year,
        alpha = fydp_year
      ),
      color = "#554449",
      size = 1
    ) +
    facet_wrap(~ organization) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(
      breaks = seq(2000, 2025, by = 5),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    ggtitle("DoD RDT&E Aircraft Engine Spending Projections by Service") +
    chart_theme +
    xlab("Fiscal Year") + 
    ylab("Constant 2019 $ Millions") 
)

ggsave(
  "budget/charts/amount_service.svg",
  plot_ebf,
  device = "svg",
  width = 12,
  height = 6,
  units = "in"
)

# --------------------------------------------------------------------------------
# engine spending by project name

engine_budget_project_future <- engine_budget %>%
  mutate(fy = as.numeric(fy)) %>%
  group_by(fydp_year, fy, project_name) %>%
  filter(amount != 0) %>%
  dplyr::summarize(amount = sum(amount, na.rm = TRUE),
                   amount_19 = sum(amount_19, na.rm = TRUE))

engine_budget_project_future_wide <-
  spread(engine_budget_project_future, key = "fy", value = "amount")

(
  plot_ebf <- ggplot(engine_budget_project_future) +
    geom_line(
      aes(
        x = fy,
        y = amount_19,
        group = fydp_year,
        alpha = fydp_year
      ),
      color = "#554449",
      size = 1
    ) +
    facet_wrap(~ project_name) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(
      breaks = seq(2000, 2025, by = 5),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    ggtitle("DoD RDT&E Aircraft Engine Spending Projections by project") +
    chart_theme +
    xlab("Fiscal Year") + 
    ylab("Constant 2019 $ Millions") 
)

# --------------------------------------------------------------------------------
# engine spending by Stage

engine_budget_stage_future <- engine_budget %>%
  mutate(fy = as.numeric(fy)) %>%
  filter(stage != "NA") %>%
  group_by(fydp_year, fy, stage) %>%
  filter(amount != 0) %>%
  dplyr::summarize(amount = sum(amount, na.rm = TRUE),
                   amount_19 = sum(amount_19, na.rm = TRUE))

engine_budget_stage_future_wide <-
  spread(engine_budget_stage_future, key = "fy", value = "amount")

(
  plot_ebf <- ggplot(engine_budget_stage_future) +
    geom_line(
      aes(
        x = fy,
        y = amount_19,
        group = fydp_year,
        alpha = fydp_year
      ),
      color = "#554449",
      size = 1
    ) +
    facet_wrap(~ stage) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(
      breaks = seq(2000, 2025, by = 5),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    ggtitle("DoD RDT&E Aircraft Engine Spending Projections by Stage") +
    chart_theme +
    xlab("Fiscal Year") + 
    ylab("Constant 2019 $ Millions")  
)

ggsave(
  "budget/charts/amount_stage.svg",
  plot_ebf,
  device = "svg",
  width = 10,
  height = 8,
  units = "in"
)

# --------------------------------------------------------------------------------
# engine spending by Service and stage

engine_budget_stage_service_future <- engine_budget %>%
  mutate(fy = as.numeric(fy)) %>%
  mutate(
    stage = recode(
      stage,
      "'(6.1) Basic Research' = '6.1';
      '(6.2) Applied Research' = '6.2';
      '(6.3) Advanced Technology Development' = '6.3';
      '(6.4) Advanced Component Development & Prototypes' = '6.4';
      '(6.5) System Development & Demonstration' = '6.5';
      '(6.7) Operational Systems Development' = '6.7'"
    )
  ) %>% 
  mutate(organization = factor(organization,
                               levels = c("Army",
                                          "Navy",
                                          "Air Force"))) %>%
  filter(stage != "NA") %>%
  group_by(fydp_year, fy, organization, stage) %>%
  filter(amount != 0) %>%
  dplyr::summarize(amount = sum(amount, na.rm = TRUE),
                   amount_19 = sum(amount_19, na.rm = TRUE))

engine_budget_stage_service_future_wide <-
  spread(engine_budget_stage_service_future, key = "fy", value = "amount")

(
  plot_ebf <- ggplot(engine_budget_stage_service_future) +
    geom_line(
      aes(
        x = fy,
        y = amount_19,
        group = fydp_year,
        alpha = fydp_year
      ),
      color = "#554449",
      size = 1
    ) +
    facet_grid(organization ~ stage) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(
      breaks = seq(2000, 2025, by = 5),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    ggtitle("DoD RDT&E Aircraft Engine Spending Projections by Stage and Service") +
    chart_theme +
    theme(strip.text.x = element_text(size = 10)) +
    theme(strip.text.y = element_text(size = 10)) +
    xlab("Fiscal Year") +
    ylab("Constant 2019 $ Millions") 
)

# ggsave(
#   "budget/charts/amount_service_stage.svg",
#   plot_ebf,
#   device = "svg",
#   width = 14,
#   height = 8,
#   units = "in"
# )

# ================================================================================
# charting engines - actual  
# --------------------------------------------------------------------------------
# clean data 
#   (note: this filtering section creates a dataset for only actual values in the 
#   ...budget line, in addition to enacted and projected for the most recent FYDP

engine_actual <- engine_budget %>%
  filter(
    fydp_year == "2000 FYDP" & fy == "1998" |
      fydp_year == "2001 FYDP" & fy == "1999" |
      fydp_year == "2002 FYDP" & fy == "2000" |
      fydp_year == "2003 FYDP" & fy == "2001" |
      fydp_year == "2004 FYDP" & fy == "2002" |
      fydp_year == "2005 FYDP" & fy == "2003" |
      fydp_year == "2006 FYDP" & fy == "2004" |
      fydp_year == "2007 FYDP" & fy == "2005" |
      fydp_year == "2008 FYDP" & fy == "2006" |
      fydp_year == "2009 FYDP" & fy == "2007" |
      fydp_year == "2010 FYDP" & fy == "2008" |
      fydp_year == "2011 FYDP" & fy == "2009" |
      fydp_year == "2012 FYDP" & fy == "2010" |
      fydp_year == "2013 FYDP" & fy == "2011" |
      fydp_year == "2014 FYDP" & fy == "2012" |
      fydp_year == "2015 FYDP" & fy == "2013" |
      fydp_year == "2016 FYDP" & fy == "2014" |
      fydp_year == "2017 FYDP" & fy == "2015" |
      fydp_year == "2018 FYDP" & fy == "2016" |
      fydp_year == "2019 FYDP" & fy == "2017" |
      fydp_year == "2019 FYDP" & fy == "2018" |
      fydp_year == "2019 FYDP" & fy == "2019" |
      fydp_year == "2019 FYDP" & fy == "2020" |
      fydp_year == "2019 FYDP" & fy == "2021" |
      fydp_year == "2019 FYDP" & fy == "2022" |
      fydp_year == "2019 FYDP" & fy == "2023"
  ) %>%
  mutate(
    stage = recode(
      stage,
      "'(6.1) Basic Research' = '6.1';
      '(6.2) Applied Research' = '6.2';
      '(6.3) Advanced Technology Development' = '6.3';
      '(6.4) Advanced Component Development & Prototypes' = '6.4';
      '(6.5) System Development & Demonstration' = '6.5';
      '(6.7) Operational Systems Development' = '6.7'"
    )
  ) %>% 
  filter(amount != 0) %>%
  group_by(fy, organization, stage, project_name) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE))

engine_actual_wide <-
  spread(engine_actual, key = "fy", value = "amount")

engine_actual$fy <- as.numeric(engine_actual$fy)

# facet --------------------------------------------------------------------------

engine_actual_1 <- engine_actual %>%
  filter(amount != 0) %>%
  group_by(fy, project_name) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE))

(
  facet <-
    ggplot() + geom_area(aes(y = amount_19, x = fy),
                         data = engine_actual_1,
                         stat = "identity") +
    geom_rect(
      aes(
        xmin = 2017,
        xmax = Inf,
        ymin = 0,
        ymax = Inf
      ),
      alpha = .02,
      fill = "grey",
      data = engine_actual_1
    ) +
    facet_wrap(~ project_name) +
    chart_theme +
    theme(strip.text.x = element_text(size = 8)) +
    theme(strip.text.y = element_text(size = 8)) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(
      breaks = seq(2000, 2023, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    ggtitle("DoD RDT&E Aircraft Engine spending by project") +
    xlab("Fiscal Year") +
    ylab("Constant 2019 $ Millions") + 
    geom_vline(
      xintercept = 2017,
      color = "#554449",
      alpha = .5,
      linetype = "solid"
    ) +
    geom_vline(
      xintercept = 2019,
      color = "#554449",
      linetype = "dotted"
    )
)

ggsave(
  "budget/charts/actual_amount_project.svg",
  facet,
  device = "svg",
  width = 20,
  height = 8,
  units = "in"
)

# facet 2 ------------------------------------------------------------------------

total <- engine_actual %>%
  group_by(fy) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  mutate(organization = "Total") %>% 
  as.data.frame(.)

total_organization <- engine_actual %>%
  group_by(fy, organization) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  as.data.frame(.)

total <- total %>%
  rbind(total_organization) %>%
  mutate(stage = "Total")

rd6.1 <-  engine_actual %>%
  filter(stage == "6.1") %>% 
  group_by(fy) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  mutate(organization = "Total") %>% 
  as.data.frame(.)

rd6.1_organization <-  engine_actual %>%
  filter(stage == "6.1") %>% 
  group_by(fy, organization) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  as.data.frame(.)

rd6.1 <- rd6.1 %>%
  rbind(rd6.1_organization) %>%
  mutate(stage = "6.1")

rd6.2 <-  engine_actual %>%
  filter(stage == "6.2") %>% 
  group_by(fy) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  mutate(organization = "Total") %>% 
  as.data.frame(.)  

rd6.2_organization <-  engine_actual %>%
  filter(stage == "6.2") %>% 
  group_by(fy, organization) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  as.data.frame(.)

rd6.2 <- rd6.2 %>%
  rbind(rd6.2_organization) %>%
  mutate(stage = "6.2")

rd6.3 <-  engine_actual %>%
  filter(stage == "6.3") %>% 
  group_by(fy) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  mutate(organization = "Total") %>% 
  as.data.frame(.)  

rd6.3_organization <-  engine_actual %>%
  filter(stage == "6.3") %>% 
  group_by(fy, organization) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  as.data.frame(.)

rd6.3 <- rd6.3 %>%
  rbind(rd6.3_organization) %>%
  mutate(stage = "6.3")

rd6.4 <-  engine_actual %>%
  filter(stage == "6.4") %>% 
  group_by(fy) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  mutate(organization = "Total") %>% 
  as.data.frame(.)  

rd6.4_organization <-  engine_actual %>%
  filter(stage == "6.4") %>% 
  group_by(fy, organization) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  as.data.frame(.)

rd6.4 <- rd6.4 %>%
  rbind(rd6.4_organization) %>%
  mutate(stage = "6.4")

rd6.5 <-  engine_actual %>%
  filter(stage == "6.5") %>% 
  group_by(fy) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  mutate(organization = "Total") %>% 
  as.data.frame(.)  

rd6.5_organization <-  engine_actual %>%
  filter(stage == "6.5") %>% 
  group_by(fy, organization) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  as.data.frame(.)

rd6.5 <- rd6.5 %>%
  rbind(rd6.5_organization) %>%
  mutate(stage = "6.5")

rd6.7 <-  engine_actual %>%
  filter(stage == "6.7") %>% 
  group_by(fy) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  mutate(organization = "Total") %>% 
  as.data.frame(.)  

rd6.7_organization <-  engine_actual %>%
  filter(stage == "6.7") %>% 
  group_by(fy, organization) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE)) %>% 
  as.data.frame(.)

rd6.7 <- rd6.7 %>%
  rbind(rd6.7_organization) %>%
  mutate(stage = "6.7")

engine_actual_organization <- engine_actual %>%
  group_by(fy, organization) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE))

total <- total %>%
  rbind(rd6.1, rd6.2, rd6.3, rd6.4, rd6.5, rd6.7) %>%
  mutate(
    stage = factor(stage, levels = c("6.1",
                                     "6.2",
                                     "6.3",
                                     "6.4",
                                     "6.5",
                                     "6.7",
                                     "Total")),
    organization = factor(organization, levels = c("Army",
                                                   "Navy",
                                                   "Air Force",
                                                   "Total"))
  )

(
  super_facet <- total %>%
    
    ggplot() +
    
    geom_area(aes(y = amount_19, x = fy),
                         # data = engine_actual_2,
                         stat = "identity") +
    facet_grid(organization ~ stage) +
    geom_rect(
      aes(
        xmin = 2017,
        xmax = Inf,
        ymin = 0,
        ymax = Inf
      ),
      alpha = .02,
      fill = "grey",
      data = engine_actual_2
    ) +
    chart_theme +
    theme(strip.text.x = element_text(size = 8)) +
    theme(strip.text.y = element_text(size = 8)) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(
      breaks = seq(2000, 2023, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    ggtitle("DoD RDT&E Aircraft Engine spending by Service and stage") +
    xlab("Fiscal Year") +
    ylab("Constant 2019 $ Millions") + 
    geom_vline(
      xintercept = 2017,
      color = "#554449",
      alpha = .5,
      linetype = "solid"
    ) +
    geom_vline(
      xintercept = 2019,
      color = "#554449",
      linetype = "dotted"
    )
)

ggsave(
  "budget/charts/actual_amount_service_stage.svg",
  super_facet,
  device = "svg",
  width = 20,
  height = 8,
  units = "in"
)

# ================================================================================
# chart engine comparison 
# --------------------------------------------------------------------------------
# read topline data 

read_topline <- read_csv("budget/data/topline.csv")

topline <- read_topline %>%
  select(fy, army, navy, air_force, dod_total, us_total) %>%
  gather(army:us_total, key = "project_name", value = "amount")

# --------------------------------------------------------------------------------
# clean and combine data
# --------------------------------------------------------------------------------

engine_actual <- filter(
  engine_budget,
  fydp_year == "2000 FYDP" & fy == "1998" |
    fydp_year == "2001 FYDP" & fy == "1999" |
    fydp_year == "2002 FYDP" & fy == "2000" |
    fydp_year == "2003 FYDP" & fy == "2001" |
    fydp_year == "2004 FYDP" & fy == "2002" |
    fydp_year == "2005 FYDP" & fy == "2003" |
    fydp_year == "2006 FYDP" & fy == "2004" |
    fydp_year == "2007 FYDP" & fy == "2005" |
    fydp_year == "2008 FYDP" & fy == "2006" |
    fydp_year == "2009 FYDP" & fy == "2007" |
    fydp_year == "2010 FYDP" & fy == "2008" |
    fydp_year == "2011 FYDP" & fy == "2009" |
    fydp_year == "2012 FYDP" & fy == "2010" |
    fydp_year == "2013 FYDP" & fy == "2011" |
    fydp_year == "2014 FYDP" & fy == "2012" |
    fydp_year == "2015 FYDP" & fy == "2013" |
    fydp_year == "2016 FYDP" & fy == "2014" |
    fydp_year == "2017 FYDP" & fy == "2015" |
    fydp_year == "2018 FYDP" & fy == "2016" |
    fydp_year == "2018 FYDP" & fy == "2017" |
    fydp_year == "2018 FYDP" & fy == "2018"
)

engine_actual <- mutate(engine_actual, amount = amount * 1000000)

engine_actual <- engine_actual %>%
  group_by(project_name, fy) %>%
  summarise(amount = sum(amount, na.rm = TRUE))

engine_actual <- filter(engine_actual, amount != 0)

engine_actual_2 <- engine_actual

engine_actual_3 <- engine_actual_2

engine_actual_3 <-
  spread(engine_actual_2, key = "project_name", value = "amount")

engine_actual_3[is.na(engine_actual_3)] <- 0

engine_actual_3 <-
  gather(
    engine_actual_3,
    "ACFT Demo Engines":"Veh Prop & Struct Tech",
    key = "project_name",
    value = "amount"
  )

data_fy <- select(engine_actual_3, fy)

engine_actual_3 <- select(engine_actual_3,-fy)

engine_actual_3 <- cbind(data_fy, engine_actual_3)

engine_actual_3$fy <- as.numeric(engine_actual_3$fy)

engine_actual_3 <- rbind(engine_actual_3, topline)
data_year <- engine_actual_3
data_year <- mutate(data_year, fy2 = fy + 1)
data_year <- select(data_year,-fy)
colnames(data_year)[colnames(data_year) == "fy2"] <- "fy"

engine_comparison <-
  left_join(engine_actual_3, data_year, by = c("project_name", "fy"))
engine_comparison <- filter(engine_comparison, fy != 1998 |
                              fy != 1997)
engine_comparison <-
  select(engine_comparison, fy, project_name, amount.x, amount.y)
engine_comparison_topline <- filter(
  engine_comparison,
  project_name == "us_total" |
    project_name == "dod_total" |
    project_name == "army" |
    project_name == "navy" |
    project_name == "air_force"
)
engine_comparison <- engine_comparison %>%
  filter(project_name != "us_total") %>%
  filter(project_name != "dod_total") %>%
  filter(project_name != "army") %>%
  filter(project_name != "navy") %>%
  filter(project_name != "air_force")

engine_comparison <- engine_comparison %>%
  group_by(fy) %>%
  summarise(
    amount.x = sum(amount.x, na.rm = TRUE),
    amount.y = sum(amount.y, na.rm = TRUE)
  ) %>%
  mutate(project_name = "Engines") %>%
  select(fy, project_name, amount.x, amount.y) %>%
  rbind(engine_comparison_topline)

engine_comparison <-
  mutate(engine_comparison, amount_change = amount.x - amount.y)
engine_comparison <-
  mutate(engine_comparison,
         amount_percent_Change = (amount.x - amount.y) / amount.y * 100)
colnames(engine_comparison)[colnames(engine_comparison) == "amount.x"] <-
  "amount"

engine_comparison <-
  select(engine_comparison,
         fy,
         project_name,
         amount,
         amount_change,
         amount_percent_Change)

engine_comparison$fy <- as.character(engine_comparison$fy)
engine_comparison$fy <- as.factor(engine_comparison$fy)

engine_comparison2 <-
  filter(engine_comparison, project_name == "Aerospace Fuels")

engine_comparison <- engine_comparison %>%
  filter()

# --------------------------------------------------------------------------------
# chart comparison 

engines_DOD_US <- engine_comparison %>%
  filter(project_name == "dod_total" |
           project_name == "Engines" | project_name == "us_total")

engines_DOD_US$fy = as.character(engines_DOD_US$fy)

engines <- engines_DOD_US %>%
  filter(project_name == "Engines")

eng.baseyear <- engines$amount[engines$fy == 2000]

DOD <- engines_DOD_US %>%
  filter(project_name == "dod_total")

dod.baseyear <- DOD$amount[DOD$fy == 2000]

US <- engines_DOD_US %>%
  filter(project_name == "us_total")

US.baseyear <- US$amount[US$fy == 2000]

base_year.eng <- engines %>%
  filter(fy >= 2000) %>%
  mutate(amount_change = amount - eng.baseyear) %>%
  mutate(amount_percent_change = (amount_change) / eng.baseyear * 100) %>%
  mutate(base = 100) %>%
  mutate(amount = base + amount_percent_change)

base_year.DOD <- DOD %>%
  filter(fy >= 2000) %>%
  mutate(amount_change = amount - dod.baseyear) %>%
  mutate(amount_percent_change = (amount_change) / dod.baseyear * 100) %>%
  mutate(base = 100) %>%
  mutate(amount = base + amount_percent_change)

base_year.US <- US %>%
  filter(fy >= 2000) %>%
  mutate(amount_change = amount - US.baseyear) %>%
  mutate(amount_percent_change = (amount_change) / US.baseyear * 100) %>%
  mutate(base = 100) %>%
  mutate(amount = base + amount_percent_change)

eng.all <- rbind(base_year.eng, base_year.DOD, base_year.US)

eng.all$project_name[eng.all$project_name == "dod_total"] = "Total DOD"
eng.all$project_name[eng.all$project_name == "us_total"] = "Total US"

eng.all <- eng.all %>%
  group_by(fy, project_name) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE))

eng.all$fy <- as.factor(eng.all$fy)

(
  eng_US_DOD.comp <- eng.all %>%
    ggplot() +
    geom_line(aes(
      x = fy, y = amount, group = 1
    ), size = 1) +
    geom_hline(yintercept = 100, color = "#554449") +
    facet_wrap(~ project_name) +
    chart_theme +
    scale_y_continuous(labels = scales::comma) +
    scale_x_discrete(
      breaks = seq(2000, 2018, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    xlab("Fiscal Year")
)

# ggsave(
#   "budget/charts/engine_DoD_US_comparison.svg",
#   eng_US_DOD.comp,
#   device = "svg",
#   width = 10,
#   height = 6,
#   units = "in"
# )

