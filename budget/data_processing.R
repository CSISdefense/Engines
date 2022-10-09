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
library(scales)
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
#   (note: separate because F135 funding is embedded into the broader F-35 line)

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



if(nrow(engine_budget %>% filter(Program.Name %in% c("F135","F136") |
                                 substring(Program.Number,1,6) %in% substring(unique(f135_f136$Program.Number),1,6)|
                                 Project.Number %in% unique(f135_f136$Project.Number)))>0){
  stop("Adding redundant data!")
} else {
  engine_budget <- engine_budget %>%
    rbind(f135_f136)
}
# --------------------------------------------------------------------------------
# # deflate data
# #   (note: used Table 10.1 - GDP and deflators used in OMB historical tables:
# #   ...1940 - 2023, FY19 = 1)
# 
# deflator <-
#   c(
#     0.68389032,
#     0.69811482,
#     0.71482434,
#     0.72639246,
#     0.74027421,
#     0.75861183,
#     0.78243359,
#     0.80788346,
#     0.82982005,
#     0.84704370,
#     0.85689803,
#     0.86443873,
#     0.88200514,
#     0.89811482,
#     0.91328192,
#     0.92990574,
#     0.94113111,
#     0.95201371,
#     0.96838046,
#     0.98354756,
#     1.00000000,
#     1.01850900,
#     1.03864610,
#     1.05946872,
#     1.08080548
#   )
# 
# fy <- c(1999:2023)

# deflate_year <- as.data.frame(cbind(fy, deflator))
# engine_budget_alternate<- engine_budget %>%
#   separate(FY, into = c("X", "FY"), sep = 1) 
# engine_budget_alternate<-csis360::deflate(data=engine_budget_alternate,
#                  money_var= "Amount",
#                  fy_var="FY",
#                  deflator_var="OMB23_GDP21"
#                  )

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
  mutate(fy = as.numeric(fy))

engine_budget<-csis360::deflate(
  engine_budget,
  money_var= "amount",
  fy_var="fy",
  deflator_var="OMB23_GDP21"
) %>%
  # dplyr::left_join(deflate_year, by = "fy") %>%
  # mutate(amount_OMB23_GDP21 = amount / deflator) %>%
  dplyr::mutate(fydp_year = as.factor(fydp_year)) %>%
  dplyr::mutate(fydp = "FYDP") %>%
  unite(fydp_year, fydp_year, fydp, sep = " ") %>%
  select(fydp_year:project_name, fy, amount_Then_Year, amount_OMB23_GDP21) %>%
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
# if(sum(engine_budget$amount,na.rm=TRUE) - sum(engine_budget_alternate$Amount.Then.Year,na.rm=TRUE) > 0.001 |
#    sum(engine_budget$amount_OMB23_GDP21,na.rm=TRUE) - sum(engine_budget_alternate$Amount.OMB.2019,na.rm=TRUE)> 0.001) stop("Deflation checksum failure")

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
  mutate(amount_Then_Year = amount_Then_Year * 1000000,
         amount_OMB23_GDP21 = amount_OMB23_GDP21 * 1000000)

# --------------------------------------------------------------------------------

engine_budget_wide <-
  spread(engine_budget, key = "fy", value = "amount_Then_Year") # to view discrepancies

engine_budget %>% group_by(project_name) %>% dplyr::summarise(amount_Then_Year=sum(amount_Then_Year,na.rm=TRUE))

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
  spread(engine_budget, key = "fy", value = "amount_Then_Year") # to view discrepancies

