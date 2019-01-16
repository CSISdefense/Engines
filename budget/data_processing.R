# ================================================================================
# The Future of Military Engines
# By Gabriel Coll
# --------------------------------------------------------------------------------
# engine-related budget numbers from the Future Years Defense Program
# ================================================================================

# cleaning and transformation ====================================================
# load packages ------------------------------------------------------------------

library(tidyverse)
library(Cairo)
library(ggthemes)
library(car)
library(extrafont)

# theme --------------------------------------------------------------------------

source("chart_theme.R")
source("money_labels.R") 

# read data ----------------------------------------------------------------------

d19 <- read.csv("19.csv")
d18 <- read.csv("18.csv")
d17 <- read.csv("17.csv")
d16 <- read.csv("16.csv")
d15 <- read.csv("15.csv")
d14 <- read.csv("14.csv")
d13 <- read.csv("13.csv")
d12 <- read.csv("12.csv")
d11 <- read.csv("11.csv")
d10 <- read.csv("10.csv")
d09 <- read.csv("09.csv")
d08 <- read.csv("08.csv")
d07 <- read.csv("07.csv")
d06 <- read.csv("06.csv")
d05 <- read.csv("05.csv")
d04 <- read.csv("04.csv")
d03 <- read.csv("03.csv")
d02 <- read.csv("02.csv")
d01 <- read.csv("01.csv")
d00 <- read.csv("00.csv")
d99 <- read.csv("99.csv")

names(d19)[1] <- "FYDP.Year"
names(d18)[1] <- "FYDP.Year"
names(d17)[1] <- "FYDP.Year"
names(d16)[1] <- "FYDP.Year"

# long data ----------------------------------------------------------------------

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

# combine data -------------------------------------------------------------------

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

# add F135 -----------------------------------------------------------------------

service_ratio <- read_csv("service_ratio.csv")

f135 <- read_csv("f135_spending.csv")

f135 <- f135 %>% 
  left_join(service_ratio)
  
f135$FY <- str_c("X", f135$FY) 

f135_navy <- f135 %>% 
  mutate(Amount = Amount * navy_ratio) %>% 
  mutate(Force = "Navy") %>% 
  select("FYDP.Year",
         "Type",
         "Force",
         "Program.Number",
         "Program.Name",
         "Project.Number",
         "Project.Name",
         "FY",
         "Amount") %>% 
  mutate(Project.Number = "2261")

f135_navy$Program.Number <- str_c(f135_navy$Program.Number, "N")

f135_air_force <- f135 %>% 
  mutate(Amount = Amount * air_force_ratio) %>% 
  mutate(Force = "Air Force") %>% 
  select("FYDP.Year",
         "Type",
         "Force",
         "Program.Number",
         "Program.Name",
         "Project.Number",
         "Project.Name",
         "FY",
         "Amount") %>% 
  mutate(Project.Number = "653831")

f135_air_force$Program.Number <- str_c(f135_air_force$Program.Number, "F")

f135 <- rbind(f135_navy, f135_air_force)

# add F136 -----------------------------------------------------------------------

service_ratio <- read_csv("service_ratio.csv")

f136 <- read_csv("f136_spending.csv")

f136 <- f136 %>% 
  left_join(service_ratio)

f136$FY <- str_c("X", f136$FY) 

f136_navy <- f136 %>% 
  mutate(Amount = Amount * navy_ratio) %>% 
  mutate(Force = "Navy") %>% 
  select("FYDP.Year",
         "Type",
         "Force",
         "Program.Number",
         "Program.Name",
         "Project.Number",
         "Project.Name",
         "FY",
         "Amount") %>% 
  mutate(Project.Number = "2261")

f136_navy$Program.Number <- str_c(f136_navy$Program.Number, "N")

f136_air_force <- f136 %>% 
  mutate(Amount = Amount * air_force_ratio) %>% 
  mutate(Force = "Air Force") %>% 
  select("FYDP.Year",
         "Type",
         "Force",
         "Program.Number",
         "Program.Name",
         "Project.Number",
         "Project.Name",
         "FY",
         "Amount") %>% 
  mutate(Project.Number = "653831")

f136_air_force$Program.Number <- str_c(f136_air_force$Program.Number, "F")

f136 <- rbind(f136_navy, f136_air_force)

# combine engine_budget with F135 and F136----------------------------------------

f135_f136 <- rbind(f135, f136)

engine_budget <- engine_budget %>% 
  rbind(f135_f136) 
# %>% 
#   group_by(FYDP.Year, 
#            Type, 
#            Force, 
#            Program.Number,
#            Program.Name, 
#            Project.Number, 
#            Project.Name, 
#            FY) %>% 
#   summarize(Amount = sum(Amount, na.rm = TRUE))

# clean data ---------------------------------------------------------------------

deflator <-
  c(
    0.7097926468,
    0.7267816693,
    0.7385433002,
    0.7526572574,
    0.7713016205,
    0.7955218679,
    0.821397456,
    0.8437009932,
    0.8612127548,
    0.8712319219,
    0.8788987629,
    0.8967590173,
    0.9131381774,
    0.9285589824,
    0.9454608817,
    0.9568740199,
    0.9679386653,
    0.984579195,
    1,
    1.016727653,
    1.035546262
  )

fy <- c(2000:2020)
deflate_year <- as.data.frame(cbind(fy, deflator))

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
  mutate(amount_18 = amount / deflator) %>%
  dplyr::mutate(fydp_year = as.factor(fydp_year)) %>%
  dplyr::mutate(fydp = "FYDP") %>%
  unite(fydp_year, fydp_year, fydp, sep = " ") %>%
  select(fydp_year:project_name, fy, amount, amount_18) %>%
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

# note: the funding in the last project (above) was moved from PE 0602203F Project 3048 starting in FY 2010
# to more accurately align efforts with organizational structure.

# join stages --------------------------------------------------------------------

stages <- read.csv("stages_join.csv")

stages <- stages %>%
  dplyr::rename(stage = "ï..Stage") %>%
  dplyr::rename(project_name = "Project.Name")

engine_budget <- engine_budget %>%
  left_join(stages, by = "project_name") %>% 
  mutate(amount = amount * 1000000)

# --------------------------------------------------------------------------------

engine_budget_wide <-
  spread(engine_budget, key = "fy", value = "amount") # to view discrepancies

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
      "F136"
    )
  ) %>%
  filter(fydp_year != "1999 FYDP")

engine_budget_wide <-
  spread(engine_budget, key = "fy", value = "amount") # to view discrepancies

# ================================================================================

engine_budget_future <- engine_budget %>%
  mutate(fy = as.numeric(fy)) %>%
  group_by(fydp_year, fy) %>%
  filter(amount != 0) %>%
  dplyr::summarize(amount = sum(amount, na.rm = TRUE))

engine_budget_future_wide <-
  spread(engine_budget_future, key = "fy", value = "amount")

(
  plot_ebf <- ggplot(engine_budget_future) +
    geom_line(aes(
      x = fy,
      y = amount,
      group = fydp_year,
      color = fydp_year
    ), size = 1) +
    # geom_smooth(aes(x = fy, y = amount, group = fydp_year, color = fydp_year), se = FALSE) +
    chart_theme +
    scale_y_continuous(labels = money_labels) + 
    xlab("fiscal year")
)

ggsave(
  "engine budget future, color by year.svg",
  plot_ebf,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)


engine_budget_future <- engine_budget %>%
  mutate(fy = as.numeric(fy)) %>%
  group_by(fydp_year, fy, organization) %>%
  filter(amount != 0) %>%
  dplyr::summarize(amount = sum(amount, na.rm = TRUE))

engine_budget_future_wide <-
  spread(engine_budget_future, key = "fy", value = "amount")

(
  plot_ebf <- ggplot(engine_budget_future) +
    geom_line(
      aes(
        x = fy,
        y = amount,
        group = fydp_year,
        alpha = fydp_year
      ),
      color = "#554449",
      size = 1
    ) +
    # geom_smooth(aes(x = fy, y = amount, group = fydp_year, color = fydp_year), se = FALSE) +
    facet_wrap( ~ organization) +
    scale_y_continuous(labels = money_labels) + 
    scale_x_continuous(
      breaks = seq(1999, 2023, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    chart_theme +
    xlab("fiscal year")
)

ggsave(
  "engine budget future, facet wrap by branch.svg",
  plot_ebf,
  device = "svg",
  width = 12,
  height = 6,
  units = "in"
)

engine_budget_future <- engine_budget %>%
  mutate(fy = as.numeric(fy)) %>%
  group_by(fydp_year, fy, project_name) %>%
  filter(amount != 0) %>%
  dplyr::summarize(amount = sum(amount, na.rm = TRUE))

engine_budget_future_wide <-
  spread(engine_budget_future, key = "fy", value = "amount")

(
  plot_ebf <- ggplot(engine_budget_future) +
    geom_line(
      aes(
        x = fy,
        y = amount,
        group = fydp_year,
        alpha = fydp_year
      ),
      color = "#554449",
      size = 1
    ) +
    # geom_smooth(aes(x = fy, y = amount, group = fydp_year, color = fydp_year), se = FALSE) +
    facet_wrap( ~ project_name) +
    scale_y_continuous(labels = money_labels) + 
    scale_x_continuous(
      breaks = seq(1999, 2023, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    chart_theme +
    xlab("fiscal year")
)

ggsave(
  "engine budget future, facet wrap by project name.svg",
  plot_ebf,
  device = "svg",
  width = 20,
  height = 10,
  units = "in"
)


engine_budget_future <- engine_budget %>%
  mutate(fy = as.numeric(fy)) %>%
  filter(stage != "NA") %>%
  group_by(fydp_year, fy, stage) %>%
  filter(amount != 0) %>%
  dplyr::summarize(amount = sum(amount, na.rm = TRUE))

engine_budget_future_wide <-
  spread(engine_budget_future, key = "fy", value = "amount")

(
  plot_ebf <- ggplot(engine_budget_future) +
    geom_line(
      aes(
        x = fy,
        y = amount,
        group = fydp_year,
        alpha = fydp_year
      ),
      color = "#554449",
      size = 1
    ) +
    # geom_smooth(aes(x = fy, y = amount, group = fydp_year, color = fydp_year), se = FALSE) +
    facet_wrap( ~ stage) +
    scale_y_continuous(labels = money_labels) + 
    scale_x_continuous(
      breaks = seq(1999, 2023, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    chart_theme +
    xlab("fiscal year")
)

ggsave(
  "engine budget future, facet wrap by stage.svg",
  plot_ebf,
  device = "svg",
  width = 10,
  height = 8,
  units = "in"
)

engine_budget_future <- engine_budget %>%
  mutate(fy = as.numeric(fy)) %>%
  mutate(stage = factor(
    stage,
    levels = c("Basic Research", 
               "Applied Research",
               "Advanced Technology Development",
               "System Development & Demonstration",
               "Operational Systems Development"))) %>% 
  mutate(organization = factor(
    organization,
    levels = c("Army", 
               "Navy",
               "Air Force"))) %>% 
  filter(stage != "NA") %>%
  group_by(fydp_year, fy, organization, stage) %>%
  filter(amount != 0) %>%
  dplyr::summarize(amount = sum(amount, na.rm = TRUE))

engine_budget_future_wide <-
  spread(engine_budget_future, key = "fy", value = "amount")

(
  plot_ebf <- ggplot(engine_budget_future) +
    geom_line(
      aes(
        x = fy,
        y = amount,
        group = fydp_year,
        alpha = fydp_year
      ),
      color = "#554449",
      size = 1
    ) +
    # geom_smooth(aes(x = fy, y = amount, group = fydp_year, color = fydp_year), se = FALSE) +
    facet_grid(organization ~ stage) +
    scale_y_continuous(labels = money_labels) + 
    scale_x_continuous(
      breaks = seq(1999, 2023, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    ggtitle("Future Years Defense Program by RDT&E Stage and Service") + 
    chart_theme +
    xlab("fiscal year")
)

ggsave(
  "engine budget future, facet grid by stage and organization.svg",
  plot_ebf,
  device = "svg",
  width = 14,
  height = 8,
  units = "in"
)

# filter the data to actual values -----------------------------------------------

# note: this filtering section creates a dataset for only actual values in
# the budget line, in addition to enacted and projected for the most recent FYDP

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
      fydp_year == "2019 FYDP" & fy == "2019"
  ) %>%
  filter(amount != 0) %>%
  group_by(fy, project_name) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE))

engine_actual_wide <-
  spread(engine_actual, key = "fy", value = "amount")

engine_actual$fy <- as.numeric(engine_actual$fy)

# facet --------------------------------------------------------------------------

(
  facet <-
    ggplot() + geom_area(aes(y = amount, x = fy), data = engine_actual,
                         stat = "identity") +
    facet_wrap( ~ project_name) +
    chart_theme +
    scale_y_continuous(labels = money_labels) + 
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    ggtitle("DoD spending related to aircraft engines by project") +
    xlab("fiscal year") +
    geom_vline(xintercept = 2016, color = "#554449")
)

ggsave(
  "DoD spending related to aircraft engines by project.svg",
  facet,
  device = "svg",
  width = 20,
  height = 8,
  units = "in"
)

engine_actual_total <- engine_actual %>%
  group_by(fy) %>%
  dplyr::summarise(amount = sum(amount, na.rm = FALSE))

# total --------------------------------------------------------------------------

(
  total <-
    ggplot() + geom_area(aes(y = amount, x = fy), data = engine_actual_total,
                         stat = "identity") +
    chart_theme +
    scale_x_continuous(breaks = seq(2000, 2020, by = 5)) +
    ggtitle("DoD spending related to aircraft engines") +
    xlab("fiscal year") +
    geom_vline(xintercept = 2016, color = "#554449")
)

ggsave(
  "DoD spending related to aircraft engines.svg",
  total,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# ================================================================================

# read topline data --------------------------------------------------------------

read_topline <- read_csv("topline.csv")

topline <- read_topline %>%
  select(fy, army, navy, air_force, dod_total, us_total) %>%
  gather(army:us_total, key = "project_name", value = "amount")

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
# engine_actual_2 <- select(engine_actual, project_name, fy, amount)
# engine_budget2 <- spread(engine_budget, key = "project_name", value = "amount")

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

engine_actual_3 <- select(engine_actual_3, -fy)

engine_actual_3 <- cbind(data_fy, engine_actual_3)

engine_actual_3$fy <- as.numeric(engine_actual_3$fy)

engine_actual_3 <- rbind(engine_actual_3, topline)
data_year <- engine_actual_3
data_year <- mutate(data_year, fy2 = fy + 1)
data_year <- select(data_year, -fy)
colnames(data_year)[colnames(data_year) == "fy2"] <- "fy"
# data_year$FY <- as.character(data_year$FY)
# engine_actual_3$FY <- as.numeric(engine_actual_3$FY)

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

# engine_comparison <- rbind(engine_comparison, engine_comparison_topline)


engine_comparison <-
  mutate(engine_comparison, amount_change = amount.x - amount.y)
engine_comparison <-
  mutate(engine_comparison,
         amount_percent_Change = (amount.x - amount.y) / amount.y * 100)
colnames(engine_comparison)[colnames(engine_comparison) == "amount.x"] <-
  "amount"
# colnames(engine_comparison)[colnames(engine_comparison)=="project_name.x"] <- "project_name"
engine_comparison <-
  select(engine_comparison,
         fy,
         project_name,
         amount,
         amount_change,
         amount_percent_Change)
# engine_comparison[ is.na(engine_comparison) ] <- 0
# engine_comparison2 <- spread(engine_comparison, key = "project_name", value = "amount")
# engine_actual4 <- sapply(names(engine_actual_3)[-1], function(x) {
#   engine_actual_3[paste0(x, "_pct")] <<- engine_actual_3[x] / sum(engine_actual_3[x])
# })
engine_comparison$fy <- as.character(engine_comparison$fy)
engine_comparison$fy <- as.factor(engine_comparison$fy)

# engine_comparison <- do.call(data.frame,lapply(engine_comparison, function(x) replace(x, is.infinite(x),NA)))

# engine_budget2 <- filter(engine_budget, project_name == "Advanced Propulsion Research")
engine_comparison2 <-
  filter(engine_comparison, project_name == "Aerospace Fuels")
# engine_comparison2 <- spread(engine_comparison2, key = "project_name", value = "amount")
# engine_comparison <- filter(engine_comparison, project_name == "Engines" |
#               project_name == "us_total")
engine_comparison <- engine_comparison %>%
  filter()

(
  percent_change <- ggplot(
    data = engine_comparison,
    aes(
      x = fy,
      y = amount_percent_Change,
      group = project_name,
      color = project_name
    )
  ) +
    scale_y_continuous(labels = money_labels) + 
    geom_line() +
    geom_hline(yintercept = 0) +
    chart_theme +
    xlab("fiscal year") +
    ylab("amount - percent change")
)
# facet_wrap(~ project_name)

ggsave(
  "fiscal year by amount percent change, color by organization and totals.svg",
  percent_change,
  device = "svg",
  width = 15,
  height = 6,
  units = "in"
)

write.csv(engine_comparison, "data4.csv")

write.csv(engine_budget, "data.csv")

write.csv(engine_actual_3, "data3.csv")


# ----------------------------------------------------------------------
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
    facet_wrap( ~ project_name) +
    chart_theme +
    scale_y_continuous(labels = money_labels) + 
    scale_x_discrete(
      breaks = seq(2000, 2018, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    #scale_y_continuous(limits = c(0:750)) +
    xlab("Fiscal Year")
)

ggsave(
  "fiscal year by amount percent change, facet wrap engines, DOD, US.svg",
  eng_US_DOD.comp,
  device = "svg",
  width = 10,
  height = 6,
  units = "in"
)

# ggplot(data=engine_comparison, aes(x=fy, y=amount_percent_Change, group=1)) +
#   geom_area()

# -------------------------
topline_budget <- topline %>%
  filter(project_name == "army" |
           project_name == "navy" | project_name == "air_force") %>%
  mutate(data_type = "topline") %>%
  dplyr::rename(organization = project_name)

topline_budget$organization[topline_budget$organization == "army"] = "Army"
topline_budget$organization[topline_budget$organization == "navy"] = "Navy"
topline_budget$organization[topline_budget$organization == "air_force"] = "Air Force"

navy <- topline_budget %>%
  filter(organization == "Navy")

n.baseyear <- navy$amount[navy$fy == 2000]

army <- topline_budget %>%
  filter(organization == "Army")

a_base_year <- army$amount[army$fy == 2000]

air_force <- topline_budget %>%
  filter(organization == "Air Force")

af.baseyear <- air_force$amount[air_force$fy == 2000]

base_year_a <- army %>%
  filter(fy >= 2000) %>%
  mutate(amount_change = amount - a_base_year) %>%
  mutate(amount_percent_change = (amount_change) / a_base_year * 100) %>%
  mutate(base = 100) %>%
  mutate(amount = base + amount_percent_change)

base_year_n <- navy %>%
  filter(fy >= 2000) %>%
  mutate(amount_change = amount - n.baseyear) %>%
  mutate(amount_percent_change = (amount_change) / n.baseyear * 100) %>%
  mutate(base = 100) %>%
  mutate(amount = base + amount_percent_change)

base_year_af <- air_force %>%
  filter(fy >= 2000) %>%
  mutate(amount_change = amount - af.baseyear) %>%
  mutate(amount_percent_change = (amount_change) / af.baseyear * 100) %>%
  mutate(base = 100) %>%
  mutate(amount = base + amount_percent_change)

topline_budget <- rbind(base_year_a, base_year_n, base_year_af)

engine_budget_g <- engine_budget %>%
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
      fydp_year == "2018 FYDP" & fy == "2017" |
      fydp_year == "2018 FYDP" & fy == "2018"
  ) %>%
  dplyr::mutate(amount = amount * 1000000) %>%
  group_by(organization, fy) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  dplyr::mutate(data_type = "engines")


navy <- engine_budget_g %>%
  filter(organization == "Navy")

n.baseyear <- navy$amount[navy$fy == 2000]

army <- engine_budget_g %>%
  filter(organization == "Army")

a_base_year <- army$amount[army$fy == 2000]

air_force <- engine_budget_g %>%
  filter(organization == "Air Force")

af.baseyear <- air_force$amount[air_force$fy == 2000]

base_year_a <- army %>%
  filter(fy >= 2000) %>%
  mutate(amount_change = amount - a_base_year) %>%
  mutate(amount_percent_change = (amount_change) / a_base_year * 100) %>%
  mutate(base = 100) %>%
  mutate(amount = base + amount_percent_change)

base_year_n <- navy %>%
  filter(fy >= 2000) %>%
  mutate(amount_change = amount - n.baseyear) %>%
  mutate(amount_percent_change = (amount_change) / n.baseyear * 100) %>%
  mutate(base = 100) %>%
  mutate(amount = base + amount_percent_change)

base_year_af <- air_force %>%
  filter(fy >= 2000) %>%
  mutate(amount_change = amount - af.baseyear) %>%
  mutate(amount_percent_change = (amount_change) / af.baseyear * 100) %>%
  mutate(base = 100) %>%
  mutate(amount = base + amount_percent_change)

engine_budget_g <- rbind(base_year_a, base_year_n, base_year_af)


topline_budget <- as.data.frame(topline_budget)
engine_budget_g <- as.data.frame(engine_budget_g)
engine_budget_g$organization <-
  as.character(engine_budget_g$organization)

eng_topline <- rbind(topline_budget, engine_budget_g)

# eng_topline <-filter(eng_topline, fy <= 2016)

(
  eng_topline1 <- eng_topline %>%
    ggplot() +
    geom_line(aes(
      x = fy, y = amount, group = 1
    ), size = 1) +
    geom_hline(yintercept = 100, color = "#554449") +
    facet_grid(data_type ~ organization) +
    chart_theme +
    scale_y_continuous(labels = money_labels) + 
    scale_x_discrete(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    #scale_y_discrete(limits = c(0:750)) +
    xlab("Fiscal Year")
)

ggsave(
  "engine to topline comparison, amount percent change by organization.svg",
  eng_topline1,
  width = 11,
  height = 6,
  units = "in",
  device = "svg"
)
#
#   ggsave("engine to topline comparison, amount percent change by organization - wo2018.pdf",
#          eng_topline1, width = 11, height = 6, units = "in",
#          device = cairo_pdf())
# #
#   ggsave("engine to topline comparison, amount percent change by organization - wo2017,2018.pdf",
#          eng_topline1, width = 11, height = 6, units = "in",
#          device = cairo_pdf())

############# deflator FY19########################
###because of the change I made to the top (FY18) these numbers are probably not applicable

# deflator <- c(0.6981148243, 0.7148243359, 0.7263924593, 0.7402742074, 0.7586118252,
# 0.7824335904, 0.8078834619, 0.8298200514, 0.8470437018, 0.8568980291,
# 0.8644387318, 0.8820051414, 0.8981148243, 0.9132819195, 0.9299057412,
# 0.9411311054, 0.9520137104, 0.9683804627, 0.9835475578)
# fy <- c(2000:2018)
# deflate_year <- as.data.frame(cbind(fy, deflator))
#
# topline_budget <- topline %>%
#   filter(project_name == "army" | project_name == "navy" | project_name == "air_force") %>%
#   mutate(data_type = "topline") %>%
#   dplyr::rename(organization = project_name) %>%
#   left_join(deflate_year, by = "fy") %>%
#   filter(fy >=2000) %>%
#   mutate(deflate19 = amount*deflator)
#
#
# topline_budget$organization[topline_budget$organization == "army"] = "Army"
# topline_budget$organization[topline_budget$organization == "navy"] = "Navy"
# topline_budget$organization[topline_budget$organization == "air_force"] = "Air Force"
#
# navy <- topline_budget %>%
#   filter(organization == "Navy")
#
# n.baseyear <- navy$deflate19[navy$fy == 2000]
#
# army <- topline_budget %>%
#   filter(organization == "Army")
#
# a_base_year <- army$deflate19[army$fy == 2000]
#
# air_force <- topline_budget %>%
#   filter(organization == "Air Force")
#
# af.baseyear <- air_force$deflate19[air_force$fy == 2000]
#
# base_year_a <- army %>%
#   filter(fy >= 2000) %>%
#   mutate(amount_change = deflate19 - a_base_year) %>%
#   mutate(amount_percent_change = (amount_change) / a_base_year * 100) %>%
#   mutate(base = 100) %>%
#   mutate(amount = base + amount_percent_change)
#
# base_year_n <- navy %>%
#   filter(fy >= 2000) %>%
#   mutate(amount_change = deflate19 - n.baseyear) %>%
#   mutate(amount_percent_change = (amount_change) / n.baseyear * 100) %>%
#   mutate(base = 100) %>%
#   mutate(amount = base + amount_percent_change)
#
# base_year_af <- air_force %>%
#   filter(fy >= 2000) %>%
#   mutate(amount_change = deflate19 - af.baseyear) %>%
#   mutate(amount_percent_change = (amount_change) / af.baseyear * 100) %>%
#   mutate(base = 100) %>%
#   mutate(amount = base + amount_percent_change)
#
# topline_budget <- rbind(base_year_a, base_year_n, base_year_af)
#
# engine_budget$fy = as.numeric(engine_budget$fy)
#
# engine_budget_g <- engine_budget %>%
#   filter(
#     fydp_year == "2000 FYDP" & fy == "1998" |
#       fydp_year == "2001 FYDP" & fy == "1999" |
#       fydp_year == "2002 FYDP" & fy == "2000" |
#       fydp_year == "2003 FYDP" & fy == "2001" |
#       fydp_year == "2004 FYDP" & fy == "2002" |
#       fydp_year == "2005 FYDP" & fy == "2003" |
#       fydp_year == "2006 FYDP" & fy == "2004" |
#       fydp_year == "2007 FYDP" & fy == "2005" |
#       fydp_year == "2008 FYDP" & fy == "2006" |
#       fydp_year == "2009 FYDP" & fy == "2007" |
#       fydp_year == "2010 FYDP" & fy == "2008" |
#       fydp_year == "2011 FYDP" & fy == "2009" |
#       fydp_year == "2012 FYDP" & fy == "2010" |
#       fydp_year == "2013 FYDP" & fy == "2011" |
#       fydp_year == "2014 FYDP" & fy == "2012" |
#       fydp_year == "2015 FYDP" & fy == "2013" |
#       fydp_year == "2016 FYDP" & fy == "2014" |
#       fydp_year == "2017 FYDP" & fy == "2015" |
#       fydp_year == "2018 FYDP" & fy == "2016" |
#       fydp_year == "2018 FYDP" & fy == "2017" |
#       fydp_year == "2018 FYDP" & fy == "2018") %>%
#   dplyr::mutate(amount = amount * 1000000) %>%
#   group_by(organization, fy) %>%
#   dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
#   dplyr::mutate(data_type = "engines") %>%
#   left_join(deflate_year, by = "fy") %>%
#   filter(fy >=2000) %>%
#   mutate(deflate19 = amount*deflator)
#
#
# navy <- engine_budget_g %>%
#   filter(organization == "Navy")
#
# n.baseyear <- navy$deflate19[navy$fy == 2000]
#
# army <- engine_budget_g %>%
#   filter(organization == "Army")
#
# a_base_year <- army$deflate19[army$fy == 2000]
#
# air_force <- engine_budget_g %>%
#   filter(organization == "Air Force")
#
# af.baseyear <- air_force$deflate19[air_force$fy == 2000]
#
# base_year_a <- army %>%
#   filter(fy >= 2000) %>%
#   mutate(amount_change = deflate19 - a_base_year) %>%
#   mutate(amount_percent_change = (amount_change) / a_base_year * 100) %>%
#   mutate(base = 100) %>%
#   mutate(amount = base + amount_percent_change)
#
# base_year_n <- navy %>%
#   filter(fy >= 2000) %>%
#   mutate(amount_change = deflate19 - n.baseyear) %>%
#   mutate(amount_percent_change = (amount_change) / n.baseyear * 100) %>%
#   mutate(base = 100) %>%
#   mutate(amount = base + amount_percent_change)
#
# base_year_af <- air_force %>%
#   filter(fy >= 2000) %>%
#   mutate(amount_change = deflate19 - af.baseyear) %>%
#   mutate(amount_percent_change = (amount_change) / af.baseyear * 100) %>%
#   mutate(base = 100) %>%
#   mutate(amount = base + amount_percent_change)
#
# engine_budget_g <- rbind(base_year_a, base_year_n, base_year_af)
#
#
# topline_budget <- as.data.frame(topline_budget)
# engine_budget_g <- as.data.frame(engine_budget_g)
# engine_budget_g$organization <- as.character(engine_budget_g$organization)
#
# eng_topline <- rbind(topline_budget, engine_budget_g)
#
# # eng_topline <-filter(eng_topline, fy <= 2016)
#
# (eng_topline1 <- eng_topline %>%
#     ggplot() +
#     geom_line(aes(x = fy, y = amount, group = 1), size = 1) +
#     geom_hline(yintercept = 100, color = "#554449") +
#     facet_grid(data_type ~ organization) +
#     chart_theme +
#     scale_x_discrete(breaks = seq(2000, 2020, by = 2),
#                      labels = function(x) {substring(as.character(x), 3, 4)})+
#     #scale_y_discrete(limits = c(0:750)) +
#     xlab("Fiscal Year"))
#
# ggsave("deflate FY19 - engine to topline comparison, amount percent change by organization.pdf",
#        eng_topline1, width = 11, height = 6, units = "in",
#        device = cairo_pdf())
#
#  # ggsave("deflate FY19 - engine to topline comparison, amount percent change by organization - wo2018.pdf",
#  #        eng_topline1, width = 11, height = 6, units = "in",
#  #        device = cairo_pdf())
#  #
#  # ggsave("deflate FY19 - engine to topline comparison, amount percent change by organization - wo2017,2018.pdf",
#  #        eng_topline1, width = 11, height = 6, units = "in",
#  #        device = cairo_pdf())

