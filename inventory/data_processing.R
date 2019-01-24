# ================================================================================
# The Future of Military Engines
# By Gabriel Coll
# --------------------------------------------------------------------------------
# inventory numbers and performance specs for aircraft and their engines
# ================================================================================

# cleaning and transformation ====================================================
# load packages ------------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(extrafont)
library(extrafontdb)
library(svglite)
# theme --------------------------------------------------------------------------

source("inventory/theme/chart_theme.R")

# read data ----------------------------------------------------------------------

intro_year <- read_csv("inventory/data/intro_year.csv")
usaf_inventory <- read_csv("inventory/data/usaf_inventory.csv")
engine_specs <- read_csv("inventory/data/engine_specs.csv")
generation <- read_csv("inventory/data/generation.csv")
relevance <- read_csv("inventory/data/relevance.csv")

intro_year <- intro_year %>%
  .[-1,] %>%
  gather(aircraft, intro_year,-year) %>%
  .[,-1]

usaf_inventory[is.na(usaf_inventory)] <- 0



# Preserve unique column names and then remove "_1" from duplicates in aircraft.
usaf_inventory <- gather(usaf_inventory, aircraft, amount,-year)
usaf_inventory$column<-usaf_inventory$aircraft
usaf_inventory$aircraft<-sub("_1$","",usaf_inventory$aircraft)

#Stop preserving and summarize by aircraft type
usaf_inventory$amount<-as.numeric(usaf_inventory$amount)
usaf_inventory <- usaf_inventory %>% group_by(year, aircraft) %>%
  summarise(amount=sum(amount,na.rm = TRUE))


usaf_inventory$aircraft<-factor(usaf_inventory$aircraft)
engine_specs$aircraft<-factor(engine_specs$aircraft)
#1st version, to get the details.
engine_specs_no_dupes <- engine_specs %>% group_by(aircraft, engine_type,  engine_number, takeoff_weight, speed, 
                                                   range, ceiling, climb_rate,  thrust_weight_aircraft, engine,  engine_company, 
                                                   thrust,  pressure_ratio, engine_weight, thrust_weight_engine ) %>%
  summarise(
    type_count=length(unique(type)),
            record_count=length(aircraft))

engine_specs_no_dupes<-engine_specs_no_dupes[order(engine_specs_no_dupes$aircraft),]

if(any(duplicated(engine_specs$aircraft))) 
  print(unique(engine_specs$aircraft[duplicated(engine_specs$aircraft)]))
  

#Output those aircraft with inconsistent details
duplicate_aircraft<-unique(engine_specs_no_dupes$aircraft[duplicated(engine_specs_no_dupes$aircraft)])
write.csv(engine_specs_no_dupes[engine_specs_no_dupes$aircraft %in% duplicate_aircraft,],
          "inventory/data/variable_engine_aircraft.csv")

#Second version. Note, not removing NAs, so any aircraft with one NA in a column will have NA in its single line.
#Alternate version is to remove NAs to allow max/min to trump.
engine_specs_no_dupes <- engine_specs %>% group_by(aircraft, takeoff_weight, speed, 
                                                   range, ceiling, climb_rate,  thrust_weight_aircraft, engine,  engine_company, 
                                                   thrust,  pressure_ratio, engine_weight, thrust_weight_engine ) %>%
  summarise(
    engine_type=ifelse(!any(is.na(engine_type)) & max(engine_type)==min(engine_type),max(engine_type),NA_character_ ),
      engine_number=ifelse(!any(is.na(engine_number)) & max(engine_number)==min(engine_number),max(engine_number),NA_integer_),
    type_list=paste(unique(type),collapse=", "),
    engine_spec_record_count=length(aircraft))
engine_specs_no_dupes$engine_type <- factor(engine_specs_no_dupes$engine_type)
engine_specs_no_dupes<-engine_specs_no_dupes[,c(1,14:17,2:13)]


if(any(duplicated(engine_specs_no_dupes$aircraft))) 
  print(unique(engine_specs_no_dupes$aircraft[duplicated(engine_specs_no_dupes$aircraft)]))
stop ("Attempt to remove duplicates failed.")

write.csv(engine_specs_no_dupes,
          "inventory/data/engine_specs_no_dupes.csv")

#Separtely create a many 2 many file that lists aircraft types, some aircraft having multiple types.
aircraft_type_m2m<-unique(engine_specs[,c("aircraft","type")])
write.csv(engine_specs_no_dupes,
          "inventory/data/aircraft_type_m2m.csv")



#*****Intro Year* and engne specs*****************
intro_year$aircraft<-factor(intro_year$aircraft)
if(any(duplicated(intro_year$aircraft))) stop ("Duplicate aircraft in intro_year")

engine <- usaf_inventory %>%
  left_join(engine_specs_no_dupes, by = "aircraft") %>%
  left_join(intro_year, by = "aircraft")
if (nrow(engine)!=nrow(usaf_inventory)) stop("Duplicates produced")

#*****Manual cleanup
engine$amount <- as.integer(as.character(engine$amount))


if(sum(engine$amount[engine$aircraft=="HC-130"])==0)
  engine<-subset(engine,aircraft!="HC-130")
levels(engine$engine_type)
engine$engine_type<-factor(engine$engine_type,c("Radial","Ramjet","Mixed","Turbofan","Turbojet","Turboprop","Turboshaft"))
engine$engine_type[engine$aircraft=="B-36" & is.na(engine$engine_type)]<-"Mixed"
engine$amount[engine$aircraft=="B-36" & is.na(engine$amount)]<-10

#***** Age


engine$intro_year <- as.integer(as.character(engine$intro_year))
engine$year <- as.integer(as.character(engine$year))
engine <- engine %>% group_by(aircraft) %>%
  mutate(min_year = min(ifelse(amount>0,year,NA),na.rm=TRUE))
write.csv(engine[engine$year<engine$first_year & engine$amount>0,],file="inventory\\data\\appear_before_intro_year.csv")

#Examining first years

engine$intro_year[engine$intro_year>engine$min_year]<-engine$min_year[engine$intro_year>engine$min_year]

if(sum(engine$amount[engine$year<engine$intro_year],na.rm = TRUE)>0) stop("Don't drop initial 0s yet")
engine<-subset(engine, amount > 0 | year>=intro_year)

engine <- engine %>%
  mutate(age = year - intro_year)

if(min(engine$age,na.rm = TRUE)<0) stop("Negative ages!")

write.csv(engine, "inventory/data/engine.csv")


engine_type <- engine %>%
  left_join(aircraft_type_m2m, by = "aircraft")


# summarize data -----------------------------------------------------------------

aircraft_total <- engine %>% group_by(aircraft) %>% summarise(total = sum(amount, na.rm = TRUE))
aircraft_total[aircraft_total$total==0,]
#Age of aircraft, regardless of type, since first introduction

#Total aircraft and total age per year
by_total <- engine %>%
  group_by(year) %>%
  summarise(total = sum(amount, na.rm = TRUE),
            total_age = sum(amount * age, na.rm = TRUE),
            avg_age = sum(amount * age, na.rm = TRUE) / sum(amount, na.rm = TRUE)
            )

# This shouldn't change the data, but it simplifies and removes a chance to introduce error.
#
# engine <- engine %>%
#   left_join(by_total, by = "year") %>%
#   mutate(total_age = amount * age / total)
# 
# by_total <- engine %>%
#   group_by(year)
# by_total <- by_total %>%
#   summarise(total_age = sum(total_age, na.rm = TRUE))

# charting =======================================================================
# total age-----------------------------------------------------------------------

(
  p_avg_age <- ggplot(data = by_total) +
    geom_area(
      aes(y = avg_age, x = year),
      stat = "identity",
      fill = "#333333",
      alpha = .90
    ) +
    ggtitle("Average platform age of the USAF inventory") +
    ylab("age") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    chart_theme
)

ggsave(
  "inventory/charts/average_age.svg",
  p_avg_age,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# total age by type --------------------------------------------------------------

# p <- engine_type %>%
#   group_by(year) %>%
#   summarise(total_amount = sum(amount, na.rm = TRUE))
# 
# p <- engine_type %>%
#   inner_join(p, by = "year")
# 
# p2 <- engine_type %>%
#   group_by(year, type) %>%
#   summarise(type_amount = sum(amount, na.rm = TRUE))
# 
# (
#   p_avg_age_type <- p %>%
#     left_join(p2, by = c("year", "type")) %>%
#     mutate(age_weight = age * amount / type_amount) %>%
#     group_by(year, type) %>%
#     summarise(average_age = sum(age_weight, na.rm = TRUE)) %>%
#     ggplot() +
#     geom_area(aes(y = average_age, x = year), stat = "identity") +
#     scale_x_continuous(
#       breaks = seq(1940, 2010, by = 20),
#       labels = function(x) {
#         substring(as.character(x), 3, 4)
#       }
#     ) +
#     facet_wrap(~ type, nrow = 1) +
#     # ylab("age") +
#     ylab(NULL) +
#     xlab(NULL) +
#     # ggtitle("Average platform age of the USAF inventory by type") +
#     chart_theme
# )

(
  p_avg_age_type <- engine_type  %>%
    group_by(year, type) %>%
    summarise(average_age = sum(age * amount,na.rm=TRUE) / 
                sum(amount,na.rm=TRUE)) %>%
    ggplot() +
    geom_area(aes(y = average_age, x = year), stat = "identity") +
    scale_x_continuous(
      breaks = seq(1940, 2010, by = 20),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    facet_wrap(~ type, nrow = 1) +
    # ylab("age") +
    ylab(NULL) +
    xlab(NULL) +
    # ggtitle("Average platform age of the USAF inventory by type") +
    chart_theme
)


ggsave(
  "inventory/charts/average_age_type.svg",
  p_avg_age_type,
  device = "svg",
  width = 10,
  height = 3,
  units = "in"
)

# plot USAF inventory # by engine type -------------------------------------------

by_engine_type <- engine %>%
  group_by(year, engine_type)

by_engine_type <- by_engine_type %>%
  filter(engine_type %in% c("Radial",
                            # "Mixed",
                            "Turbofan",
                            "Turbojet",
                            "Turboprop",
                            "Turboshaft")) %>%
  summarise(amount = sum(amount, na.rm = TRUE))

(
  p_type <- ggplot(data = by_engine_type) +
    geom_area(aes(
      y = amount, x = year, fill = engine_type
    ), stat = "identity") +
    ggtitle("USAF inventory amount by engine type") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    guides(fill = guide_legend(
      keywidth = 1,
      keyheight = 1,
      nrow = 5
    )) +
    chart_theme +
    scale_fill_manual(
      values = c(
        "Radial" = "#4D7FA3",
        # "Mixed" = "#AAAAAA", #Pick a color!
        "Turbofan" = "#C74745",
        "Turbojet" = "#0E9E87",
        "Turboprop" = "#566377",
        "Turboshaft" = "#F2BC57"
      )
    )+labs(caption="Note: Excludes ramjet and mixed engines as well as aircraft with inconsistent classification.")
)

ggsave(file="year and engine amount by engine type",
       p_type,
       device = "svg",
       width = 8,
       height = 6,
       units = "in"
       )

# plot number of engines ---------------------------------------------------------

engine <- engine %>%
  mutate(engine_amount = amount * engine_number)

p_engine <- engine %>%
  group_by(year, engine_type) %>%
  summarise(engine_amount = sum(engine_amount, na.rm = TRUE))

(
  p <- ggplot() +
    geom_area(
      aes(y = engine_amount, x = year, fill = engine_type),
      data = p_engine,
      stat = "identity"
    ) +
    ggtitle("USAF engine inventory by type") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    guides(fill = guide_legend(
      keywidth = 1,
      keyheight = 1,
      nrow = 5
    )) +
    chart_theme +
    scale_fill_manual(
      values = c(
        "Radial" = "#4D7FA3",
        "Turbofan" = "#C74745",
        "Turbojet" = "#0E9E87",
        "Turboprop" = "#566377",
        "Turboshaft" = "#F2BC57"
      )
    ) +
    ylab("amount")
)

ggsave(
  "inventory/charts/engine_amount.svg",
  p,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# plot number of engines by aircraft type ----------------------------------------

p <- engine %>%
  group_by(year, type) %>%
  filter(type != "Helicopter") %>%
  filter(type != "Trainer") %>%
  summarise(amount = sum(engine_amount, na.rm = TRUE))

(
  p <- ggplot(data = p) +
    geom_area(aes(y = amount, x = year), stat = "identity") +
    facet_wrap( ~ type, nrow = 1) +
    chart_theme +
    ylab(NULL) +
    xlab(NULL) +
    scale_x_continuous(
      breaks = seq(1940, 2010, by = 20),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "inventory/charts/engine_amount_type.svg",
  p,
  device = "svg",
  width = 10,
  height = 3,
  units = "in"
)

# introduction rate for USAF aircraft --------------------------------------------

introduction <- engine %>%
  group_by(aircraft, type) %>%
  summarize(intro_year = mean(intro_year, na.rm = TRUE)) %>%
  filter(intro_year >= 1950)

(
  p_introduction <-
    ggplot(introduction, aes(x = intro_year, fill = factor(type))) +
    geom_dotplot(
      stackgroups = TRUE,
      binwidth = 1.2,
      binpositions = "all",
      stackdir = "center"
    ) +
    ylab(NULL) +
    # axis.text.y = element_blank() +
    chart_theme +
    scale_x_continuous(
      breaks = seq(1940, 2010, by = 5),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    xlab("Year of Introduction")
)

# introduction rate dotplot ------------------------------------------------------

(
  p_introduction <- ggplot(introduction, aes(x = intro_year)) +
    geom_dotplot(
      stackgroups = TRUE,
      binwidth = .75,
      binpositions = "all",
      stackdir = "center",
      fill = "#333333",
      alpha = .9
    ) +
    ylab("# of New Platforms") +
    ggtitle("Introduction Rate for the USAF Inventory") + 
    # axis.text.y = element_blank() +
    chart_theme +
    scale_x_continuous(
      breaks = seq(1940, 2010, by = 5),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(), 
      panel.grid.minor.y = element_blank()
    )  +
    xlab("Year of Introduction")
)

ggsave(
  "inventory/charts/intro_year.svg",
  p_introduction,
  device = "svg",
  width = 8,
  height = 3.5,
  units = "in"
)

# introduction rate for USAF aircraft by type ------------------------------------

(
  p_introduction <- ggplot(introduction, aes(x = intro_year)) +
    geom_dotplot(
      stackgroups = TRUE,
      binwidth = 2.5,
      binpositions = "all",
      stackdir = "center",
      fill = "#333333",
      alpha = .9
    ) +
    ylab(NULL) +
    facet_wrap(~ type, scales = "free_y", nrow = 1) +
    # axis.text.y = element_blank() +
    chart_theme +
    scale_x_continuous(
      breaks = seq(1940, 2010, by = 20),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    ) +
    
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    xlab(NULL)
)

ggsave(
  "inventory/charts/intro_year_type.svg",
  p_introduction,
  device = "svg",
  width = 10,
  height = 2,
  units = "in"
)

# peak inventory for FighterAttack by generation ---------------------------------

engine <- engine %>%
  left_join(generation, by = "aircraft")

generation <- engine %>%
  filter(type == "FighterAttack") %>% 
  group_by(aircraft, intro_year, relevance, generation) %>%
  summarise(peak_inventory = mean(peak_inventory, na.rm = TRUE)) %>%
  filter(relevance != "Old") %>% 
  mutate(generation = factor(
    generation,
    levels = c("Other", 
               "First",
               "Second",
               "Third",
               "Fourth",
               "Fifth")))

(
  p_peak_inventory_generation <- ggplot(data = generation) +
    geom_point(
      mapping = aes(
        x = intro_year,
        y = peak_inventory,
        color = generation,
        shape = relevance
      ),
      size = 3
    ) +
    # facet_wrap( ~ generation, nrow = 3) +
    scale_color_manual(
      labels = c(
        "First" = "First",
        "Second" = "Second",
        "Third" = "Third",
        "Fourth" = "Fourth",
        "Fifth" = "Fifth", 
        "Other" = "Unlabeled"), 
      values = c(
        "Other" = "#CFCDC8",
        "First" = "#4D7FA3",
        "Second" = "#C74745",
        "Third" = "#0E9E87",
        "Fourth" = "#566377",
        "Fifth" = "#F2BC57"
      )
    ) + 
    scale_x_continuous(
      breaks = seq(1940, 2010, by = 10)
    ) + 
    scale_shape_manual(labels=c("In Production","Complete"),
                       values=c(1,19)) + 
    chart_theme +
    ggtitle(
      "Fighter Attack inventory by generation"
    ) +
    xlab("Year of Introduction") +
    ylab("Peak Inventory")
)

ggsave(
  "inventory/charts/peak_generation.svg",
  p_peak_inventory_generation,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# --------------------------------------------------------------------------------

inventory <- engine %>%
  filter(relevance != "Old") %>%
  group_by(aircraft, intro_year, relevance, generation, type, engine_type) %>%
  summarise(peak_inventory = mean(peak_inventory, na.rm = TRUE))

(
  p_peak_inventory <- ggplot(data = inventory) +
    geom_point(mapping = aes(
      x = intro_year, y = peak_inventory, color = relevance
    )) +
    facet_wrap(~ type, nrow = 1) +
    xlab("year of introduction") +
    ylab("peak inventory") +
    ggtitle("Peak inventory by aircraft type") +
    chart_theme +
    scale_color_manual(labels=c("In Production","Complete"),
                       values=c("#C74745", "#4D7FA3")) + 
    scale_x_continuous(
      breaks = seq(1940, 2010, by = 20),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )
)

ggsave(
  "inventory/charts/peak_inventory_type.svg",
  p_peak_inventory,
  device = "svg",
  width = 12,
  height = 4,
  units = "in"
)

# engine type chart --------------------------------------------------------------

inventory_engine_type <- engine %>%
  group_by(year, engine_type) %>%
  summarise(amount = sum(amount, na.rm = TRUE)) %>%
  filter(engine_type != "NA")

(
  p_engine_type <- ggplot() +
    geom_area(aes(y = amount, x = year), data = inventory_engine_type, stat =
                "identity") +
    facet_wrap( ~ engine_type, nrow = 3) +
    chart_theme
)

(
  p_engine_type <- ggplot() +
    geom_area(
      aes(y = amount, x = year, fill = engine_type),
      data = inventory_engine_type,
      position = "stack"
    ) +
    chart_theme
)

# generation chart ---------------------------------------------------------------

generation <- engine %>%
  group_by(year, generation) %>%
  summarise(amount = sum(amount, na.rm = TRUE)) 

(
  p_generation <- ggplot() +
    geom_area(
      aes(y = amount, x = year, fill = generation),
      data = generation,
      position = "stack"
    ) +
    guides(fill = guide_legend(
      keywidth = 1,
      keyheight = 1,
      nrow = 5
    )) +
    chart_theme
)

# --------------------------------------------------------------------------------
# plot number of engines by engine type ------------------------------------------

p <- engine %>%
  group_by(year, engine_type) %>%
  summarise(amount = sum(engine_amount, na.rm = TRUE)) %>%
  filter(engine_type != "NA")

(
  p <- ggplot(data = p) +
    geom_area(aes(
      y = amount, x = year, fill = engine_type
    ), position = "stack") +
    guides(fill = guide_legend(
      keywidth = 1,
      keyheight = 1,
      nrow = 5
    )) +
    chart_theme
)

# --------------------------------------------------------------------------------

(
  p <- engine %>%
    filter(type == "FighterAttack") %>% 
    group_by(year, generation) %>%
    summarise(amount = sum(engine_amount, na.rm = TRUE)) %>%
    mutate(generation = factor(
      generation,
      levels = c("Other", 
                 "First",
                 "Second",
                 "Third",
                 "Fourth",
                 "Fifth")
    )) %>%
    ggplot() +
    geom_area(aes(
      y = amount, x = year, fill = generation
    ), alpha = .9, position = "stack") +
    guides(fill = guide_legend(
      keywidth = 1,
      keyheight = 1,
      nrow = 6
    )) +
    scale_x_continuous(
      breaks = seq(1940, 2010, by = 10)
    ) + 
    scale_fill_manual(
      labels = c(
        "Other" = "Unlabeled",
        "First" = "First",
        "Second" = "Second",
        "Third" = "Third",
        "Fourth" = "Fourth",
        "Fifth" = "Fifth"), 
      values = c(
        "Other" = "#A6A5A1",
        "First" = "#4D7FA3",
        "Second" = "#C74745",
        "Third" = "#0E9E87",
        "Fourth" = "#566377",
        "Fifth" = "#F2BC57"
      )
    ) + 
    ggtitle("FighterAttack inventory by generation") +
    chart_theme
)

ggsave(
  "inventory/charts/inventory_generation.svg",
  p,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# plot average aircraft specs for USAF fighter/attack aircraft ===================
# takeoff weight -----------------------------------------------------------------

p <- engine %>%
  filter(type == "FighterAttack") %>%
  group_by(year) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE))

p <- engine %>%
  inner_join(p, by = "year")

(
  p_takeoff_weight <- p %>%
    mutate(age_weight = takeoff_weight * amount / total_amount) %>%
    group_by(year) %>%
    summarise(takeoff_weight = sum(age_weight, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(
      aes(y = takeoff_weight, x = year),
      stat = "identity",
      alpha = .90
    ) +
    chart_theme +
    ylab("takeoff weight (lbs)") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    ggtitle("Average takeoff weight for USAF fighter/attack aircraft")
)

ggsave(
  "inventory/charts/takeoff_weight.svg",
  p_takeoff_weight,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# speed --------------------------------------------------------------------------

(
  p_speed <- p %>%
    mutate(age_weight = speed * amount / total_amount) %>%
    group_by(year) %>%
    summarise(speed = sum(age_weight, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = speed, x = year), stat = "identity", alpha = .90) +
    chart_theme +
    ylab("speed (mph)") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    ggtitle("Average speed for USAF fighter/attack aircraft")
)
ggsave(
  "inventory/charts/speed.svg",
  p_speed,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# range --------------------------------------------------------------------------

(
  p_range <- p %>%
    mutate(age_weight = range * amount / total_amount) %>%
    group_by(year) %>%
    summarise(range = sum(age_weight, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = range, x = year), stat = "identity", alpha = .90) +
    chart_theme +
    ylab("range (mi)") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    ggtitle("Average range for USAF fighter/attack aircraft")
)

ggsave(
  "inventory/charts/range.svg",
  p_range,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# ceiling ------------------------------------------------------------------------

(
  p_ceiling <- p %>%
    mutate(age_weight = ceiling * amount / total_amount) %>%
    group_by(year) %>%
    summarise(ceiling = sum(age_weight, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = ceiling, x = year), stat = "identity", alpha = .90) +
    chart_theme +
    ylab("ceiling (ft)") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    ggtitle("Average ceiling for USAF fighter/attack aircraft")
)

ggsave(
  "inventory/charts/ceiling.svg",
  p_ceiling,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# climb rate ---------------------------------------------------------------------

(
  p_climb_rate <- p %>%
    mutate(age_weight = climb_rate * amount / total_amount) %>%
    group_by(year) %>%
    summarise(climb_rate = sum(age_weight, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = climb_rate, x = year), stat = "identity", alpha = .90) +
    chart_theme +
    ylab("climb rate (ft/min)") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    ggtitle("Average climb rate for USAF fighter/attack aircraft")
)

ggsave(
  "inventory/charts/climb.svg",
  p_climb_rate,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# trust to weight ----------------------------------------------------------------

(
  p_thrust_weight_aircraft <- p %>%
    mutate(age_weight = thrust_weight_aircraft * amount / total_amount) %>%
    group_by(year) %>%
    summarise(thrust_weight_aircraft = sum(age_weight, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(
      aes(y = thrust_weight_aircraft, x = year),
      stat = "identity",
      alpha = .90
    ) +
    chart_theme +
    ylab("thrust to weight ratio") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    ggtitle("Average thrust to weight ratio for USAF fighter/attack aircraft")
)

ggsave(
  "inventory/charts/thrust_weight_ratio.svg",
  p_thrust_weight_aircraft,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# plot average engine specs for USAF fighter/attack jet engines ==================
# thrust -------------------------------------------------------------------------

p <- engine %>%
  filter(type == "FighterAttack") %>%
  filter(engine_type == "Turbojet" | engine_type == "Turbofan") %>%
  group_by(year) %>%
  summarise(total_amount = sum(engine_amount, na.rm = TRUE))

p <- engine %>%
  inner_join(p, by = "year")

(
  p_thrust <- p %>%
    mutate(age_weight = thrust * engine_amount / total_amount) %>%
    group_by(year) %>%
    summarise(average_age = sum(age_weight, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = average_age, x = year), stat = "identity", alpha = .90) +
    chart_theme +
    ylab("thrust (lbs)") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    ggtitle("Average thrust for USAF fighter/attack jet engines")
)

ggsave(
  "inventory/charts/thrust.svg",
  p_thrust,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# pressure ratio -----------------------------------------------------------------

(
  p_pressure_ratio <- p %>%
    mutate(age_weight = pressure_ratio * engine_amount / total_amount) %>%
    group_by(year) %>%
    summarise(pressure_ratio = sum(age_weight, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(
      aes(y = pressure_ratio, x = year),
      stat = "identity",
      alpha = .90
    ) +
    chart_theme +
    ylab("pressure ratio") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    ggtitle("Average pressure ratio for USAF fighter/attack jet engines")
)

ggsave(
  "inventory/charts/pressure.svg",
  p_pressure_ratio,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# engine weight ------------------------------------------------------------------

(
  p_engine_weight <- p %>%
    mutate(age_weight = engine_weight * engine_amount / total_amount) %>%
    group_by(year) %>%
    summarise(engine_weight = sum(age_weight, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(
      aes(y = engine_weight, x = year),
      stat = "identity",
      alpha = .90
    ) +
    chart_theme +
    ylab("engine weight (lbs)") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    ggtitle("Average engine weight for USAF fighter/attack jet engines")
)

ggsave(
  "inventory/charts/engine_weight.svg",
  p_engine_weight,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# thrust to weight ---------------------------------------------------------------

(
  p_thrust_weight_engine <- p %>%
    mutate(age_weight = thrust_weight_engine * engine_amount / total_amount) %>%
    group_by(year) %>%
    summarise(thrust_weight_engine = sum(age_weight, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(
      aes(y = thrust_weight_engine, x = year),
      stat = "identity",
      alpha = .90
    ) +
    chart_theme +
    ylab("thrust to weight ratio") +
    scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
    ggtitle(
      "Average thrust to weight ratio for USAF fighter/attack jet engines"
    )
)

ggsave(
  "inventory/charts/thrust_weight_ratio_engine.svg",
  p_thrust_weight_engine,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# ================================================================================