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

# theme --------------------------------------------------------------------------

source("chart_theme.R")

# read data ----------------------------------------------------------------------

intro_year <- read_csv("intro_year.csv")
usaf_inventory <- read_csv("usaf_inventory.csv")
engine_specs <- read_csv("engine_specs.csv")
generation <- read_csv("generation.csv")
relevance <- read_csv("relevance.csv")

intro_year <- intro_year %>%
  .[-1,] %>%
  gather(aircraft, intro_year,-year) %>%
  .[,-1]

usaf_inventory[is.na(usaf_inventory)] <- 0

usaf_inventory <- gather(usaf_inventory, aircraft, amount,-year)

engine <- usaf_inventory %>%
  inner_join(engine_specs, by = "aircraft") %>%
  left_join(intro_year, by = "aircraft")

write.csv(engine, "engine.csv")

# summarize data -----------------------------------------------------------------

engine$amount <- as.integer(as.character(engine$amount))
engine$intro_year <- as.integer(as.character(engine$intro_year))

engine <- engine %>%
  mutate(age = year - intro_year)

by_total <- engine %>%
  group_by(year) %>%
  summarise(total = sum(amount, na.rm = TRUE))

engine <- engine %>%
  left_join(by_total, by = "year") %>%
  mutate(total_age = amount * age / total)

by_total <- engine %>%
  group_by(year)
by_total <- by_total %>%
  summarise(total_age = sum(total_age, na.rm = TRUE))

# charting =======================================================================
# total age-----------------------------------------------------------------------

(
  p_total_age <- ggplot(data = by_total) +
    geom_area(
      aes(y = total_age, x = year),
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
  "charts/average_age.svg",
  p_total_age,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# total age by type --------------------------------------------------------------

p <- engine %>%
  group_by(year) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE))

p <- engine %>%
  inner_join(p, by = "year")

p2 <- engine %>%
  group_by(year, type) %>%
  summarise(type_amount = sum(amount, na.rm = TRUE))

(
  p_total_age_type <- p %>%
    left_join(p2, by = c("year", "type")) %>%
    mutate(age_weight = age * amount / type_amount) %>%
    group_by(year, type) %>%
    summarise(average_age = sum(age_weight, na.rm = TRUE)) %>%
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
  "charts/average_age_type.svg",
  p_total_age_type,
  device = "svg",
  width = 10,
  height = 3,
  units = "in"
)

# plot USAF inventory # by engine type -------------------------------------------

by_type <- engine %>%
  group_by(year, engine_type)

by_type <- by_type %>%
  filter(engine_type %in% c("Radial",
                            "Turbofan",
                            "Turbojet",
                            "Turboprop",
                            "Turboshaft")) %>%
  summarise(amount = sum(amount, na.rm = TRUE))

(
  p_type <- ggplot(data = by_type) +
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
        "Turbofan" = "#C74745",
        "Turbojet" = "#0E9E87",
        "Turboprop" = "#566377",
        "Turboshaft" = "#F2BC57"
      )
    )
)

# ggsave("charts/USAF inventory amount by engine type.svg", p_type,
#        device = "svg", width = 8, height = 6, units = "in")

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
  "charts/engine_amount.svg",
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
  "charts/engine_amount_type.svg",
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

# ggsave("charts/dotplot intro year, aircraft type.svg", p_introduction,
#        device = "svg", width = 8, height = 6, units = "in")


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
  "charts/intro_year.svg",
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
  "charts/intro_year_type.svg",
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
  # filter(generation != "Other") %>%
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
      "Figther Attack inventory by generation"
    ) +
    xlab("Year of Introduction") +
    ylab("Peak Inventory")
)

ggsave(
  "charts/peak_generation.svg",
  p_peak_inventory_generation,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# --------------------------------------------------------------------------------

inventory <- engine %>%
  filter(relevance != "Old") %>%
  # filter(engine_type != "NA") %>%
  group_by(aircraft, intro_year, relevance, generation, type, engine_type) %>%
  summarise(peak_inventory = mean(peak_inventory, na.rm = TRUE))

(
  p_peak_inventory <- ggplot(data = inventory) +
    geom_point(mapping = aes(
      x = intro_year, y = peak_inventory, color = relevance
    )) +
    # geom_vline(mapping = aes(xintercept = intro_year), color = "#554449") +
    facet_wrap(~ type, nrow = 1) +
    # theme_fivethirtyeight()
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
  "charts/peak_inventory_type.svg",
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

# ggsave("charts/inventory engine type area color.svg", p_engine_type,
#        device = "svg", width = 8, height = 6, units = "in")

# generation chart ---------------------------------------------------------------

generation <- engine %>%
  group_by(year, generation) %>%
  summarise(amount = sum(amount, na.rm = TRUE)) 
  # filter(generation != "Other")

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

# ggsave("charts/year and amout by generation area color.svg", p_generation,
#        device = "svg", width = 8, height = 6, units = "in")

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

ggsave(
  "charts/year and engine amount by engine type.svg",
  p,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

# --------------------------------------------------------------------------------

(
  p <- engine %>%
    filter(type == "FighterAttack") %>% 
    group_by(year, generation) %>%
    summarise(amount = sum(engine_amount, na.rm = TRUE)) %>%
    # filter(generation != "Other") %>%
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
  "charts/inventory_generation.svg",
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
  "charts/takeoff_weight.svg",
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
  "charts/speed.svg",
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
  "charts/range.svg",
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
  "charts/ceiling.svg",
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
  "charts/climb.svg",
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
  "charts/thrustweightratio.svg",
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
  "charts/thrust.svg",
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
  "charts/pressure.svg",
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
  "charts/engine_weight.svg",
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
  "charts/thrustweightratio_engine.svg",
  p_thrust_weight_engine,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

