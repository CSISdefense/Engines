# ================================================================================
# The Future of Military Engines
# By Gabriel Coll
# --------------------------------------------------------------------------------
# contract analysis
# ================================================================================


load(file="contracts/app/engine_contract.Rdata")
# ================================================================================
# charting engines 
# --------------------------------------------------------------------------------
# engine contracts 

(
  total <- engine_contracts %>%
    group_by(Fiscal.Year) %>%
    dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount_OMB19_19, x = Fiscal.Year), alpha = .9 , stat = "identity") +
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

(
  Vendor.Size <- engine_contracts %>%
    filter(Vendor.Size != "Unlabeled") %>%
    group_by(Fiscal.Year, Vendor.Size) %>%
    dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount_OMB19_19, x = Fiscal.Year), alpha = .9, stat = "identity") +
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
sum((engine_contracts %>% filter(Competition.multisum == "Unlabeled"))$amount_OMB19_19 )

# engine contracts by CompetitionClassification  
(
  Competiton <- engine_contracts %>%
    filter(Competition.multisum != "Unlabeled") %>%
    group_by(Fiscal.Year, Competition.multisum) %>%
    dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
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
    geom_area(aes(y = amount_OMB19_19, x = Fiscal.Year), alpha = .9, stat = "identity") +
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


(
  Competiton_Share <- engine_contracts %>%
    # filter(Competition.multisum != "Unlabeled") %>%
    # group_by(Fiscal.Year, Competition.multisum,SimpleArea) %>%
    group_by(Fiscal.Year, Competition.multisum) %>%
    dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
    # group_by(Fiscal.Year, SimpleArea) %>%#Competition.multisum,
    group_by(Fiscal.Year) %>%#Competition.multisum,
    mutate(
      amount_share = amount_OMB19_19/ sum(amount_OMB19_19, na.rm = TRUE),
      Competiton = factor(
      Competition.multisum,
      levels=c("No Comp.",
               "1 Offer"   ,
               "2+ Offers" ,
               "Unlabeled"),
      labels = c(
        "No Competition",
        "Competition with single offer",
        "Effective Competition",
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
    geom_line(aes(y = amount_share, 
                  x = Fiscal.Year,
                  linetype=Competiton)) +#, alpha = .9, stat = "identity"
    # facet_wrap(~ SimpleArea, nrow = 1) +#Competiton
    chart_theme +
    ggtitle("DoD Aircraft Engine Contract Obligations\nby Extent of Competition") +
    xlab("Fiscal Year") +
    ylab("Percent of Contract Obligations") +
    # scale_y_continuous(labels = money_labels) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )+theme(legend.position = "bottom")+
    guides(linetype=guide_legend(nrow=2))+
             scale_linetype_manual(values=c("solid","twodash","dashed","dotted"))
)


ggsave(
  "contracts/charts/share_competition.svg",
  Competiton_Share,
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
    dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
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
    geom_area(aes(y = amount_OMB19_19, x = Fiscal.Year), alpha = .9, stat = "identity") +
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
  dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  as.data.frame(.)

total_category <- engine_contracts %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
  as.data.frame(.)

total <- total %>%
  rbind(total_category) %>%
  mutate(SubCustomer = "Total")

army <- engine_contracts %>%
  filter(SubCustomer == "Army") %>%
  group_by(Fiscal.Year) %>%
  dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  as.data.frame(.)

army_category <- engine_contracts %>%
  filter(SubCustomer == "Army") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
  as.data.frame(.)

army <- army %>%
  rbind(army_category) %>%
  mutate(SubCustomer = "Army")

navy <- engine_contracts %>%
  group_by(Fiscal.Year) %>%
  filter(SubCustomer == "Navy") %>%
  dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

navy_category <- engine_contracts %>%
  filter(SubCustomer == "Navy") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
  as.data.frame(.)

navy <- navy %>%
  rbind(navy_category) %>%
  mutate(SubCustomer = "Navy")

air_force <- engine_contracts %>%
  group_by(Fiscal.Year) %>%
  filter(SubCustomer == "Air Force") %>%
  dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

air_force_category <- engine_contracts %>%
  filter(SubCustomer == "Air Force") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
  as.data.frame(.)

air_force <- air_force %>%
  rbind(air_force_category) %>%
  mutate(SubCustomer = "Air Force")

dla <- engine_contracts %>%
  group_by(Fiscal.Year) %>%
  filter(SubCustomer == "DLA") %>%
  dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

dla_category <- engine_contracts %>%
  filter(SubCustomer == "DLA") %>%
  group_by(Fiscal.Year, SimpleArea) %>%
  dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE)) %>%
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
    geom_area(aes(x = Fiscal.Year, y = amount_OMB19_19), alpha = .9) +
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
#   select(Fiscal.Year, SubCustomer, SimpleArea, amount_OMB19_19, type)

comparison_contracts <- engine_contracts %>%
  select(Fiscal.Year, SubCustomer, SimpleArea, amount_OMB19_19, type) %>%
  rbind(topline_contracts %>%
          select(Fiscal.Year, SubCustomer, SimpleArea, amount_OMB19_19, type)) %>%
  group_by(Fiscal.Year, SubCustomer, SimpleArea, type) %>%
  dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE))

dyear <- comparison_contracts %>%
  group_by() %>%
  dplyr::rename(amount_OMB19_19.lagged = amount_OMB19_19) %>%
  mutate(Fiscal.Year = Fiscal.Year + 1) # %>%
# select(-fyb)

# comparison_contracts <- comparison_contracts %>% filter(Fiscal.Year >= 2001)
#Add lagged year
sumcheck<-sum(comparison_contracts$amount_OMB19_19,na.rm=TRUE)
comparison_contracts <- comparison_contracts %>%
  left_join(dyear, by = c("Fiscal.Year", "SubCustomer", "SimpleArea", "type"))# %>%
if(sumcheck!=sum(comparison_contracts$amount_OMB19_19,na.rm=TRUE)) stop("Sum Check Error")


dyear <- comparison_contracts %>%
  filter(Fiscal.Year==2000) %>%
  dplyr::rename(amount_OMB19_19.2000 = amount_OMB19_19) %>%
  group_by()%>%
  select(-amount_OMB19_19.lagged,-Fiscal.Year)

#Add 2000
sumcheck<-sum(comparison_contracts$amount_OMB19_19,na.rm=TRUE)
comparison_contracts <- comparison_contracts %>%
  left_join(dyear, by = c("SubCustomer", "SimpleArea", "type")) %>%
  mutate(amount_change = amount_OMB19_19 - amount_OMB19_19.lagged,
         baseline_change = amount_OMB19_19 - amount_OMB19_19.2000) #%>% 
if(sumcheck!=sum(comparison_contracts$amount_OMB19_19,na.rm=TRUE)) stop("Sum Check Error")





rm(dyear)

(
  comparison_contracts_overall <- comparison_contracts %>%
    group_by(Fiscal.Year, type) %>%
    dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE),
                     amount_OMB19_19.lagged = sum(amount_OMB19_19.lagged, na.rm = TRUE),
                     amount_change = sum(amount_change, na.rm = TRUE),
                     amount_OMB19_19.2000 = sum(amount_OMB19_19.2000, na.rm = TRUE),
                     baseline_change = sum(baseline_change, na.rm = TRUE)) %>%
    mutate(amount_percent_change = (amount_change) / amount_OMB19_19.lagged,
           baseline_percent_change = (baseline_change) / amount_OMB19_19.2000) #%>%
  # dplyr::rename(amount_OMB19_19 = amount_OMB19_19) %>%
  # select(Fiscal.Year, amount_OMB19_19, amount_change, amount_percent_change, type) #%>%
  # This has already been done, which is good, as I don't htink it makes sense to sum percent changes like that.
  # group_by(Fiscal.Year, type) %>%
  # dplyr::summarise(
  #   amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE),
  #   amount_change = sum(amount_change, na.rm = TRUE),
  #   amount_percent_change = sum(amount_percent_change, na.rm = TRUE)
  # )
)


# --------------------------------------------------------------------------------
# percent change comparison 
# 
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
    ylab("Cumulative Percent Change, Inflation Adjusted (2000=0%)") +
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



(
  comparison_contracts_subcustomer <- comparison_contracts %>%
    group_by(Fiscal.Year, SubCustomer, type) %>%
    dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE),
                     amount_OMB19_19.lagged = sum(amount_OMB19_19.lagged, na.rm = TRUE),
                     amount_change = sum(amount_change, na.rm = TRUE),
                     amount_OMB19_19.2000 = sum(amount_OMB19_19.2000, na.rm = TRUE),
                     baseline_change = sum(baseline_change, na.rm = TRUE)) %>%
    mutate(amount_percent_change = (amount_change) / amount_OMB19_19.lagged,
           baseline_percent_change = (baseline_change) / amount_OMB19_19.2000) #%>%
)

levels(factor(comparison_contracts_subcustomer$type))
max((comparison_contracts_subcustomer %>% 
       filter(SubCustomer=="Other DoD" &
                type =="Engines"))$amount_OMB19_19
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
    ylab("Cumulative Percent Change, Inflation Adjusted (2000=0%)") +
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


(
  comparison_contracts_area <- comparison_contracts %>%
    group_by(Fiscal.Year, SimpleArea, type) %>%
    dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE),
                     amount_OMB19_19.lagged = sum(amount_OMB19_19.lagged, na.rm = TRUE),
                     amount_change = sum(amount_change, na.rm = TRUE),
                     amount_OMB19_19.2000 = sum(amount_OMB19_19.2000, na.rm = TRUE),
                     baseline_change = sum(baseline_change, na.rm = TRUE)) %>%
    mutate(amount_percent_change = (amount_change) / amount_OMB19_19.lagged,
           baseline_percent_change = (baseline_change) / amount_OMB19_19.2000) #%>%
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
    ylab("Cumulative Percent Change, Inflation Adjusted (2000=0%)") +
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

# -----------------------------------------------------------------------------------
# Product / R&D and topline.



  comparison_contracts_total <-
    comparison_contracts %>%
    group_by(Fiscal.Year, type) %>%
    dplyr::summarise(amount_OMB19_19 = sum(amount_OMB19_19, na.rm = TRUE),
                     amount_OMB19_19.lagged = sum(amount_OMB19_19.lagged, na.rm = TRUE),
                     amount_change = sum(amount_change, na.rm = TRUE),
                     amount_OMB19_19.2000 = sum(amount_OMB19_19.2000, na.rm = TRUE),
                     baseline_change = sum(baseline_change, na.rm = TRUE)) %>%
    mutate(amount_percent_change = (amount_change) / amount_OMB19_19.lagged,
           baseline_percent_change = (baseline_change) / amount_OMB19_19.2000) #%>%
comparison_contracts_total$SimpleArea<-"Total"
(comparison_contracts_area_total<-rbind(comparison_contracts_total,
                                       comparison_contracts_area)

)


summary(comparison_contracts_area_total$SimpleArea)
(
  graph_contracts_overall_area <- comparison_contracts_area_total %>%
    filter(!is.na(SimpleArea) & !SimpleArea %in% c("Services","Unlabeled")) %>%
    mutate(type = factor(type, levels = c("Topline", "Engines"))) %>%
    ggplot() +
    geom_line(
      aes(x = Fiscal.Year, 
          y = baseline_percent_change,
          linetype=SimpleArea),
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
    ylab("Cumulative Percent Change, Inflation Adjusted (2000=0%)") +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(
      breaks = seq(2000, 2020, by = 2),
      labels = function(x) {
        substring(as.character(x), 3, 4)
      }
    )+scale_linetype_manual(
      values=c("solid","dotted","dashed")
    )
)

ggsave(
  "contracts/charts/percent_change_total_area.svg",
  graph_contracts_overall_area,
  width = 9,
  height = 6,
  units = "in",
  device = "svg"
)
