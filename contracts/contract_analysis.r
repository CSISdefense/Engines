# ================================================================================
# The Future of Military Engines
# Created by Gabriel Coll 
# Maintained by Greg Sanders
# --------------------------------------------------------------------------------
# contract analysis
# ================================================================================
library(tidyverse)
library(csis360)
source("contracts/theme/chart_theme.R")
source("contracts/theme/money_labels.R")

load(file="contracts/app/engine_contract.Rdata")
# ================================================================================
# charting engines 
# --------------------------------------------------------------------------------
# engine contracts 

(
  total <- engine_contracts %>%
    group_by(Fiscal_Year) %>%
    dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = Action_Obligation_OMB23_GDP21, x = Fiscal_Year), alpha = .9 , stat = "identity") +
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
    ylab("Constant 2021 $")
)

ggsave(
  "contracts/charts/amount_total.svg",
  total,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)



write.csv(total$data %>%
            spread(key=Fiscal_Year, value=Action_Obligation_OMB23_GDP21),
          file="contracts/charts/amount_total.csv",row.names = FALSE)


# --------------------------------------------------------------------------------
# engine contracts by Vendor Size 

(
  Shiny.VendorSize <- engine_contracts %>%
    filter(Shiny.VendorSize != "Unlabeled") %>%
    group_by(Fiscal_Year, Shiny.VendorSize) %>%
    dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = Action_Obligation_OMB23_GDP21, x = Fiscal_Year), alpha = .9, stat = "identity") +
    facet_wrap(~ Shiny.VendorSize, nrow = 2) +
    chart_theme +
    ggtitle("DoD Aircraft Engine Contract Obligations by Vendor Size") +
    xlab("Fiscal Year") +
    ylab("Constant 2021 $") +
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
  Shiny.VendorSize,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)

write.csv(Shiny.VendorSize$data,# %>%
            # spread(key=Fiscal_Year, value=Action_Obligation_OMB23_GDP21),
          file="contracts/charts/amount_vendor_size.csv",row.names = FALSE)

# --------------------------------------------------------------------------------
sum((engine_contracts %>% filter(Competition.multisum == "Unlabeled"))$Action_Obligation_OMB23_GDP21 )

# engine contracts by CompetitionClassification  
(
  Competition <- engine_contracts %>%
    filter(Competition.multisum != "Unlabeled") %>%
    group_by(Fiscal_Year, Competition.multisum) %>%
    dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
    mutate(Competition = factor(
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
    geom_area(aes(y = Action_Obligation_OMB23_GDP21, x = Fiscal_Year), alpha = .9, stat = "identity") +
    facet_wrap(~ Competition, nrow = 1) +
    chart_theme +
    ggtitle("DoD Aircraft Engine Contract Obligations\nby Extent of Competition") + 
    xlab("Fiscal Year") +
    ylab("Constant 2021 $") +
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
  Competition,
  device = "svg",
  width = 8,
  height = 4.25,
  units = "in"
)




write.csv(Competition$data,# %>%
            # spread(key=Fiscal_Year, value=Action_Obligation_OMB23_GDP21),
          file="contracts/charts/amount_competition.csv",row.names = FALSE)





(
  Competition_Share <- engine_contracts %>%
    # filter(Competition.multisum != "Unlabeled") %>%
    # group_by(Fiscal_Year, Competition.multisum,SimpleArea) %>%
    group_by(Fiscal_Year, Competition.multisum) %>%
    dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
    # group_by(Fiscal_Year, SimpleArea) %>%#Competition.multisum,
    group_by(Fiscal_Year) %>%#Competition.multisum,
    mutate(
      amount_share = Action_Obligation_OMB23_GDP21/ sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE),
      Competition = factor(
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
                  x = Fiscal_Year,
                  linetype=Competition)) +#, alpha = .9, stat = "identity"
    # facet_wrap(~ SimpleArea, nrow = 1) +#Competition
    chart_theme +
    scale_y_continuous(label=scales::percent_format(accuracy = 1)) +#,breaks=c(-0.5,0,0.5,1,1.5)
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
  Competition_Share,
  device = "svg",
  width = 5.5,
  height = 4,
  units = "in"
)

write.csv(Competition_Share$data,# %>%
            # spread(key=Fiscal_Year, value=amount_share),
          file="contracts/charts/share_competition.csv",row.names = FALSE)


#----------------------------------------------
#Simple Area
labels_and_colors<-prepare_labels_and_colors(engine_contracts %>% select(-PricingMechanism))
column_key<-get_column_key(engine_contracts)
(
  SimpleArea<-build_plot(
    data=engine_contracts ,
    chart_geom = "Bar Chart",
    share = FALSE,
    labels_and_colors=labels_and_colors,
    # NA, #VAR.ncol
    x_var="Fiscal_Year", #x_var
    y_var="Action_Obligation_OMB23_GDP21", #VAR.y.variable
    color_var="SimpleArea", #color_var
    facet_var="SimpleArea", #facet_var
    column_key=column_key,
    format=TRUE,
    ytextposition=FALSE
  )+
    # theme(strip.text.y = element_text(angle=0))+
    # theme(axis.text.x = element_text(angle=90))+
    #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
    #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
    #             )+labs(title=NULL,
    # x="Fiscal Year",
    # y="Percent of Obligations",
    # color="Competition")+
    theme(legend.position = "right")+
    labs(y="Obligations (Constant 2021 $s), Variable Scale")+
    facet_grid(SimpleArea~.,scales="free_y")
)

ggsave600dpi(SimpleArea,file="contracts/charts/SimpleArea.png",width=5,height=6,size = 11)

#-----------------------------------------------------------------------
# R&D phase


# 
# (
#   RnDphase<-build_plot(
#     data=engine_contracts %>% filter(SimpleArea=="R&D"),
#     chart_geom = "Bar Chart",
#     share = FALSE,
#     labels_and_colors=labels_and_colors,
#     # NA, #VAR.ncol
#     x_var="Fiscal_Year", #x_var
#     y_var="Action_Obligation_OMB23_GDP21", #VAR.y.variable
#     color_var="ProductServiceOrRnDarea", #color_var
#     # facet_var="Competition.sum", #facet_var
#     column_key=column_key,
#     format=TRUE,
#     ytextposition=FALSE
#   )+
#     # theme(strip.text.y = element_text(angle=0))+
#     # theme(axis.text.x = element_text(angle=90))+
#     #    scale_y_continuous("Percent of Obligations", labels = percent_format(accuracy=1))+
#     #      facet_grid(.~Competition.sum ,scales="free_x", space="free_x"
#     #             )+labs(title=NULL,
#     # x="Fiscal Year",
#     # y="Percent of Obligations",
#     # color="Competition")+
#     theme(legend.position = "right")+
#     labs(y="Obligations (Constant 2021 $s)")
# )
# 
# ggsave600dpi("..//Output//psr_RnDPhase.png", RnDphase, 
#              width=12, height= 6, units="in",size=12, lineheight=1.2
# )
# 
# 
# write.csv(file="..//Output//psr_RnDPhase.csv",row.names = FALSE, na = "",
#           pivot_wider(RnDphase$data,id_cols=c(ProductServiceOrRnDarea),
#                       names_from=Fiscal_Year,values_from=Action_Obligation_OMB23_GDP21)%>%
#             arrange(ProductServiceOrRnDarea))

# 
# output_TY<-engine_contracts %>% filter(SimpleArea=="R&D") %>%
#   format_data_for_plot(fy_var="Fiscal_Year",y_var="Action_Obligation_Then_Year",color_var = "ProductServiceOrRnDarea",
#                        labels_and_colors=labels_and_colors) %>%
#   pivot_wider(id_cols=c(ProductServiceOrRnDarea),
#               names_from=Fiscal_Year,values_from=Action_Obligation_Then_Year)%>%
#   arrange(ProductServiceOrRnDarea)
# 
# write.csv(file="..//Output//psr_RnDPhase_current.csv",row.names = FALSE, na = "",
#           output_TY)
# 
# wb <- loadWorkbook("..//Output//DoD_Acq_Trends_Contracts.xlsx", create = TRUE)
# createSheet(wb, name = "R&D")
# writeWorksheet(wb, output_TY, sheet = "R&D", startRow = 15, startCol = 13)
# saveWorkbook(wb)


# --------------------------------------------------------------------------------
# engine contracts by Contract Type 
levels(factor(engine_contracts$PricingMechanism))
(
  PricingMechanism <- engine_contracts %>%
    filter(PricingMechanism != "Other") %>%
    group_by(Fiscal_Year, PricingMechanism) %>%
    dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
    mutate(PricingMechanism = factor(
      PricingMechanism,
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
    geom_area(aes(y = Action_Obligation_OMB23_GDP21, x = Fiscal_Year), alpha = .9, stat = "identity") +
    facet_wrap(~ PricingMechanism, nrow = 1) +
    chart_theme +
    ggtitle("DoD Aircraft Engine Contract Obligations by Contract Type") +
    xlab("Fiscal Year") +
    ylab("Constant 2021 $") +
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
  PricingMechanism,
  device = "svg",
  width = 12,
  height = 4,
  units = "in"
)

write.csv(PricingMechanism$data,# %>%
            # spread(key=Fiscal_Year, value=Action_Obligation_OMB23_GDP21),
          file="contracts/charts/amount_contract_type.csv",row.names = FALSE)


# --------------------------------------------------------------------------------
# super facet: engine contracts by service and SimpleArea

#This could be done in fewer steps but on skimming it seems to check out.

total <- engine_contracts %>%
  group_by(Fiscal_Year) %>%
  dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  as.data.frame(.)

total_category <- engine_contracts %>%
  group_by(Fiscal_Year, SimpleArea) %>%
  dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
  as.data.frame(.)

total <- total %>%
  rbind(total_category) %>%
  mutate(SubCustomer = "Total")

army <- engine_contracts %>%
  filter(SubCustomer == "Army") %>%
  group_by(Fiscal_Year) %>%
  dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  as.data.frame(.)

army_category <- engine_contracts %>%
  filter(SubCustomer == "Army") %>%
  group_by(Fiscal_Year, SimpleArea) %>%
  dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
  as.data.frame(.)

army <- army %>%
  rbind(army_category) %>%
  mutate(SubCustomer = "Army")

navy <- engine_contracts %>%
  group_by(Fiscal_Year) %>%
  filter(SubCustomer == "Navy") %>%
  dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

navy_category <- engine_contracts %>%
  filter(SubCustomer == "Navy") %>%
  group_by(Fiscal_Year, SimpleArea) %>%
  dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
  as.data.frame(.)

navy <- navy %>%
  rbind(navy_category) %>%
  mutate(SubCustomer = "Navy")

air_force <- engine_contracts %>%
  group_by(Fiscal_Year) %>%
  filter(SubCustomer == "Air Force") %>%
  dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

air_force_category <- engine_contracts %>%
  filter(SubCustomer == "Air Force") %>%
  group_by(Fiscal_Year, SimpleArea) %>%
  dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
  as.data.frame(.)

air_force <- air_force %>%
  rbind(air_force_category) %>%
  mutate(SubCustomer = "Air Force")

dla <- engine_contracts %>%
  group_by(Fiscal_Year) %>%
  filter(SubCustomer == "DLA") %>%
  dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
  mutate(SimpleArea = "Total") %>%
  mutate(SimpleArea = as.factor(SimpleArea)) %>%
  as.data.frame(.)

dla_category <- engine_contracts %>%
  filter(SubCustomer == "DLA") %>%
  group_by(Fiscal_Year, SimpleArea) %>%
  dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE)) %>%
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
    geom_area(aes(x = Fiscal_Year, y = Action_Obligation_OMB23_GDP21), alpha = .9) +
    facet_grid(SubCustomer ~ SimpleArea) +
    chart_theme +
    xlab("Fiscal Year") +
    ylab("Constant 2021 $") +
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

write.csv(super_facet$data %>%
            spread(key=Fiscal_Year, value=Action_Obligation_OMB23_GDP21),
          file="contracts/charts/amount_customer_category.csv",row.names = FALSE)


# ================================================================================
# charting engines and topline 
# --------------------------------------------------------------------------------
# join engines and topline 

engine_contracts <- engine_contracts %>%
  mutate(type = "Engines")

topline_contracts <- topline_contracts %>%
  mutate(type = "Topline")
colnames(topline_contracts)[colnames(topline_contracts)=="ContractingSubCustomer"]<-"SubCustomer"

# engine_contracts <- engine_contracts %>%
#   select(Fiscal_Year, SubCustomer, SimpleArea, Action_Obligation_OMB23_GDP21, type)

comparison_contracts <- engine_contracts %>%
  select(Fiscal_Year, SubCustomer, SimpleArea, Action_Obligation_OMB23_GDP21, type) %>%
  rbind(topline_contracts %>%
          select(Fiscal_Year, SubCustomer, SimpleArea, Action_Obligation_OMB23_GDP21, type)) %>%
  group_by(Fiscal_Year, SubCustomer, SimpleArea, type) %>%
  dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE))

comparison_contracts$SimpleArea<-factor(comparison_contracts$SimpleArea)
levels(comparison_contracts$SimpleArea)<-
  list("Products"=c("Products","Products (All)"),
       'Services'=c("Services","Services (Non-R&D)"),
       "R&D"=c("R&D"),
       "Total"="Total",
       'Unlabeled'="Unlabeled"
  )


dyear <- comparison_contracts %>%
  group_by() %>%
  dplyr::rename(Action_Obligation_OMB23_GDP21.lagged = Action_Obligation_OMB23_GDP21) %>%
  mutate(Fiscal_Year = Fiscal_Year + 1) # %>%
# select(-fyb)

# comparison_contracts <- comparison_contracts %>% filter(Fiscal_Year >= 2001)
#Add lagged year
sumcheck<-sum(comparison_contracts$Action_Obligation_OMB23_GDP21,na.rm=TRUE)
comparison_contracts <- comparison_contracts %>%
  left_join(dyear, by = c("Fiscal_Year", "SubCustomer", "SimpleArea", "type"))# %>%
if(sumcheck!=sum(comparison_contracts$Action_Obligation_OMB23_GDP21,na.rm=TRUE)) stop("Sum Check Error")


dyear <- comparison_contracts %>%
  filter(Fiscal_Year==2000) %>%
  dplyr::rename(Action_Obligation_OMB23_GDP21.2000 = Action_Obligation_OMB23_GDP21) %>%
  group_by()%>%
  select(-Action_Obligation_OMB23_GDP21.lagged,-Fiscal_Year)

#Add 2000
sumcheck<-sum(comparison_contracts$Action_Obligation_OMB23_GDP21,na.rm=TRUE)
comparison_contracts <- comparison_contracts %>%
  left_join(dyear, by = c("SubCustomer", "SimpleArea", "type")) %>%
  mutate(amount_change = Action_Obligation_OMB23_GDP21 - Action_Obligation_OMB23_GDP21.lagged,
         baseline_change = Action_Obligation_OMB23_GDP21 - Action_Obligation_OMB23_GDP21.2000) #%>% 
if(sumcheck!=sum(comparison_contracts$Action_Obligation_OMB23_GDP21,na.rm=TRUE)) stop("Sum Check Error")





rm(dyear)

(
  comparison_contracts_overall <- comparison_contracts %>%
    group_by(Fiscal_Year, type) %>%
    dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE),
                     Action_Obligation_OMB23_GDP21.lagged = sum(Action_Obligation_OMB23_GDP21.lagged, na.rm = TRUE),
                     amount_change = sum(amount_change, na.rm = TRUE),
                     Action_Obligation_OMB23_GDP21.2000 = sum(Action_Obligation_OMB23_GDP21.2000, na.rm = TRUE),
                     baseline_change = sum(baseline_change, na.rm = TRUE)) %>%
    mutate(amount_percent_change = (amount_change) / Action_Obligation_OMB23_GDP21.lagged,
           baseline_percent_change = (baseline_change) / Action_Obligation_OMB23_GDP21.2000) #%>%
  # dplyr::rename(Action_Obligation_OMB23_GDP21 = Action_Obligation_OMB23_GDP21) %>%
  # select(Fiscal_Year, Action_Obligation_OMB23_GDP21, amount_change, amount_percent_change, type) #%>%
  # This has already been done, which is good, as I don't htink it makes sense to sum percent changes like that.
  # group_by(Fiscal_Year, type) %>%
  # dplyr::summarise(
  #   Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE),
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
      aes(x = Fiscal_Year, y = baseline_percent_change),
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
    scale_y_continuous(labels = scales::percent) +
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

write.csv(graph_contracts_overall$data,# %>%
            # spread(key=Fiscal_Year, value=Action_Obligation_OMB23_GDP21),
          file="contracts/charts/percent_change_total.csv",row.names = FALSE)



# --------------------------------------------------------------------------------
# percent change comparison by SubCustomer



(
  comparison_contracts_subcustomer <- comparison_contracts %>%
    group_by(Fiscal_Year, SubCustomer, type) %>%
    dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE),
                     Action_Obligation_OMB23_GDP21.lagged = sum(Action_Obligation_OMB23_GDP21.lagged, na.rm = TRUE),
                     amount_change = sum(amount_change, na.rm = TRUE),
                     Action_Obligation_OMB23_GDP21.2000 = sum(Action_Obligation_OMB23_GDP21.2000, na.rm = TRUE),
                     baseline_change = sum(baseline_change, na.rm = TRUE)) %>%
    mutate(amount_percent_change = (amount_change) / Action_Obligation_OMB23_GDP21.lagged,
           baseline_percent_change = (baseline_change) / Action_Obligation_OMB23_GDP21.2000) #%>%
)

levels(factor(comparison_contracts_subcustomer$type))
max((comparison_contracts_subcustomer %>% 
       filter(SubCustomer=="Other DoD" &
                type =="Engines"))$Action_Obligation_OMB23_GDP21
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
      aes(x = Fiscal_Year, y = baseline_percent_change),
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
    scale_y_continuous(labels = scales::percent) +
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

write.csv(graph_contracts_subcustomer$data,# %>%
            # spread(key=Fiscal_Year, value=Action_Obligation_OMB23_GDP21),
          file="contracts/charts/percent_change_customer.csv",row.names = FALSE)


# --------------------------------------------------------------------------------
# percent change comparison by SimpleArea 
# 


(
  comparison_contracts_area <- comparison_contracts %>%
    group_by(Fiscal_Year, SimpleArea, type) %>%
    dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE),
                     Action_Obligation_OMB23_GDP21.lagged = sum(Action_Obligation_OMB23_GDP21.lagged, na.rm = TRUE),
                     amount_change = sum(amount_change, na.rm = TRUE),
                     Action_Obligation_OMB23_GDP21.2000 = sum(Action_Obligation_OMB23_GDP21.2000, na.rm = TRUE),
                     baseline_change = sum(baseline_change, na.rm = TRUE)) %>%
    mutate(amount_percent_change = (amount_change) / Action_Obligation_OMB23_GDP21.lagged,
           baseline_percent_change = (baseline_change) / Action_Obligation_OMB23_GDP21.2000) #%>%
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
      aes(x = Fiscal_Year, y = baseline_percent_change),
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
    scale_y_continuous(labels = scales::percent) +
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

write.csv(graph_contracts_area$data, #%>%
            # spread(key=Fiscal_Year, value=Action_Obligation_OMB23_GDP21),
          file="contracts/charts/percent_change_category.csv",row.names = FALSE)


# -----------------------------------------------------------------------------------
# Product / R&D and topline.



  comparison_contracts_total <-
    comparison_contracts %>%
    group_by(Fiscal_Year, type) %>%
    dplyr::summarise(Action_Obligation_OMB23_GDP21 = sum(Action_Obligation_OMB23_GDP21, na.rm = TRUE),
                     Action_Obligation_OMB23_GDP21.lagged = sum(Action_Obligation_OMB23_GDP21.lagged, na.rm = TRUE),
                     amount_change = sum(amount_change, na.rm = TRUE),
                     Action_Obligation_OMB23_GDP21.2000 = sum(Action_Obligation_OMB23_GDP21.2000, na.rm = TRUE),
                     baseline_change = sum(baseline_change, na.rm = TRUE)) %>%
    mutate(amount_percent_change = (amount_change) / Action_Obligation_OMB23_GDP21.lagged,
           baseline_percent_change = (baseline_change) / Action_Obligation_OMB23_GDP21.2000) #%>%
comparison_contracts_total$SimpleArea<-"Total"
(comparison_contracts_area_total<-rbind(comparison_contracts_total,
                                       comparison_contracts_area)

)

summary(factor(comparison_contracts_area_total$SimpleArea))
(
  graph_contracts_overall_area <- comparison_contracts_area_total %>%
    filter(!is.na(SimpleArea) & !SimpleArea %in% c("Services","Unlabeled")) %>%
    mutate(type = factor(type, levels = c("Topline", "Engines"))) %>%
    ggplot() +
    geom_line(
      aes(x = Fiscal_Year, 
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
    scale_y_continuous(labels = scales::percent, breaks = c(-0.5,0,0.5,1,1.5)) +
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

write.csv(graph_contracts_overall_area$data, #%>%
            # spread(key=Fiscal_Year, value=Action_Obligation_OMB23_GDP21),
          file="contracts/charts/percent_change_total_area.csv",row.names = FALSE)

