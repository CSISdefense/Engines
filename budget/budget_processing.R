# ================================================================================
# The Future of Military Engines
# By Gabriel Coll and Gregory Sanders
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
library(readr)
# --------------------------------------------------------------------------------
# read data (each President's budget has a separate data table)

d23 <- read_csv("budget/data/23.csv")
d22 <- read_csv("budget/data/22.csv")
d21 <- read_csv("budget/data/21.csv")
d20 <- read_csv("budget/data/20.csv")
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
d23 <- d23 %>%
  gather(`Prior Years`:`FY 2027`, key = "FY", value = "Amount")
d22 <- d22 %>%
  gather(`Prior Years`:`FY 2026`, key = "FY", value = "Amount")
d21 <- d21 %>%
  gather(`Prior Years`:`FY 2025`, key = "FY", value = "Amount")
d20 <- d20 %>%
  gather(`Prior Years`:`FY 2024`, key = "FY", value = "Amount")
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


#### Green book data
gb<-read_csv("budget/data/FY23 PB Green Book Table 6-1.csv",skip = 3)
rdte<-gb[6,c(1,5:ncol(gb)-1)]
rdte<-rdte%>%pivot_longer(cols=-1)
colnames(rdte)<-c("PublicLawTitle","FY","TOA")
rdte$FY<-as.numeric(substr(rdte$FY,4,7))
rdte<-rdte%>% csis360::deflate(
  money_var= "TOA",
  fy_var="FY",
  deflator_var="OMB23_GDP21"
)

  rdtedept<-read_csv("budget/data/PB23_T6-6_RDTE_TOA.csv")
  colnames(rdtedept)[1]<-"Organization"
  rdtedept<-rdtedept%>%pivot_longer(cols=-Organization,names_to = "FY",values_to="ServiceTOA")
  # rdtedept$Account<-"RDT&E"
  rdtedept$ServiceTOA<-rdtedept$ServiceTOA*1000000
  rdtedept <- rdtedept %>% filter(Organization!="Total RDT&E")
  rdtedept$Organization<-factor(rdtedept$Organization)
  levels(rdtedept$Organization)<-list(
    "Other DoD"=c("DIR OF OPER, T&E,DEF" ,"DIR OF T&E, DEF.","RDT&E,DEF.AGENCIES" ),
    "Air Force"=c("RDT&E, AIR FORCE","RDT&E, Space Force"),
    "Army"="RDT&E, ARMY",
    "Navy"="RDT&E, NAVY"
  )
  
  rdtedept<-rdtedept %>% group_by(FY,Organization) %>%
    mutate(FY=as.numeric(FY)) %>%
    summarise(ServiceTOA=sum(ServiceTOA,na.rm = TRUE))
  rdtedept<-rdtedept%>% csis360::deflate(
    money_var= "ServiceTOA",
    fy_var="FY",
    deflator_var="OMB23_GDP21"
  )

# --------------------------------------------------------------------------------
# combine data 
#   (notes: the data tables for each President's budget have to be combined)

label_r1d<-function(x,PByear){
  if(!"Notes" %in% colnames(x)) x$Notes<-NA
  x$FYDP.Year<-PByear
  x<-standardize_variable_names(x,replace_special = TRUE)
  x
}

d20<-label_r1d(d20,2020)
d21<-label_r1d(d21,2021)
d22<-label_r1d(d22,2022)
d23<-label_r1d(d23,2023)
engine_r1d<-rbind(d20,d21,d22,d23)
engine_r1d<-standardize_variable_names(engine_r1d,path="..\\Lookup-Tables\\style\\")

engine_r1d$row<-row.names(engine_r1d)
engine_r1d$TypeCol<-engine_r1d$Type
engine_r1d<-engine_r1d %>% pivot_wider(names_from="TypeCol",values_from="PE.Project.Accomplishments.Planned.Programs.Title") %>% 
  select(-row)
engine_r1d$ProgramElementTitle[!is.na(engine_r1d$PE)]<-engine_r1d$PE[!is.na(engine_r1d$PE)]
engine_r1d<-engine_r1d %>% select(-PE)
colnames(engine_r1d)[colnames(engine_r1d)=="Project"]<-"ProjectName"
colnames(engine_r1d)[colnames(engine_r1d)=="A/PP"]<-"SubProjectName"
engine_r1d$ProgramElementTitle[!is.na(engine_r1d$CA)]<-engine_r1d$CA[!is.na(engine_r1d$CA)]
engine_r1d<-engine_r1d %>% select(-CA)

#Drop prior years and base/oco breakouts
engine_r1d<-engine_r1d%>%  filter(FY!="Prior Years") %>% 
  mutate(FY=sub("FY ","",FY))%>%
  filter(!substr(FY,5,999)  %in% c(" Base"," OCO"))  %>% 
  mutate(FY=as.numeric(sub(" Total","",FY)))
  
#PB 2022 no FYDP fix
engine_r1d<-engine_r1d %>% filter(PByear!=2022 | FY <=2022)
engine_r1d$Organization[engine_r1d$Organization=="USAF"]<-"Air Force"

#---- Chosing what subprojects to keep. Only AETP in PB2020 is significant spending. See also F-135/F-136 handled for back years below.
# This is necessary b/c A/PP and CA do not extend past the President's Budget and this can't be summed with the FYDP
# Fortunately the AETP program is in PBY2020 is being transitioned to another.
engine_r1d%>%group_by(Type,PByear) %>%  filter(Type %in% c("A/PP","CA") & SubProjectName!="Adaptive Engine Transition Program (AETP)") %>%
  summarise(Amount=sum(Amount,na.rm = TRUE))
View(engine_r1d %>% filter(!is.na(Amount)&Type %in% c("A/PP","CA")))
engine_r1d<-engine_r1d %>% filter(Type=="Project"|SubProjectName=="Adaptive Engine Transition Program (AETP)" )
engine_r1d$ProjectName[is.na(engine_r1d$ProjectName)]<-engine_r1d$SubProjectName[is.na(engine_r1d$ProjectName)]
engine_r1d <- engine_r1d   %>% select(-SubProjectName)





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


engine_budget<-standardize_variable_names(engine_budget)
if(any(is.na(engine_budget$ProjectName))){
  stop("Missing ProjectName")
  View(engine_budget %>% filter(is.na(ProjectName)))
}

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
  # colnames(engine_budget)[!colnames(engine_budget) %in% colnames(f135_f136)]
  # colnames(f135_f136)[!colnames(f135_f136) %in% colnames(engine_budget)]
  f135_f136<-standardize_variable_names(f135_f136)
  engine_budget <- engine_budget %>%
    rbind(f135_f136)
}

# --------------------------------------------------------------------------------
# clean and summarize data
#   (note: the funding in the last project was moved from PE 0602203F to 
#   ... Project 3048 starting in FY 2010 to more accurately align efforts with 
#   ... organizational structure.)

engine_budget <- engine_budget %>%
  separate(FY, into = c("X", "FY"), sep = 1) %>%
  select(-X)%>%
  dplyr::rename(
    # PByear = FYDP.Year,
    AccountTitle = Type,
    Organization = Force,
    ProgramElement = Program.Number,
    ProgramElementTitle = Program.Name,
    ProjectNumber    = Project.Number,
    ProjectName = Project.Name,
    Amount = Amount,
    FY = FY
  ) %>%
  mutate(FY = as.numeric(FY))

engine_budget$Description<-NA
engine_budget$BudgetActivity<-NA
engine_budget$Notes<-NA
engine_budget$Type<-NA
engine_r1d <- engine_r1d  %>% select(-LineNumber)
engine_r1d$AccountTitle<-"RDT&E"
colnames(engine_budget)[!colnames(engine_budget) %in% colnames(engine_r1d)]
colnames(engine_r1d)[!colnames(engine_r1d) %in% colnames(engine_budget)]

engine_budget<-rbind(engine_budget,engine_r1d)
rm(engine_r1d)


engine_budget<-csis360::deflate(
  engine_budget,
  money_var= "Amount",
  fy_var="FY",
  deflator_var="OMB23_GDP21"
) %>%
  # dplyr::left_join(deflate_year, by = "FY") %>%
  # mutate(Amount_OMB23_GDP21 = Amount / deflator) %>%
  dplyr::mutate(PByear = as.factor(PByear)) %>%
  dplyr::mutate(fydp = "FYDP") %>%
  unite(PByear, PByear, fydp, sep = " ") %>%
  # select(PByear:ProjectName, FY, Amount_Then_Year, Amount_OMB23_GDP21) %>%
  dplyr::mutate(
    ProjectName = recode(
      ProjectName,
      "'Acft Demo Engines' = 'ACFT Demo Engines';
      'ACFT Demo Engines ' = 'ACFT Demo Engines';
      'Aircraft Demonstration Engine' = 'ACFT Demo Engines';
      'Aircraft Demonstration Engines' = 'ACFT Demo Engines';
      'Adv Propulsion Rsch' = 'Advanced Propulsion Research';
      'Adv Propulsion' = 'Advanced Propulsion Research';
      'Aerospace Fuels and Atmospheric Propulsion' = 'Aerospace Fuels';
      'Aircraft Engine Component Improvement Program (CIP) (USN)' = 'Aircraft Engine Component Improvement Program (USN)';
      'Aircraft Engine Component Improvement Program (CIP) (USA)' = 'Aircraft Engine Component Improvement Program (USA)';
       'A/C Component Improv Prog' = 'Aircraft Engine Component Improvement Program (USA)';
      'Aircraft Engine Component Improvement Program' = 'Aircraft Engine Component Improvement Program (USAF)';
      'Aircraft Engine CIP' = 'Aircraft Engine Component Improvement Program (USN)';
      'Aircraft Component Improvement Program (CIP)' = 'Aircraft Engine Component Improvement Program (USA)';
      'A/C Eng Comp Imp (CIP)' = 'Aircraft Engine Component Improvement Program (USN)';
      'Acft Engines Comp Imp Prog' = 'Aircraft Engine Component Improvement Program (USN)';
      'F-35' = 'Aircraft Engine Component Improvement Program (F135)';
      'Aircraft Propulsion Subsystem Integration' = 'Aircraft Propulsion Subsystems Int';
      'Vehicle Propulsion and Structures Technology' = 'Veh Prop & Struct Tech';
      'Propulsion and Power Component Improvement Program' = 'Aircraft Engine Component Improvement Program (USN)';
      'Aircraft Engine Component Improvement Program (CIP)' = 'Aircraft Engine Component Improvement Program (USA)';
      'A/C Compon Improv Prog' = 'Aircraft Engine Component Improvement Program (USA)';
      'F135 Aircraft Engine Component Improvement Program' = 'Aircraft Engine Component Improvement Program (F135)';
      'Materials for Structures, Propulsion and Subsystems' = 'Materials for Structures, Propulsion, and Subsystems';
      'Fuels and Lubrication' = 'Combustion and Mechanical Systems';
      'Propulsion' = 'Advanced Aerospace Propulsion';
      'Aerospace Fuel Technology' = 'Combustion and Mechanical Systems';
        'Advanced Engine Development/Transition Prioritization'='Adaptive Engine Transition Program/Next Gen Adaptive Propulsion';
      'Transition Prioritization'='Adaptive Engine Transition Program/Next Gen Adaptive Propulsion';
      'Advanced Engine Development / Adaptive Engine Transition Program'='Adaptive Engine Transition Program/Next Gen Adaptive Propulsion';
      'Prototyping / Adaptive Engine Transition Program'='Adaptive Engine Transition Program/Next Gen Adaptive Propulsion';
      'Adaptive Engine Transition Program (AETP)'='Adaptive Engine Transition Program/Next Gen Adaptive Propulsion';
      'Advanced Engine Dev'='Adaptive Engine Transition Program/Next Gen Adaptive Propulsion';
      'Propulsion '='Propulsion'"
    )
  )

# if(sum(engine_budget$Amount,na.rm=TRUE) - sum(engine_budget_alternate$Amount.Then.Year,na.rm=TRUE) > 0.001 |
#    sum(engine_budget$Amount_OMB23_GDP21,na.rm=TRUE) - sum(engine_budget_alternate$Amount.OMB.2019,na.rm=TRUE)> 0.001) stop("Deflation checksum failure")

# --------------------------------------------------------------------------------
# join stages 
#   (note: these are the stages of R&D, i.e. basic, applied research)

stages<-csis360::remove_bom(stages)

stages <- stages %>%
  dplyr::rename(stage = "Stage") %>% #remove-bom covers this
  dplyr::rename(ProjectName = "Project.Name") %>% 
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

if(any(duplicated(stages$ProjectName))) stop("Duplicate Project Name")
engine_budget <- engine_budget %>%
  left_join(stages, by = "ProjectName") %>%
  mutate(Amount_Then_Year = Amount_Then_Year * 1000000,
         Amount_OMB23_GDP21 = Amount_OMB23_GDP21 * 1000000)

engine_budget$stage[is.na(engine_budget$stage)]<-engine_budget$BudgetActivity[is.na(engine_budget$stage)]
engine_budget <- engine_budget %>%
  dplyr::mutate(
    stage = recode(
      stage,
      "'1' = '(6.1) Basic Research';
      '2' = '(6.2) Applied Research';
      '3' = '(6.3) Advanced Technology Development';
      '4' = '(6.4) Advanced Component Development & Prototypes';
      '5' = '(6.5) System Development & Demonstration';
      '7' = '(6.7) Operational Systems Development'"
    )
  )

if(any(is.na(engine_budget$stage))){
  View(engine_budget %>% filter(is.na(stage)))
  summary(factor(engine_budget$stage))
  stop("Missing stage")
}
# --------------------------------------------------------------------------------

engine_budget_wide <-
  spread(engine_budget, key = "FY", value = "Amount_Then_Year") # to view discrepancies

engine_budget %>% group_by(ProjectName) %>% dplyr::summarise(Amount_Then_Year=sum(Amount_Then_Year,na.rm=TRUE))

#I'm not sure why you're filtering these.
engine_budget <- engine_budget %>%
  filter(PByear != "1999 FYDP")# %>%
  # filter(
  #   !ProjectName %in% c(
  #     # "ACFT Demo Engines",
  #     # "Advanced Aerospace Propulsion",
  #     # "Advanced Propulsion Research",
  #     # "Advanced Propulsion Technology",
  #     # "Advanced Turbine Engine Gas Generator",
  #     # "Aerospace Fuels",
  #     # "Aircraft Engine Component Improvement Program (F135)",
  #     # "Aircraft Engine Component Improvement Program (USA)",
  #     # "Aircraft Engine Component Improvement Program (USAF)",
  #     # "Aircraft Engine Component Improvement Program (USN)",
  #     # "Aircraft Propulsion Subsystems Int",
  #     # "AV-8B",
  #     "Aviation Advanced Technology Initiatives", #Overly broad, includes a lot of non-engine tech
  #     # "Combustion and Mechanical Systems",
  #     # "Improved Turbine Engine Program",
  #     "Materials",#Mentions high temperature but not propulsion specific.
  #     # "Materials for Structures, Propulsion, and Subsystems",
  #       "Materials Technology for Sustainment", #This does not ppear relevant, removing from dt
  #     "Materials Transition", #Mentions propulsion and has high temperature, but that's less than quarter of spend
  #     "Propulsion", 
  #     # "Turbine Engine Technology",
  #     "Vectored Thrust Ducted Propeller (CA)", #Trusting prior call, seems more rotor/propeller than engine oriented
  #     # "Veh Prop & Struct Tech",
  #     # "F135",
  #     # "F136",
  #     # "Advanced Engine Development/Transition Program"
  #   )
  # )

engine_budget_wide <-
  spread(engine_budget, key = "FY", value = "Amount_Then_Year") # to view discrepancies

save(engine_budget, engine_budget_wide, rdte,rdtedept,file="budget/engine_budget.rda")

write.csv(
  engine_budget %>%
    group_by(ProgramElement,ProjectNumber,ProjectName) %>%
    mutate(MinPByear=min(PByear),
           MaxPByear=max(PByear)) %>%
    group_by(FY, ProgramElement,ProjectNumber,ProjectName,MinPByear,MaxPByear) %>%
    summarise(Amount_Then_Year=sum(Amount_Then_Year,na.rm=TRUE)) %>%
    
    pivot_wider(
      names_from=FY,values_from=Amount_Then_Year) %>%
    arrange(ProgramElement,ProjectNumber,ProjectName),
  file="budget/charts/Amount_org_pe_pn_project.csv",
  na = "",
  row.names = FALSE
)
