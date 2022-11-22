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
library(csis360)
library(readr)
# --------------------------------------------------------------------------------
# add theme

source("contracts/theme/chart_theme.R")
source("contracts/theme/money_labels.R")
# path<-"C:\\Users\\gsand\\Repositories\\Lookup-Tables\\"
# path<-"https://raw.githubusercontent.com/CSISdefense/Lookup-Tables/master/"
path<-"K:\\Users\\Greg\\Repositories\\Lookup-Tables\\"

# --------------------------------------------------------------------------------
# Read engine contract data ####
engine_contracts <-
  read_delim("contracts/data/Project.SP_EngineAllVendorHistoryCompetitionBudgetMechanismVendorSizeProdServAreaSubCustomer.txt",
             na =c("NA","NULL"),delim="\t", guess_max = 900000)
colnames(engine_contracts)
#674730
engine_contracts<-engine_contracts %>% select(-dunsnumber...22)
colnames(engine_contracts)[colnames(engine_contracts)=="dunsnumber...14"]<-"dunsnumber"
engine_contracts$progsourceaccount <-text_to_number(engine_contracts$progsourceaccount )
# debug(read_and_join_experiment)
engine_contracts<-read_and_join_experiment(engine_contracts,
                                      "Budget_MainAccountCode.csv",
                                      by=c("progsourceagency"="TreasuryAgencyCode",
                                           "progsourceaccount"="MainAccountCode"),
                                      add_var=c("ColorOfMoney"),
                                      skip_check_var = c("ColorOfMoney"),
                                      path=path,
                                      dir="budget/",
                                      case_sensitive = FALSE
                                      )

# engine_contracts[nrow(engine_contracts),1]
while(engine_contracts[nrow(engine_contracts),1] %in% c("0","Return Value","0\r" ,"Return Value\r"))
  engine_contracts<-engine_contracts[1:nrow(engine_contracts)-1,]

# undebug(apply_standard_lookups)
engine_contracts<-apply_standard_lookups(engine_contracts,path=path)

engine_contracts %>% filter(Fiscal_Year>=2011 & Fiscal_Year<=2017) %>%
  group_by(progsourceagency,progsourceaccount,ColorOfMoney) %>%
  summarise(Action_Obligation_OMB23_GDP21=sum(Action_Obligation_OMB23_GDP21,na.rm=TRUE)) %>%
  arrange(-Action_Obligation_OMB23_GDP21)

write.csv(engine_contracts %>% filter(Fiscal_Year>=2011 & Fiscal_Year<=2017) %>%
  group_by(progsourceagency,progsourceaccount,ColorOfMoney) %>%
  summarise(Action_Obligation_OMB23_GDP21=sum(Action_Obligation_OMB23_GDP21,na.rm=TRUE)),
  file="contracts/data/BudgetAccount.csv",row.names = FALSE)

engine_contracts %>% filter(Fiscal_Year>=2011 & Fiscal_Year<=2017) %>%
  group_by(ColorOfMoney,fundingrequestingagencyid,fundingrequestingofficeid) %>%
  summarise(Action_Obligation_OMB23_GDP21=sum(Action_Obligation_OMB23_GDP21,na.rm=TRUE)) %>%
  arrange(-Action_Obligation_OMB23_GDP21)



engine_contracts<-engine_contracts %>% 
  mutate(derived_link=paste("https://www.usaspending.gov/award/CONT_AWD_",PIID,"_",agencyid,"_",
                            ifelse(is.na(idvpiid)|idvpiid=="","-NONE-",idvpiid),"_",
                            ifelse(is.na(idvagencyid)|idvagencyid=="","-NONE-",idvagencyid),"/",sep=""))

summary(engine_contracts$derived_link==engine_contracts$usaspending_permalink)

# View(engine_contracts %>% select(usaspending_permalink,derived_link,agencyid,PIID,idvagencyid,idvpiid)%>% filter(engine_contracts$derived_link!=engine_contracts$usaspending_permalink))

write.csv(engine_contracts %>% #filter(Fiscal_Year>=2011 & Fiscal_Year<=2017) %>%
            group_by(usaspending_permalink,derived_link,SubCustomer.JPO,SimpleArea.engines,ProjectID,Project.Name,ColorOfMoney,Fiscal_Year) %>%
            summarise(Action_Obligation_OMB23_GDP21=sum(Action_Obligation_OMB23_GDP21,na.rm=TRUE)),
          file="contracts/data/TopContract.csv",row.names = FALSE)



# --------------------------------------------------------------------------------
# read topline contract data

# topline_contracts <- read.delim("contracts/data/Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer.txt",
#                                      na.strings=c("NA","NULL"),sep="\t")
topline_contracts <- read.delim("contracts/data/Defense_Project.SP_ProdServPlatformOffice.txt",
                                     na.strings=c("NA","NULL"),sep="\t")

while(topline_contracts[nrow(topline_contracts),1] %in% c("0","Return Value","0\r" ,"Return Value\r",
                                                                    "An error occurred while executing batch. Error message is: One or more errors occurred."))
  topline_contracts<-topline_contracts[1:nrow(topline_contracts)-1,]
# 
# if(colnames(topline_contracts)[1]=="productorservicecode"&colnames(topline_contracts)[5]=="ProductOrServiceCode")
#   topline_contracts<-topline_contracts[,2:ncol(topline_contracts)]
# topline_contracts<-standardize_variable_names(topline_contracts)

# topline_contracts<-topline_contracts%>% select(Fiscal_Year, Contracting_Agency_ID, ProductOrServiceCode, ProjectID,PlatformPortfolio, Action_Obligation) %>%
#   group_by(Fiscal_Year, Contracting_Agency_ID, ProductOrServiceCode, ProjectID,PlatformPortfolio) %>%
#   dplyr::summarise(Action_Obligation = sum(Action_Obligation, na.rm = TRUE))

# undebug(apply_standard_lookups)
topline_contracts<-apply_standard_lookups(topline_contracts,
                                          path=path)

topline_contracts<-topline_contracts%>% filter(Customer=="Defense")
# if ("SubCustomer.platform" %in% names(engine_contracts) & "ProjectName" %in% names(topline_contracts)){
#   topline_contracts$SubCustomer.JPO<-as.character(topline_contracts$SubCustomer.platform)
#   topline_contracts$SubCustomer.JPO[topline_contracts$ProjectName %in% c("JSF (F-35) ","JSF (F-35)") & !is.na(topline_contracts$ProjectName)&engine_contracts$SubCustomer.platform=="Navy"]<-"F-35 JPO"
#   topline_contracts$SubCustomer.JPO<-factor(topline_contracts$SubCustomer.JPO)
# }

# --------------------------------------------------------------------------------
# clean and summarize data
# engine_contracts$Fiscal_Year<-text_to_number(engine_contracts$Fiscal_Year)
#   
# --------------------------------------------------------------------------------
# write a new dataset for powerbi

summary(factor(engine_contracts$Action_Obligation_OMB23_GDP21))

# if ("SubCustomer.platform" %in% names(engine_contracts) & "Project.Name" %in% names(engine_contracts)){
#   engine_contracts$SubCustomer.JPO<-as.character(engine_contracts$SubCustomer.platform)
#   engine_contracts$SubCustomer.JPO[engine_contracts$Project.Name %in% c("JSF (F-35) ","JSF (F-35)") & !is.na(engine_contracts$Project.Name)&engine_contracts$SubCustomer.platform=="Navy"]<-"F-35 JPO"
#   engine_contracts$SubCustomer.JPO<-factor(engine_contracts$SubCustomer.JPO)
# }

servicelist<-c('J028','J029','H128','H129','H228','H229','H328','H329','H928','H929','K028','K029','L028','L029','N028','N029','W028','W029')
nrow(engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                           ProductOrServiceArea == "Engines & Power Plants"))))
#309,099
nrow(engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                           (ProductOrServiceArea == "Engines & Power Plants" |
                                                         ClaimantProgramCode == 'A1B')))))
#660,558
nrow(engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                           (ProductOrServiceArea == "Engines & Power Plants" |
                                                              ClaimantProgramCode == 'A1B'|
                                                              ProductOrServiceCode %in% servicelist)))))
#664.475
nrow(engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                           (ProductOrServiceArea == "Engines & Power Plants" |
                                                              ClaimantProgramCode == 'A1B'|
                                                              ProductOrServiceCode %in% servicelist)))|
                                                          (ProductOrServiceCode %in% servicelist &
                                                           ClaimantProgramCode %in% c('C9E','S1','S10'))))
#674,784


write.csv(engine_contracts %>% filter(ProductOrServiceCode %in% servicelist),file="contracts/data/engine_ambiguous_service_contracts.csv",
          row.names = FALSE)

nrow(engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                           (ProductOrServiceArea == "Engines & Power Plants" |
                                                              ClaimantProgramCode == 'A1B'|
                                                              ProductOrServiceCode %in% servicelist)))|
                                   (ProductOrServiceCode %in% servicelist &
                                      ClaimantProgramCode %in% c('C9E','S1','S10')&
                                      (fundingrequestingofficeid %in% c('N00019') |
                                         FundingMajorCommandName=="NAVAIR" |  MajorCommandName=="NAVAIR" |
                                         SubCustomer=="Air Force"))))


#666,663

#Narrow down engine services, when no platform is available, to customers known to be focused on aircraft
engine_contracts<-engine_contracts %>% filter(Customer=="Defense" & (((PlatformPortfolio == "Aircraft") & 
                                                      (ProductOrServiceArea == "Engines & Power Plants" |
                                                         ClaimantProgramCode == 'A1B'|
                                                         ProductOrServiceCode %in% servicelist)))|
                              (ProductOrServiceCode %in% servicelist &
                                 ClaimantProgramCode %in% c('C9E','S1','S10')&
                                 (fundingrequestingofficeid %in% c('N00019') |
                                    FundingMajorCommandName=="NAVAIR" |  MajorCommandName=="NAVAIR" |
                                    SubCustomer=="Air Force")))


#Adjust topline_contracts to match
topline_contracts$PlatformPorfolio.engine<-topline_contracts$PlatformPortfolio
summary(factor(topline_contracts$PlatformPorfolio.engine))

nrow(topline_contracts %>% filter((Customer=="Defense" & PlatformPorfolio.engine != "Aircraft") & 
                                                            (ProductOrServiceCode %in% servicelist &
                                                               claimantprogramcode %in% c('C9E','S1','S10')&
                                                               (fundingrequestingofficeid %in% c('N00019') |
                                                                  FundingMajorCommandName=="NAVAIR" |  MajorCommandName=="NAVAIR" |
                                                                  SubCustomer=="Air Force"))))
         

topline_contracts$PlatformPorfolio.engine[
  (topline_contracts$Customer=="Defense" & topline_contracts$PlatformPorfolio.engine != "Aircraft") & 
  (topline_contracts$ProductOrServiceCode %in% servicelist &
     topline_contracts$claimantprogramcode %in% c('C9E','S1','S10')&
     (topline_contracts$fundingrequestingofficeid %in% c('N00019') |
        topline_contracts$FundingMajorCommandName=="NAVAIR" |  topline_contracts$MajorCommandName=="NAVAIR" |
        topline_contracts$SubCustomer=="Air Force"))]<-"Aircraft"

summary(factor(topline_contracts$PlatformPorfolio.engine))

engine_contracts$SimpleArea.AETP<-engine_contracts$SimpleArea
engine_contracts$SimpleArea.AETP[engine_contracts$ProjectID==2260 | engine_contracts$SimpleArea=="R&D"]<-"R&D and AETP"
summary(factor(engine_contracts$SimpleArea.AETP))

topline_contracts$SimpleArea.AETP<-as.character(topline_contracts$SimpleArea)
topline_contracts$SimpleArea.AETP[topline_contracts$ProjectID==2260 | topline_contracts$SimpleArea=="R&D"]<-"R&D and AETP"
summary(factor(topline_contracts$SimpleArea.AETP))
levels( factor( engine_contracts$ProductServiceOrRnDarea))
engine_contracts$ProductServiceOrRnDarea.project<-engine_contracts$ProductServiceOrRnDarea
engine_contracts$ProductServiceOrRnDarea.project[engine_contracts$ProjectID==2260]<-
  "Advanced Component Development & Prototypes (6.4)"

engine_contracts$SubCustomer.engines<-engine_contracts$SubCustomer.JPO
levels(engine_contracts$SubCustomer.engines)<-list(
  "Army"="Army",
  "Navy"="Navy",
  "F-35 MDAP"=c("F-35 JPO","F-35 MDAP"),
  "Air Force"="Air Force",
  "DLA and Other DoD"=c("DLA","Other DoD"))
engine_contracts$SubCustomer.engines[engine_contracts$Project.Name=="JSF (F-35)"]<-"F-35 MDAP"


topline_contracts$SubCustomer.engines<-topline_contracts$SubCustomer.JPO
levels(topline_contracts$SubCustomer.engines)<-list(
  "Army"="Army",
  "Navy"="Navy",
  "F-35 MDAP"=c("F-35 JPO","F-35 MDAP"),
  "Air Force"="Air Force",
  "DLA and Other DoD"=c("DLA","Other DoD")
  )
topline_contracts$SubCustomer.engines[topline_contracts$Project.Name=="JSF (F-35)"]<-"F-35 MDAP"

summary(factor(engine_contracts$ContractingAgencyName))
summary(factor(topline_contracts$ContractingAgencyName))

engine_contracts<-engine_contracts %>% mutate(AllContractor=ifelse(is.na(ParentID),dunsnumber,ParentID))
biz_engine_contracts<-engine_contracts %>% dplyr::rename(
  fy = Fiscal_Year,
  amount = Action_Obligation_OMB23_GDP21,
  category = SimpleArea,
  platform_portfolio = PlatformPortfolio,
  customer_2 = Customer,
  customer = SubCustomer,
  competition = Competition.multisum,
  contract_type = PricingMechanism,
  vendor_size = VendorSize,
  parent = ParentID,
  AllContractor = AllContractor,
  project = Project.Name
)

write.csv(biz_engine_contracts, "contracts/app/power_bi.csv")


save(topline_contracts ,engine_contracts,file="contracts/app/engine_contract.Rdata")
# load(file="contracts/app/engine_contract.Rdata")

load(file="contracts/app/engine_contract.Rdata")
engine_sbirstter<-

  
#-----------------------------------------------------------------------
# Top 10 Data processing
##############################################################
# Get Top 10 rows of aggregated Amount and # of action ####
##############################################################
library(dplyr)
library(tidyr)
library(csis360)
library(readr)


# engine_vendor <-   read_delim("contracts/data/Project.SP_EngineAllVendorHistoryCompetitionFundingMechanismVendorSizeProdServAreaSubCustomer.txt",
                              # na =c("NA","NULL"),delim="\t")
  # read.delim("contracts/data/Project.SP_EngineAllVendorHistoryCompetitionFundingMechanismVendorSizeProdServAreaSubCustomer.txt",
  #                                    na.strings=c("NA","NULL"),sep="\t")

# engine_vendor<-remove_bom(engine_vendor)
# engine_vendor<-standardize_variable_names(engine_vendor)
engine_vendor<-engine_contracts

engine_vendor<-engine_vendor%>% 
  group_by(Fiscal_Year,AllContractor) %>% # group_by(Fiscal_Year,IsDefense,PlatformPortfolioUAV) %>%
  summarise(NumberOfActions=sum(NumberOfActions,na.rm=TRUE),
            Action_Obligation_OMB23_GDP21=sum(Action_Obligation_OMB23_GDP21,na.rm=TRUE)) %>%
  group_by(Fiscal_Year) %>%
  dplyr::mutate(pos=rank(-Action_Obligation_OMB23_GDP21))%>%
  arrange(Fiscal_Year,pos)

engine_vendor_overall<-engine_vendor%>% 
  group_by(AllContractor) %>% # group_by(Fiscal_Year,IsDefense,PlatformPortfolioUAV) %>%
  summarise(NumberOfActions=sum(NumberOfActions,na.rm=TRUE),
            Action_Obligation_OMB23_GDP21=sum(Action_Obligation_OMB23_GDP21,na.rm=TRUE),
            MinOfFiscalYear=min(Fiscal_Year),
            MaxOfFiscalYear=max(Fiscal_Year),
            ) %>%
  group_by() %>%
  dplyr::mutate(pos=rank(-Action_Obligation_OMB23_GDP21))%>%
  arrange(pos)

write.csv(engine_vendor_overall, file="contracts/data/TopEngineVendors.csv")

save(file="contracts/data/engine_vendor.rda",engine_vendor)
colnames(engine_vendor)


# SBIR STR ####
engine_sbirsttr <-
  read_delim("contracts/data/Project.SP_EngineAllVendorHistoryCompetitionBudgetMechanismVendorSizeProdServAreaSubCustomer.txt",
             na =c("NA","NULL"),delim="\t", guess_max = 900000)
engine_sbirsttr<-engine_sbirsttr%>% filter(research_code!="")

engine_sbirsttr<-apply_standard_lookups(engine_sbirsttr)
engine_sbirsttr<-engine_sbirsttr %>% 
engine_sbirsttr_description <-
  read_delim("contracts/data/engine_sbirsttr_text.txt",
             na =c("NA","NULL"),delim="\t", guess_max = 900000)
engine_sbirsttr<-engine_sbirsttr %>%
  left_join(engine_sbirsttr_description)

write.csv(file="contracts/data/engine_sbirsttr.csv",engine_sbirsttr,row.names = FALSE)

# OTA ####


label_engine<-function(x,col="Description_of_Requirement"){
  x<-as.data.frame(x)
  # grep("UAS|Unmanned|Uninhabited|Uncrewed|Remotely Crewed",x$Description_of_Requirement)
  # x$UAS[grep("RPA",x[,col],ignore.case = TRUE)]<-TRUE Doesn't seem to be any.
  x$Engine<-NA
  x$Engine[grep("engine[^e]",x[,col],ignore.case = TRUE)]<-TRUE
  sum(x$Engine,na.rm=TRUE)
  
  # x$Engine[grep("QUASAR",x[,col])]<-NA
  # sum(x$Engine,na.rm=TRUE)
  # x$Engine[grep("UAV",x[,col])]<-TRUE
  # sum(x$Engine,na.rm=TRUE)
  # x$Engine[grep("Unmanned",x[,col],ignore.case = TRUE)]<-TRUE
  # sum(x$Engine,na.rm=TRUE)
  # 
  # x$Engine[grep("Uncrewed",x[,col],ignore.case = TRUE)]<-TRUE
  # sum(x$Engine,na.rm=TRUE)
  # 
  # # Excluding Counter UAS
  # x$CUAS[grep("Counter Unmanned",x[,col],ignore.case = TRUE)]<-TRUE
  # sum(x$CUAS,na.rm=TRUE)
  # x$CUAS[grep("C-UAS",x[,col],ignore.case = TRUE)]<-TRUE
  # x$CUAS[grep("CSUAS",x[,col],ignore.case = TRUE)]<-TRUE
  # 
  # x$CUAS[grep("Counter-Unmanned",x[,col],ignore.case = TRUE)]<-TRUE
  # sum(x$CUAS,na.rm=TRUE)
  # 
  # x$UGS[grep("Unmanned Ground",x[,col],ignore.case = TRUE)]<-TRUE
  # 
  # x$Maritime[grep("Unmanned Surface",x[,col],ignore.case = TRUE)]<-TRUE
  # x$Maritime[grep("Unmanned Maritime",x[,col],ignore.case = TRUE)]<-TRUE
  # 
  # x$Maritime[grep("UNDERWATER UNMANNED",x[,col],ignore.case = TRUE)]<-TRUE
  # x$Maritime[grep("UNMANNED UNDERWATER",x[,col],ignore.case = TRUE)]<-TRUE
  # 
  # x$Maritime[grep("UNMANNED UNDERSEA",x[,col],ignore.case = TRUE)]<-TRUE
  # sum(x$CUAS,na.rm=TRUE)
  # 
  # 
  # x$Engine[x$CUAS==TRUE]<-NA
  # 
  # x$Engine[x$Maritime==TRUE]<-NA
  # x$Engine[x$UGS==TRUE]<-NA
  # sum(x$Engine,na.rm=TRUE)
  
  
  # 
  # x$mq[grep("MQ-",x[,col],ignore.case = TRUE)]<-TRUE
  # # x$mq[grep("MQT",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("MQSA",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("MESSAGE QUEUE",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("WEBSPHERE MQ",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("CMQA",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("NMQ",x[,col],ignore.case = TRUE)]<-NA
  # # x$mq[grep("MQU",x[,col],ignore.case = TRUE)]<-NA #Covers most proper words
  # # x$mq[grep("OFMQ",x[,col],ignore.case = TRUE)]<-NA
  # sum(x$mq,na.rm = TRUE)
  # x$rq[grep("RQ-",x[,col],ignore.case = TRUE)]<-TRUE
  # # x$rq[grep("RQU",x[,col],ignore.case = TRUE)]<-NA #Covers most proper words
  # # x$rq[grep("RQMT",x[,col],ignore.case = TRUE)]<-NA
  # x$rq[grep("AN/SRQ-",x[,col],ignore.case = TRUE)]<-NA
  # x$rq[grep("AN/ARQ-",x[,col],ignore.case = TRUE)]<-NA
  # # x$rq[grep("TRQ",x[,col],ignore.case = TRUE)]<-NA
  # # x$rq[grep("SRQ",x[,col],ignore.case = TRUE)]<-NA
  # # x$rq[grep("AHRQ",x[,col],ignore.case = TRUE)]<-NA
  # sum(x$rq,na.rm=TRUE)
  # 
  # x$any_uas<-NA
  # x$any_uas<-x$Engine|x$mq|x$rq
  x
}



OTA_data_current <- read_delim(
  "contracts//data//OTA_All_Fields.csv",delim = ",",
  col_names = TRUE, guess_max = 500000,na=c("NA","NULL"),skip = 2)

OTA_data_current<-apply_standard_lookups(OTA_data_current)

# debug(label_engine)
OTA_data_current<-label_engine(OTA_data_current,
                               col="Description_of_Requirement")
sum(OTA_data_current$Engine,na.rm=TRUE)
summary(factor(OTA_data_current$Engine))
OTA_engine<-OTA_data_current %>% filter(ProductServiceOrRnDarea == "Engines & Power Plants"  |
                                          Engine==TRUE|
  ProductOrServiceCode %in% c(
  'J028','J029','H128','H129','H228','H229','H328','H329','H928','H929',
  'K028','K029','L028','L029','N028','N029','W028','W029'))
write_csv(file="contracts//data//OTA_engine.csv",OTA_engine)


summary(factor(engine_contracts$MainAccountTitle))