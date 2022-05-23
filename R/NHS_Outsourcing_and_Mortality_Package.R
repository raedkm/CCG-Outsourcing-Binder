#NHS Outsourcing and Mortality R Package

Download_CCG_payments <- function(){myDataCCG <- download_zenodo(doi="10.5281/zenodo.5054717", path = ".", parallel = T, quiet = T)
unzip("NHSSpend-v.1.0.2.zip")
unzip("crahal-NHSSpend-1846777/data/data_final/payments_ccg_final.zip")
myDataCCG <- read.csv("payments_ccg_final.csv")}


Create_Annual_CCG_dataset <- function(myDataCCG){
  
  ###Assign Date Variable###
  
  myDataCCG$date <- as.Date(myDataCCG$date, format =  "%Y-%m-%d")
  
  
  #Remove payments before the formal creation of CCGs in april 2013
  myDataCCG <- myDataCCG[which(myDataCCG$date>="2013-04-01"),]
  
  SIC <- unique(read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CCG-Outsourcing/main/Data/Companies_House_SIC_lookup.csv")))
  
  
  ###Merge Procurement Data with CH Data###
  myDataCCG <- merge(myDataCCG, SIC, by= "supplier", all.x=TRUE)
  #rm(SIC)
  
  
  ####retrieve numeric sic codes from the data####
  
  myDataCCG$sic <- as.character(regmatches(myDataCCG$SICCode.SicText_1, gregexpr("[[:digit:]]+", myDataCCG$SICCode.SicText_1)))
  
  ####reducing sic digits to divisions and groups####
  myDataCCG$sic2dig <- substr(myDataCCG$sic, start = 1, stop = 2)
  myDataCCG$sic3dig <- substr(myDataCCG$sic, start = 1, stop = 3)
  
  myDataCCG$sic2dig <- as.double(myDataCCG$sic2dig)
  myDataCCG$sic3dig <- as.double(myDataCCG$sic3dig)
  myDataCCG$sic <- as.double(myDataCCG$sic)
  
  myDataCCG$sic2dig[is.na(myDataCCG$sic2dig)] <- 0
  myDataCCG$sic3dig[is.na(myDataCCG$sic3dig)] <- 0
  myDataCCG$sic[is.na(myDataCCG$sic)] <- 0
  
  
  ####isolate industries of interest####
  
  myDataCCG <- myDataCCG %>% mutate(SocialCare = ifelse(myDataCCG$sic2dig=="87"| myDataCCG$sic2dig=="88", 1,0))
  myDataCCG <- myDataCCG %>% mutate(HealthServices = ifelse(myDataCCG$sic2dig=="86", 1,0))
  myDataCCG <- myDataCCG %>% mutate(FoundationalServices = ifelse(myDataCCG$sic2dig=="56"| myDataCCG$sic2dig=="49", 1,0))
  myDataCCG <- myDataCCG %>% mutate(Buildings = ifelse(myDataCCG$sic2dig=="68"|myDataCCG$sic2dig=="41", 1,0))
  myDataCCG <- myDataCCG %>% mutate(ProfessionalServices = ifelse(myDataCCG$sic2dig=="70"|  myDataCCG$sic2dig=="82"| myDataCCG$sic2dig=="62"| myDataCCG$sic2dig=="69"| myDataCCG$sic2dig=="74"| myDataCCG$sic2dig=="63"| myDataCCG$sic2dig=="85", 1,0))
  myDataCCG <- myDataCCG %>% mutate(NotAssigned = ifelse(myDataCCG$sic2dig!="86"&myDataCCG$sic2dig!="87"&myDataCCG$sic2dig!="88"&myDataCCG$sic2dig!="56"&myDataCCG$sic2dig!="49"&myDataCCG$sic2dig!="68"&myDataCCG$sic2dig!="41"&myDataCCG$sic2dig!="70"&myDataCCG$sic2dig!="82"&myDataCCG$sic2dig!="62"&myDataCCG$sic2dig!="69"&myDataCCG$sic2dig!="74"&myDataCCG$sic2dig!="63"&myDataCCG$sic2dig!="85"&myDataCCG$sic2dig!="0", 1,0))
  myDataCCG <- myDataCCG %>% mutate(HospitalServices = ifelse(myDataCCG$sic=="86101", 1,0))
  myDataCCG <- myDataCCG %>% mutate(GPServices = ifelse(myDataCCG$sic=="86210", 1,0))
  myDataCCG <- myDataCCG %>% mutate(MedicalNursingHomes = ifelse(myDataCCG$sic=="86102", 1,0))
  myDataCCG <- myDataCCG %>% mutate(SpecialistPractices = ifelse(myDataCCG$sic=="86220", 1,0))
  myDataCCG <- myDataCCG %>% mutate(Dental = ifelse(myDataCCG$sic=="86230", 1,0))
  myDataCCG <- myDataCCG %>% mutate(OtherHealth = ifelse(myDataCCG$sic=="86900", 1,0))
  myDataCCG <- myDataCCG %>% mutate(nogpHealthServices = ifelse(myDataCCG$sic2dig=="86"&myDataCCG$sic!="86210"&myDataCCG$sic!="86230", 1,0))
  myDataCCG <- myDataCCG %>% mutate(Transport = ifelse(myDataCCG$sic2dig=="49", 1,0))
  myDataCCG <- myDataCCG %>% mutate(Food = ifelse(myDataCCG$sic2dig=="56", 1,0))
  myDataCCG <- myDataCCG %>% mutate(Management = ifelse(myDataCCG$sic2dig=="70", 1,0))
  
  
  myDataCCG <- myDataCCG %>% mutate(PrivateSector = ifelse((myDataCCG$match_type=="Companies House"&is.na(myDataCCG$CharityRegNo))|(myDataCCG$CompanyName!=""& is.na(myDataCCG$CharityRegNo))| (myDataCCG$CompanyCategory!=""&is.na(myDataCCG$CharityRegNo))|(myDataCCG$audit_type=="3"&myDataCCG$match_type=="No Match"&myDataCCG$CompanyName!=""),1,0))
  
  myDataCCG <- myDataCCG %>% mutate(nogpPrivateSector = ifelse(myDataCCG$CompanyName==""| !is.na(myDataCCG$CharityRegNo)&myDataCCG$GPServices==1&myDataCCG$Dental==1,0,1))
  
  
  myDataCCG <- myDataCCG %>% mutate(CharitySector = ifelse(is.na(myDataCCG$CharityRegNo), 0,1))
  
  
  
  
  ####create month and year variable####
  
  myDataCCG$month <- format(myDataCCG$date,"%m/%y")
  
  myDataCCG$year <- format(myDataCCG$date,"%Y")
  
  
  ####make factor variables for aggregating by####
  
  myDataCCG$dept <- factor(myDataCCG$dept)
  myDataCCG$month <- factor(myDataCCG$month)
  ####total ammount per month####
  
  totalsumCCG <- myDataCCG[c("dept","year","month","amount")]
  totalsumCCG <- aggregate(. ~dept+month+year, data=totalsumCCG, sum)
  
  totalsumCCGprivate <- myDataCCG[c("dept","year","month","amount", "PrivateSector")]
  totalsumCCGprivate <- totalsumCCGprivate[which(totalsumCCGprivate$PrivateSector==1),]
  totalsumCCGprivate <- aggregate(. ~dept+month+year, data=totalsumCCGprivate, sum)
  totalsumCCGprivate <- totalsumCCGprivate[-c(5)]
  
  nogptotalsumCCG <- myDataCCG[which(myDataCCG$GPServices==0&myDataCCG$Dental==0),]
  nogptotalsumCCG <- nogptotalsumCCG[c("dept","year","month","amount")]
  nogptotalsumCCG <- aggregate(. ~dept+month+year, data=nogptotalsumCCG, sum)
  
  names(totalsumCCG)[names(totalsumCCG)=="amount"] <- "Total_Procurement_Spend"
  names(nogptotalsumCCG)[names(nogptotalsumCCG)=="amount"] <- "NOGPTotal_Procurement_Spend"
  names(totalsumCCGprivate)[names(totalsumCCGprivate)=="amount"] <- "Total_Private_sector_spend"
  
  ####Total spend in private sector, third sector and select sectors####
  
  PrivateCCG <- myDataCCG[which(myDataCCG$PrivateSector==1),]
  PrivateCCG <- PrivateCCG[c("dept","year","month","amount")]
  PrivateCCG <- aggregate(. ~dept+month+year, data=PrivateCCG, sum)
  names(PrivateCCG)[names(PrivateCCG)=="amount"] <- "Private_Sector_Procurement_Spend"
  
  NOGPPrivateCCG <- myDataCCG[which(myDataCCG$nogpPrivateSector==1),]
  NOGPPrivateCCG <- NOGPPrivateCCG[c("dept","year","month","amount")]
  NOGPPrivateCCG <- aggregate(. ~dept+month+year, data=NOGPPrivateCCG, sum)
  names(NOGPPrivateCCG)[names(NOGPPrivateCCG)=="amount"] <- "nogpPrivate_Sector_Procurement_Spend"
  
  CharityCCG <- myDataCCG[which(myDataCCG$CharitySector==1),]
  CharityCCG <- CharityCCG[c("dept","year","month","amount")]
  CharityCCG <- aggregate(. ~dept+month+year, data=CharityCCG, sum)
  names(CharityCCG)[names(CharityCCG)=="amount"] <- "Charity_Sector_Procurement_Spend"
  
  TransportCCG <- myDataCCG[which(myDataCCG$Transport==1),]
  TransportCCG <- TransportCCG[c("dept","year","month","amount")]
  TransportCCG <- aggregate(. ~dept+month+year, data=TransportCCG, sum)
  names(TransportCCG)[names(TransportCCG)=="amount"] <- "Transport_Procurement_Spend"
  
  FoodCCG <- myDataCCG[which(myDataCCG$Food==1),]
  FoodCCG <- FoodCCG[c("dept","year","month","amount")]
  FoodCCG <- aggregate(. ~dept+month+year, data=FoodCCG, sum)
  names(FoodCCG)[names(FoodCCG)=="amount"] <- "Food_Procurement_Spend"
  
  ManagementCCG <- myDataCCG[which(myDataCCG$Management==1),]
  ManagementCCG <- ManagementCCG[c("dept","year","month","amount")]
  ManagementCCG <- aggregate(. ~dept+month+year, data=ManagementCCG, sum)
  names(ManagementCCG)[names(ManagementCCG)=="amount"] <- "Management_Procurement_Spend"
  
  SocialCareCCG <- myDataCCG[which(myDataCCG$SocialCare==1),]
  SocialCareCCG <- SocialCareCCG[c("dept","year","month","amount")]
  SocialCareCCG <- aggregate(. ~dept+month+year, data=SocialCareCCG, sum)
  names(SocialCareCCG)[names(SocialCareCCG)=="amount"] <- "Social_Care_Procurement_Spend"
  
  NotAssignedCCG <- myDataCCG[which(myDataCCG$NotAssigned==1),]
  NotAssignedCCG <- NotAssignedCCG[c("dept","year","month","amount")]
  NotAssignedCCG <- aggregate(. ~dept+month+year, data=NotAssignedCCG, sum)
  names(NotAssignedCCG)[names(NotAssignedCCG)=="amount"] <- "NotAssigned_Procurement_Spend"
  
  
  HealthCCG <- myDataCCG[which(myDataCCG$HealthServices==1),]
  HealthCCG <- HealthCCG[c("dept","year","month","amount")]
  HealthCCG <- aggregate(. ~dept+month+year, data=HealthCCG, sum)
  names(HealthCCG)[names(HealthCCG)=="amount"] <- "Health_Services_Procurement_Spend"
  
  nogpHealthCCG <- myDataCCG[which(myDataCCG$nogpHealthServices==1),]
  nogpHealthCCG <- nogpHealthCCG[c("dept","year","month","amount")]
  nogpHealthCCG <- aggregate(. ~dept+month+year, data=nogpHealthCCG, sum)
  names(nogpHealthCCG)[names(nogpHealthCCG)=="amount"] <- "nogpHealth_Services_Procurement_Spend"
  
  FoundationalCCG <- myDataCCG[which(myDataCCG$FoundationalServices==1),]
  FoundationalCCG <- FoundationalCCG[c("dept","year","month","amount")]
  FoundationalCCG <- aggregate(. ~dept+month+year, data=FoundationalCCG, sum)
  names(FoundationalCCG)[names(FoundationalCCG)=="amount"] <- "Foundational_Services_Procurement_Spend"
  
  
  BuildingCCG <- myDataCCG[which(myDataCCG$Buildings==1),]
  BuildingCCG <- BuildingCCG[c("dept","year","month","amount")]
  BuildingCCG <- aggregate(. ~dept+month+year, data=BuildingCCG, sum)
  names(BuildingCCG)[names(BuildingCCG)=="amount"] <- "Building_Maintenance_Procurement_Spend"
  
  
  ProfessionalServicesCCG <- myDataCCG[which(myDataCCG$ProfessionalServices==1),]
  ProfessionalServicesCCG <- ProfessionalServicesCCG[c("dept","year","month","amount")]
  ProfessionalServicesCCG <- aggregate(. ~dept+month+year, data=ProfessionalServicesCCG, sum)
  names(ProfessionalServicesCCG)[names(ProfessionalServicesCCG)=="amount"] <- "Professional_Services_Spend"
  
  
  
  HospitalCCG <- myDataCCG[which(myDataCCG$HospitalServices==1),]
  HospitalCCG <- HospitalCCG[c("dept","year","month","amount")]
  HospitalCCG <- aggregate(. ~dept+month+year, data=HospitalCCG, sum)
  names(HospitalCCG)[names(HospitalCCG)=="amount"] <- "Hospital_Services_Procurement_Spend"
  
  
  GPCCG <- myDataCCG[which(myDataCCG$GPServices==1),]
  GPCCG <- GPCCG[c("dept","year","month","amount")]
  GPCCG <- aggregate(. ~dept+month+year, data=GPCCG, sum)
  names(GPCCG)[names(GPCCG)=="amount"] <- "GP_Services_Procurement_Spend"
  
  MedicalNursingHomesCCG <- myDataCCG[which(myDataCCG$MedicalNursingHomes==1),]
  MedicalNursingHomesCCG <- MedicalNursingHomesCCG[c("dept","year","month","amount")]
  MedicalNursingHomesCCG <- aggregate(. ~dept+month+year, data=MedicalNursingHomesCCG, sum)
  names(MedicalNursingHomesCCG)[names(MedicalNursingHomesCCG)=="amount"] <- "Medical_Nursing_Homes_Procurement_Spend"
  
  DentalCCG <- myDataCCG[which(myDataCCG$Dental==1),]
  DentalCCG <- DentalCCG[c("dept","year","month","amount")]
  DentalCCG <- aggregate(. ~dept+month+year, data=DentalCCG, sum)
  names(DentalCCG)[names(DentalCCG)=="amount"] <- "Dental_Services_Procurement_Spend"
  
  SpecialistPracticesCCG <- myDataCCG[which(myDataCCG$SpecialistPractices==1),]
  SpecialistPracticesCCG <- SpecialistPracticesCCG[c("dept","year","month","amount")]
  SpecialistPracticesCCG <- aggregate(. ~dept+month+year, data=SpecialistPracticesCCG, sum)
  names(SpecialistPracticesCCG)[names(SpecialistPracticesCCG)=="amount"] <- "Specialist_Services_Procurement_Spend"
  
  OtherHealthCCG <- myDataCCG[which(myDataCCG$OtherHealth==1),]
  OtherHealthCCG <- OtherHealthCCG[c("dept","year","month","amount")]
  OtherHealthCCG <- aggregate(. ~dept+month+year, data=OtherHealthCCG, sum)
  names(OtherHealthCCG)[names(OtherHealthCCG)=="amount"] <- "Other_Health_Services_Procurement_Spend"
  
  
  
  
  ####merge panel dataset####
  
  
  
  MyPanelDataCCG <- merge(totalsumCCG, SocialCareCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, HospitalCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, GPCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, OtherHealthCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, PrivateCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, CharityCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, HealthCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, BuildingCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, SpecialistPracticesCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, NotAssignedCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, FoundationalCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, ProfessionalServicesCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, DentalCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, MedicalNursingHomesCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, NOGPPrivateCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, nogptotalsumCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, nogpHealthCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, FoodCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, TransportCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, ManagementCCG, by=c("dept", "month", "year"), all=TRUE)
  MyPanelDataCCG <- merge(MyPanelDataCCG, totalsumCCGprivate, by=c("dept", "month", "year"), all=TRUE)
  
  ####Turn NAs which have observations in other spending columns into observed 0s####
  
  MyPanelDataCCG$Social_Care_Procurement_Spend[is.na(MyPanelDataCCG$Social_Care_Procurement_Spend)] =0
  MyPanelDataCCG$Charity_Sector_Procurement_Spend[is.na(MyPanelDataCCG$Charity_Sector_Procurement_Spend)] =0
  MyPanelDataCCG$Building_Maintenance_Procurement_Spend[is.na(MyPanelDataCCG$Building_Maintenance_Procurement_Spend)] =0
  MyPanelDataCCG$Health_Services_Procurement_Spend[is.na(MyPanelDataCCG$Health_Services_Procurement_Spend)] =0
  MyPanelDataCCG$NotAssigned_Procurement_Spend[is.na(MyPanelDataCCG$NotAssigned_Procurement_Spend)] =0
  MyPanelDataCCG$Foundational_Services_Procurement_Spend[is.na(MyPanelDataCCG$Foundational_Services_Procurement_Spend)] =0
  MyPanelDataCCG$Dental_Services_Procurement_Spend[is.na(MyPanelDataCCG$Dental_Services_Procurement_Spend)] =0
  MyPanelDataCCG$Hospital_Services_Procurement_Spend[is.na(MyPanelDataCCG$Hospital_Services_Procurement_Spend)] =0
  MyPanelDataCCG$GP_Services_Procurement_Spend[is.na(MyPanelDataCCG$GP_Services_Procurement_Spend)] =0
  MyPanelDataCCG$Private_Sector_Procurement_Spend[is.na(MyPanelDataCCG$Private_Sector_Procurement_Spend)] =0
  MyPanelDataCCG$Other_Health_Services_Procurement_Spend[is.na(MyPanelDataCCG$Other_Health_Services_Procurement_Spend)] =0
  MyPanelDataCCG$Specialist_Services_Procurement_Spend[is.na(MyPanelDataCCG$Specialist_Services_Procurement_Spend)] =0
  MyPanelDataCCG$Professional_Services_Spend[is.na(MyPanelDataCCG$Professional_Services_Spend)] =0
  MyPanelDataCCG$Medical_Nursing_Homes_Procurement_Spend[is.na(MyPanelDataCCG$Medical_Nursing_Homes_Procurement_Spend)] =0
  MyPanelDataCCG$nogpPrivate_Sector_Procurement_Spend[is.na(MyPanelDataCCG$nogpPrivate_Sector_Procurement_Spend)] =0
  MyPanelDataCCG$nogpHealth_Services_Procurement_Spend[is.na(MyPanelDataCCG$nogpHealth_Services_Procurement_Spend)] =0
  MyPanelDataCCG$Food_Procurement_Spend[is.na(MyPanelDataCCG$Food_Procurement_Spend)] =0
  MyPanelDataCCG$Transport_Procurement_Spend[is.na(MyPanelDataCCG$Transport_Procurement_Spend)] =0
  MyPanelDataCCG$Management_Procurement_Spend[is.na(MyPanelDataCCG$Management_Procurement_Spend)] =0
  
  ####Create annual panel dataset####
  
  MyAnnualDataCCG <- MyPanelDataCCG[-c(2)] 
  MyAnnualDataCCG <- aggregate(. ~dept+year, data=MyAnnualDataCCG, sum, na.rm=TRUE, na.action=NULL)
  
  #create variables as a percent of total expenditure
  MyAnnualDataCCG$Social_Care_Procurement_Spend <-(MyAnnualDataCCG$Social_Care_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Hospital_Services_Procurement_Spend <-(MyAnnualDataCCG$Hospital_Services_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$GP_Services_Procurement_Spend <-(MyAnnualDataCCG$GP_Services_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Private_Sector_Procurement_Spend <-(MyAnnualDataCCG$Private_Sector_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Charity_Sector_Procurement_Spend <-(MyAnnualDataCCG$Charity_Sector_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Health_Services_Procurement_Spend <-(MyAnnualDataCCG$Health_Services_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Other_Health_Services_Procurement_Spend <-(MyAnnualDataCCG$Other_Health_Services_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Building_Maintenance_Procurement_Spend <-(MyAnnualDataCCG$Building_Maintenance_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$NotAssigned_Procurement_Spend <-(MyAnnualDataCCG$NotAssigned_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Medical_Nursing_Homes_Procurement_Spend <-(MyAnnualDataCCG$Medical_Nursing_Homes_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Foundational_Services_Procurement_Spend <-(MyAnnualDataCCG$Foundational_Services_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Dental_Services_Procurement_Spend <-(MyAnnualDataCCG$Dental_Services_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Professional_Services_Spend <-(MyAnnualDataCCG$Professional_Services_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Specialist_Services_Procurement_Spend <-(MyAnnualDataCCG$Specialist_Services_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Food_Procurement_Spend <-(MyAnnualDataCCG$Food_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Transport_Procurement_Spend <-(MyAnnualDataCCG$Transport_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  MyAnnualDataCCG$Total_Management_Procurement_Spend <-MyAnnualDataCCG$Management_Procurement_Spend
  MyAnnualDataCCG$Management_Procurement_Spend <-(MyAnnualDataCCG$Management_Procurement_Spend/MyAnnualDataCCG$Total_Procurement_Spend)*100
  
  
  MyAnnualDataCCG$nogpPrivate_Sector_Procurement_Spend <-(MyAnnualDataCCG$nogpPrivate_Sector_Procurement_Spend/MyAnnualDataCCG$NOGPTotal_Procurement_Spend)*100
  MyAnnualDataCCG$nogpHealth_Services_Procurement_Spend <-(MyAnnualDataCCG$nogpHealth_Services_Procurement_Spend/MyAnnualDataCCG$NOGPTotal_Procurement_Spend)*100
  
  #create lagged variable
  MyAnnualDataCCGlags <- MyAnnualDataCCG[c("dept","year","Private_Sector_Procurement_Spend")]
  MyAnnualDataCCGlags$lag_check_private <- MyAnnualDataCCGlags$Private_Sector_Procurement_Spend
  MyAnnualDataCCGlags <- MyAnnualDataCCGlags[-c(3)]
  MyAnnualDataCCGlags$year <- as.character(MyAnnualDataCCGlags$year)
  MyAnnualDataCCGlags$year <- as.double(MyAnnualDataCCGlags$year)+1
  MyAnnualDataCCG <- merge(MyAnnualDataCCG, MyAnnualDataCCGlags, by = c("dept", "year"),all=T)
  
  #merge with controls
  controldata <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CCG-Outsourcing/main/Data/Annual_Mortality_and_Control_Variables_ccg.csv"))
  
  timeinvariant <- unique(controldata[c("dept","ccg19cd","BNG_E","BNG_N","LONG", "LAT", "Shape__Are", "Shape__Len", "IMD_2019_Extent", "Health_Proportion_10_per","RGN19CD", "RGN19NM","RUC11CD", "RUC11","Broad_RUC11" , "Total_Rural_population_2011", "Total_Urban_population_2011", "id", "number_of_doctors", "Treatable_Mortality_2013" )])
  timevariant <- controldata[c("dept", "year","over70", "over75", "over80", "treatable_mortality_deaths","Treatable_Mortality_Rate", "Preventatable_Mortality_Rate","Managerial_occupation", "Professional_occupation" ,"Unemployment_percent","Qual_lvl4_percent", "BAME_percent","Est_Population" , "Local_Authority_Expenditure", "Claimant_percent", "GDHI_per_person", "CCGpop", "Local_Authority_Spend_per_pop", "professional_and_managerial" )]
  
  timeinvariant <- timeinvariant[complete.cases(timeinvariant$id),]
  
  MyAnnualDataCCG <- merge(MyAnnualDataCCG, timevariant, by=c("dept", "year"), all=T)
  MyAnnualDataCCG <- merge(MyAnnualDataCCG, timeinvariant, by=c("dept"), all.x=T)
  
  ####Create Lagged Variables####
  MyAnnualDataCCG$total_spend_10millions <- MyAnnualDataCCG$Total_Procurement_Spend/10000000
  
  MyAnnualDataCCGlags <- MyAnnualDataCCG[c("dept","year","Private_Sector_Procurement_Spend","treatable_mortality_deaths" ,"total_spend_10millions", "Treatable_Mortality_Rate", "Local_Authority_Spend_per_pop", "Claimant_percent", "CCGpop", "Unemployment_percent", "BAME_percent", "Qual_lvl4_percent", "GDHI_per_person", "professional_and_managerial", "Total_Private_sector_spend")]
  
  MyAnnualDataCCGlags$year <- as.character(MyAnnualDataCCGlags$year)
  
  MyAnnualDataCCGlags$year <- as.double(MyAnnualDataCCGlags$year)+1
  
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="Private_Sector_Procurement_Spend"] <- "Lagged_Private_Procurement"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="total_spend_10millions"] <- "Lagged_Total_Spend"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="Treatable_Mortality_Rate"] <- "Lagged_Treatable_Mortality_Rate"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="Local_Authority_Spend_per_pop"] <- "Lagged_Local_Authority_Spend_per_pop"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="Claimant_percent"] <- "Lagged_Claimant_percent"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="CCGpop"] <- "Lagged_CCGpop"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="Unemployment_percent"] <- "Lagged_Unemployment_percent"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="BAME_percent"] <- "Lagged_BAME_percent"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="Qual_lvl4_percent"] <- "Lagged_Qual_lvl4_percent"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="GDHI_per_person"] <- "Lagged_GDHI_per_person"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="professional_and_managerial"] <- "Lagged_professional_and_managerial"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="Total_Private_sector_spend"] <- "Lagged_Total_Private_sector_spend"
  names(MyAnnualDataCCGlags)[names(MyAnnualDataCCGlags)=="treatable_mortality_deaths"] <- "Lagged_treatable_mortality_deaths"
  
  MyAnnualDataCCG <- merge(MyAnnualDataCCG, MyAnnualDataCCGlags, by = c("dept", "year"),all=T)
  MyAnnualDataCCG <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$dept),]
  
}


Create_table_1 <- function(MyAnnualDataCCG) {
  
  #CCG data
  plmdata <- pdata.frame(MyAnnualDataCCG, index = c("dept", "year"))
  
  completegpdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$number_of_doctors),]
  completegpdata <- completegpdata[complete.cases(completegpdata$Private_Sector_Procurement_Spend),]
  
  completetreatdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$Treatable_Mortality_2013),]
  completetreatdata <- completetreatdata[complete.cases(completetreatdata$Private_Sector_Procurement_Spend),]
  
  
  #LA data
  
  MyAnnualDataCCG1315 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2013"|MyAnnualDataCCG$year=="2014"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1315$threeyears <- "X2013.2015"  
  
  MyAnnualDataCCG1416 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2014"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1416$threeyears <- "X2014.2016"  
  
  MyAnnualDataCCG1517 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1517$threeyears <- "X2015.2017"  
  
  MyAnnualDataCCG1618 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2018"),]
  MyAnnualDataCCG1618$threeyears <- "X2016.2018"  
  
  MyAnnualDataCCG1719 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2018"|MyAnnualDataCCG$year=="2019"),]
  MyAnnualDataCCG1719$threeyears <- "X2017.2019"  
  
  MyAnnualDataCCG1820 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2018"|MyAnnualDataCCG$year=="2019"|MyAnnualDataCCG$year=="2020"),]
  MyAnnualDataCCG1820$threeyears <- "X2018.2020"  
  
  MyAnnualDataCCG1315 <- MyAnnualDataCCG1315[-c(2)]
  MyAnnualDataCCG1416 <- MyAnnualDataCCG1416[-c(2)]
  MyAnnualDataCCG1517 <- MyAnnualDataCCG1517[-c(2)]
  MyAnnualDataCCG1618 <- MyAnnualDataCCG1618[-c(2)]
  MyAnnualDataCCG1719 <- MyAnnualDataCCG1719[-c(2)]
  MyAnnualDataCCG1820 <- MyAnnualDataCCG1820[-c(2)]
  
  MyAnnualDataCCG1315 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1315, sum)
  MyAnnualDataCCG1416 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1416, sum)
  MyAnnualDataCCG1517 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1517, sum)
  MyAnnualDataCCG1618 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1618, sum)
  MyAnnualDataCCG1719 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1719, sum)
  MyAnnualDataCCG1820 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1820, sum)
  
  threeyrspend <- rbind(MyAnnualDataCCG1315,MyAnnualDataCCG1416,MyAnnualDataCCG1517,MyAnnualDataCCG1618,MyAnnualDataCCG1719,MyAnnualDataCCG1820 )
  
  
  
  LAcontrols <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CCG-Outsourcing/main/Data/Three_Year_Mortality_and_Control_Variables_LA.csv"))
  LAdata <- merge(LAcontrols, unique(MyAnnualDataCCG[c("dept", "ccg19cd")]), by= "ccg19cd", all.x=T)  
  
  LAdata <- merge(LAdata, threeyrspend, by=c("dept", "threeyears"),all=T)  
  
  LAdata$Private_Sector_Procurement_Spend <- (LAdata$Total_Private_sector_spend/LAdata$Total_Procurement_Spend)*100
  
  #create LA lags
  LA_lags <- LAdata[c("Private_Sector_Procurement_Spend", "Local_Authority_Expenditure", "Total_Procurement_Spend", "LAD19CD", "year")]
  LA_lags$year <- as.character(LA_lags$year)
  
  LA_lags$year <- as.double(LA_lags$year)+1
  LA_lags$total_spend_10millions <- LA_lags$Total_Procurement_Spend/10000000
  
  names(LA_lags)[names(LA_lags)=="Private_Sector_Procurement_Spend"] <- "Lagged_Private_Procurement"
  names(LA_lags)[names(LA_lags)=="total_spend_10millions"] <- "Lagged_Total_Spend"
  names(LA_lags)[names(LA_lags)=="Local_Authority_Expenditure"] <- "Lagged_Local_Authority_Spend_per_pop"
  
  LAdata <- merge(LAdata, LA_lags, by=c("LAD19CD", "year"),all=T)
  names(LAdata)[names(LAdata)=="Est_Population"] <- "CCGpop"
  names(LAdata)[names(LAdata)=="Treatable_mortality_rate"] <- "Treatable_Mortality_Rate"
  LAdata$Lagged_Local_Authority_Spend_per_pop <- LAdata$Lagged_Local_Authority_Spend_per_pop/LAdata$CCGpop
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  fit <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ number_of_doctors, data = completegpdata))
  fit2 <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ Treatable_Mortality_2013, data = completetreatdata))
  
  FinalFE <- plm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial,  data=plmdata, index = c("dept", "year"), effect = "twoway", model = "within")
  FinalFD <- plm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop +Lagged_Total_Spend+ Claimant_percent + log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent+log(GDHI_per_person)+professional_and_managerial ,  data=plmdata, index = c("dept", "year"),  model = "fd")
  FinalCBPS_GP <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), weights = fit$fitted, data=completegpdata)
  FinalCBPS_mortality <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), weights = fit2$fitted, data=completetreatdata)
  FinalMLM <- lmerTest::lmer(log(Treatable_Mortality_Rate) ~ 1 + Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+ Lagged_Total_Spend+Claimant_percent+log(CCGpop)+Unemployment_percent+BAME_percent+Qual_lvl4_percent+log(GDHI_per_person)+professional_and_managerial+factor(year)+ (1 | ccg19cd), data = LAdata, control = lmerControl(optimizer = "bobyqa"))
  
  
  class(FinalMLM) <- "lmerMod"
  
  
  rows <- tribble(~term,          ~`ln. Treatable Mortality [95% ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [95% ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [95% ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [95% ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [95% ci]`,  ~`p-value`, 
                  'CCG Fixed Effects', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes','Yes',  'Yes','No','No',
                  'Time Fixed Effects', 'Yes',  'Yes','Yes','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',
                  'Clustered Standard Errors', 'Yes',  'Yes','Yes','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',
                  'Demographic Control Variables', 'Yes',  'Yes','Yes','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',)
  
  cm <- c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)", 
          "Lagged_Local_Authority_Spend_per_pop" = "LA Spend (£000s per person)", 
          "Lagged_Total_Spend" = "Total CCG Spend (£Ms)", 
          "log(CCGpop)"="Population Size",
          "log(GDHI_per_person)" = "Average Disposable H.hold Income")
  
  
  FinalFEsum <- as.list(modelsummary(FinalFE, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  FinalFDsum <- as.list(modelsummary(FinalFD, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  FinalCBPS_GPsum <- as.list(modelsummary(FinalCBPS_GP, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  FinalCBPS_mortalitysum <- as.list(modelsummary(FinalCBPS_mortality, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  FinalMLMsum <- as.list(modelsummary(FinalMLM, output = "modelsummary_list", statistic = c("conf.int","p={p.value}"), statistic_override=list(sqrt(diag(vcovCR(FinalMLM, type = "CR2"))))))
  
  
  FinalFEsum$tidy$p.value <- coef_test(FinalFE, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$p
  FinalFEsum$tidy$std.error <- coef_test(FinalFE, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  FinalFEsum$tidy$conf.low <- FinalFEsum$tidy$estimate-(1.96*coef_test(FinalFE, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE)
  FinalFEsum$tidy$conf.high <- FinalFEsum$tidy$estimate+(1.96*coef_test(FinalFE, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE)
  FinalFEsum$tidy$estimate <- FinalFEsum$tidy$estimate
  
  FinalFDsum$tidy$p.value <- coef_test(FinalFD, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$p
  FinalFDsum$tidy$std.error <- coef_test(FinalFD, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  FinalFDsum$tidy$conf.low <- FinalFDsum$tidy$estimate-(1.96*coef_test(FinalFD, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE)
  FinalFDsum$tidy$conf.high <- FinalFDsum$tidy$estimate+(1.96*coef_test(FinalFD, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE)
  FinalFDsum$tidy$estimate <- FinalFDsum$tidy$estimate
  
  FinalCBPS_GPsum$tidy$p.value <- coef_test(FinalCBPS_GP, vcov = "CR2", cluster = completegpdata$dept, test = "Satterthwaite")$p
  FinalCBPS_GPsum$tidy$std.error <- coef_test(FinalCBPS_GP, vcov = "CR2", cluster = completegpdata$dept, test = "Satterthwaite")$SE
  FinalCBPS_GPsum$tidy$conf.low <- FinalCBPS_GPsum$tidy$estimate-(1.96*coef_test(FinalCBPS_GP, vcov = "CR2", cluster = completegpdata$dept, test = "Satterthwaite")$SE)
  FinalCBPS_GPsum$tidy$conf.high <- FinalCBPS_GPsum$tidy$estimate+(1.96*coef_test(FinalCBPS_GP, vcov = "CR2", cluster = completegpdata$dept, test = "Satterthwaite")$SE)
  FinalCBPS_GPsum$tidy$estimate <- FinalCBPS_GPsum$tidy$estimate
  
  FinalCBPS_mortalitysum$tidy$p.value <- coef_test(FinalCBPS_mortality, vcov = "CR2", cluster = completetreatdata$dept, test = "Satterthwaite")$p
  FinalCBPS_mortalitysum$tidy$std.error <- coef_test(FinalCBPS_mortality, vcov = "CR2", cluster = completetreatdata$dept, test = "Satterthwaite")$SE
  FinalCBPS_mortalitysum$tidy$conf.low <- FinalCBPS_mortalitysum$tidy$estimate-(1.96*coef_test(FinalCBPS_mortality, vcov = "CR2", cluster = completetreatdata$dept, test = "Satterthwaite")$SE)
  FinalCBPS_mortalitysum$tidy$conf.high <- FinalCBPS_mortalitysum$tidy$estimate+(1.96*coef_test(FinalCBPS_mortality, vcov = "CR2", cluster = completetreatdata$dept, test = "Satterthwaite")$SE)
  FinalCBPS_mortalitysum$tidy$estimate <- FinalCBPS_mortalitysum$tidy$estimate
  
  
  modelsummary(list("ln. Treatable Mortality [.95 ci]"=FinalFEsum,"p-value"=FinalFEsum,"ln. Treatable Mortality [.95 ci]"=FinalFDsum,"p-value"=FinalFDsum,"ln. Treatable Mortality [.95 ci]"=FinalCBPS_GPsum,"p-value"=FinalCBPS_GPsum,"ln. Treatable Mortality [.95 ci]"=FinalCBPS_mortalitysum,"p-value"=FinalCBPS_mortalitysum,"ln. Treatable Mortality [.95 ci]"=FinalMLMsum,"p-value"=FinalMLMsum),
               coef_omit = "Intercept|dept|year", add_rows = rows, 
               coef_map=cm,fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
               notes = list('Table reports results from multivariate longitudinal regression models.',
                            'Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            'Tr. Mortality, Population and Income are log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'For full model expressions see supplementary material (S.2,p.4)',
                            'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)',
                            'Satterthwaite degrees of freedom used in MLM',
                            'Demographic Control variables include: Degree education (percent), Managerial or professional occupation (percent), Ethnic minority (percent), Unemployment rate (percent) and Claimant Rate (percent)'),
               output = "kableExtra") %>%
    add_header_above(c(" ", "Fixed Effects" = 2, "First Differences" = 2, "Covariate Balancing (1)" = 2, "Covariate Balancing (2)" = 2, "Multi-Level Model" = 2))
    # tab_spanner(label = 'Fixed Effects', columns = 2:3) \%>\%
    # tab_spanner(label = 'First Differences', columns = 4:5) %>%
    # tab_spanner(label = 'Covariate Balancing (1)', columns = 6:7) %>%
    # tab_spanner(label = 'Covariate Balancing (2)', columns = 8:9) %>%
    # tab_spanner(label = 'Multi-Level Model', columns = 10:11)
  

}


Create_figure_1 <- function(MyDataCCG) {
  myDataCCGfig1 <- myDataCCG
  #format date variable
  myDataCCGfig1$date <- as.Date(myDataCCGfig1$date, format =  "%Y-%m-%d")
  
  #remove observations before April 2013
  
  myDataCCGfig1 <- myDataCCGfig1[which(myDataCCGfig1$date>="2013-04-01"),]
  
  
  #identify private sector companies - 4 possible conditions: matched with companies house and has no charities registration number. Has a companies number and no charities registration number. has a company category and no charities registration number. Has a company number, and was placed as no match after my manual check. Treat numbers as double to induce NAs
  
  myDataCCGfig1$CompanyCategory[myDataCCGfig1$CompanyCategory==""] <- NA 
  
  #Attach SIC code
  SIC <- unique(read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CCG-Outsourcing/main/Data/Companies_House_SIC_lookup.csv")))
  
  ####Keep expenditures only####
  #myDataCCGfig1 <- myDataCCGfig1[which(myDataCCGfig1$amount>0),]
  #myDataTrust <- myDataTrust[which(myDataTrust$amount>0),]
  
  
  myDataCCGfig1 <- merge(myDataCCGfig1, SIC, by= "supplier", all.x=TRUE)
  ####retrieve numeric sic codes from the data####
  
  myDataCCGfig1$sic <- as.character(regmatches(myDataCCGfig1$SICCode.SicText_1, gregexpr("[[:digit:]]+", myDataCCGfig1$SICCode.SicText_1)))
  ####reducing sic digits to divisions and groups####
  myDataCCGfig1$sic2dig <- substr(myDataCCGfig1$sic, start = 1, stop = 2)
  myDataCCGfig1$sic3dig <- substr(myDataCCGfig1$sic, start = 1, stop = 3)
  
  myDataCCGfig1$sic2dig <- as.double(myDataCCGfig1$sic2dig)
  myDataCCGfig1$sic3dig <- as.double(myDataCCGfig1$sic3dig)
  myDataCCGfig1$sic <- as.double(myDataCCGfig1$sic)
  
  myDataCCGfig1$sic2dig[is.na(myDataCCGfig1$sic2dig)] <- 0
  myDataCCGfig1$sic3dig[is.na(myDataCCGfig1$sic3dig)] <- 0
  myDataCCGfig1$sic[is.na(myDataCCGfig1$sic)] <- 0
  
  
  ####isolate industries of interest####
  
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(ResidentialCare = ifelse(myDataCCGfig1$sic2dig=="87", 1,0))
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(HealthServices = ifelse(myDataCCGfig1$sic2dig=="86", 1,0))
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(ManagementConsultancy = ifelse(myDataCCGfig1$sic2dig=="70", 1,0))
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(BusinessSupport = ifelse(myDataCCGfig1$sic2dig=="82", 1,0))
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(SocialWork = ifelse(myDataCCGfig1$sic2dig=="88", 1,0))
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(RealEstate = ifelse(myDataCCGfig1$sic2dig=="68", 1,0))
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(PublicAdministration = ifelse(myDataCCGfig1$sic2dig=="84", 1,0))
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(PublicAdministration = ifelse(myDataCCGfig1$sic2dig=="84", 1,0))
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(ComputingServices = ifelse(myDataCCGfig1$sic2dig=="62", 1,0))
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(LegalAccounting = ifelse(myDataCCGfig1$sic2dig=="69", 1,0))
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(Transport = ifelse(myDataCCGfig1$sic2dig=="49", 1,0))
  
  myDataCCGfig1 <- myDataCCGfig1 %>% mutate(PrivateSector = ifelse((myDataCCGfig1$match_type=="Companies House"&is.na(as.double(myDataCCGfig1$CharityRegNo)))|(!is.na(as.double(myDataCCGfig1$CompanyNumber))& is.na(as.double(myDataCCGfig1$CharityRegNo)))| (!is.na(myDataCCGfig1$CompanyCategory)&is.na(as.double(myDataCCGfig1$CharityRegNo)))|(myDataCCGfig1$audit_type=="3"&myDataCCGfig1$match_type=="No Match"&!is.na(as.double(myDataCCGfig1$CompanyNumber))),1,0))
  
  #add arbitrary group variable for plotting purposes
  myDataCCGfig1$group <- 1
  myDataCCGfig1$group <- factor(myDataCCGfig1$group)
  
  #separare private and total amounts
  myDataCCGfig1private <- myDataCCGfig1[which(myDataCCGfig1$PrivateSector==1),]
  myDataCCGfig1health <- myDataCCGfig1[which(myDataCCGfig1$HealthServices==1),]
  myDataCCGfig1residential <- myDataCCGfig1[which(myDataCCGfig1$ResidentialCare==1),]
  myDataCCGfig1socialwork <- myDataCCGfig1[which(myDataCCGfig1$SocialWork==1),]
  myDataCCGfig1businesssup <- myDataCCGfig1[which(myDataCCGfig1$BusinessSupport==1),]
  myDataCCGfig1legal <- myDataCCGfig1[which(myDataCCGfig1$LegalAccounting==1),]
  myDataCCGfig1transport <- myDataCCGfig1[which(myDataCCGfig1$Transport==1),]
  myDataCCGfig1computing <- myDataCCGfig1[which(myDataCCGfig1$ComputingServices==1),]
  myDataCCGfig1management <- myDataCCGfig1[which(myDataCCGfig1$ManagementConsultancy==1),]
  myDataCCGfig1publicad <- myDataCCGfig1[which(myDataCCGfig1$PublicAdministration==1),]
  myDataCCGfig1realestate <- myDataCCGfig1[which(myDataCCGfig1$RealEstate==1),]
  myDataCCGfig1all <- myDataCCGfig1
  
  #select date and amount variables
  myDataCCGfig1private <- myDataCCGfig1private[c(3,7)]
  myDataCCGfig1realestate <- myDataCCGfig1realestate[c(3,7)]
  myDataCCGfig1health <- myDataCCGfig1health[c(3,7)]
  myDataCCGfig1socialwork <- myDataCCGfig1socialwork[c(3,7)]
  myDataCCGfig1residential <- myDataCCGfig1residential[c(3,7)]
  myDataCCGfig1transport <- myDataCCGfig1transport[c(3,7)]
  myDataCCGfig1businesssup <- myDataCCGfig1businesssup[c(3,7)]
  myDataCCGfig1management <- myDataCCGfig1management[c(3,7)]
  myDataCCGfig1computing <- myDataCCGfig1computing[c(3,7)]
  myDataCCGfig1legal <- myDataCCGfig1legal[c(3,7)]
  myDataCCGfig1publicad <- myDataCCGfig1publicad[c(3,7)]
  myDataCCGfig1all <- myDataCCGfig1all[c(3,7)]
  
  #sum amount spent on each day
  myDataCCGfig1private <- aggregate(myDataCCGfig1private[-1], myDataCCGfig1private["date"], sum)
  myDataCCGfig1health <- aggregate(myDataCCGfig1health[-1], myDataCCGfig1health["date"], sum)
  myDataCCGfig1transport <- aggregate(myDataCCGfig1transport[-1], myDataCCGfig1transport["date"], sum)
  myDataCCGfig1legal <- aggregate(myDataCCGfig1legal[-1], myDataCCGfig1legal["date"], sum)
  myDataCCGfig1socialwork <- aggregate(myDataCCGfig1socialwork[-1], myDataCCGfig1socialwork["date"], sum)
  myDataCCGfig1businesssup <- aggregate(myDataCCGfig1businesssup[-1], myDataCCGfig1businesssup["date"], sum)
  myDataCCGfig1management <- aggregate(myDataCCGfig1management[-1], myDataCCGfig1management["date"], sum)
  myDataCCGfig1publicad <- aggregate(myDataCCGfig1publicad[-1], myDataCCGfig1publicad["date"], sum)
  myDataCCGfig1residential <- aggregate(myDataCCGfig1residential[-1], myDataCCGfig1residential["date"], sum)
  myDataCCGfig1computing <- aggregate(myDataCCGfig1computing[-1], myDataCCGfig1computing["date"], sum)
  myDataCCGfig1realestate <- aggregate(myDataCCGfig1realestate[-1], myDataCCGfig1realestate["date"], sum)
  myDataCCGfig1all <- aggregate(myDataCCGfig1all[-1], myDataCCGfig1all["date"], sum)
  
  #order by date
  myDataCCGfig1private <- myDataCCGfig1private[order(myDataCCGfig1private$date),]
  myDataCCGfig1health <- myDataCCGfig1health[order(myDataCCGfig1health$date),]
  myDataCCGfig1residential <- myDataCCGfig1residential[order(myDataCCGfig1residential$date),]
  myDataCCGfig1socialwork <- myDataCCGfig1socialwork[order(myDataCCGfig1socialwork$date),]
  myDataCCGfig1publicad <- myDataCCGfig1publicad[order(myDataCCGfig1publicad$date),]
  myDataCCGfig1transport <- myDataCCGfig1transport[order(myDataCCGfig1transport$date),]
  myDataCCGfig1computing <- myDataCCGfig1computing[order(myDataCCGfig1computing$date),]
  myDataCCGfig1businesssup <- myDataCCGfig1businesssup[order(myDataCCGfig1businesssup$date),]
  myDataCCGfig1management <- myDataCCGfig1management[order(myDataCCGfig1management$date),]
  myDataCCGfig1legal <- myDataCCGfig1legal[order(myDataCCGfig1legal$date),]
  myDataCCGfig1realestate <- myDataCCGfig1realestate[order(myDataCCGfig1realestate$date),]
  myDataCCGfig1all <- myDataCCGfig1all[order(myDataCCGfig1all$date),]
  
  #create rolling sum variables
  myDataCCGfig1private <- myDataCCGfig1private %>% mutate(
    cum_rolling_365_private = sum_run(
      x = myDataCCGfig1private$amount, 
      k = 365, 
      idx = myDataCCGfig1private$date)
  )
  
  myDataCCGfig1health <- myDataCCGfig1health %>% mutate(
    cum_rolling_365_health = sum_run(
      x = myDataCCGfig1health$amount, 
      k = 365, 
      idx = myDataCCGfig1health$date)
  )
  
  myDataCCGfig1residential <- myDataCCGfig1residential %>% mutate(
    cum_rolling_365_residential = sum_run(
      x = myDataCCGfig1residential$amount, 
      k = 365, 
      idx = myDataCCGfig1residential$date)
  )
  
  myDataCCGfig1socialwork <- myDataCCGfig1socialwork %>% mutate(
    cum_rolling_365_socialwork = sum_run(
      x = myDataCCGfig1socialwork$amount, 
      k = 365, 
      idx = myDataCCGfig1socialwork$date)
  )
  
  myDataCCGfig1legal <- myDataCCGfig1legal %>% mutate(
    cum_rolling_365_legal = sum_run(
      x = myDataCCGfig1legal$amount, 
      k = 365, 
      idx = myDataCCGfig1legal$date)
  )
  
  myDataCCGfig1publicad <- myDataCCGfig1publicad %>% mutate(
    cum_rolling_365_pubad = sum_run(
      x = myDataCCGfig1publicad$amount, 
      k = 365, 
      idx = myDataCCGfig1publicad$date)
  )
  
  myDataCCGfig1transport <- myDataCCGfig1transport %>% mutate(
    cum_rolling_365_transport = sum_run(
      x = myDataCCGfig1transport$amount, 
      k = 365, 
      idx = myDataCCGfig1transport$date)
  )
  
  myDataCCGfig1businesssup <- myDataCCGfig1businesssup %>% mutate(
    cum_rolling_365_bussup = sum_run(
      x = myDataCCGfig1businesssup$amount, 
      k = 365, 
      idx = myDataCCGfig1businesssup$date)
  )
  
  myDataCCGfig1management <- myDataCCGfig1management %>% mutate(
    cum_rolling_365_management = sum_run(
      x = myDataCCGfig1management$amount, 
      k = 365, 
      idx = myDataCCGfig1management$date)
  )
  
  myDataCCGfig1computing <- myDataCCGfig1computing %>% mutate(
    cum_rolling_365_computing = sum_run(
      x = myDataCCGfig1computing$amount, 
      k = 365, 
      idx = myDataCCGfig1computing$date)
  )
  
  myDataCCGfig1realestate <- myDataCCGfig1realestate %>% mutate(
    cum_rolling_365_realestate = sum_run(
      x = myDataCCGfig1realestate$amount, 
      k = 365, 
      idx = myDataCCGfig1realestate$date)
  )
  
  myDataCCGfig1all <- myDataCCGfig1all %>% mutate(
    cum_rolling_365_all = sum_run(
      x = myDataCCGfig1all$amount, 
      k = 365, 
      idx = myDataCCGfig1all$date)
  )
  
  #combine datasets
  myDataCCGfig1rolling <- merge(myDataCCGfig1all, myDataCCGfig1private, by="date",all=T)
  myDataCCGfig1rolling <- merge(myDataCCGfig1rolling, myDataCCGfig1health[c(1,3)], by="date",all=T)
  myDataCCGfig1rolling <- merge(myDataCCGfig1rolling, myDataCCGfig1publicad[c(1,3)], by="date",all=T)
  myDataCCGfig1rolling <- merge(myDataCCGfig1rolling, myDataCCGfig1realestate[c(1,3)], by="date",all=T)
  myDataCCGfig1rolling <- merge(myDataCCGfig1rolling, myDataCCGfig1computing[c(1,3)], by="date",all=T)
  myDataCCGfig1rolling <- merge(myDataCCGfig1rolling, myDataCCGfig1management[c(1,3)], by="date",all=T)
  myDataCCGfig1rolling <- merge(myDataCCGfig1rolling, myDataCCGfig1businesssup[c(1,3)], by="date",all=T)
  myDataCCGfig1rolling <- merge(myDataCCGfig1rolling, myDataCCGfig1residential[c(1,3)], by="date",all=T)
  myDataCCGfig1rolling <- merge(myDataCCGfig1rolling, myDataCCGfig1socialwork[c(1,3)], by="date",all=T)
  myDataCCGfig1rolling <- merge(myDataCCGfig1rolling, myDataCCGfig1legal[c(1,3)], by="date",all=T)
  myDataCCGfig1rolling <- merge(myDataCCGfig1rolling, myDataCCGfig1transport[c(1,3)], by="date",all=T)
  
  #replace NAs with previous cumulative spends
  
  myDataCCGfig1rolling[which(myDataCCGfig1rolling$date=="2013-04-01"),]$cum_rolling_365_pubad <-0
  myDataCCGfig1rolling[which(myDataCCGfig1rolling$date=="2013-04-01"),]$cum_rolling_365_realestate <-0
  myDataCCGfig1rolling[which(myDataCCGfig1rolling$date=="2013-04-01"),]$cum_rolling_365_computing <-0
  myDataCCGfig1rolling[which(myDataCCGfig1rolling$date=="2013-04-01"),]$cum_rolling_365_management <-0
  myDataCCGfig1rolling[which(myDataCCGfig1rolling$date=="2013-04-01"),]$cum_rolling_365_bussup <-0
  myDataCCGfig1rolling[which(myDataCCGfig1rolling$date=="2013-04-01"),]$cum_rolling_365_socialwork <-0
  myDataCCGfig1rolling[which(myDataCCGfig1rolling$date=="2013-04-01"),]$cum_rolling_365_residential <-0
  myDataCCGfig1rolling[which(myDataCCGfig1rolling$date=="2013-04-01"),]$cum_rolling_365_legal <-0
  myDataCCGfig1rolling[which(myDataCCGfig1rolling$date=="2013-04-01"),]$cum_rolling_365_transport <-0
  
  myDataCCGfig1rolling$cum_rolling_365_private2 <- na.locf(myDataCCGfig1rolling$cum_rolling_365_private)
  myDataCCGfig1rolling$cum_rolling_365_pubad2 <- na.locf(myDataCCGfig1rolling$cum_rolling_365_pubad)
  myDataCCGfig1rolling$cum_rolling_365_computing2 <- na.locf(myDataCCGfig1rolling$cum_rolling_365_computing)
  myDataCCGfig1rolling$cum_rolling_365_legal2 <- na.locf(myDataCCGfig1rolling$cum_rolling_365_legal)
  myDataCCGfig1rolling$cum_rolling_365_management2 <- na.locf(myDataCCGfig1rolling$cum_rolling_365_management)
  myDataCCGfig1rolling$cum_rolling_365_realestate2 <- na.locf(myDataCCGfig1rolling$cum_rolling_365_realestate)
  myDataCCGfig1rolling$cum_rolling_365_transport2 <- na.locf(myDataCCGfig1rolling$cum_rolling_365_transport)
  myDataCCGfig1rolling$cum_rolling_365_health2 <- na.locf(myDataCCGfig1rolling$cum_rolling_365_health)
  myDataCCGfig1rolling$cum_rolling_365_bussup2 <- na.locf(myDataCCGfig1rolling$cum_rolling_365_bussup)
  myDataCCGfig1rolling$cum_rolling_365_residential2 <- na.locf(myDataCCGfig1rolling$cum_rolling_365_residential)
  myDataCCGfig1rolling$cum_rolling_365_socialwork2 <- na.locf(myDataCCGfig1rolling$cum_rolling_365_socialwork)
  
  #work out private sector spend as a % of total spend in any given 365 day period
  myDataCCGfig1rolling$Rolling_Percent <- myDataCCGfig1rolling$cum_rolling_365_private2/myDataCCGfig1rolling$cum_rolling_365_all*100
  
  myDataCCGfig1rolling$cum_rolling_365_health2 <- myDataCCGfig1rolling$cum_rolling_365_health2/myDataCCGfig1rolling$cum_rolling_365_all*100
  
  myDataCCGfig1rolling$cum_rolling_365_pubad2 <- myDataCCGfig1rolling$cum_rolling_365_pubad2/myDataCCGfig1rolling$cum_rolling_365_all*100
  
  myDataCCGfig1rolling$cum_rolling_365_realestate2 <- myDataCCGfig1rolling$cum_rolling_365_realestate2/myDataCCGfig1rolling$cum_rolling_365_all*100
  
  myDataCCGfig1rolling$cum_rolling_365_computing2 <- myDataCCGfig1rolling$cum_rolling_365_computing2/myDataCCGfig1rolling$cum_rolling_365_all*100
  
  myDataCCGfig1rolling$cum_rolling_365_management2 <- myDataCCGfig1rolling$cum_rolling_365_management2/myDataCCGfig1rolling$cum_rolling_365_all*100
  
  myDataCCGfig1rolling$cum_rolling_365_bussup2 <- myDataCCGfig1rolling$cum_rolling_365_bussup2/myDataCCGfig1rolling$cum_rolling_365_all*100
  
  myDataCCGfig1rolling$cum_rolling_365_residential2 <- myDataCCGfig1rolling$cum_rolling_365_residential2/myDataCCGfig1rolling$cum_rolling_365_all*100
  
  myDataCCGfig1rolling$cum_rolling_365_socialwork2 <- myDataCCGfig1rolling$cum_rolling_365_socialwork2/myDataCCGfig1rolling$cum_rolling_365_all*100
  
  myDataCCGfig1rolling$cum_rolling_365_legal2 <- myDataCCGfig1rolling$cum_rolling_365_legal2/myDataCCGfig1rolling$cum_rolling_365_all*100
  
  myDataCCGfig1rolling$cum_rolling_365_transport2 <- myDataCCGfig1rolling$cum_rolling_365_transport2/myDataCCGfig1rolling$cum_rolling_365_all*100
  
  
  
  plotdata <- myDataCCGfig1[c("date","amount","PrivateSector")]
  
  plotdata$month <- format(plotdata$date,"%m/%y")
  
  plotdataPriv <- plotdata[which(plotdata$PrivateSector==1),]
  names(plotdataPriv)[names(plotdataPriv)=="amount"] <- "private_spend"
  
  plotdata <- aggregate(. ~month, data=plotdata, sum)
  plotdataPriv <- aggregate(. ~month, data=plotdataPriv, sum)
  
  plotdata <- merge(plotdata[c(1,3)], plotdataPriv[c(1,3)], by="month", all=T)
  
  library(lubridate)
  plotdata$month <- as.character(plotdata$month)
  plotdata$month <- my(plotdata$month)
  
  plotdata$outsourcing <- plotdata$private_spend/plotdata$amount
  
  
  one <- myDataCCGfig1rolling[c("date", "cum_rolling_365_health2")]
  two <- myDataCCGfig1rolling[c("date", "cum_rolling_365_legal2")]
  three <- myDataCCGfig1rolling[c("date", "cum_rolling_365_socialwork2")]
  four <- myDataCCGfig1rolling[c("date", "cum_rolling_365_residential2")]
  five <- myDataCCGfig1rolling[c("date", "cum_rolling_365_bussup2")]
  six <- myDataCCGfig1rolling[c("date", "cum_rolling_365_management2")]
  seven <- myDataCCGfig1rolling[c("date", "cum_rolling_365_computing2")]
  eight <- myDataCCGfig1rolling[c("date", "cum_rolling_365_realestate2")]
  nine <- myDataCCGfig1rolling[c("date", "cum_rolling_365_pubad2")]
  ten <- myDataCCGfig1rolling[c("date", "cum_rolling_365_transport2")]
  
  names(one)[names(one)=="cum_rolling_365_health2"] <- "rolling_per"
  names(two)[names(two)=="cum_rolling_365_legal2"] <- "rolling_per"
  names(three)[names(three)=="cum_rolling_365_socialwork2"] <- "rolling_per"
  names(four)[names(four)=="cum_rolling_365_residential2"] <- "rolling_per"
  names(five)[names(five)=="cum_rolling_365_bussup2"] <- "rolling_per"
  names(six)[names(six)=="cum_rolling_365_management2"] <- "rolling_per"
  names(seven)[names(seven)=="cum_rolling_365_computing2"] <- "rolling_per"
  names(eight)[names(eight)=="cum_rolling_365_realestate2"] <- "rolling_per"
  names(nine)[names(nine)=="cum_rolling_365_pubad2"] <- "rolling_per"
  names(ten)[names(ten)=="cum_rolling_365_transport2"] <- "rolling_per"
  
  one$rolling_per <- (one$rolling_per-one[which(one$date=="2014-04-05"),]$rolling_per)/one[which(one$date=="2014-04-05"),]$rolling_per
  two$rolling_per <- (two$rolling_per-two[which(two$date=="2014-04-05"),]$rolling_per)/two[which(two$date=="2014-04-05"),]$rolling_per
  three$rolling_per <- (three$rolling_per-three[which(three$date=="2014-04-05"),]$rolling_per)/three[which(three$date=="2014-04-05"),]$rolling_per
  four$rolling_per <- (four$rolling_per-four[which(four$date=="2014-04-05"),]$rolling_per)/four[which(four$date=="2014-04-05"),]$rolling_per
  five$rolling_per <- (five$rolling_per-five[which(five$date=="2014-04-05"),]$rolling_per)/five[which(five$date=="2014-04-05"),]$rolling_per
  six$rolling_per <- (six$rolling_per-six[which(six$date=="2014-04-05"),]$rolling_per)/six[which(six$date=="2014-04-05"),]$rolling_per
  seven$rolling_per <- (seven$rolling_per-seven[which(seven$date=="2014-04-05"),]$rolling_per)/seven[which(seven$date=="2014-04-05"),]$rolling_per
  eight$rolling_per <- (eight$rolling_per-eight[which(eight$date=="2014-04-05"),]$rolling_per)/eight[which(eight$date=="2014-04-05"),]$rolling_per
  nine$rolling_per <- (nine$rolling_per-nine[which(nine$date=="2014-04-05"),]$rolling_per)/nine[which(nine$date=="2014-04-05"),]$rolling_per
  ten$rolling_per <- (ten$rolling_per-ten[which(ten$date=="2014-04-05"),]$rolling_per)/ten[which(ten$date=="2014-04-05"),]$rolling_per
  
  one$division <- "Healthcare\nServices"
  two$division <- "Legal\nand\nAccounting"
  three$division <- "Social\nWork"
  four$division <- "Residential\nCare"
  five$division <- "Business\nSupport"
  six$division <- "Management\nConsultancy"
  seven$division <- "It\nSupport"
  eight$division <- "Real\nEstate"
  nine$division <- "Public\nAdministration"
  ten$division <- "Transport"
  
  sparkplots <- rbind(one, two, three, four, five, six, seven, eight, nine, ten)
  
  #loadfonts(device = "win")
  
  sparkplots$date <- as.Date(sparkplots$date, format =  "%Y-%m-%d")
  
  theme_Publication <- function(base_size=9, base_family="Helvetica") {
    library(grid)
    library(ggthemes)
    (theme_foundation(base_size=base_size, base_family=base_family)
      + theme(plot.title = element_text(size = rel(1.2), hjust = 0.5),
              panel.background = element_rect(colour = NA),
              plot.background = element_rect(colour = NA),
              panel.border = element_rect(colour = NA),
              axis.title = element_text(size = rel(1)),
              axis.title.y = element_text(angle=90,vjust =2),
              axis.title.x = element_text(vjust = -0.2),
              axis.text = element_text(), 
              axis.line = element_line(colour="black"),
              axis.ticks = element_line(),
              panel.grid.major = element_line(colour="#f0f0f0"),
              panel.grid.minor = element_blank(),
              legend.key = element_rect(colour = NA),
              legend.position = "bottom",
              legend.direction = "horizontal",
              legend.key.size= unit(0.2, "cm"),
              legend.margin = unit(0, "cm"),
              legend.title = element_text(),
              plot.margin=unit(c(10,5,5,5),"mm"),
              strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
              strip.text = element_text()
      ))
    
  }
  
  
  spark <- ggplot(sparkplots, aes(x=date, y=rolling_per*100))+
    theme_Publication()+
    geom_line()+
    facet_wrap(~division,  ncol = 10)+
    scale_y_continuous(limits = c(-100,500))+
    scale_x_date(limits = as.Date(c("2014-04-05","2020-02-29")),  breaks = as.Date( c("2014-04-05","2020-02-29")), labels = c("2014", "2020"))+
    labs(y="For-profit Outsourcing\n(% change)", x="")+
    geom_hline(yintercept=0, linetype="dashed", colour = "grey")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.6), 
          panel.spacing = unit(0.5, "lines"),
          strip.text.x = element_text(size = 4),
          text=element_text(size=7))
  
  
  a <- myDataCCGfig1rolling[c("date","cum_rolling_365_health2")]
  myDataCCGfig1rolling$Other <- myDataCCGfig1rolling$Rolling_Percent-myDataCCGfig1rolling$cum_rolling_365_health2
  b <- myDataCCGfig1rolling[c("date","Other")]
  names(a)[names(a)=="cum_rolling_365_health2"] <- "rolled"
  
  names(b)[names(b)=="Other"] <- "rolled"
  a$provision <- "Healthcare Companies"
  b$provision <- "Other Companies"
  
  stacked <- rbind(a,b)
  
  stacked$`Provision type` <- factor(stacked$provision , levels=c("Other Companies", "Healthcare Companies") )
  cbPalette <- c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  stacked$date <- as.Date(stacked$date, format =  "%Y-%m-%d")
  
  
  #plot result
  plot1 <- ggplot(stacked, aes(x=date, y=rolled, fill=`Provision type`))+
    geom_area()+
    labs(x = "", y = "For-profit Outsourcing (%)")+
    scale_x_date(limits = as.Date(c("2013-07-01","2020-02-29")))+
    scale_y_continuous(limits = c(0,7.5))+theme_Publication()+
    scale_fill_manual(values=cbPalette)+theme(text=element_text(size=7))
  
  #warmingplot
  
  theme_strip <- theme_minimal()+
    theme(axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
    )
  
  library(RColorBrewer)
  col_strip <- brewer.pal(11, "RdBu")
  
  
  
  warmingstripes <- ggplot(plotdata,
                           aes(x = month, y = 1, fill = outsourcing*100))+
    geom_tile()+
    scale_x_date(limits = as.Date(c("2013-07-01","2020-02-29")))+
    scale_y_continuous(expand = c(0, 0))+
    scale_fill_gradientn(colors = rev(col_strip), name = "For-profit\nOutsourcing (%)")+
    guides(fill = guide_colorbar(barwidth = 1))+
    theme_strip+
    labs(x="")+theme(text=element_text(size=7), legend.key.size = unit(0.4, "cm"))
  
  
  
  
  
  mapdata <- MyAnnualDataCCG[c("Total_Private_sector_spend","Total_Procurement_Spend", "ccg19cd")]
  
  mapdata <- aggregate(.~ ccg19cd ,sum,data=mapdata, na.rm=T, na.action=NULL)
  projcrs <- "EPSG:27700"
  
  mapshape <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BGC_v2/FeatureServer/0/query?where=1%3D1&outFields=*&geometry=&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=geojson")
  names(mapshape)[names(mapshape)=="CCG19CD"] <- "ccg19cd"
  
  mapshape <- st_as_sf(mapshape)
  
  mapdata <- merge(mapshape, mapdata, by="ccg19cd", all.x=T)
  mapdata$outsourcing <- (mapdata$Total_Private_sector_spend/mapdata$Total_Procurement_Spend)*100
  
  
  theme_map <- function(...) {
    theme_minimal() +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #    panel.background = element_rect(fill = "transparent"), # bg of the panel
        #     plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        # legend.background = element_rect(fill = "transparent", color=NA), # get rid of legend bg
        #  legend.box.background = element_rect(fill = "transparent", color=NA),
        # panel.border = element_blank(),legend.title=element_text(size=8), 
        #  legend.text=element_text(size=7),legend.key.size = unit(0.3, "cm"),
        ...
      )
  }
  
  no_classes <- 6
  
  
  quantiles <- quantile(mapdata$outsourcing, 
                        probs = seq(0, 1, length.out = no_classes + 1), na.rm=T)
  
  # here I define custom labels (the default ones would be ugly)
  labels <- c()
  for(idx in 1:length(quantiles)){
    labels <- c(labels, paste0(round(quantiles[idx], 2), 
                               " - ", 
                               round(quantiles[idx + 1], 2)))
  }
  # I need to remove the last label 
  # because that would be something like "66.62 - NA"
  labels <- labels[1:length(labels)-1]
  
  # here I actually create a new 
  # variable on the dataset with the quantiles
  mapdata$outsourcing_quantiles <- cut(mapdata$outsourcing, 
                                       breaks = quantiles, 
                                       labels = labels, 
                                       include.lowest = T)
  
  
  library(RColorBrewer)
  private_map <- ggplot(data = mapdata ) +
    geom_sf(aes(fill = outsourcing_quantiles), color = NA) +
    theme_map()+
    labs(x = NULL, 
         y = NULL)+
    scale_fill_brewer(
      palette = "OrRd",
      name = "For-profit Outsourcing\n(%, 2013-2020)", na.value="grey")+
    theme(text=element_text(size=7), legend.key.size = unit(0.5, "cm"))
  
  
  
  
  library(cowplot)
  time <- cowplot::plot_grid(plot1, spark, ncol=1 ,labels="AUTO")
  map <- cowplot::plot_grid(time, private_map, align = "hv",ncol=2 ,labels=c("","C"))
  all <- cowplot::plot_grid(map, warmingstripes, align = "hv",ncol=1 ,labels=c("","D"), rel_heights = c(5,1))
  
  all
  
}


Create_figure_2 <- function(MyAnnualDataCCG){ 
  
  sum2 <- MyAnnualDataCCG[which(MyAnnualDataCCG$year=="2014"|MyAnnualDataCCG$year=="2015"|MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2018" ),]
  
  finalFEusingLM <- lm(treatable_mortality_deaths~Lagged_Total_Private_sector_spend+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+over70+professional_and_managerial +factor(dept)+factor(year),  data=sum2)
  
  multiplier <- finalFEusingLM$coefficients["Lagged_Total_Private_sector_spend"]
  multiplierlower <- confint(finalFEusingLM, "Lagged_Total_Private_sector_spend")[c(1)]
  multiplierupper <- confint(finalFEusingLM, "Lagged_Total_Private_sector_spend")[c(2)]
  
  sum2$spendchange <- sum2$Total_Private_sector_spend-sum2$Lagged_Total_Private_sector_spend
  sum2$deathchange <- sum2$treatable_mortality_deaths-sum2$Lagged_treatable_mortality_deaths
  
  sum2 <- sum2[complete.cases(sum2$dept),]
  
  sum2$adddeaths <- ((sum2$spendchange))*multiplier
  sum2$adddeathslwr <- ((sum2$spendchange))*multiplierlower
  sum2$adddeathsupr <- ((sum2$spendchange))*multiplierupper
  
  sum2$group=1
  
  sum2$syntheticvalue <- sum2$treatable_mortality_deaths 
  sum2$syntheticupper <- sum2$treatable_mortality_deaths 
  sum2$syntheticlower <- sum2$treatable_mortality_deaths 
  
  sum2[which(sum2$year==2014),]$syntheticvalue <- sum2[which(sum2$year==2014),]$treatable_mortality_deaths
  sum2[which(sum2$year==2014),]$syntheticupper <- sum2[which(sum2$year==2014),]$treatable_mortality_deaths
  sum2[which(sum2$year==2014),]$syntheticlower <- sum2[which(sum2$year==2014),]$treatable_mortality_deaths
  
  
  sum2[which(sum2$year==2015),]$syntheticvalue <- sum2[which(sum2$year==2014),]$syntheticvalue+(sum2[which(sum2$year==2015),]$deathchange)-(sum2[which(sum2$year==2015),]$adddeaths)
  sum2[which(sum2$year==2015),]$syntheticupper <- sum2[which(sum2$year==2014),]$syntheticupper+(sum2[which(sum2$year==2015),]$deathchange)-(sum2[which(sum2$year==2015),]$adddeathsupr)
  sum2[which(sum2$year==2015),]$syntheticlower <- sum2[which(sum2$year==2014),]$syntheticlower+(sum2[which(sum2$year==2015),]$deathchange)-(sum2[which(sum2$year==2015),]$adddeathslwr)
  
  sum2[which(sum2$year==2016),]$syntheticvalue <- sum2[which(sum2$year==2015),]$syntheticvalue+(sum2[which(sum2$year==2016),]$deathchange)-(sum2[which(sum2$year==2016),]$adddeaths)
  sum2[which(sum2$year==2016),]$syntheticupper <- sum2[which(sum2$year==2015),]$syntheticupper+(sum2[which(sum2$year==2016),]$deathchange)-(sum2[which(sum2$year==2016),]$adddeathsupr)
  sum2[which(sum2$year==2016),]$syntheticlower <- sum2[which(sum2$year==2015),]$syntheticlower+(sum2[which(sum2$year==2016),]$deathchange)-(sum2[which(sum2$year==2016),]$adddeathslwr)
  
  
  sum2[which(sum2$year==2017),]$syntheticvalue <- sum2[which(sum2$year==2016),]$syntheticvalue+(sum2[which(sum2$year==2017),]$deathchange)-(sum2[which(sum2$year==2017),]$adddeaths)
  sum2[which(sum2$year==2017),]$syntheticupper<- sum2[which(sum2$year==2016),]$syntheticupper+(sum2[which(sum2$year==2017),]$deathchange)-(sum2[which(sum2$year==2017),]$adddeathsupr)
  sum2[which(sum2$year==2017),]$syntheticlower <- sum2[which(sum2$year==2016),]$syntheticlower+(sum2[which(sum2$year==2017),]$deathchange)-(sum2[which(sum2$year==2017),]$adddeathslwr)
  
  sum2[which(sum2$year==2018),]$syntheticvalue <- sum2[which(sum2$year==2017),]$syntheticvalue+(sum2[which(sum2$year==2018),]$deathchange)-(sum2[which(sum2$year==2018),]$adddeaths)
  sum2[which(sum2$year==2018),]$syntheticupper<- sum2[which(sum2$year==2017),]$syntheticupper+(sum2[which(sum2$year==2018),]$deathchange)-(sum2[which(sum2$year==2018),]$adddeathsupr)
  sum2[which(sum2$year==2018),]$syntheticlower <- sum2[which(sum2$year==2017),]$syntheticlower+(sum2[which(sum2$year==2018),]$deathchange)-(sum2[which(sum2$year==2018),]$adddeathslwr)
  
  sum2[which(sum2$year==2019),]$syntheticvalue <- sum2[which(sum2$year==2018),]$syntheticvalue+(sum2[which(sum2$year==2019),]$deathchange)-(sum2[which(sum2$year==2019),]$adddeaths)
  sum2[which(sum2$year==2019),]$syntheticupper<- sum2[which(sum2$year==2018),]$syntheticupper+(sum2[which(sum2$year==2019),]$deathchange)-(sum2[which(sum2$year==2019),]$adddeathsupr)
  sum2[which(sum2$year==2019),]$syntheticlower <- sum2[which(sum2$year==2018),]$syntheticlower+(sum2[which(sum2$year==2019),]$deathchange)-(sum2[which(sum2$year==2019),]$adddeathslwr)
  
  
  tot <- sum(sum2[complete.cases(sum2$syntheticvalue),]$treatable_mortality_deaths, na.rm=T)-sum(sum2$syntheticvalue, na.rm=T)
  lwr <- sum(sum2[complete.cases(sum2$syntheticvalue),]$treatable_mortality_deaths, na.rm=T)-sum(sum2$syntheticupper, na.rm=T)
  upr <- sum(sum2[complete.cases(sum2$syntheticvalue),]$treatable_mortality_deaths, na.rm=T)-sum(sum2$syntheticlower, na.rm=T)
  
  
  #prelist <- sum2[which(sum2$year!="2014"&sum2$year!="2019"),]
  prelist <- sum2[complete.cases(sum2$syntheticvalue),]
  prelist <- prelist[complete.cases(prelist$ccg19cd),]
  
  prelistn <- prelist %>% group_by(ccg19cd) %>%tally()
  
  prelistn <- prelistn[which(prelistn$n>=5),]
  
  prelistn$keep <- 1
  
  
  sum2yr <-   sum2[c("ccg19cd","year","syntheticvalue","syntheticupper","treatable_mortality_deaths" ,"syntheticlower")]
  
  sum2yr <- merge(sum2yr, prelistn, by="ccg19cd", all.x=T)  
  
  sum2yr$year <- factor(sum2yr$year)
  
  sum2yr <- sum2yr[which(sum2yr$keep==1),]
  
  sum2yr <-   sum2yr[c("year","syntheticvalue","syntheticupper" ,"syntheticlower")]
  
  sum2yr <- aggregate(. ~year, data=sum2yr, sum,  na.rm=TRUE, na.action=NULL)
  
  sum2yr$group=1
  
  theme_Publication <- function(base_size=9) {
    library(grid)
    library(ggthemes)
    (theme_foundation(base_size=base_size)
      + theme(plot.title = element_text(size = rel(1.2), hjust = 0.5),
              text = element_text(),
              panel.background = element_rect(colour = NA),
              plot.background = element_rect(colour = NA),
              panel.border = element_rect(colour = NA),
              axis.title = element_text(size = rel(1)),
              axis.title.y = element_text(angle=90,vjust =2),
              axis.title.x = element_text(vjust = -0.2),
              axis.text = element_text(), 
              axis.line = element_line(colour="black"),
              axis.ticks = element_line(),
              panel.grid.major = element_line(colour="#f0f0f0"),
              panel.grid.minor = element_blank(),
              legend.key = element_rect(colour = NA),
              legend.position = "bottom",
              legend.direction = "horizontal",
              legend.key.size= unit(0.2, "cm"),
              legend.margin = unit(0, "cm"),
              legend.title = element_text(),
              plot.margin=unit(c(10,5,5,5),"mm"),
              strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
              strip.text = element_text()
      ))
    
  }
  
  fullobserved <- MyAnnualDataCCG[c("ccg19cd", "year", "treatable_mortality_deaths")]
  fullobserved <- merge(fullobserved, prelistn, by="ccg19cd", all.x=T)
  fullobserved <- fullobserved[which(fullobserved$keep==1),]
  fullobserved <- fullobserved[c("year", "treatable_mortality_deaths")]
  fullobserved <- aggregate(. ~year, data=fullobserved, sum,  na.rm=TRUE, na.action=NULL)
  
  sum2yrs <- merge(sum2yr, fullobserved, by="year", all=T)
  
  sum2yrs <- sum2yrs[which(sum2yrs$year!="2005"&sum2yrs$year!="2019"&sum2yrs$year!="2020"&sum2yrs$year!="2021"&sum2yrs$year!="2022"),]
  extrafont::loadfonts()
  
  sum2yrs$time <- as.double(as.character(sum2yrs$year))
  sum2yrs$group <- 1
  
  plot2 <- 
    ggplot(sum2yrs, aes(x = time, y = treatable_mortality_deaths, group=group)) +
    geom_line()+
    geom_line(aes(y=syntheticvalue),linetype = "dashed", color = "darkgray")+
    geom_ribbon(aes(ymin = syntheticlower, ymax = syntheticupper), 
                alpha=0.1, 
                linetype="dashed",
                color="lightgray")+
    ylim(c(16500,20000))+
    geom_vline(xintercept = 2013.33 ,linetype = "dotted")+
    labs(x="Year", y="Treatable Deaths (Total of 87 CCGs)")+
    geom_text(x=2013.33, y=19000, label="2012 Health and Social Care Act implemented", 
              size = 9/.pt)+
    geom_text(x=2015.3, y=17500, label = "Mortality if no extra outsourcing", size = 5/.pt, color = "darkgray", angle = 39)+
    geom_text(x=2015.3, y=17900, label = "Observed mortality", size = 5/.pt, angle = 39)+
    theme_Publication()+
    scale_x_continuous(breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),
                       labels=c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
  

  plot2
  
}


Create_figure_3 <- function(MyAnnualDataCCG){
  ####sinaplot_random####
  
  plan(multisession)
  set.seed(1001)
  
  MySyntheticDataCCG <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$Lagged_Private_Procurement),]
  
  randomerror1 <- (5+10.*pracma::rand(944,500))/10
  randomerror2 <- (6+8.*pracma::rand(944,500))/10
  randomerror3 <- (7+6.*pracma::rand(944,500))/10
  randomerror4 <- (8+4.*pracma::rand(944,500))/10
  randomerror5 <- (9+2.*pracma::rand(944,500))/10
  
  
  randomerror1 <- randomerror1*MySyntheticDataCCG$Lagged_Private_Procurement
  randomerror2 <- randomerror2*MySyntheticDataCCG$Lagged_Private_Procurement
  randomerror3 <- randomerror3*MySyntheticDataCCG$Lagged_Private_Procurement
  randomerror4 <- randomerror4*MySyntheticDataCCG$Lagged_Private_Procurement
  randomerror5 <- randomerror5*MySyntheticDataCCG$Lagged_Private_Procurement
  
  plan(multisession)
  
  my_lms1 <- future_lapply(1:500, function(x) summary(lm(log(MySyntheticDataCCG$Treatable_Mortality_Rate) ~ randomerror1[,x]+factor(MySyntheticDataCCG$dept)+factor(MySyntheticDataCCG$year)+log(MySyntheticDataCCG$GDHI_per_person)+MySyntheticDataCCG$Claimant_percent+  log(MySyntheticDataCCG$CCGpop) +MySyntheticDataCCG$Unemployment_percent +MySyntheticDataCCG$BAME_percent+MySyntheticDataCCG$Qual_lvl4_percent +MySyntheticDataCCG$Lagged_Total_Spend+MySyntheticDataCCG$Lagged_Local_Authority_Spend_per_pop+MySyntheticDataCCG$professional_and_managerial)))
  
  
  df1 <- my_lms1 %>%
    map("coefficients") %>% 
    do.call(rbind.data.frame, .) %>% 
    rownames_to_column %>% 
    as_tibble %>% 
    setNames(c("predictor", "b", "SE", "t", "p")) %>% 
    dplyr::arrange(p) %>% 
    dplyr::filter(str_detect(predictor, "random"))
  
  rm(my_lms1)
  
  
  my_lms2 <- future_lapply(1:500, function(x) summary(lm(log(MySyntheticDataCCG$Treatable_Mortality_Rate) ~ randomerror2[,x]+factor(MySyntheticDataCCG$dept)+factor(MySyntheticDataCCG$year)+log(MySyntheticDataCCG$GDHI_per_person)+MySyntheticDataCCG$Claimant_percent+  log(MySyntheticDataCCG$CCGpop) +MySyntheticDataCCG$Unemployment_percent +MySyntheticDataCCG$BAME_percent+MySyntheticDataCCG$Qual_lvl4_percent +MySyntheticDataCCG$Lagged_Total_Spend+MySyntheticDataCCG$Lagged_Local_Authority_Spend_per_pop+MySyntheticDataCCG$professional_and_managerial)))
  df2 <- my_lms2 %>%
    map("coefficients") %>% 
    do.call(rbind.data.frame, .) %>% 
    rownames_to_column %>% 
    as_tibble %>% 
    setNames(c("predictor", "b", "SE", "t", "p")) %>% 
    dplyr::arrange(p) %>% 
    dplyr::filter(str_detect(predictor, "random"))
  
  rm(my_lms2)
  
  
  my_lms3 <- future_lapply(1:500, function(x) summary(lm(log(MySyntheticDataCCG$Treatable_Mortality_Rate) ~ randomerror3[,x]+factor(MySyntheticDataCCG$dept)+factor(MySyntheticDataCCG$year)+log(MySyntheticDataCCG$GDHI_per_person)+MySyntheticDataCCG$Claimant_percent+  log(MySyntheticDataCCG$CCGpop) +MySyntheticDataCCG$Unemployment_percent +MySyntheticDataCCG$BAME_percent+MySyntheticDataCCG$Qual_lvl4_percent +MySyntheticDataCCG$Lagged_Total_Spend+MySyntheticDataCCG$Lagged_Local_Authority_Spend_per_pop+MySyntheticDataCCG$professional_and_managerial)))
  df3 <- my_lms3 %>%
    map("coefficients") %>% 
    do.call(rbind.data.frame, .) %>% 
    rownames_to_column %>% 
    as_tibble %>% 
    setNames(c("predictor", "b", "SE", "t", "p")) %>% 
    dplyr::arrange(p) %>% 
    dplyr::filter(str_detect(predictor, "random"))
  
  rm(my_lms3)
  
  
  
  my_lms4 <- future_lapply(1:500, function(x) summary(lm(log(MySyntheticDataCCG$Treatable_Mortality_Rate) ~ randomerror4[,x]+factor(MySyntheticDataCCG$dept)+factor(MySyntheticDataCCG$year)+log(MySyntheticDataCCG$GDHI_per_person)+MySyntheticDataCCG$Claimant_percent+  log(MySyntheticDataCCG$CCGpop) +MySyntheticDataCCG$Unemployment_percent +MySyntheticDataCCG$BAME_percent+MySyntheticDataCCG$Qual_lvl4_percent +MySyntheticDataCCG$Lagged_Total_Spend+MySyntheticDataCCG$Lagged_Local_Authority_Spend_per_pop+MySyntheticDataCCG$professional_and_managerial)))
  df4 <- my_lms4 %>%
    map("coefficients") %>% 
    do.call(rbind.data.frame, .) %>% 
    rownames_to_column %>% 
    as_tibble %>% 
    setNames(c("predictor", "b", "SE", "t", "p")) %>% 
    dplyr::arrange(p) %>% 
    dplyr::filter(str_detect(predictor, "random"))
  
  rm(my_lms4)
  
  
  
  my_lms5 <- future_lapply(1:500, function(x) summary(lm(log(MySyntheticDataCCG$Treatable_Mortality_Rate) ~ randomerror5[,x]+factor(MySyntheticDataCCG$dept)+factor(MySyntheticDataCCG$year)+log(MySyntheticDataCCG$GDHI_per_person)+MySyntheticDataCCG$Claimant_percent+  log(MySyntheticDataCCG$CCGpop) +MySyntheticDataCCG$Unemployment_percent +MySyntheticDataCCG$BAME_percent+MySyntheticDataCCG$Qual_lvl4_percent +MySyntheticDataCCG$Lagged_Total_Spend+MySyntheticDataCCG$Lagged_Local_Authority_Spend_per_pop+MySyntheticDataCCG$professional_and_managerial)))
  
  df5 <- my_lms5 %>%
    map("coefficients") %>% 
    do.call(rbind.data.frame, .) %>% 
    rownames_to_column %>% 
    as_tibble %>% 
    setNames(c("predictor", "b", "SE", "t", "p")) %>% 
    dplyr::arrange(p) %>% 
    dplyr::filter(str_detect(predictor, "random"))
  
  rm(my_lms5)
  
  
  df1$`Maximum Error Size` <- "50%" 
  df2$`Maximum Error Size` <- "40%" 
  df3$`Maximum Error Size` <- "30%" 
  df4$`Maximum Error Size` <- "20%" 
  df5$`Maximum Error Size` <- "10%" 
  
  randomdf <- rbind(df1,df2,df3,df4,df5)
  
  randomdf <- randomdf %>% mutate(`Significant Pvalue` = ifelse(randomdf$p<0.05, "Yes","No"))
  library(ggforce)
  
  sina_plot <- ggplot(randomdf, aes(y=b, x=`Maximum Error Size`))+
    geom_violin()+
    geom_sina(aes(color = `Significant Pvalue`, group = `Maximum Error Size`),size =.4, alpha = 0.7)+
    labs(y="Outsourcing Coefficient", x = "Max Error")+
    geom_hline(yintercept=0.0038, linetype='dashed', colour = "black")+
    theme_minimal()+
    theme(legend.key.size = unit(3,"line"),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          axis.title = element_text(size = 18))+ 
    guides(colour = guide_legend(override.aes = list(size=8)))
  
  sina_plot
  
}


#fullmodel
Create_S.1 <- function(MyAnnualDataCCG) {
  
  #CCG data
  plmdata <- pdata.frame(MyAnnualDataCCG, index = c("dept", "year"))
  
  completegpdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$number_of_doctors),]
  completegpdata <- completegpdata[complete.cases(completegpdata$Private_Sector_Procurement_Spend),]
  
  completetreatdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$Treatable_Mortality_2013),]
  completetreatdata <- completetreatdata[complete.cases(completetreatdata$Private_Sector_Procurement_Spend),]
  
  #LA data
  
  MyAnnualDataCCG1315 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2013"|MyAnnualDataCCG$year=="2014"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1315$threeyears <- "X2013.2015"  
  
  MyAnnualDataCCG1416 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2014"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1416$threeyears <- "X2014.2016"  
  
  MyAnnualDataCCG1517 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1517$threeyears <- "X2015.2017"  
  
  MyAnnualDataCCG1618 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2018"),]
  MyAnnualDataCCG1618$threeyears <- "X2016.2018"  
  
  MyAnnualDataCCG1719 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2018"|MyAnnualDataCCG$year=="2019"),]
  MyAnnualDataCCG1719$threeyears <- "X2017.2019"  
  
  MyAnnualDataCCG1820 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2018"|MyAnnualDataCCG$year=="2019"|MyAnnualDataCCG$year=="2020"),]
  MyAnnualDataCCG1820$threeyears <- "X2018.2020"  
  
  MyAnnualDataCCG1315 <- MyAnnualDataCCG1315[-c(2)]
  MyAnnualDataCCG1416 <- MyAnnualDataCCG1416[-c(2)]
  MyAnnualDataCCG1517 <- MyAnnualDataCCG1517[-c(2)]
  MyAnnualDataCCG1618 <- MyAnnualDataCCG1618[-c(2)]
  MyAnnualDataCCG1719 <- MyAnnualDataCCG1719[-c(2)]
  MyAnnualDataCCG1820 <- MyAnnualDataCCG1820[-c(2)]
  
  MyAnnualDataCCG1315 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1315, sum)
  MyAnnualDataCCG1416 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1416, sum)
  MyAnnualDataCCG1517 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1517, sum)
  MyAnnualDataCCG1618 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1618, sum)
  MyAnnualDataCCG1719 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1719, sum)
  MyAnnualDataCCG1820 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1820, sum)
  
  threeyrspend <- rbind(MyAnnualDataCCG1315,MyAnnualDataCCG1416,MyAnnualDataCCG1517,MyAnnualDataCCG1618,MyAnnualDataCCG1719,MyAnnualDataCCG1820 )
  
  
  
  LAcontrols <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CCG-Outsourcing/main/Data/Three_Year_Mortality_and_Control_Variables_LA.csv"))
  LAdata <- merge(LAcontrols, unique(MyAnnualDataCCG[c("dept", "ccg19cd")]), by= "ccg19cd", all.x=T)  
  
  LAdata <- merge(LAdata, threeyrspend, by=c("dept", "threeyears"),all=T)  
  
  LAdata$Private_Sector_Procurement_Spend <- (LAdata$Total_Private_sector_spend/LAdata$Total_Procurement_Spend)*100
  
  #create LA lags
  LA_lags <- LAdata[c("Private_Sector_Procurement_Spend", "Local_Authority_Expenditure", "Total_Procurement_Spend", "LAD19CD", "year")]
  LA_lags$year <- as.character(LA_lags$year)
  
  LA_lags$year <- as.double(LA_lags$year)+1
  LA_lags$total_spend_10millions <- LA_lags$Total_Procurement_Spend/10000000
  
  names(LA_lags)[names(LA_lags)=="Private_Sector_Procurement_Spend"] <- "Lagged_Private_Procurement"
  names(LA_lags)[names(LA_lags)=="total_spend_10millions"] <- "Lagged_Total_Spend"
  names(LA_lags)[names(LA_lags)=="Local_Authority_Expenditure"] <- "Lagged_Local_Authority_Spend_per_pop"
  
  LAdata <- merge(LAdata, LA_lags, by=c("LAD19CD", "year"),all=T)
  names(LAdata)[names(LAdata)=="Est_Population"] <- "CCGpop"
  names(LAdata)[names(LAdata)=="Treatable_mortality_rate"] <- "Treatable_Mortality_Rate"
  LAdata$Lagged_Local_Authority_Spend_per_pop <- LAdata$Lagged_Local_Authority_Spend_per_pop/LAdata$CCGpop
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  fit <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ number_of_doctors, data = completegpdata))
  fit2 <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ Treatable_Mortality_2013, data = completetreatdata))
  
  FinalFE <- plm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial,  data=plmdata, index = c("dept", "year"), effect = "twoway", model = "within")
  FinalFD <- plm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop +Lagged_Total_Spend+ Claimant_percent + log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent+log(GDHI_per_person)+professional_and_managerial ,  data=plmdata, index = c("dept", "year"),  model = "fd")
  FinalCBPS_GP <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), weights = fit$fitted, data=completegpdata)
  FinalCBPS_mortality <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), weights = fit2$fitted, data=completetreatdata)
  FinalMLM <- lmerTest::lmer(log(Treatable_Mortality_Rate) ~ 1 + Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+ Lagged_Total_Spend+Claimant_percent+log(CCGpop)+Unemployment_percent+BAME_percent+Qual_lvl4_percent+log(GDHI_per_person)+professional_and_managerial+factor(year)+ (1 | ccg19cd), data = LAdata, control = lmerControl(optimizer = "bobyqa"))
  
  
  class(FinalMLM) <- "lmerMod"
  
  Ct1 <- coef_test(FinalFE, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  Ct2 <- coef_test(FinalFD, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  Ct3 <- coef_test(FinalCBPS_GP, vcov = "CR2", cluster = completegpdata$dept, test = "Satterthwaite")$SE
  Ct4 <- coef_test(FinalCBPS_mortality, vcov = "CR2", cluster = completetreatdata$dept, test = "Satterthwaite")$SE
  
  names(Ct1) <- names(FinalFE$coefficients)
  names(Ct2) <- names(FinalFD$coefficients)
  names(Ct3) <- names(FinalCBPS_GP$coefficients)
  names(Ct4) <- names(FinalCBPS_mortality$coefficients)
  
  
  
  
  rows <- tribble(~term,          ~`FE`,  ~`FD`, ~`CBPS (1)`, ~`CBPS (2)`, ~`MLM`,
                  'CCG Fixed Effects', 'Yes',  'Yes','Yes',  'Yes','No',
                  'Time Fixed Effects', 'Yes',  'Yes','Yes','Yes','Yes',
                  'Clustered Standard Errors', 'Yes',  'Yes','Yes','Yes','Yes',)
  
  
  modelsummary(list("FE"=FinalFE,"FD"=FinalFD,"CBPS (1)"=FinalCBPS_GP,"CBPS (2)"=FinalCBPS_mortality,"MLM"=FinalMLM),
               statistic_override=list(Ct1,Ct2,Ct3,Ct4, sqrt(diag(vcovCR(FinalMLM, type = "CR2")))), coef_omit = "Intercept|dept|year",
               coef_rename = c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)", 
                               "Lagged_Local_Authority_Spend_per_pop" = "LA Spend (£000s per person)", 
                               "Lagged_Total_Spend" = "Total CCG Spend (£Ms)", "Claimant_percent" = "Claimant Rate (%)",
                               "log(CCGpop)"="Population size","Unemployment_percent" = "Unemployment Rate (%)",
                               "BAME_percent" = "Ethnic Minority (%)", "Qual_lvl4_percent" = "Degree Education (%)",
                               "log(GDHI_per_person)" = "Average Disposable H.hold Income", 
                               "professional_and_managerial" = "Managerial/Professional occupation (%)"),
               add_rows = rows,stars = T ,fmt = 5,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            "Tr. Mortality, Population and GDHI are log transformed, 'Ln' denotes the natural log of outcome variable.", 
                            'For full model expressions see supplementary material (sX)',
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)',
                            'Satterthwaite degrees of freedom used in MLM'),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2:6) 
  
  
}

#non-logged treatable mortality
Create_S.3 <- function(MyAnnualDataCCG) {
  
  #CCG data
  plmdata <- pdata.frame(MyAnnualDataCCG, index = c("dept", "year"))
  
  completegpdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$number_of_doctors),]
  completegpdata <- completegpdata[complete.cases(completegpdata$Private_Sector_Procurement_Spend),]
  
  
  completetreatdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$Treatable_Mortality_2013),]
  completetreatdata <- completetreatdata[complete.cases(completetreatdata$Private_Sector_Procurement_Spend),]
  
  #LA data
  
  MyAnnualDataCCG1315 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2013"|MyAnnualDataCCG$year=="2014"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1315$threeyears <- "X2013.2015"  
  
  MyAnnualDataCCG1416 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2014"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1416$threeyears <- "X2014.2016"  
  
  MyAnnualDataCCG1517 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1517$threeyears <- "X2015.2017"  
  
  MyAnnualDataCCG1618 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2018"),]
  MyAnnualDataCCG1618$threeyears <- "X2016.2018"  
  
  MyAnnualDataCCG1719 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2018"|MyAnnualDataCCG$year=="2019"),]
  MyAnnualDataCCG1719$threeyears <- "X2017.2019"  
  
  MyAnnualDataCCG1820 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2018"|MyAnnualDataCCG$year=="2019"|MyAnnualDataCCG$year=="2020"),]
  MyAnnualDataCCG1820$threeyears <- "X2018.2020"  
  
  MyAnnualDataCCG1315 <- MyAnnualDataCCG1315[-c(2)]
  MyAnnualDataCCG1416 <- MyAnnualDataCCG1416[-c(2)]
  MyAnnualDataCCG1517 <- MyAnnualDataCCG1517[-c(2)]
  MyAnnualDataCCG1618 <- MyAnnualDataCCG1618[-c(2)]
  MyAnnualDataCCG1719 <- MyAnnualDataCCG1719[-c(2)]
  MyAnnualDataCCG1820 <- MyAnnualDataCCG1820[-c(2)]
  
  MyAnnualDataCCG1315 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1315, sum)
  MyAnnualDataCCG1416 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1416, sum)
  MyAnnualDataCCG1517 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1517, sum)
  MyAnnualDataCCG1618 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1618, sum)
  MyAnnualDataCCG1719 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1719, sum)
  MyAnnualDataCCG1820 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1820, sum)
  
  threeyrspend <- rbind(MyAnnualDataCCG1315,MyAnnualDataCCG1416,MyAnnualDataCCG1517,MyAnnualDataCCG1618,MyAnnualDataCCG1719,MyAnnualDataCCG1820 )
  
  
  
  LAcontrols <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CCG-Outsourcing/main/Data/Three_Year_Mortality_and_Control_Variables_LA.csv"))
  LAdata <- merge(LAcontrols, unique(MyAnnualDataCCG[c("dept", "ccg19cd")]), by= "ccg19cd", all.x=T)  
  
  LAdata <- merge(LAdata, threeyrspend, by=c("dept", "threeyears"),all=T)  
  
  LAdata$Private_Sector_Procurement_Spend <- (LAdata$Total_Private_sector_spend/LAdata$Total_Procurement_Spend)*100
  
  #create LA lags
  LA_lags <- LAdata[c("Private_Sector_Procurement_Spend", "Local_Authority_Expenditure", "Total_Procurement_Spend", "LAD19CD", "year")]
  LA_lags$year <- as.character(LA_lags$year)
  
  LA_lags$year <- as.double(LA_lags$year)+1
  LA_lags$total_spend_10millions <- LA_lags$Total_Procurement_Spend/10000000
  
  names(LA_lags)[names(LA_lags)=="Private_Sector_Procurement_Spend"] <- "Lagged_Private_Procurement"
  names(LA_lags)[names(LA_lags)=="total_spend_10millions"] <- "Lagged_Total_Spend"
  names(LA_lags)[names(LA_lags)=="Local_Authority_Expenditure"] <- "Lagged_Local_Authority_Spend_per_pop"
  
  LAdata <- merge(LAdata, LA_lags, by=c("LAD19CD", "year"),all=T)
  names(LAdata)[names(LAdata)=="Est_Population"] <- "CCGpop"
  names(LAdata)[names(LAdata)=="Treatable_mortality_rate"] <- "Treatable_Mortality_Rate"
  LAdata$Lagged_Local_Authority_Spend_per_pop <- LAdata$Lagged_Local_Authority_Spend_per_pop/LAdata$CCGpop
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  fit <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ number_of_doctors, data = completegpdata))
  fit2 <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ Treatable_Mortality_2013, data = completetreatdata))
  
  FinalFE <- plm(Treatable_Mortality_Rate~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial,  data=plmdata, index = c("dept", "year"), effect = "twoway", model = "within")
  FinalFD <- plm(Treatable_Mortality_Rate~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop +Lagged_Total_Spend+ Claimant_percent + log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent+log(GDHI_per_person)+professional_and_managerial ,  data=plmdata, index = c("dept", "year"),  model = "fd")
  FinalCBPS_GP <- lm(Treatable_Mortality_Rate~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), weights = fit$fitted, data=completegpdata)
  FinalCBPS_mortality <- lm(Treatable_Mortality_Rate~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), weights = fit2$fitted, data=completetreatdata)
  FinalMLM <- lmerTest::lmer(Treatable_Mortality_Rate ~ 1 + Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+ Lagged_Total_Spend+Claimant_percent+log(CCGpop)+Unemployment_percent+BAME_percent+Qual_lvl4_percent+log(GDHI_per_person)+professional_and_managerial+factor(year)+ (1 | ccg19cd), data = LAdata, control = lmerControl(optimizer = "bobyqa"))
  
  
  class(FinalMLM) <- "lmerMod"
  
  Ct1 <- coef_test(FinalFE, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  Ct2 <- coef_test(FinalFD, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  Ct3 <- coef_test(FinalCBPS_GP, vcov = "CR2", cluster = completegpdata$dept, test = "Satterthwaite")$SE
  Ct4 <- coef_test(FinalCBPS_mortality, vcov = "CR2", cluster = completetreatdata$dept, test = "Satterthwaite")$SE
  
  names(Ct1) <- names(FinalFE$coefficients)
  names(Ct2) <- names(FinalFD$coefficients)
  names(Ct3) <- names(FinalCBPS_GP$coefficients)
  names(Ct4) <- names(FinalCBPS_mortality$coefficients)
  
  
  
  rows <- tribble(~term,          ~`FE`,  ~`FD`, ~`CBPS (1)`, ~`CBPS (2)`, ~`MLM`,
                  'CCG Fixed Effects', 'Yes',  'Yes','Yes',  'Yes','No',
                  'Time Fixed Effects', 'Yes',  'Yes','Yes','Yes','Yes',
                  'Clustered Standard Errors', 'Yes',  'Yes','Yes','Yes','Yes',)
  
  
  modelsummary(list("FE"=FinalFE,"FD"=FinalFD,"CBPS (1)"=FinalCBPS_GP,"CBPS (2)"=FinalCBPS_mortality,"MLM"=FinalMLM),
               statistic_override=list(Ct1,Ct2,Ct3,Ct4, sqrt(diag(vcovCR(FinalMLM, type = "CR2")))), coef_omit = "Intercept|dept|year",
               coef_rename = c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)", 
                               "Lagged_Local_Authority_Spend_per_pop" = "LA Spend (£000s per person)", 
                               "Lagged_Total_Spend" = "Total CCG Spend (£Ms)", "Claimant_percent" = "Claimant Rate (%)",
                               "log(CCGpop)"="Population size","Unemployment_percent" = "Unemployment Rate (%)",
                               "BAME_percent" = "Ethnic Minority (%)", "Qual_lvl4_percent" = "Degree Education (%)",
                               "log(GDHI_per_person)" = "Average Disposable H.hold Income", 
                               "professional_and_managerial" = "Managerial/Professional occupation (%)"),
               add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            ' Population and GDHI are log transformed.', 
                            'For full model expressions see supplementary material (sX)',
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)',
                            'Satterthwaite degrees of freedom used in MLM'),
               output = "gt")%>%
    tab_spanner(label = 'T. Mortality', columns = 2:6)
    
  
  
}

#SIC divisions groupings
Create_S.4.1 <- function(MyAnnualDataCCG) {
  
  #CCG data
  plmdata <- pdata.frame(MyAnnualDataCCG, index = c("dept", "year"))
  
  healthreg <- plm(log(Treatable_Mortality_Rate)~lag(Health_Services_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data=plmdata, effect = "twoway", model = "within")
  professionalreg <- plm(log(Treatable_Mortality_Rate)~lag(Professional_Services_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data=plmdata, effect = "twoway", model = "within")
  nogroupreg <- plm(log(Treatable_Mortality_Rate)~lag(NotAssigned_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data=plmdata, effect = "twoway", model = "within")
  buildingreg <- plm(log(Treatable_Mortality_Rate)~lag(Building_Maintenance_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data=plmdata, effect = "twoway", model = "within")
  socialcarereg <- plm(log(Treatable_Mortality_Rate)~lag(Social_Care_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data=plmdata, effect = "twoway", model = "within")
  foundationalreg <- plm(log(Treatable_Mortality_Rate)~lag(Foundational_Services_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data=plmdata, effect = "twoway", model = "within")
  
  C1 <- coef_test(healthreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  C2 <- coef_test(professionalreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  C3 <- coef_test(nogroupreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  C4 <- coef_test(buildingreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  C5 <- coef_test(socialcarereg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  C6 <- coef_test(foundationalreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  
  
  names(C1) <- names(healthreg$coefficients)
  names(C2) <- names(professionalreg$coefficients)
  names(C3) <- names(nogroupreg$coefficients)
  names(C4) <- names(buildingreg$coefficients)
  names(C5) <- names(socialcarereg$coefficients)
  names(C6) <- names(foundationalreg$coefficients)
  
  
  
  rows <- tribble(~term,          ~`(1)`,  ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`,
                  'CCG Fixed Effects', 'Yes',  'Yes','Yes',  'Yes','Yes','Yes',
                  'Time Fixed Effects', 'Yes',  'Yes','Yes','Yes','Yes','Yes',
                  'Clustered Standard Errors', 'Yes',  'Yes','Yes','Yes','Yes','Yes',)
  
  cm <- c("lag(Health_Services_Procurement_Spend)" = "For-profit Human Health Outsourcing (%)", 
          "lag(Professional_Services_Spend)" = "For-profit Professional services Outsourcing (%)", 
          "lag(NotAssigned_Procurement_Spend)" = "For-profit 'Other' Outsourcing (%)", 
          "lag(Building_Maintenance_Procurement_Spend)" = "For-profit Building construction and maintenance Outsourcing (%)", 
          "lag(Social_Care_Procurement_Spend)" = "For-profit Social Care Outsourcing (%)", 
          "lag(Foundational_Services_Procurement_Spend)" = "For-profit Foundational Services Outsourcing (%)", 
          "lag(Local_Authority_Spend_per_pop)"="LA Spend (£000s per person)",
          "lag(total_spend_10millions)" = "Total CCG Spend (£Ms)", "Claimant_percent" = "Claimant Rate (%)",
          "log(CCGpop)"="Population size","Unemployment_percent" = "Unemployment Rate (%)",
          "BAME_percent" = "Ethnic Minority (%)", "Qual_lvl4_percent" = "Degree Education (%)",
          "log(GDHI_per_person)" = "Average Disposable H.hold Income", 
          "professional_and_managerial" = "Managerial/Professional occupation (%)")
  
  modelsummary(list("(1)"=healthreg,"(2)"=professionalreg,"(3)"=nogroupreg,"(4)"=buildingreg,"(5)"=socialcarereg, "(6)"=foundationalreg),
               statistic_override=list(C1,C2,C3,C4, C5,C6), coef_omit = "Intercept|dept|year",
               coef_map = cm,
               add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            'Tr. mortaility, Population and GDHI are log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)',
                            'Satterthwaite degrees of freedom used in MLM'),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2:7) 
  
  
}

#SIC sub-health codes  
Create_S.4.2 <- function(MyAnnualDataCCG) {
  
  plmdata <- pdata.frame(MyAnnualDataCCG, index = c("dept", "year"))
  
  
  hospreg <- plm(log(Treatable_Mortality_Rate)~lag(Hospital_Services_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data= plmdata , effect = "twoway", model = "within")
  otherreg <- plm(log(Treatable_Mortality_Rate)~lag(Other_Health_Services_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person) +professional_and_managerial, index = c("dept", "year"), data=  plmdata, effect = "twoway", model = "within")
  dentalreg <- plm(log(Treatable_Mortality_Rate)~lag(Dental_Services_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data= plmdata , effect = "twoway", model = "within")
  medicalreg <- plm(log(Treatable_Mortality_Rate)~lag(Medical_Nursing_Homes_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data=plmdata, effect = "twoway", model = "within")
  specialreg <- plm(log(Treatable_Mortality_Rate)~lag(Specialist_Services_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data=plmdata, effect = "twoway", model = "within")
  gpreg <- plm(log(Treatable_Mortality_Rate)~lag(GP_Services_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data=plmdata, effect = "twoway", model = "within")
  
  
  
  C1 <- coef_test(hospreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  C2 <- coef_test(otherreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  C3 <- coef_test(dentalreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  C4 <- coef_test(medicalreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  C5 <- coef_test(specialreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  C6 <- coef_test(gpreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  
  names(C1) <- names(hospreg$coefficients)
  names(C2) <- names(otherreg$coefficients)
  names(C3) <- names(dentalreg$coefficients)
  names(C4) <- names(medicalreg$coefficients)
  names(C5) <- names(specialreg$coefficients)
  names(C6) <- names(gpreg$coefficients)
  
  
  
  rows <- tribble(~term,          ~`(1)`,  ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`,
                  'CCG Fixed Effects', 'Yes',  'Yes','Yes',  'Yes','Yes','Yes',
                  'Time Fixed Effects', 'Yes',  'Yes','Yes','Yes','Yes','Yes',
                  'Clustered Standard Errors', 'Yes',  'Yes','Yes','Yes','Yes','Yes',)
  
  cm <- c("lag(Hospital_Services_Procurement_Spend)" = "For-profit Hospital Outsourcing (%)", 
          "lag(Dental_Services_Procurement_Spend)" = "For-profit Dental Outsourcing (%)", 
          "lag(Medical_Nursing_Homes_Procurement_Spend)" = "For-profit Medical Nursing Homes Outsourcing (%)", 
          "lag(Specialist_Services_Procurement_Spend)" = "For-profit Specialist Services Outsourcing (%)", 
          "lag(GP_Services_Procurement_Spend)" = "For-profit General Medical Outsourcing (%)", 
          "lag(Other_Health_Services_Procurement_Spend)" = "For-profit Other health Outsourcing (%)", 
          "lag(Local_Authority_Spend_per_pop)"="LA Spend (£000s per person)",
          "lag(total_spend_10millions)" = "Total CCG Spend (£Ms)", "Claimant_percent" = "Claimant Rate (%)",
          "log(CCGpop)"="Population size","Unemployment_percent" = "Unemployment Rate (%)",
          "BAME_percent" = "Ethnic Minority (%)", "Qual_lvl4_percent" = "Degree Education (%)",
          "log(GDHI_per_person)" = "Average Disposable H.hold Income", 
          "professional_and_managerial" = "Managerial/Professional occupation (%)")
  
  modelsummary(list("(1)"=hospreg,"(2)"=dentalreg,"(3)"=medicalreg,"(4)"=specialreg,"(5)"=gpreg, "(6)"=otherreg),
               statistic_override=list(C1,C3,C4, C5,C6,C2), coef_omit = "Intercept|dept|year",
               coef_map = cm,
               add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            'Tr. mortaility, Population and GDHI are log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)',
                            'Satterthwaite degrees of freedom used in MLM'),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2:7) 
  
  
}

#Non-GP regs
Create_S.4.3 <- function(MyAnnualDataCCG) {
  
  plmdata <- pdata.frame(MyAnnualDataCCG, index = c("dept", "year"))
  
  
  nongpreg <- plm(log(Treatable_Mortality_Rate)~lag(nogpPrivate_Sector_Procurement_Spend)+lag(Local_Authority_Spend_per_pop) +lag(total_spend_10millions)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial , index = c("dept", "year"), data= plmdata , effect = "twoway", model = "within")
  
  
  
  C1 <- coef_test(nongpreg, vcov = "CR2", cluster = MyAnnualDataCCG$dept, test = "Satterthwaite")$SE
  
  names(C1) <- names(nongpreg$coefficients)
  
  
  
  
  rows <- tribble(~term,          ~`(1)`,  
                  'CCG Fixed Effects', 'Yes',  
                  'Time Fixed Effects', 'Yes', 
                  'Clustered Standard Errors', 'Yes',
                  'Control Variables', 'Yes',  )
  
  cm <- c("lag(nogpPrivate_Sector_Procurement_Spend)" = "For-profit Outsourcing (General Medical Practices removed) (%)")
  
  modelsummary(list("(1)"=nongpreg),
               statistic_override=list(C1), coef_omit = "Intercept|dept|year",
               coef_map = cm,
               add_rows = rows,stars = T ,fmt=5,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            'Tr. mortaility, Population and GDHI are log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)'    ),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2) 
  
  
}

#include fixed effects coefficients
Create_S.5 <- function(MyAnnualDataCCG) {
  
  #CCG data
  
  FinalFE <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(year)+factor(dept),  data=MyAnnualDataCCG)
  
  Ct1 <- coef_test(FinalFE, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  
  names(Ct1) <- names(FinalFE$coefficients)
  
  
  rows <- tribble(~term,          ~`FE`,
                  'CCG Fixed Effects', 'Yes',
                  'Time Fixed Effects', 'Yes', 
                  'Clustered Standard Errors', 'Yes', )
  
  
  modelsummary(list("FE"=FinalFE),
               statistic_override=list(Ct1),
               coef_rename = c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)", 
                               "Lagged_Local_Authority_Spend_per_pop" = "LA Spend (£000s per person)", 
                               "Lagged_Total_Spend" = "Total CCG Spend (£Ms)", "Claimant_percent" = "Claimant Rate (%)",
                               "log(CCGpop)"="Population size","Unemployment_percent" = "Unemployment Rate (%)",
                               "BAME_percent" = "Ethnic Minority (%)", "Qual_lvl4_percent" = "Degree Education (%)",
                               "log(GDHI_per_person)" = "Average Disposable H.hold Income", 
                               "professional_and_managerial" = "Managerial/Professional occupation (%)"),
               add_rows = rows,stars = T ,fmt = 5,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            "Tr. Mortality, Population and GDHI are log transformed, 'Ln' denotes the natural log of outcome variable.", 
                            'For full model expressions see supplementary material (sX)',
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)'),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2)
  
  
}

#npCBPS plot
Create_S.6 <- function(MyAnnualDataCCG) {
  plmdata <- pdata.frame(MyAnnualDataCCG, index = c("dept", "year"))
  
  completegpdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$number_of_doctors),]
  completegpdata <- completegpdata[complete.cases(completegpdata$Private_Sector_Procurement_Spend),]
  
  completetreatdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$Treatable_Mortality_2013),]
  completetreatdata <- completetreatdata[complete.cases(completetreatdata$Private_Sector_Procurement_Spend),]
  
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  fit <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ number_of_doctors, data = completegpdata))
  fit2 <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ Treatable_Mortality_2013, data = completetreatdata))
  
  plot1 <-   bal.plot(fit, var.name = "number_of_doctors", which = "both")
  plot2 <-  bal.plot(fit2, var.name = "Treatable_Mortality_2013", which = "both")
  
  CBPSplot <- cowplot::plot_grid(plot1,plot2, ncol=1 ,labels="AUTO")  
  
  CBPSplot
}

#Absolute Deaths reg - outsourcing
Create_S.7 <- function(MyAnnualDataCCG) {
  plmdata <- pdata.frame(MyAnnualDataCCG, index = c("dept", "year"))
  plmdata$Lagged_Total_Private_sector_spend_ms <- plmdata$Lagged_Total_Private_sector_spend/1000000
  
  FinalFE <- plm(treatable_mortality_deaths~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+over70,  data=plmdata, index = c("dept", "year"), effect = "twoway", model = "within")
  FinalFEspend <- plm(treatable_mortality_deaths~Lagged_Total_Private_sector_spend_ms+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+over70,  data=plmdata, index = c("dept", "year"), effect = "twoway", model = "within")
  FinalFEspendcropped <- plm(treatable_mortality_deaths~Lagged_Total_Private_sector_spend_ms+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+over70,  data=plmdata[which(plmdata$year!=2014&plmdata$year!=2021),], index = c("dept", "year"), effect = "twoway", model = "within")
  
  
  Ct1 <- coef_test(FinalFE, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  Ct2 <- coef_test(FinalFEspend, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  Ct3 <- coef_test(FinalFEspendcropped, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  
  names(Ct1) <- names(FinalFE$coefficients)
  names(Ct2) <- names(FinalFEspend$coefficients)
  names(Ct3) <- names(FinalFEspendcropped$coefficients)
  
  
  
  
  rows <- tribble(~term,          ~`(1)`,  ~`(2)`, ~`(3)`,
                  'CCG Fixed Effects', 'Yes',  'Yes','Yes', 
                  'Time Fixed Effects', 'Yes',  'Yes','Yes',
                  'Clustered Standard Errors', 'Yes',  'Yes', 'Yes',
                  'Control variables', 'Yes',  'Yes', 'Yes')
  
  cm <- c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)", 
          "Lagged_Total_Private_sector_spend_ms" = "Private sector spend (£ms)")
  
  
  
  modelsummary(list("(1)"=FinalFE,"(2)"=FinalFEspend,"(3)"=FinalFEspendcropped),
               statistic_override=list(Ct1,Ct2,Ct3), coef_map = cm, coef_omit = "Intercept|dept|year",
               fmt = 4, add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing/Spend, LA Spend, and CCG Spend have a one year lag.',
                            'Population and GDHI are log transformed', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)'),
               output = "gt")%>%
    tab_spanner(label = 'Treatable Deaths (n)', columns = 2:4) 
}

#2019 figure 2
Create_S.8 <- function(MyAnnualDataCCG) { 
  
  sum2 <- MyAnnualDataCCG[which(MyAnnualDataCCG$year=="2014"|MyAnnualDataCCG$year=="2015"|MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2018"|MyAnnualDataCCG$year=="2019" ),]
  
  finalFEusingLM <- lm(treatable_mortality_deaths~Lagged_Total_Private_sector_spend+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+over75+professional_and_managerial +factor(dept)+factor(year),  data=sum2)
  
  multiplier <- finalFEusingLM$coefficients["Lagged_Total_Private_sector_spend"]
  multiplierlower <- confint(finalFEusingLM, "Lagged_Total_Private_sector_spend")[c(1)]
  multiplierupper <- confint(finalFEusingLM, "Lagged_Total_Private_sector_spend")[c(2)]
  
  sum2$spendchange <- sum2$Total_Private_sector_spend-sum2$Lagged_Total_Private_sector_spend
  sum2$deathchange <- sum2$treatable_mortality_deaths-sum2$Lagged_treatable_mortality_deaths
  
  sum2 <- sum2[complete.cases(sum2$dept),]
  
  sum2$adddeaths <- ((sum2$spendchange))*multiplier
  sum2$adddeathslwr <- ((sum2$spendchange))*multiplierlower
  sum2$adddeathsupr <- ((sum2$spendchange))*multiplierupper
  
  sum2$group=1
  
  sum2$syntheticvalue <- sum2$treatable_mortality_deaths 
  sum2$syntheticupper <- sum2$treatable_mortality_deaths 
  sum2$syntheticlower <- sum2$treatable_mortality_deaths 
  
  sum2[which(sum2$year==2014),]$syntheticvalue <- sum2[which(sum2$year==2014),]$treatable_mortality_deaths
  sum2[which(sum2$year==2014),]$syntheticupper <- sum2[which(sum2$year==2014),]$treatable_mortality_deaths
  sum2[which(sum2$year==2014),]$syntheticlower <- sum2[which(sum2$year==2014),]$treatable_mortality_deaths
  
  
  sum2[which(sum2$year==2015),]$syntheticvalue <- sum2[which(sum2$year==2014),]$syntheticvalue+(sum2[which(sum2$year==2015),]$deathchange)-(sum2[which(sum2$year==2015),]$adddeaths)
  sum2[which(sum2$year==2015),]$syntheticupper <- sum2[which(sum2$year==2014),]$syntheticupper+(sum2[which(sum2$year==2015),]$deathchange)-(sum2[which(sum2$year==2015),]$adddeathsupr)
  sum2[which(sum2$year==2015),]$syntheticlower <- sum2[which(sum2$year==2014),]$syntheticlower+(sum2[which(sum2$year==2015),]$deathchange)-(sum2[which(sum2$year==2015),]$adddeathslwr)
  
  sum2[which(sum2$year==2016),]$syntheticvalue <- sum2[which(sum2$year==2015),]$syntheticvalue+(sum2[which(sum2$year==2016),]$deathchange)-(sum2[which(sum2$year==2016),]$adddeaths)
  sum2[which(sum2$year==2016),]$syntheticupper <- sum2[which(sum2$year==2015),]$syntheticupper+(sum2[which(sum2$year==2016),]$deathchange)-(sum2[which(sum2$year==2016),]$adddeathsupr)
  sum2[which(sum2$year==2016),]$syntheticlower <- sum2[which(sum2$year==2015),]$syntheticlower+(sum2[which(sum2$year==2016),]$deathchange)-(sum2[which(sum2$year==2016),]$adddeathslwr)
  
  
  sum2[which(sum2$year==2017),]$syntheticvalue <- sum2[which(sum2$year==2016),]$syntheticvalue+(sum2[which(sum2$year==2017),]$deathchange)-(sum2[which(sum2$year==2017),]$adddeaths)
  sum2[which(sum2$year==2017),]$syntheticupper<- sum2[which(sum2$year==2016),]$syntheticupper+(sum2[which(sum2$year==2017),]$deathchange)-(sum2[which(sum2$year==2017),]$adddeathsupr)
  sum2[which(sum2$year==2017),]$syntheticlower <- sum2[which(sum2$year==2016),]$syntheticlower+(sum2[which(sum2$year==2017),]$deathchange)-(sum2[which(sum2$year==2017),]$adddeathslwr)
  
  sum2[which(sum2$year==2018),]$syntheticvalue <- sum2[which(sum2$year==2017),]$syntheticvalue+(sum2[which(sum2$year==2018),]$deathchange)-(sum2[which(sum2$year==2018),]$adddeaths)
  sum2[which(sum2$year==2018),]$syntheticupper<- sum2[which(sum2$year==2017),]$syntheticupper+(sum2[which(sum2$year==2018),]$deathchange)-(sum2[which(sum2$year==2018),]$adddeathsupr)
  sum2[which(sum2$year==2018),]$syntheticlower <- sum2[which(sum2$year==2017),]$syntheticlower+(sum2[which(sum2$year==2018),]$deathchange)-(sum2[which(sum2$year==2018),]$adddeathslwr)
  
  sum2[which(sum2$year==2019),]$syntheticvalue <- sum2[which(sum2$year==2018),]$syntheticvalue+(sum2[which(sum2$year==2019),]$deathchange)-(sum2[which(sum2$year==2019),]$adddeaths)
  sum2[which(sum2$year==2019),]$syntheticupper<- sum2[which(sum2$year==2018),]$syntheticupper+(sum2[which(sum2$year==2019),]$deathchange)-(sum2[which(sum2$year==2019),]$adddeathsupr)
  sum2[which(sum2$year==2019),]$syntheticlower <- sum2[which(sum2$year==2018),]$syntheticlower+(sum2[which(sum2$year==2019),]$deathchange)-(sum2[which(sum2$year==2019),]$adddeathslwr)
  
  
  tot <- sum(sum2[complete.cases(sum2$syntheticvalue),]$treatable_mortality_deaths, na.rm=T)-sum(sum2$syntheticvalue, na.rm=T)
  lwr <- sum(sum2[complete.cases(sum2$syntheticvalue),]$treatable_mortality_deaths, na.rm=T)-sum(sum2$syntheticupper, na.rm=T)
  upr <- sum(sum2[complete.cases(sum2$syntheticvalue),]$treatable_mortality_deaths, na.rm=T)-sum(sum2$syntheticlower, na.rm=T)
  
  
  #prelist <- sum2[which(sum2$year!="2014"&sum2$year!="2019"),]
  prelist <- sum2[complete.cases(sum2$syntheticvalue),]
  prelist <- prelist[complete.cases(prelist$ccg19cd),]
  
  prelistn <- prelist %>% group_by(ccg19cd) %>%tally()
  
  prelistn <- prelistn[which(prelistn$n>=6),]
  
  prelistn$keep <- 1
  
  
  sum2yr <-   sum2[c("ccg19cd","year","syntheticvalue","syntheticupper","treatable_mortality_deaths" ,"syntheticlower")]
  
  sum2yr <- merge(sum2yr, prelistn, by="ccg19cd", all.x=T)  
  
  sum2yr$year <- factor(sum2yr$year)
  
  sum2yr <- sum2yr[which(sum2yr$keep==1),]
  
  sum2yr <-   sum2yr[c("year","syntheticvalue","syntheticupper" ,"syntheticlower")]
  
  sum2yr <- aggregate(. ~year, data=sum2yr, sum,  na.rm=TRUE, na.action=NULL)
  
  sum2yr$group=1
  
  theme_Publication <- function(base_size=9, base_family="TT Times New Roman") {
    library(grid)
    library(ggthemes)
    (theme_foundation(base_size=base_size, base_family=base_family)
      + theme(plot.title = element_text(size = rel(1.2), hjust = 0.5),
              text = element_text(),
              panel.background = element_rect(colour = NA),
              plot.background = element_rect(colour = NA),
              panel.border = element_rect(colour = NA),
              axis.title = element_text(size = rel(1)),
              axis.title.y = element_text(angle=90,vjust =2),
              axis.title.x = element_text(vjust = -0.2),
              axis.text = element_text(), 
              axis.line = element_line(colour="black"),
              axis.ticks = element_line(),
              panel.grid.major = element_line(colour="#f0f0f0"),
              panel.grid.minor = element_blank(),
              legend.key = element_rect(colour = NA),
              legend.position = "bottom",
              legend.direction = "horizontal",
              legend.key.size= unit(0.2, "cm"),
              legend.margin = unit(0, "cm"),
              legend.title = element_text(),
              plot.margin=unit(c(10,5,5,5),"mm"),
              strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
              strip.text = element_text()
      ))
    
  }
  
  fullobserved <- MyAnnualDataCCG[c("ccg19cd", "year", "treatable_mortality_deaths")]
  fullobserved <- merge(fullobserved, prelistn, by="ccg19cd", all.x=T)
  fullobserved <- fullobserved[which(fullobserved$keep==1),]
  fullobserved <- fullobserved[c("year", "treatable_mortality_deaths")]
  fullobserved <- aggregate(. ~year, data=fullobserved, sum,  na.rm=TRUE, na.action=NULL)
  
  sum2yrs <- merge(sum2yr, fullobserved, by="year", all=T)
  
  sum2yrs <- sum2yrs[which(sum2yrs$year!="2005"&sum2yrs$year!="2020"&sum2yrs$year!="2021"&sum2yrs$year!="2022"),]
  extrafont::loadfonts()
  
  sum2yrs$time <- as.double(as.character(sum2yrs$year))
  sum2yrs$group <- 1
  
  plot2 <- 
    ggplot(sum2yrs, aes(x = time, y = treatable_mortality_deaths, group=group)) +
    geom_line()+
    geom_line(aes(y=syntheticvalue),linetype = "dashed", color = "darkgray")+
    geom_ribbon(aes(ymin = syntheticlower, ymax = syntheticupper), 
                alpha=0.1, 
                linetype="dashed",
                color="lightgray")+
    ylim(c(11250,14000))+
    geom_vline(xintercept = 2013.33 ,linetype = "dotted")+
    labs(x="Year", y="Treatable Deaths (Total of 51 CCGs)")+
    geom_text(x=2013.33, y=13750, label="2012 Health and Social Care Act implemented", 
              size = 9/.pt, aes(family = "serif"))+
    geom_text(x=2016, y=12150, label = "Mortality if no extra outsourcing", size = 5/.pt, aes(family = "serif"), color = "darkgray", angle = 38)+
    geom_text(x=2016, y=12450, label = "Observed mortality", size = 5/.pt, aes(family = "serif"), angle = 38)+
    theme_Publication()+
    scale_x_continuous(breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
                       labels=c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"))
  
plot2  
  
  
  
}

#preventable mortality rate
Create_S.9 <- function(MyAnnualDataCCG) {
  
  #CCG data
  plmdata <- pdata.frame(MyAnnualDataCCG, index = c("dept", "year"))
  
  completegpdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$number_of_doctors),]
  completegpdata <- completegpdata[complete.cases(completegpdata$Private_Sector_Procurement_Spend),]
  
  completetreatdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$Treatable_Mortality_2013),]
  completetreatdata <- completetreatdata[complete.cases(completetreatdata$Private_Sector_Procurement_Spend),]
  
  
  #LA data
  
  MyAnnualDataCCG1315 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2013"|MyAnnualDataCCG$year=="2014"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1315$threeyears <- "X2013.2015"  
  
  MyAnnualDataCCG1416 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2014"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1416$threeyears <- "X2014.2016"  
  
  MyAnnualDataCCG1517 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2015"),]
  MyAnnualDataCCG1517$threeyears <- "X2015.2017"  
  
  MyAnnualDataCCG1618 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2018"),]
  MyAnnualDataCCG1618$threeyears <- "X2016.2018"  
  
  MyAnnualDataCCG1719 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2018"|MyAnnualDataCCG$year=="2019"),]
  MyAnnualDataCCG1719$threeyears <- "X2017.2019"  
  
  MyAnnualDataCCG1820 <- MyAnnualDataCCG[c("dept", "year","Total_Procurement_Spend","Total_Private_sector_spend" )][which(MyAnnualDataCCG$year=="2018"|MyAnnualDataCCG$year=="2019"|MyAnnualDataCCG$year=="2020"),]
  MyAnnualDataCCG1820$threeyears <- "X2018.2020"  
  
  MyAnnualDataCCG1315 <- MyAnnualDataCCG1315[-c(2)]
  MyAnnualDataCCG1416 <- MyAnnualDataCCG1416[-c(2)]
  MyAnnualDataCCG1517 <- MyAnnualDataCCG1517[-c(2)]
  MyAnnualDataCCG1618 <- MyAnnualDataCCG1618[-c(2)]
  MyAnnualDataCCG1719 <- MyAnnualDataCCG1719[-c(2)]
  MyAnnualDataCCG1820 <- MyAnnualDataCCG1820[-c(2)]
  
  MyAnnualDataCCG1315 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1315, sum)
  MyAnnualDataCCG1416 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1416, sum)
  MyAnnualDataCCG1517 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1517, sum)
  MyAnnualDataCCG1618 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1618, sum)
  MyAnnualDataCCG1719 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1719, sum)
  MyAnnualDataCCG1820 <- aggregate(. ~dept+threeyears, data=MyAnnualDataCCG1820, sum)
  
  threeyrspend <- rbind(MyAnnualDataCCG1315,MyAnnualDataCCG1416,MyAnnualDataCCG1517,MyAnnualDataCCG1618,MyAnnualDataCCG1719,MyAnnualDataCCG1820 )
  
  
  
  LAcontrols <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CCG-Outsourcing/main/Data/Three_Year_Mortality_and_Control_Variables_LA.csv"))
  LAdata <- merge(LAcontrols, unique(MyAnnualDataCCG[c("dept", "ccg19cd")]), by= "ccg19cd", all.x=T)  
  
  LAdata <- merge(LAdata, threeyrspend, by=c("dept", "threeyears"),all=T)  
  
  LAdata$Private_Sector_Procurement_Spend <- (LAdata$Total_Private_sector_spend/LAdata$Total_Procurement_Spend)*100
  
  #create LA lags
  LA_lags <- LAdata[c("Private_Sector_Procurement_Spend", "Local_Authority_Expenditure", "Total_Procurement_Spend", "LAD19CD", "year")]
  LA_lags$year <- as.character(LA_lags$year)
  
  LA_lags$year <- as.double(LA_lags$year)+1
  LA_lags$total_spend_10millions <- LA_lags$Total_Procurement_Spend/10000000
  
  names(LA_lags)[names(LA_lags)=="Private_Sector_Procurement_Spend"] <- "Lagged_Private_Procurement"
  names(LA_lags)[names(LA_lags)=="total_spend_10millions"] <- "Lagged_Total_Spend"
  names(LA_lags)[names(LA_lags)=="Local_Authority_Expenditure"] <- "Lagged_Local_Authority_Spend_per_pop"
  
  LAdata <- merge(LAdata, LA_lags, by=c("LAD19CD", "year"),all=T)
  names(LAdata)[names(LAdata)=="Est_Population"] <- "CCGpop"
  names(LAdata)[names(LAdata)=="Treatable_mortality_rate"] <- "Treatable_Mortality_Rate"
  LAdata$Lagged_Local_Authority_Spend_per_pop <- LAdata$Lagged_Local_Authority_Spend_per_pop/LAdata$CCGpop
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  fit <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ number_of_doctors, data = completegpdata))
  fit2 <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ Treatable_Mortality_2013, data = completetreatdata))
  
  FinalFE <- plm(log(Preventatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial,  data=plmdata, index = c("dept", "year"), effect = "twoway", model = "within")
  FinalFD <- plm(log(Preventatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop +Lagged_Total_Spend+ Claimant_percent + log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent+log(GDHI_per_person)+professional_and_managerial ,  data=plmdata, index = c("dept", "year"),  model = "fd")
  FinalCBPS_GP <- lm(log(Preventatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), weights = fit$fitted, data=completegpdata)
  FinalCBPS_mortality <- lm(log(Preventatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), weights = fit2$fitted, data=completetreatdata)
  FinalMLM <- lmerTest::lmer(log(Preventable_mortality_rate) ~ 1 + Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+ Lagged_Total_Spend+Claimant_percent+log(CCGpop)+Unemployment_percent+BAME_percent+Qual_lvl4_percent+log(GDHI_per_person)+professional_and_managerial+factor(year)+ (1 | ccg19cd), data = LAdata, control = lmerControl(optimizer = "bobyqa"))
  
  
  class(FinalMLM) <- "lmerMod"
  
  Ct1 <- coef_test(FinalFE, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  Ct2 <- coef_test(FinalFD, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  Ct3 <- coef_test(FinalCBPS_GP, vcov = "CR2", cluster = completegpdata$dept, test = "Satterthwaite")$SE
  Ct4 <- coef_test(FinalCBPS_mortality, vcov = "CR2", cluster = completetreatdata$dept, test = "Satterthwaite")$SE
  
  names(Ct1) <- names(FinalFE$coefficients)
  names(Ct2) <- names(FinalFD$coefficients)
  names(Ct3) <- names(FinalCBPS_GP$coefficients)
  names(Ct4) <- names(FinalCBPS_mortality$coefficients)
  
  
  
  
  rows <- tribble(~term,          ~`FE`,  ~`FD`, ~`CBPS (1)`, ~`CBPS (2)`, ~`MLM`,
                  'CCG Fixed Effects', 'Yes',  'Yes','Yes',  'Yes','No',
                  'Time Fixed Effects', 'Yes',  'Yes','Yes','Yes','Yes',
                  'Clustered Standard Errors', 'Yes',  'Yes','Yes','Yes','Yes',)
  
  
  modelsummary(list("FE"=FinalFE,"FD"=FinalFD,"CBPS (1)"=FinalCBPS_GP,"CBPS (2)"=FinalCBPS_mortality,"MLM"=FinalMLM),
               statistic_override=list(Ct1,Ct2,Ct3,Ct4, sqrt(diag(vcovCR(FinalMLM, type = "CR2")))), coef_omit = "Intercept|dept|year",
               coef_rename = c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)", 
                               "Lagged_Local_Authority_Spend_per_pop" = "LA Spend (£000s per person)", 
                               "Lagged_Total_Spend" = "Total CCG Spend (£Ms)", "Claimant_percent" = "Claimant Rate (%)",
                               "log(CCGpop)"="Population size","Unemployment_percent" = "Unemployment Rate (%)",
                               "BAME_percent" = "Ethnic Minority (%)", "Qual_lvl4_percent" = "Degree Education (%)",
                               "log(GDHI_per_person)" = "Average Disposable H.hold Income", 
                               "professional_and_managerial" = "Managerial/Professional occupation (%)"),
               fmt = 4, add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            'P. Mortality, Population and GDHI are log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)',
                            'Satterthwaite degrees of freedom used in MLM'),
               output = "gt")%>%
    tab_spanner(label = 'ln(P. Mortality)', columns = 2:6) 
  
  
}

#Dropping CCGs in turn
Create_S.10.1 <- function(MyAnnualDataCCG) {
  
  loopdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$ccg19cd),]
  loopdata <- loopdata[complete.cases(loopdata$Lagged_Private_Procurement),]
  
  output_loop=list(NA)
  
  for ( i in unique(loopdata$ccg19cd) ){
    #create a subset data 
    data_sub <- subset(loopdata,ccg19cd!= i)
    assign(paste("lm",i,sep=""), output_loop[[i]] <- summary(lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+factor(ccg19cd)+factor(year)+Lagged_Local_Authority_Spend_per_pop+log(Lagged_GDHI_per_person)+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +Lagged_Total_Spend+Managerial_occupation, data = data_sub)))
  }
  
  df <- output_loop %>%
    map("coefficients") %>% 
    do.call(rbind.data.frame, .) %>% 
    rownames_to_column %>% 
    as_tibble %>% 
    setNames(c("predictor", "b", "SE", "t", "p")) %>% 
    dplyr::arrange(p) %>% 
    dplyr::filter(!str_detect(predictor, "(Intercept)")) %>% 
    mutate(predictor = str_sub(predictor, start = 1, end = str_length(predictor)-3))
  
  df <- within(df, predictor<-data.frame(do.call('rbind', strsplit(as.character(predictor), '.', fixed=TRUE))))
  
  
  df <- df[which(df$predictor$X2=="Lagged_Private_Procurem"), ]
  
  
  eaststaff <- df[which(df$predictor$X1=="E38000053"),]
  
  pplot <- ggplot(df, aes(x = reorder(predictor$X1, p), y = p))+
    geom_point() +
    geom_hline(yintercept = .05, color = "red")+
    theme(axis.text.x = element_blank())+
    geom_point(data=eaststaff, colour="red")+
    geom_text(data=eaststaff, label="East Staffordshire CCG", vjust=1, hjust=1)+
    xlab("Regressions removing one CCG")+ ylab("P Value")
  
  bplot <- ggplot(df, aes(x = reorder(predictor$X1, b), y = b))+
    geom_point() +
    theme(axis.text.x = element_blank())+
    geom_point(data=eaststaff, colour="red")+
    geom_text(data=eaststaff, label="East Staffordshire CCG", vjust=1, hjust=-0.1)+
    xlab("Regressions removing one CCG")+ ylab("Effect Size")
  
  loopeddropplot <-  cowplot::plot_grid(bplot, pplot, cols = 2, labels = "AUTO")
  loopeddropplot
  
}

#No East Staffordshire
Create_S.10.2 <- function(MyAnnualDataCCG) {
  MyAnnualDataCCGnoES <- MyAnnualDataCCG[which(MyAnnualDataCCG$ccg19cd!="E38000053"), ]
  
  plmdata <- pdata.frame(MyAnnualDataCCGnoES, index = c("dept", "year"))
  
  completegpdata <- MyAnnualDataCCGnoES[complete.cases(MyAnnualDataCCGnoES$number_of_doctors),]
  completegpdata <- completegpdata[complete.cases(MyAnnualDataCCGnoES$Private_Sector_Procurement_Spend),]
  
  completetreatdata <- MyAnnualDataCCGnoES[complete.cases(MyAnnualDataCCGnoES$Treatable_Mortality_2013),]
  completetreatdata <- completetreatdata[complete.cases(completetreatdata$Private_Sector_Procurement_Spend),]
  
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  fit <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ number_of_doctors, data = completegpdata))
  fit2 <- quiet(npCBPS(Private_Sector_Procurement_Spend ~ Treatable_Mortality_2013, data = completetreatdata))
  
  FinalFE <- plm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial,  data=plmdata, index = c("dept", "year"), effect = "twoway", model = "within")
  FinalFD <- plm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop +Lagged_Total_Spend+ Claimant_percent + log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent+log(GDHI_per_person)+professional_and_managerial ,  data=plmdata, index = c("dept", "year"),  model = "fd")
  FinalCBPS_GP <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), weights = fit$fitted, data=completegpdata)
  FinalCBPS_mortality <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), weights = fit2$fitted, data=completetreatdata)
  
  
  Ct1 <- coef_test(FinalFE, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  Ct2 <- coef_test(FinalFD, vcov = "CR2", cluster = plmdata$dept, test = "Satterthwaite")$SE
  Ct3 <- coef_test(FinalCBPS_GP, vcov = "CR2", cluster = completegpdata$dept, test = "Satterthwaite")$SE
  Ct4 <- coef_test(FinalCBPS_mortality, vcov = "CR2", cluster = completetreatdata$dept, test = "Satterthwaite")$SE
  
  names(Ct1) <- names(FinalFE$coefficients)
  names(Ct2) <- names(FinalFD$coefficients)
  names(Ct3) <- names(FinalCBPS_GP$coefficients)
  names(Ct4) <- names(FinalCBPS_mortality$coefficients)
  
  
  
  
  rows <- tribble(~term,          ~`FE`,  ~`FD`, ~`CBPS (1)`, ~`CBPS (2)`, 
                  'CCG Fixed Effects', 'Yes',  'Yes','Yes',  'Yes',
                  'Time Fixed Effects', 'Yes',  'Yes','Yes','Yes',
                  'Clustered Standard Errors', 'Yes',  'Yes','Yes','Yes',)
  
  
  modelsummary(list("FE"=FinalFE,"FD"=FinalFD,"CBPS (1)"=FinalCBPS_GP,"CBPS (2)"=FinalCBPS_mortality),
               statistic_override=list(Ct1,Ct2,Ct3,Ct4), coef_omit = "Intercept|dept|year",
               coef_rename = c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)", 
                               "Lagged_Local_Authority_Spend_per_pop" = "LA Spend (£000s per person)", 
                               "Lagged_Total_Spend" = "Total CCG Spend (£Ms)", "Claimant_percent" = "Claimant Rate (%)",
                               "log(CCGpop)"="Population size","Unemployment_percent" = "Unemployment Rate (%)",
                               "BAME_percent" = "Ethnic Minority (%)", "Qual_lvl4_percent" = "Degree Education (%)",
                               "log(GDHI_per_person)" = "Average Disposable H.hold Income", 
                               "professional_and_managerial" = "Managerial/Professional occupation (%)"),
               fmt = 4, add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            "Tr. Mortality, Population and GDHI are log transformed, 'Ln' denotes the natural log of outcome variable.", 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)'
               ),
               output = "gt")%>%
    tab_spanner(label = 'ln(Tr. Mortality)', columns = 2:5) 
  
  
  
}

#Specification Curve
Create_S.11 <- function(MyAnnualDataCCG) {
  
  completeccgdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$Treatable_Mortality_Rate),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$Private_Sector_Procurement_Spend),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$total_spend_10millions),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$Lagged_Total_Spend),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$Claimant_percent),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$CCGpop),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$Unemployment_percent),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$BAME_percent),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$Qual_lvl4_percent),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$Lagged_GDHI_per_person),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$Lagged_Local_Authority_Spend_per_pop),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$dept),]
  completeccgdata <- completeccgdata[complete.cases(completeccgdata$year),]
  
  
  full.model <- plm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(Lagged_GDHI_per_person)+professional_and_managerial, index = c("dept", "year"), data=completeccgdata, effect = "twoway", model = "within")
  
  logLik.plm <- function(full.model){
    out <- -plm::nobs(full.model) * log(2 * var(full.model$residuals) * pi)/2 - deviance(full.model)/(2 * var(full.model$residuals))
    
    attr(out,"df") <- nobs(full.model) - full.model$df.residual
    attr(out,"nobs") <- plm::nobs(full.model)
    return(out)
  }
  
  options(na.action = "na.fail")
  
  models.sca <- MuMIn::dredge(full.model, rank = "AIC", extra = "BIC")
  
  options(na.action = "na.omit")
  
  #.rs.restartR()
  library(tidyr)
  library(plm)
  library(ggplot2)
  library(dplyr)
  
  (model_params =  MuMIn::get.models(models.sca, subset = TRUE) %>%
      tibble() %>%
      rename("model" = ".") %>%
      mutate(tidied = purrr::map(model, broom::tidy),
             model_num = row_number()) %>%
      dplyr::select(model_num, tidied) %>%
      unnest(cols = c(tidied)) %>%
      dplyr::select(model_num, term, estimate) %>%
      spread(term, estimate)) %>%
    dplyr::select(-starts_with("sd"))
  
  (model_ps = MuMIn::get.models(models.sca, subset = TRUE) %>%
      tibble() %>%
      rename("model" = ".") %>%
      mutate(tidied = purrr::map(model, broom::tidy),
             model_num = row_number()) %>%
      dplyr::select(model_num, tidied) %>%
      unnest(cols = c(tidied)) %>%
      filter(term == "Lagged_Private_Procurement") %>%
      ungroup() %>%
      dplyr::select(model_num, estimate, std.error, p.value))
  
  
  
  plot.data = left_join(model_ps, model_params, by = "model_num") %>%
    arrange(estimate) %>%
    mutate(specification = row_number(),
           significant.p = ifelse(p.value < .05, "yes", "no")) %>%
    gather(variable, value, -estimate, -specification, -model_num, -std.error, -p.value, -significant.p) %>% 
    mutate(variable = gsub("[()]", "", variable),
           variable = gsub("Intercept", "intercept", variable),
           variable = gsub("as.factor(vs)1", "vs", variable)) %>%
    spread(variable, value)  
  
  # get names of variables included in model
  variable.names = names(dplyr::select(plot.data, -estimate, -specification, -model_num, -std.error, -p.value, -significant.p))
  
  # plot top panel
  top = plot.data %>%
    ggplot(aes(specification, estimate, color = significant.p)) +
    geom_point(size=1) +
    #geom_hline(yintercept = null.df$AIC, linetype = "dashed", color = "lightblue") +
    scale_color_manual(values = c("black", "red")) +
    labs(x = "", y = "regression coefficient\n") + 
    theme_minimal(base_size = 11) +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 9),
          axis.text = element_text(color = "black"),
          axis.line = element_line(colour = "black"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  # set plotting order for variables based on number of times it's included in better fitting models
  order = plot.data %>%
    arrange(estimate) %>%
    mutate(significant.p.num = ifelse(significant.p == "yes", 1, 0)) %>%
    gather(variable, value, eval(variable.names)) %>% 
    filter(!is.na(value)) %>%
    group_by(variable) %>%
    mutate(order = sum(significant.p.num)) %>%
    dplyr::select(variable, order) %>%
    unique()
  
  # rename variables and plot bottom panel
  bottom = plot.data %>%
    gather(variable, value, eval(variable.names)) %>% 
    mutate(value = ifelse(!is.na(value), "|", ""),
           variable = ifelse(variable == "(Intercept)", "intercept",
                             ifelse(variable == "as.factor(vs)1", "vs", variable))) %>%
    left_join(., order, by = "variable") %>%
    ggplot(aes(specification, reorder(variable, order), color = significant.p)) +
    geom_text(aes(label = value)) +
    scale_color_manual(values = c("black", "red")) +
    labs(x = "\nspecification number", y = "variables\n") + 
    theme_minimal(base_size = 11) +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 9),
          axis.text = element_text(color = "black"),
          axis.line = element_line(colour = "black"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  # join panels
  (wt = cowplot::plot_grid(top, bottom, ncol = 1, align = "v", labels = c('A', 'B')))
wt  
}

#Random error plus specification curve
Create_S.12 <- function(MyAnnualDataCCG) {
  plan(multisession)
  set.seed(1001)
  
  MySyntheticDataCCG <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$Lagged_Private_Procurement),]
  MySyntheticDataCCG <- MySyntheticDataCCG[complete.cases(MySyntheticDataCCG$dept),]
  MySyntheticDataCCG <- MySyntheticDataCCG[complete.cases(MySyntheticDataCCG$year),]
  MySyntheticDataCCG <- MySyntheticDataCCG[complete.cases(MySyntheticDataCCG$Lagged_GDHI_per_person),]
  MySyntheticDataCCG <- MySyntheticDataCCG[complete.cases(MySyntheticDataCCG$Lagged_Local_Authority_Spend_per_pop),]
  MySyntheticDataCCG <- MySyntheticDataCCG[complete.cases(MySyntheticDataCCG$Treatable_Mortality_Rate),]
  MySyntheticDataCCG <- MySyntheticDataCCG[complete.cases(MySyntheticDataCCG$Qual_lvl4_percent),]
  MySyntheticDataCCG <- MySyntheticDataCCG[complete.cases(MySyntheticDataCCG$CCGpop),]
  MySyntheticDataCCG <- MySyntheticDataCCG[complete.cases(MySyntheticDataCCG$professional_and_managerial),]
  MySyntheticDataCCG <- MySyntheticDataCCG[complete.cases(MySyntheticDataCCG$Claimant_percent),]
  MySyntheticDataCCG <- MySyntheticDataCCG[complete.cases(MySyntheticDataCCG$Unemployment_percent),]
  MySyntheticDataCCG <- MySyntheticDataCCG[complete.cases(MySyntheticDataCCG$BAME_percent),]
  
  randomerror1 <- (5+10.*pracma::rand(609,20))/10
  randomerror2 <- (6+8.*pracma::rand(609,20))/10
  randomerror3 <- (7+6.*pracma::rand(609,20))/10
  randomerror4 <- (8+4.*pracma::rand(609,20))/10
  randomerror5 <- (9+2.*pracma::rand(609,20))/10
  
  
  randomerror1 <- randomerror1*MySyntheticDataCCG$Lagged_Private_Procurement
  randomerror2 <- randomerror2*MySyntheticDataCCG$Lagged_Private_Procurement
  randomerror3 <- randomerror3*MySyntheticDataCCG$Lagged_Private_Procurement
  randomerror4 <- randomerror4*MySyntheticDataCCG$Lagged_Private_Procurement
  randomerror5 <- randomerror5*MySyntheticDataCCG$Lagged_Private_Procurement
  
  plan(multisession)
  
  my_lms1 <- future_lapply(1:20, function(x) summary(lm(log(MySyntheticDataCCG$Treatable_Mortality_Rate) ~ randomerror1[,x]+factor(MySyntheticDataCCG$dept)+factor(MySyntheticDataCCG$year)+log(MySyntheticDataCCG$GDHI_per_person)+MySyntheticDataCCG$Claimant_percent+  log(MySyntheticDataCCG$CCGpop) +MySyntheticDataCCG$Unemployment_percent +MySyntheticDataCCG$BAME_percent+MySyntheticDataCCG$Qual_lvl4_percent +MySyntheticDataCCG$Lagged_Total_Spend+MySyntheticDataCCG$Lagged_Local_Authority_Spend_per_pop+MySyntheticDataCCG$professional_and_managerial)))
  my_lms2 <- future_lapply(1:20, function(x) summary(lm(log(MySyntheticDataCCG$Treatable_Mortality_Rate) ~ randomerror2[,x]+factor(MySyntheticDataCCG$dept)+factor(MySyntheticDataCCG$year)+log(MySyntheticDataCCG$GDHI_per_person)+MySyntheticDataCCG$Claimant_percent+  log(MySyntheticDataCCG$CCGpop) +MySyntheticDataCCG$Unemployment_percent +MySyntheticDataCCG$BAME_percent+MySyntheticDataCCG$Qual_lvl4_percent +MySyntheticDataCCG$Lagged_Total_Spend+MySyntheticDataCCG$Lagged_Local_Authority_Spend_per_pop+MySyntheticDataCCG$professional_and_managerial)))
  my_lms3 <- future_lapply(1:20, function(x) summary(lm(log(MySyntheticDataCCG$Treatable_Mortality_Rate) ~ randomerror3[,x]+factor(MySyntheticDataCCG$dept)+factor(MySyntheticDataCCG$year)+log(MySyntheticDataCCG$GDHI_per_person)+MySyntheticDataCCG$Claimant_percent+  log(MySyntheticDataCCG$CCGpop) +MySyntheticDataCCG$Unemployment_percent +MySyntheticDataCCG$BAME_percent+MySyntheticDataCCG$Qual_lvl4_percent +MySyntheticDataCCG$Lagged_Total_Spend+MySyntheticDataCCG$Lagged_Local_Authority_Spend_per_pop+MySyntheticDataCCG$professional_and_managerial)))
  my_lms4 <- future_lapply(1:20, function(x) summary(lm(log(MySyntheticDataCCG$Treatable_Mortality_Rate) ~ randomerror4[,x]+factor(MySyntheticDataCCG$dept)+factor(MySyntheticDataCCG$year)+log(MySyntheticDataCCG$GDHI_per_person)+MySyntheticDataCCG$Claimant_percent+  log(MySyntheticDataCCG$CCGpop) +MySyntheticDataCCG$Unemployment_percent +MySyntheticDataCCG$BAME_percent+MySyntheticDataCCG$Qual_lvl4_percent +MySyntheticDataCCG$Lagged_Total_Spend+MySyntheticDataCCG$Lagged_Local_Authority_Spend_per_pop+MySyntheticDataCCG$professional_and_managerial)))
  my_lms5 <- future_lapply(1:20, function(x) summary(lm(log(MySyntheticDataCCG$Treatable_Mortality_Rate) ~ randomerror5[,x]+factor(MySyntheticDataCCG$dept)+factor(MySyntheticDataCCG$year)+log(MySyntheticDataCCG$GDHI_per_person)+MySyntheticDataCCG$Claimant_percent+  log(MySyntheticDataCCG$CCGpop) +MySyntheticDataCCG$Unemployment_percent +MySyntheticDataCCG$BAME_percent+MySyntheticDataCCG$Qual_lvl4_percent +MySyntheticDataCCG$Lagged_Total_Spend+MySyntheticDataCCG$Lagged_Local_Authority_Spend_per_pop+MySyntheticDataCCG$professional_and_managerial)))
  
  
  x <- seq(1,20)
  
  options(na.action = "na.fail")
  
  cl <- makeCluster(detectCores())
  
  clusterExport(cl, "x")
  clusterEvalQ(cl, x)
  
  clusterExport(cl, "completeccgdata")
  clusterEvalQ(cl, completeccgdata)
  
  clusterExport(cl, "my_lms1") 
  clusterEvalQ(cl, my_lms1)
  
  clusterExport(cl, "randomerror1") 
  clusterEvalQ(cl, randomerror1)
  
  clusterExport(cl, "my_lms2") 
  clusterEvalQ(cl, my_lms2)
  
  clusterExport(cl, "randomerror2") 
  clusterEvalQ(cl, randomerror2)
  
  clusterExport(cl, "my_lms3") 
  clusterEvalQ(cl, my_lms3)
  
  clusterExport(cl, "randomerror3") 
  clusterEvalQ(cl, randomerror3)
  
  clusterExport(cl, "my_lms4") 
  clusterEvalQ(cl, my_lms4)
  
  clusterExport(cl, "randomerror4") 
  clusterEvalQ(cl, randomerror4)
  
  clusterExport(cl, "my_lms5") 
  clusterEvalQ(cl, my_lms5)
  
  clusterExport(cl, "randomerror5") 
  clusterEvalQ(cl, randomerror5)
  
  clusterEvalQ(cl, { 
    library(MuMIn) 
  })  
  clusterEvalQ(cl, { 
    library(purrr) 
  })  
  clusterEvalQ(cl, { 
    library(tidyr) 
  })  
  clusterEvalQ(cl, { 
    library(dplyr) 
  })  
  
  
  
  models.sca1 <- parLapply(cl, my_lms1[x],  MuMIn::dredge)    
  models.sca2 <- parLapply(cl, my_lms2[x],  MuMIn::dredge)    
  models.sca3 <- parLapply(cl, my_lms3[x],  MuMIn::dredge)    
  models.sca4 <- parLapply(cl, my_lms4[x],  MuMIn::dredge)    
  models.sca5 <- parLapply(cl, my_lms5[x],  MuMIn::dredge) 
  
  models.sca1 <- parLapply(cl, models.sca1,function(x)
    x[complete.cases(x$year),])
  models.sca1 <- parLapply(cl, models.sca1,function(x)
    x[complete.cases(x$dept),])
  models.sca1 <- parLapply(cl, models.sca1,function(x)
    x[complete.cases(x$randomerror1),])
  
  models.sca2 <- parLapply(cl, models.sca2,function(x)
    x[complete.cases(x$year),])
  models.sca2 <- parLapply(cl, models.sca2,function(x)
    x[complete.cases(x$dept),])
  models.sca2 <- parLapply(cl, models.sca2,function(x)
    x[complete.cases(x$randomerror2),])
  
  models.sca3 <- parLapply(cl, models.sca3,function(x)
    x[complete.cases(x$year),])
  models.sca3 <- parLapply(cl, models.sca3,function(x)
    x[complete.cases(x$dept),])
  models.sca3 <- parLapply(cl, models.sca3,function(x)
    x[complete.cases(x$randomerror3),])
  
  models.sca4 <- parLapply(cl, models.sca4,function(x)
    x[complete.cases(x$year),])
  models.sca4 <- parLapply(cl, models.sca4,function(x)
    x[complete.cases(x$dept),])
  models.sca4 <- parLapply(cl, models.sca4,function(x)
    x[complete.cases(x$randomerror4),])
  
  models.sca5 <- parLapply(cl, models.sca5,function(x)
    x[complete.cases(x$year),])
  models.sca5 <- parLapply(cl, models.sca5,function(x)
    x[complete.cases(x$dept),])
  models.sca5 <- parLapply(cl, models.sca5,function(x)
    x[complete.cases(x$randomerror5),])
  
  options(na.action = "na.omit")
  stopCluster(cl)
  
  model_params_list1 <-future_lapply(models.sca1[x], MuMIn::get.models, subset = TRUE)
  model_params_list1 <- lapply(model_params_list1[x], tibble)
  model_params_list1 <- lapply(model_params_list1[x], rename,"model" = "<named list>")
  model_params_list1 <- lapply(model_params_list1[x], dplyr::mutate,(tidied = (map(model, broom::tidy))),
                               (model_num = row_number()))
  model_params_list1 <- lapply(model_params_list1[x], rename,"tidied" = "(tidied = (map(model, broom::tidy)))")
  model_params_list1 <- lapply(model_params_list1[x], rename,"model_num" = "(model_num = row_number())")
  model_params_list1 <- lapply(model_params_list1[x], select,model_num, tidied)
  model_params_list1 <- lapply(model_params_list1[x], unnest,cols = c(tidied))
  model_params_list1terms <- lapply(model_params_list1[x], select,model_num, term, estimate)
  model_params_list1p <- lapply(model_params_list1[x], select,model_num, term, p.value)
  model_params_list1terms <- lapply(model_params_list1terms[x], spread,term, estimate)
  model_params_list1p <- lapply(model_params_list1p[x], spread,term, p.value)
  model_params_list1terms <- lapply(model_params_list1terms[x], select,-starts_with("sd"))
  model_params_list1terms <- lapply(model_params_list1terms[x], select,-starts_with("dept"))
  model_params_list1terms <- lapply(model_params_list1terms[x], select,-starts_with("year"))
  model_params_list1p <- lapply(model_params_list1p[x], select,-starts_with("sd"))
  model_params_list1p <- lapply(model_params_list1p[x], select,-starts_with("dept"))
  model_params_list1p <- lapply(model_params_list1p[x], select,-starts_with("year"))
  
  
  
  model_params_list2 <-future_lapply(models.sca2[x], MuMIn::get.models, subset = TRUE)
  model_params_list2 <- lapply(model_params_list2[x], tibble)
  model_params_list2 <- lapply(model_params_list2[x], rename,"model" = "<named list>")
  model_params_list2 <- lapply(model_params_list2[x], dplyr::mutate,(tidied = (map(model, broom::tidy))),
                               (model_num = row_number()))
  model_params_list2 <- lapply(model_params_list2[x], rename,"tidied" = "(tidied = (map(model, broom::tidy)))")
  model_params_list2 <- lapply(model_params_list2[x], rename,"model_num" = "(model_num = row_number())")
  model_params_list2 <- lapply(model_params_list2[x], select,model_num, tidied)
  model_params_list2 <- lapply(model_params_list2[x], unnest,cols = c(tidied))
  model_params_list2terms <- lapply(model_params_list2[x], select,model_num, term, estimate)
  model_params_list2p <- lapply(model_params_list2[x], select,model_num, term, p.value)
  model_params_list2terms <- lapply(model_params_list2terms[x], spread,term, estimate)
  model_params_list2p <- lapply(model_params_list2p[x], spread,term, p.value)
  model_params_list2terms <- lapply(model_params_list2terms[x], select,-starts_with("sd"))
  model_params_list2terms <- lapply(model_params_list2terms[x], select,-starts_with("dept"))
  model_params_list2terms <- lapply(model_params_list2terms[x], select,-starts_with("year"))
  model_params_list2p <- lapply(model_params_list2p[x], select,-starts_with("sd"))
  model_params_list2p <- lapply(model_params_list2p[x], select,-starts_with("dept"))
  model_params_list2p <- lapply(model_params_list2p[x], select,-starts_with("year"))
  
  model_params_list3 <-future_lapply(models.sca3[x], MuMIn::get.models, subset = TRUE)
  model_params_list3 <- lapply(model_params_list3[x], tibble)
  model_params_list3 <- lapply(model_params_list3[x], rename,"model" = "<named list>")
  model_params_list3 <- lapply(model_params_list3[x], dplyr::mutate,(tidied = (map(model, broom::tidy))),
                               (model_num = row_number()))
  model_params_list3 <- lapply(model_params_list3[x], rename,"tidied" = "(tidied = (map(model, broom::tidy)))")
  model_params_list3 <- lapply(model_params_list3[x], rename,"model_num" = "(model_num = row_number())")
  model_params_list3 <- lapply(model_params_list3[x], select,model_num, tidied)
  model_params_list3 <- lapply(model_params_list3[x], unnest,cols = c(tidied))
  model_params_list3terms <- lapply(model_params_list3[x], select,model_num, term, estimate)
  model_params_list3p <- lapply(model_params_list3[x], select,model_num, term, p.value)
  model_params_list3terms <- lapply(model_params_list3terms[x], spread,term, estimate)
  model_params_list3p <- lapply(model_params_list3p[x], spread,term, p.value)
  model_params_list3terms <- lapply(model_params_list3terms[x], select,-starts_with("sd"))
  model_params_list3terms <- lapply(model_params_list3terms[x], select,-starts_with("dept"))
  model_params_list3terms <- lapply(model_params_list3terms[x], select,-starts_with("year"))
  model_params_list3p <- lapply(model_params_list3p[x], select,-starts_with("sd"))
  model_params_list3p <- lapply(model_params_list3p[x], select,-starts_with("dept"))
  model_params_list3p <- lapply(model_params_list3p[x], select,-starts_with("year"))
  
  
  
  model_params_list4 <-future_lapply(models.sca4[x], MuMIn::get.models, subset = TRUE)
  model_params_list4 <- lapply(model_params_list4[x], tibble)
  model_params_list4 <- lapply(model_params_list4[x], rename,"model" = "<named list>")
  model_params_list4 <- lapply(model_params_list4[x], dplyr::mutate,(tidied = (map(model, broom::tidy))),
                               (model_num = row_number()))
  model_params_list4 <- lapply(model_params_list4[x], rename,"tidied" = "(tidied = (map(model, broom::tidy)))")
  model_params_list4 <- lapply(model_params_list4[x], rename,"model_num" = "(model_num = row_number())")
  model_params_list4 <- lapply(model_params_list4[x], select,model_num, tidied)
  model_params_list4 <- lapply(model_params_list4[x], unnest,cols = c(tidied))
  model_params_list4terms <- lapply(model_params_list4[x], select,model_num, term, estimate)
  model_params_list4p <- lapply(model_params_list4[x], select,model_num, term, p.value)
  model_params_list4terms <- lapply(model_params_list4terms[x], spread,term, estimate)
  model_params_list4p <- lapply(model_params_list4p[x], spread,term, p.value)
  model_params_list4terms <- lapply(model_params_list4terms[x], select,-starts_with("sd"))
  model_params_list4terms <- lapply(model_params_list4terms[x], select,-starts_with("dept"))
  model_params_list4terms <- lapply(model_params_list4terms[x], select,-starts_with("year"))
  model_params_list4p <- lapply(model_params_list4p[x], select,-starts_with("sd"))
  model_params_list4p <- lapply(model_params_list4p[x], select,-starts_with("dept"))
  model_params_list4p <- lapply(model_params_list4p[x], select,-starts_with("year"))
  
  
  model_params_list5 <-future_lapply(models.sca5[x], MuMIn::get.models, subset = TRUE)
  model_params_list5 <- lapply(model_params_list5[x], tibble)
  model_params_list5 <- lapply(model_params_list5[x], rename,"model" = "<named list>")
  model_params_list5 <- lapply(model_params_list5[x], dplyr::mutate,(tidied = (map(model, broom::tidy))),
                               (model_num = row_number()))
  model_params_list5 <- lapply(model_params_list5[x], rename,"tidied" = "(tidied = (map(model, broom::tidy)))")
  model_params_list5 <- lapply(model_params_list5[x], rename,"model_num" = "(model_num = row_number())")
  model_params_list5 <- lapply(model_params_list5[x], select,model_num, tidied)
  model_params_list5 <- lapply(model_params_list5[x], unnest,cols = c(tidied))
  model_params_list5terms <- lapply(model_params_list5[x], select,model_num, term, estimate)
  model_params_list5p <- lapply(model_params_list5[x], select,model_num, term, p.value)
  model_params_list5terms <- lapply(model_params_list5terms[x], spread,term, estimate)
  model_params_list5p <- lapply(model_params_list5p[x], spread,term, p.value)
  model_params_list5terms <- lapply(model_params_list5terms[x], select,-starts_with("sd"))
  model_params_list5terms <- lapply(model_params_list5terms[x], select,-starts_with("dept"))
  model_params_list5terms <- lapply(model_params_list5terms[x], select,-starts_with("year"))
  model_params_list5p <- lapply(model_params_list5p[x], select,-starts_with("sd"))
  model_params_list5p <- lapply(model_params_list5p[x], select,-starts_with("dept"))
  model_params_list5p <- lapply(model_params_list5p[x], select,-starts_with("year"))
  
  
  
  Ps_binded1 <- bind_rows(model_params_list1p, .id="df")
  Ps_binded2 <- bind_rows(model_params_list2p, .id="df")
  Ps_binded3 <- bind_rows(model_params_list3p, .id="df")
  Ps_binded4 <- bind_rows(model_params_list4p, .id="df")
  Ps_binded5 <- bind_rows(model_params_list5p, .id="df")
  
  params_binded1 <- bind_rows(model_params_list1terms, .id="df")
  params_binded2 <- bind_rows(model_params_list2terms, .id="df")
  params_binded3 <- bind_rows(model_params_list3terms, .id="df")
  params_binded4 <- bind_rows(model_params_list4terms, .id="df")
  params_binded5 <- bind_rows(model_params_list5terms, .id="df")
  
  Ps_binded1 <- Ps_binded1[c(1,2,12)]
  Ps_binded2 <- Ps_binded2[c(1,2,12)]
  Ps_binded3 <- Ps_binded3[c(1,2,12)]
  Ps_binded4 <- Ps_binded4[c(1,2,12)]
  Ps_binded5 <- Ps_binded5[c(1,2,12)]
  
  
  names(Ps_binded1)[names(Ps_binded1)=="randomerror1[, x]"] <- "p.value"
  names(Ps_binded2)[names(Ps_binded2)=="randomerror2[, x]"] <- "p.value"
  names(Ps_binded3)[names(Ps_binded3)=="randomerror3[, x]"] <- "p.value"
  names(Ps_binded4)[names(Ps_binded4)=="randomerror4[, x]"] <- "p.value"
  names(Ps_binded5)[names(Ps_binded5)=="randomerror5[, x]"] <- "p.value"
  
  
  plotdata1 <- merge(Ps_binded1, params_binded1, by=c("df", "model_num"))
  plotdata2 <- merge(Ps_binded2, params_binded2, by=c("df", "model_num"))
  plotdata3 <- merge(Ps_binded3, params_binded3, by=c("df", "model_num"))
  plotdata4 <- merge(Ps_binded4, params_binded4, by=c("df", "model_num"))
  plotdata5 <- merge(Ps_binded5, params_binded5, by=c("df", "model_num"))
  
  plotdata1$Error_Bins <- "0.5-1.5" 
  plotdata2$Error_Bins <- "0.6-1.4" 
  plotdata3$Error_Bins <- "0.7-1.3" 
  plotdata4$Error_Bins <- "0.8-1.2" 
  plotdata5$Error_Bins <- "0.9-1.1" 
  
  
  names(plotdata1)[names(plotdata1)=="randomerror1...x."] <- "randomerror"
  names(plotdata2)[names(plotdata2)=="randomerror2...x."] <- "randomerror"
  names(plotdata3)[names(plotdata3)=="randomerror3...x."] <- "randomerror"
  names(plotdata4)[names(plotdata4)=="randomerror4...x."] <- "randomerror"
  names(plotdata5)[names(plotdata5)=="randomerror5...x."] <- "randomerror"
  
  
  
  plot.data.full <- rbind(params_binded1,params_binded2,params_binded3,params_binded4,params_binded5)
  
  
  plot.data.full <- plot.data.full[complete.cases(plot.data.full$randomerror),]
  
  maybe <- (plot.data.full)
  
  
  maybe$completeccgdata.BAME_percent[!is.na(maybe$completeccgdata.BAME_percent)] <- 1
  maybe$completeccgdata.Claimant_percent[!is.na(maybe$completeccgdata.Claimant_percent)] <- 1
  maybe$completeccgdata.lagged_la_spend[!is.na(maybe$completeccgdata.lagged_la_spend)] <- 1
  maybe$completeccgdata.Lagged_Total_Spend[!is.na(maybe$completeccgdata.Lagged_Total_Spend)] <- 1
  maybe$completeccgdata.Qual_lvl4_percent[!is.na(maybe$completeccgdata.Qual_lvl4_percent)] <- 1
  maybe$completeccgdata.Unemployment_percent[!is.na(maybe$completeccgdata.Unemployment_percent)] <- 1
  maybe$log.completeccgdata.CCGpop.[!is.na(maybe$log.completeccgdata.CCGpop.)] <- 1
  maybe$log.completeccgdata.GDHI_per_person.[!is.na(maybe$log.completeccgdata.GDHI_per_person.)] <- 1
  
  maybe <- maybe[order(maybe$completeccgdata.BAME_percent),]
  maybe <- maybe[order(maybe$completeccgdata.Claimant_percent),]
  maybe <- maybe[order(maybe$completeccgdata.lagged_la_spend),]
  maybe <- maybe[order(maybe$completeccgdata.Lagged_Total_Spend),]
  maybe <- maybe[order(maybe$completeccgdata.Qual_lvl4_percent),]
  maybe <- maybe[order(maybe$completeccgdata.Unemployment_percent),]
  maybe <- maybe[order(maybe$log.completeccgdata.CCGpop.),]
  maybe <- maybe[order(maybe$log.completeccgdata.GDHI_per_person.),]
  
  maybe$model_number <- factor(rep(c(1:256), each=150))
  maybe <- maybe[c(4,5,14, 16)]
  
  plot.data.full <- merge(plot.data.full, maybe, by=c("randomerror", "X.Intercept.", "p.value"))#and intercept and p-value please
  
  averageest <- aggregate(. ~model_number, data=maybe[c(3,4)], mean)
  names(averageest)[names(averageest)=="randomerror"] <- "average_estimate"
  
  plot.data.full <- merge(plot.data.full, averageest, by="model_number", all.x = T)
  plot.data.full <- plot.data.full[order(plot.data.full$average_estimate),]
  plot.data.full$specification <- factor(rep(c(1:256), each=150))
  
  selectcases <- plot.data.full[which(plot.data.full$specification=="1"|plot.data.full$specification=="16"|plot.data.full$specification=="32"|plot.data.full$specification=="48"|plot.data.full$specification=="54"|plot.data.full$specification=="70"|plot.data.full$specification=="86"|plot.data.full$specification=="102"|plot.data.full$specification=="118"|plot.data.full$specification=="134"|plot.data.full$specification=="150"|plot.data.full$specification=="176"|plot.data.full$specification=="192"|plot.data.full$specification=="208"|plot.data.full$specification=="224"|plot.data.full$specification=="240"|plot.data.full$specification=="256"),]
  
  
  
  sina_plot <- ggplot(plot.data.full, aes(y=randomerror, x=Error_Bins))+
    geom_violin()+
    geom_sina(aes(color = Significant_Pvalue, group = Error_Bins),size =.4, alpha = 0.7)+
    labs(y="Outsourcing Coefficient", x = "Random Error Multiplier Bins")
  # geom_hline(yintercept=0.004, linetype='dashed', colour = "grey")+
  
  sina_plot
  
  
}

#Matching
Create_S.13 <- function(MyAnnualDataCCG) {
  matchdata <- MyAnnualDataCCG
  matchdata$procyearchange <- matchdata$Private_Sector_Procurement_Spend- matchdata$Lagged_Private_Procurement
  
  matchdata <- matchdata %>% mutate(binaryzero = ifelse(matchdata$Lagged_Private_Procurement>4, 1,0))
  matchdata <- matchdata %>% mutate(binaryzerofive = ifelse(matchdata$Lagged_Private_Procurement>7, 1,0))
  matchdata <- matchdata %>% mutate(binarytwofive = ifelse(matchdata$Lagged_Private_Procurement>10, 1,0))
  
  matchdata$binaryzero <- factor(matchdata$binaryzero)
  matchdata$binaryzerofive <- factor(matchdata$binaryzerofive)
  matchdata$binarytwofive <- factor(matchdata$binarytwofive)
  
  
  completematchdata <- matchdata[complete.cases(MyAnnualDataCCG$number_of_doctors),]
  completematchdata <- completematchdata[complete.cases(completematchdata$Treatable_Mortality_Rate),]
  completematchdata <- completematchdata[complete.cases(completematchdata$binaryzero),]
  completematchdata <- completematchdata[complete.cases(completematchdata$Lagged_Private_Procurement),]
  completematchdata <- completematchdata[complete.cases(completematchdata$Lagged_GDHI_per_person),]
  completematchdata <- completematchdata[complete.cases(completematchdata$Lagged_Local_Authority_Spend_per_pop),]
  completematchdata <- completematchdata[complete.cases(completematchdata$BAME_percent),]
  completematchdata <- completematchdata[complete.cases(completematchdata$Qual_lvl4_percent),]
  completematchdata <- completematchdata[complete.cases(completematchdata$Unemployment_percent),]
  completematchdata <- completematchdata[complete.cases(completematchdata$Claimant_percent),]
  completematchdata <- completematchdata[complete.cases(completematchdata$professional_and_managerial),]
  completematchdata <- completematchdata[complete.cases(completematchdata$CCGpop),]
  completematchdata <- completematchdata[complete.cases(completematchdata$dept),]
  
  
  
  #m.out <- matchit(binaryzero ~ number_of_doctors, method = "nearest",distance = "glm", data = completematchdata)
  m.out1 <- matchit(binaryzero ~ number_of_doctors, data = completematchdata, method = "full", distance = "glm", link = "probit")
  m.out2 <- matchit(binaryzerofive ~ number_of_doctors, data = completematchdata, method = "full", distance = "glm", link = "probit")
  m.out5 <- matchit(binarytwofive ~ number_of_doctors, data = completematchdata, method = "full", distance = "glm", link = "probit")
  
  m.out3 <- matchit(binaryzero ~ Treatable_Mortality_2013, data = completematchdata, method = "full", distance = "glm", link = "probit")
  m.out4 <- matchit(binaryzerofive ~ Treatable_Mortality_2013, data = completematchdata, method = "full", distance = "glm", link = "probit")
  m.out6 <- matchit(binarytwofive ~ Treatable_Mortality_2013, data = completematchdata, method = "full", distance = "glm", link = "probit")
  
  
  m.data1 <- match.data(m.out1)
  m.data2 <- match.data(m.out2)
  m.data3 <- match.data(m.out3)
  m.data4 <- match.data(m.out4)
  m.data6 <- match.data(m.out6)
  m.data5 <- match.data(m.out5)
  
  
  fit1 <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), data = m.data1, weights = weights)
  fit2 <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), data = m.data2, weights = weights)
  fit3 <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), data = m.data3, weights = weights)
  fit4 <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), data = m.data4, weights = weights)
  fit5 <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), data = m.data5, weights = weights)
  fit6 <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year), data = m.data6, weights = weights)
  
  
  
  C1 <- coef_test(fit1, vcov = "CR2", cluster = m.data1$dept, test = "Satterthwaite")$SE
  C2 <- coef_test(fit2, vcov = "CR2", cluster = m.data2$dept, test = "Satterthwaite")$SE
  C3 <- coef_test(fit3, vcov = "CR2", cluster = m.data3$dept, test = "Satterthwaite")$SE
  C4 <- coef_test(fit4, vcov = "CR2", cluster = m.data4$dept, test = "Satterthwaite")$SE
  C5 <- coef_test(fit5, vcov = "CR2", cluster = m.data5$dept, test = "Satterthwaite")$SE
  C6 <- coef_test(fit6, vcov = "CR2", cluster = m.data6$dept, test = "Satterthwaite")$SE
  
  names(C1) <- names(fit1$coefficients)
  names(C2) <- names(fit2$coefficients)
  names(C3) <- names(fit3$coefficients)
  names(C4) <- names(fit4$coefficients)
  names(C5) <- names(fit5$coefficients)
  names(C6) <- names(fit6$coefficients)
  
  
  
  rows <- tribble(~term,          ~`(1)`,  ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`,
                  'CCG Fixed Effects', 'Yes',  'Yes','Yes',  'Yes','Yes','Yes',
                  'Time Fixed Effects', 'Yes',  'Yes','Yes','Yes','Yes','Yes',
                  'Clustered Standard Errors', 'Yes',  'Yes','Yes','Yes','Yes','Yes',
                  'Control Variables', 'Yes',  'Yes','Yes','Yes','Yes','Yes',)
  
  cm <- c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)")
  
  modelsummary(list("(1)"=fit1,"(2)"=fit2,"(3)"=fit3,"(4)"=fit4,"(5)"=fit5, "(6)"=fit6),
               statistic_override=list(C1,C2,C3,C4, C5,C6), coef_omit = "Intercept|dept|year",
               coef_map = cm,fmt=4,
               add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            'Tr. mortaility, Population and GDHI are log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)'
               ),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2:7) 
}

#Quadratic term
Create_S.14 <- function(MyAnnualDataCCG) {
  
  quadratic_modelfull<- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+I(Lagged_Private_Procurement^2)+Lagged_Local_Authority_Spend_per_pop+Lagged_Total_Spend+Claimant_percent+  log(CCGpop) +Unemployment_percent +BAME_percent+Qual_lvl4_percent +log(GDHI_per_person)+professional_and_managerial+factor(dept)+factor(year),  data=MyAnnualDataCCG, na.action=na.exclude)
  
  
  
  C1 <- coef_test(quadratic_modelfull, vcov = "CR2", cluster = m.data1$dept, test = "Satterthwaite")$SE
  
  names(C1) <- names(quadratic_modelfull$coefficients)
  
  
  
  rows <- tribble(~term,          ~`(1)`, 
                  'CCG Fixed Effects', 'Yes', 
                  'Time Fixed Effects', 'Yes',
                  'Clustered Standard Errors','Yes',
                  'Control Variables', 'Yes')
  
  cm <- c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)",
          "I(Lagged_Private_Procurement^2)" = "Quadratic Outsourcing Term")
  
  modelsummary(list("(1)"=quadratic_modelfull),
               statistic_override=list(C1), coef_omit = "Intercept|dept|year",
               coef_map = cm,fmt=4,
               add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            'Tr. mortaility, Population and GDHI are log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)'
               ),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2) 
  }

#Conditional Growth Curve model
Create_S.14.1 <- function(MyAnnualDataCCG) {
  
  growdata <- MyAnnualDataCCG
  growdata$time <- as.double(growdata$year)
  
  growmodel<- lmerTest::lmer(log(Treatable_Mortality_Rate)~Private_Sector_Procurement_Spend*time+(1+time|ccg19cd),  data=growdata, na.action=na.exclude)
  
  
  rows <- tribble(~term,          ~`(1)`, 
                  'Clustered Standard Errors','Yes',
                  'Control Variables', 'Yes')
  
  cm <- c("Private_Sector_Procurement_Spend" = "For-profit Outsourcing (%)",
          "time" = "Time", "Private_Sector_Procurement_Spend:time" = "Interaction term")
  
  modelsummary(list("(1)"=growmodel),
               statistic_override=list(sqrt(diag(vcovCR(growmodel, type = "CR2")))), coef_omit = "Intercept|dept|year",
               coef_map = cm,fmt=4,
               add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Tr. mortaility is log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)'
               ),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2) 
}

#Unconditional
Create_S.14.2 <- function(MyAnnualDataCCG) {
  
  growdata <- MyAnnualDataCCG
  growdata$time <- as.double(growdata$year)
  
  growmodel<- lmerTest::lmer(Private_Sector_Procurement_Spend~time+(time|ccg19cd),  data=growdata, na.action=na.exclude)
  
  
  rows <- tribble(~term,          ~`(1)`,  
                  'Clustered Standard Errors','Yes' )
  
  cm <- c("Private_Sector_Procurement_Spend" = "For-profit Outsourcing (%)",
          "time" = "Time", "Private_Sector_Procurement_Spend:time" = "Interaction term")
  
  modelsummary(list("(1)"=growmodel),
               statistic_override=list(sqrt(diag(vcovCR(growmodel, type = "CR2")))), coef_omit = "Intercept|dept|year",
               coef_map = cm,fmt=4,
               add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Tr. mortaility is log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)'
               ),
               output = "gt")%>%
    tab_spanner(label = 'For-profit Outsourcing (%)', columns = 2) 
}

#DiD
Create_S.15.1 <- function(MyAnnualDataCCG) {
  
  
  myoutsourcechange <- MyAnnualDataCCG[which(MyAnnualDataCCG$year!="2014"),]
  
  myoutsourcechange$outsourcechange <- myoutsourcechange$Private_Sector_Procurement_Spend-myoutsourcechange$Lagged_Private_Procurement
  
  myoutsourcechange <- myoutsourcechange[c("ccg19cd","outsourcechange")]
  
  keep <- myoutsourcechange[complete.cases(myoutsourcechange),] %>% dplyr::group_by(ccg19cd) %>% dplyr::summarize(nobs = n()) %>%mutate(keepdid = ifelse(nobs>2,1,0))
  
  myoutsourcechange <- myoutsourcechange %>% mutate(outsourced= ifelse(outsourcechange>0,1,0))
  
  myoutsourcechange <- aggregate(. ~ccg19cd, myoutsourcechange[c("ccg19cd", "outsourced")], sum, na.rm=TRUE, na.action=NULL)
  
  myoutsourcechange <- myoutsourcechange %>% mutate(treat= ifelse(outsourced>0,1,0))
  
  MyAnnualDataCCGdid <- MyAnnualDataCCG %>% mutate(time= ifelse(year=="2010",0,ifelse(year=="2011",0,ifelse(year=="2012",0,ifelse(year=="2013",0, ifelse(year=="2019",1, ifelse(year=="2018",1, ifelse(year=="2017",1, ifelse(year=="2016",1,ifelse(year=="2015",1, ifelse(year=="2014",1, NA)))))))))))
  MyAnnualDataCCGdid <- merge(MyAnnualDataCCGdid, myoutsourcechange, by="ccg19cd", all.x=T )
  MyAnnualDataCCGdid <- merge(MyAnnualDataCCGdid, keep, by="ccg19cd", all.x=T )
  MyAnnualDataCCGdid <- MyAnnualDataCCGdid[which(MyAnnualDataCCGdid$keepdid==T),]
  
  
  did <-  lm(log(Treatable_Mortality_Rate)~treat*time ,data=MyAnnualDataCCGdid)
  
  cm <- c("treat" = "Treatment",
          "time" = "Time",
          "treat:time" = "Treatment*Time")
  
  modelsummary(list("(1)"=did),
               coef_omit = "Intercept|dept|year",
               coef_map = cm,fmt=4,
               stars = T ,statistic = c("std.error", "conf.int"),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2) 
  
}

#DiD graph
Create_S.15.2 <- function(MyAnnualDataCCG) {
  
  
  myoutsourcechange <- MyAnnualDataCCG[which(MyAnnualDataCCG$year!="2014"),]
  
  myoutsourcechange$outsourcechange <- myoutsourcechange$Private_Sector_Procurement_Spend-myoutsourcechange$Lagged_Private_Procurement
  
  myoutsourcechange <- myoutsourcechange[c("ccg19cd","outsourcechange")]
  
  keep <- myoutsourcechange[complete.cases(myoutsourcechange),] %>% dplyr::group_by(ccg19cd) %>% dplyr::summarize(nobs = n()) %>%mutate(keepdid = ifelse(nobs>2,1,0))
  
  myoutsourcechange <- myoutsourcechange %>% mutate(outsourced= ifelse(outsourcechange>0,1,0))
  
  myoutsourcechange <- aggregate(. ~ccg19cd, myoutsourcechange[c("ccg19cd", "outsourced")], sum, na.rm=TRUE, na.action=NULL)
  
  myoutsourcechange <- myoutsourcechange %>% mutate(treat= ifelse(outsourced>0,1,0))
  
  MyAnnualDataCCGdid <- MyAnnualDataCCG %>% mutate(time= ifelse(year=="2010",0,ifelse(year=="2011",0,ifelse(year=="2012",0,ifelse(year=="2013",1, ifelse(year=="2019",1, ifelse(year=="2018",1, ifelse(year=="2017",1, ifelse(year=="2016",1,ifelse(year=="2015",1, ifelse(year=="2014",1, NA)))))))))))
  MyAnnualDataCCGdid <- merge(MyAnnualDataCCGdid, myoutsourcechange, by="ccg19cd", all.x=T )
  MyAnnualDataCCGdid <- merge(MyAnnualDataCCGdid, keep, by="ccg19cd", all.x=T )
  MyAnnualDataCCGdid <- MyAnnualDataCCGdid[which(MyAnnualDataCCGdid$keepdid==T),]
  
  
  didplot <- ggplot(MyAnnualDataCCGdid, aes(x=as.double(year), y=log(Treatable_Mortality_Rate), fill=factor(treat), colour=factor(treat), group=factor(treat)))+
    geom_smooth()
  
  sumdata <- MyAnnualDataCCGdid[c("treat", "year", "Treatable_Mortality_Rate")]
  sumdata <- aggregate(. ~treat+year, data=sumdata, mean)
  
  didgraph <- ggplot() + geom_line(data=MyAnnualDataCCGdid[which(MyAnnualDataCCGdid$year=="2010"|MyAnnualDataCCGdid$year=="2011"|MyAnnualDataCCGdid$year=="2012"|MyAnnualDataCCGdid$year=="2013"|MyAnnualDataCCGdid$year=="2014"|MyAnnualDataCCGdid$year=="2014"|MyAnnualDataCCGdid$year=="2015"|MyAnnualDataCCGdid$year=="2016"|MyAnnualDataCCGdid$year=="2017"|MyAnnualDataCCGdid$year=="2018"|MyAnnualDataCCGdid$year=="2019"),],aes(x=year,y=Treatable_Mortality_Rate,group=ccg19cd,color=factor(treat)),
                                   size=1,alpha=0.20) + # plot the individual lines
    geom_line(data=sumdata,aes(x=year,y=Treatable_Mortality_Rate,group=factor(treat),color=factor(treat)),
              size=2) + # plot the averages for each group
    geom_vline(xintercept = "2014") + # intervention point
    scale_color_manual(values=c("red","blue"), # label our groups
                       labels=c("Control Average","Treatment Average")) +
    labs(title="Difference in Differences",
         x="Time",
         y="Treatable Mortality Rate") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom")
  
  didgraph
}

#Austerity interaction
Create_S.16.1 <-  function(MyAnnualDataCCG) {
  
  intdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$dept),]
  intdata <- intdata[complete.cases(intdata$year),]
  intdata <- intdata[complete.cases(intdata$Lagged_Private_Procurement),]
  intdata <- intdata[complete.cases(intdata$Treatable_Mortality_Rate),]
  intdata <- intdata[complete.cases(intdata$Lagged_Total_Spend),]
  intdata <- intdata[complete.cases(intdata$Lagged_Local_Authority_Spend_per_pop),]
  
  LA<- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement*Lagged_Local_Authority_Spend_per_pop+factor(dept)+factor(year),  data=intdata, na.action=na.exclude)
  CCG <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement*Lagged_Total_Spend+factor(dept)+factor(year),  data=intdata, na.action=na.exclude)
  
  
  
  C1 <- coef_test(LA, vcov = "CR2", cluster = intdata$dept, test = "Satterthwaite")$SE
  C2 <- coef_test(CCG, vcov = "CR2", cluster = intdata$dept, test = "Satterthwaite")$SE
  
  names(C1) <- names(LA$coefficients)
  names(C2) <- names(CCG$coefficients)
  
  
  
  rows <- tribble(~term,          ~`(1)`, ~`(2)`,
                  'CCG Fixed Effects', 'Yes','Yes', 
                  'Time Fixed Effects', 'Yes','Yes',
                  'Clustered Standard Errors','Yes', 'Yes')
  
  cm <- c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)",
          "Lagged_Total_Spend" = "Total CCG Spend (£Ms)",
          "Lagged_Private_Procurement:Lagged_Total_Spend" = "Outsourcing*CCG Spend",
          "Lagged_Local_Authority_Spend_per_pop"= "LA spend (£000s per person)",
          "Lagged_Private_Procurement:Lagged_Local_Authority_Spend_per_pop" = "Outsourcing*LA Spend")
  
  modelsummary(list("(1)"=CCG, "(2)" = LA),
               statistic_override=list(C2, C1), coef_omit = "Intercept|dept|year",
               coef_map = cm,fmt=4,
               add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            'Tr. mortaility, Population and GDHI are log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)'
               ),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2) 
}

#demographic interaction
Create_S.16.2 <-  function(MyAnnualDataCCG) {
  
  intdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$dept),]
  intdata <- intdata[complete.cases(intdata$year),]
  intdata <- intdata[complete.cases(intdata$Lagged_Private_Procurement),]
  intdata <- intdata[complete.cases(intdata$Treatable_Mortality_Rate),]
  intdata <- intdata[complete.cases(intdata$Claimant_percent),]
  intdata <- intdata[complete.cases(intdata$BAME_percent),]
  intdata <- intdata[complete.cases(intdata$Unemployment_percent),]
  intdata <- intdata[complete.cases(intdata$Qual_lvl4_percent),]
  intdata <- intdata[complete.cases(intdata$professional_and_managerial),]
  intdata <- intdata[complete.cases(intdata$CCGpop),]
  intdata <- intdata[complete.cases(intdata$GDHI_per_person),]
  
  GDHI<- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement*log(GDHI_per_person)+factor(dept)+factor(year),  data=intdata, na.action=na.exclude)
  CCGPOP <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement*log(CCGpop)+factor(dept)+factor(year),  data=intdata, na.action=na.exclude)
  CCGOcc <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement*professional_and_managerial+factor(dept)+factor(year),  data=intdata, na.action=na.exclude)
  CCGeth <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement*BAME_percent+factor(dept)+factor(year),  data=intdata, na.action=na.exclude)
  CCGclaim <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement*Claimant_percent+factor(dept)+factor(year),  data=intdata, na.action=na.exclude)
  CCGunemp <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement*Unemployment_percent+factor(dept)+factor(year),  data=intdata, na.action=na.exclude)
  CCGqual <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement*Qual_lvl4_percent+factor(dept)+factor(year),  data=intdata, na.action=na.exclude)
  
  
  
  C1 <- coef_test(GDHI, vcov = "CR2", cluster = intdata$dept, test = "Satterthwaite")$SE
  C2 <- coef_test(CCGPOP, vcov = "CR2", cluster = intdata$dept, test = "Satterthwaite")$SE
  C3 <- coef_test(CCGOcc, vcov = "CR2", cluster = intdata$dept, test = "Satterthwaite")$SE
  C4 <- coef_test(CCGeth, vcov = "CR2", cluster = intdata$dept, test = "Satterthwaite")$SE
  C5 <- coef_test(CCGclaim, vcov = "CR2", cluster = intdata$dept, test = "Satterthwaite")$SE
  C6 <- coef_test(CCGunemp, vcov = "CR2", cluster = intdata$dept, test = "Satterthwaite")$SE
  C7 <- coef_test(CCGqual, vcov = "CR2", cluster = intdata$dept, test = "Satterthwaite")$SE
  
  names(C1) <- names(GDHI$coefficients)
  names(C2) <- names(CCGPOP$coefficients)
  names(C3) <- names(CCGOcc$coefficients)
  names(C4) <- names(CCGeth$coefficients)
  names(C5) <- names(CCGclaim$coefficients)
  names(C6) <- names(CCGunemp$coefficients)
  names(C7) <- names(CCGqual$coefficients)
  
  
  
  rows <- tribble(~term,          ~`(1)`, ~`(2)`, ~`(3)`,~`(4)`,~`(5)`,~`(6)`,~`(7)`,
                  'CCG Fixed Effects', 'Yes','Yes', 'Yes','Yes', 'Yes','Yes', 'Yes',
                  'Time Fixed Effects', 'Yes','Yes','Yes','Yes', 'Yes','Yes', 'Yes',
                  'Clustered Standard Errors','Yes', 'Yes','Yes','Yes', 'Yes','Yes', 'Yes')
  
  
  
  cm <- c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)",
          "log(GDHI_per_person)" = "Average Disposable H.hold Income", 
          "Lagged_Private_Procurement:log(GDHI_per_person)" = "Outsourcing*Income",
          "log(CCGpop)"="Population size",
          "Lagged_Private_Procurement:log(CCGpop)"="Outsourcing*Population",
          "professional_and_managerial" = "Managerial/Professional occupation (%)",
          "Lagged_Private_Procurement:professional_and_managerial" = "Outsourcing*Occupation",
          "BAME_percent" = "Ethnic Minority (%)", 
          "Lagged_Private_Procurement:BAME_percent" = "Outsourcing*Ethnicity", 
          "Claimant_percent" = "Claimant Rate (%)",
          "Lagged_Private_Procurement:Claimant_percent" = "Outsourcing*Claimant Rate",
          "Unemployment_percent" = "Unemployment Rate (%)",
          "Lagged_Private_Procurement:Unemployment_percent" = "Outsourcing*Unemployment Rate",
          "Qual_lvl4_percent" = "Degree Education (%)",
          "Lagged_Private_Procurement:Qual_lvl4_percent" = "Outsourcing*Education"
          
  )
  
  
  modelsummary(list("(1)"=GDHI, "(2)" = CCGPOP, "(3)" = CCGOcc, "(4)" = CCGeth, "(5)" = CCGclaim, "(6)" = CCGunemp, "(7)" = CCGqual),
               statistic_override=list(C1, C2, C3, C4, C5, C6, C7), coef_omit = "Intercept|dept|year",
               coef_map = cm,fmt=4,
               add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            'Tr. mortaility, Population and GDHI are log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)'
               ),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2:8) 
}

#Deprivation interaction
Create_S.16.3 <-  function(MyAnnualDataCCG) {
  
  intdata <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$dept),]
  intdata <- intdata[complete.cases(intdata$year),]
  intdata <- intdata[complete.cases(intdata$Lagged_Private_Procurement),]
  intdata <- intdata[complete.cases(intdata$Treatable_Mortality_Rate),]
  intdata <- intdata[complete.cases(intdata$IMD_2019_Extent),]
  
  dep<- lmerTest::lmer(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement*IMD_2019_Extent+factor(year)+(1|dept),  data=intdata)
  
  
  
  
  rows <- tribble(~term,          ~`(1)`,
                  'CCG Random Effects', 'Yes', 
                  'Time Fixed Effects', 'Yes',
                  'Clustered Standard Errors','Yes')
  
  cm <- c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)",
          "IMD_2019_Extent" = "Deprivation (2019)",
          "Lagged_Private_Procurement:IMD_2019_Extent" = "Outsourcing*Deprivation")
  
  modelsummary(list("(1)"=dep),
               statistic_override=list(sqrt(diag(vcovCR(dep, type = "CR2")))), coef_omit = "Intercept|dept|year",
               coef_map = cm,fmt=4,
               add_rows = rows,stars = T ,statistic = c("std.error", "conf.int"),
               notes = list('Outsourcing, LA Spend, and CCG Spend have a one year lag.',
                            'Tr. mortaility, Population and GDHI are log transformed, "Ln" denotes the natural log of outcome variable.', 
                            'Robust SEs are clustered at individual level and use a bias-reduced linearization estimator (CR2)'
               ),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2) 
}

#Lisa clusters
Create_S.17 <-  function(MyAnnualDataCCG) {
  
  
  mapdata <- MyAnnualDataCCG[c("Total_Private_sector_spend","Total_Procurement_Spend", "ccg19cd")]
  
  mapdata <- aggregate(.~ ccg19cd ,sum,data=mapdata, na.rm=T, na.action=NULL)
  projcrs <- "EPSG:27700"
  
  mapshape <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BGC_v2/FeatureServer/0/query?where=1%3D1&outFields=*&geometry=&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=geojson")
  names(mapshape)[names(mapshape)=="CCG19CD"] <- "ccg19cd"
  
  mapshape <- st_as_sf(mapshape)
  
  mapdata <- merge(mapshape, mapdata, by="ccg19cd", all.x=T)
  mapdata$outsourcing <- (mapdata$Total_Private_sector_spend/mapdata$Total_Procurement_Spend)*100
  
  
  # #remove isle of wight which can't have neighbours!
  # 
  mapdata <- mapdata[which(mapdata$ccg19cd!="E38000087"),]
  
  #remove missing data
  
  mapdata <- mapdata[which(!is.na(mapdata$outsourcing)),]
  
  
  
  neighbours <- poly2nb(mapdata, queen = FALSE)
  listw <- nb2listw(neighbours, zero.policy=TRUE)
  
  
  local <- localmoran(x = mapdata$outsourcing, listw = nb2listw(neighbours,  zero.policy=TRUE))
  
  
  
  #LISA clusters
  
  quadrant <- vector(mode="numeric",length=nrow(local))
  
  # centers the variable of interest around its mean
  m.private <- mapdata$outsourcing - mean(mapdata$outsourcing, na.rm=TRUE)     
  
  # centers the local Moran's around the mean
  m.local <- local[,1] - mean(local[,1], na.rm=TRUE) 
  
  
  
  # significance threshold
  signif <- 0.05
  # 
  # # builds a data quadrant
  quadrant[m.private >0 & m.local>0] <- 4  
  quadrant[m.private <0 & m.local<0] <- 1      
  quadrant[m.private <0 & m.local>0] <- 2
  quadrant[m.private >0 & m.local<0] <- 3
  quadrant[local[,5]>signif] <- 0   
  # 
  # # plot in r
  brks <- c(0,1,2,3,4)
  colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
  

  plot(mapdata[c(3)],border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
  legend("bottomleft", legend = c("insignificant","low-low","low-high","high-low","high-high"),
         fill=colors,bty="n")

  
  
}


#zoomed in maps
Create_S.18 <- function(MyAnnualDataCCG) {
  
  
  
  
  mapdata <- MyAnnualDataCCG[c("Total_Private_sector_spend","Total_Procurement_Spend", "ccg19cd")]
  
  mapdata <- aggregate(.~ ccg19cd ,sum,data=mapdata, na.rm=T, na.action=NULL)
  projcrs <- "EPSG:27700"
  
  mapshape <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BGC_v2/FeatureServer/0/query?where=1%3D1&outFields=*&geometry=&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=geojson")
  names(mapshape)[names(mapshape)=="CCG19CD"] <- "ccg19cd"
  
  mapshape <- st_as_sf(mapshape)
  
  mapdata <- merge(mapshape, mapdata, by="ccg19cd", all.x=T)
  mapdata$outsourcing <- (mapdata$Total_Private_sector_spend/mapdata$Total_Procurement_Spend)*100
  
  
  theme_map <- function(...) {
    theme_minimal() +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #    panel.background = element_rect(fill = "transparent"), # bg of the panel
        #     plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        # legend.background = element_rect(fill = "transparent", color=NA), # get rid of legend bg
        #  legend.box.background = element_rect(fill = "transparent", color=NA),
        # panel.border = element_blank(),legend.title=element_text(size=8), 
        #  legend.text=element_text(size=7),legend.key.size = unit(0.3, "cm"),
        ...
      )
  }
  
  no_classes <- 6
  
  
  quantiles <- quantile(mapdata$outsourcing, 
                        probs = seq(0, 1, length.out = no_classes + 1), na.rm=T)
  
  # here I define custom labels (the default ones would be ugly)
  labels <- c()
  for(idx in 1:length(quantiles)){
    labels <- c(labels, paste0(round(quantiles[idx], 2), 
                               " - ", 
                               round(quantiles[idx + 1], 2)))
  }
  # I need to remove the last label 
  # because that would be something like "66.62 - NA"
  labels <- labels[1:length(labels)-1]
  
  # here I actually create a new 
  # variable on the dataset with the quantiles
  mapdata$outsourcing_quantiles <- cut(mapdata$outsourcing, 
                                       breaks = quantiles, 
                                       labels = labels, 
                                       include.lowest = T)
  
  region <- unique(MyAnnualDataCCG[c("RGN19NM", "ccg19cd")])
  mapdata <- merge(mapdata, region, by="ccg19cd", all.x=T)
  london <- mapdata[which(mapdata$RGN19NM=="London"),]
  nw <- mapdata[which(mapdata$RGN19NM=="North West"),]
  wm <- mapdata[which(mapdata$RGN19NM=="West Midlands"),]
  
  
  
  
  library(RColorBrewer)
  wmmap <- ggplot(data = wm ) +
    geom_sf(aes(fill = outsourcing_quantiles), color = NA) +
    theme_map()+
    labs(x = NULL, 
         y = NULL)+
    scale_fill_brewer(
      palette = "OrRd",
      name = "For-profit Outsourcing\n(%, 2013-2020)", na.value="grey")+
    theme(text=element_text(size=7),legend.position = "None" ,legend.key.size = unit(0.5, "cm"))
  
  london <- ggplot(data = london ) +
    geom_sf(aes(fill = outsourcing_quantiles), color = NA) +
    theme_map()+
    labs(x = NULL, 
         y = NULL)+
    scale_fill_brewer(
      palette = "OrRd",
      name = "For-profit Outsourcing\n(%, 2013-2020)", na.value="grey")+
    theme(text=element_text(size=7), legend.key.size = unit(0.5, "cm"))
  
  nwmap <- ggplot(data = nw ) +
    geom_sf(aes(fill = outsourcing_quantiles), color = NA) +
    theme_map()+
    labs(x = NULL, 
         y = NULL)+
    scale_fill_brewer(
      palette = "OrRd",
      name = "For-profit Outsourcing\n(%, 2013-2020)", na.value="grey")+
    theme(text=element_text(size=7),legend.position = "None", legend.key.size = unit(0.5, "cm"))
  
  once <- cowplot::plot_grid(nwmap, wmmap, ncol=2 ,labels="AUTO")
  twice <- cowplot::plot_grid(NULL, london, NULL ,ncol=3, rel_widths = c(0.4, 1.2, 0.4) ,labels=c("", "C", ""))
  all <- cowplot::plot_grid(once, twice, ncol=1 )
all  
  
}

#Ethnicity breakdown
Create_S.19 <- function(MyAnnualDataCCG) {
  
  mixed <- read_csv("Data/mixed2.csv")
  black <- read_csv("Data/Black_blackbritish.csv")
  bangladeshipakastani <- read_csv("Data/bangladeshi pakastani.csv")
  indian <- read_csv("Data/indian.csv")
  
  
  mixed <- melt(mixed,id =  c("LAD19CD"), variable.name = "year")
  names(mixed)[names(mixed)=="value"] <- "mixed"
  
  black <- melt(black,id =  c("LAD19CD"), variable.name = "year")
  names(black)[names(black)=="value"] <- "black"
  
  bangladeshipakastani <- melt(bangladeshipakastani,id =  c("LAD19CD"), variable.name = "year")
  names(bangladeshipakastani)[names(bangladeshipakastani)=="value"] <- "bangladeshi_pakistani"
  
  indian <- melt(indian,id =  c("LAD19CD"), variable.name = "year")
  names(indian)[names(indian)=="value"] <- "indian"
  
  localauthority <-read_csv("Data/LSOAtoCCGtoLAD19.csv")
  
  localauthority <- localauthority[c(4,9, 10)]
  
  localauthority <- unique(localauthority)
  
  panelcontrols2 <- merge(localauthority, indian, by=c("LAD19CD"), all=T)
  panelcontrols2 <- merge(panelcontrols2, bangladeshipakastani, by=c("LAD19CD", "year"), all=T)
  panelcontrols2 <- merge(panelcontrols2, black, by=c("LAD19CD", "year"), all=T)
  panelcontrols2 <- merge(panelcontrols2, mixed, by=c("LAD19CD", "year"), all=T)
  
  panelcontrols2 <- panelcontrols2[-c(1,4)]
  panelcontrols2$indian <- as.double(panelcontrols2$indian)
  panelcontrols2$black <- as.double(panelcontrols2$black)
  panelcontrols2$bangladeshi_pakistani <- as.double(panelcontrols2$bangladeshi_pakistani)
  panelcontrols2$mixed <- as.double(panelcontrols2$mixed)
  
  panelcontrols2 <- aggregate(panelcontrols2, by=list(panelcontrols2$ccg19cd,panelcontrols2$year), FUN=mean, na.rm=TRUE)
  
  panelcontrols2 <- panelcontrols2[-c(3,4)]
  
  
  names(panelcontrols2)[names(panelcontrols2)=="Group.1"] <- "ccg19cd"
  names(panelcontrols2)[names(panelcontrols2)=="Group.2"] <- "year"
  
  
  ethnicitydata <- merge(MyAnnualDataCCG, panelcontrols2, by=c("ccg19cd", "year"), all.x = T)
  ethnicitydata <- ethnicitydata[complete.cases(ethnicitydata$dept),]
  ethnicitydata <- ethnicitydata[complete.cases(ethnicitydata$black),]
  ethnicitydata <- ethnicitydata[complete.cases(ethnicitydata$indian),]
  ethnicitydata <- ethnicitydata[complete.cases(ethnicitydata$mixed),]
  ethnicitydata <- ethnicitydata[complete.cases(ethnicitydata$bangladeshi_pakistani),]
  ethnicitydata <- ethnicitydata[complete.cases(ethnicitydata$Lagged_Private_Procurement),]
  ethnicitydata <- ethnicitydata[complete.cases(ethnicitydata$Treatable_Mortality_Rate),]
  
  
  black<- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+black+factor(dept)+factor(year),  data=ethnicitydata, na.action=na.exclude)
  mixed <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+mixed+factor(dept)+factor(year),  data=ethnicitydata, na.action=na.exclude)
  indian <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+indian+factor(dept)+factor(year),  data=ethnicitydata, na.action=na.exclude)
  bang <- lm(log(Treatable_Mortality_Rate)~Lagged_Private_Procurement+bangladeshi_pakistani+factor(dept)+factor(year),  data=ethnicitydata, na.action=na.exclude)
  
  
  
  C1 <- coef_test(black, vcov = "CR2", cluster = ethnicitydata$dept, test = "Satterthwaite")$SE
  C2 <- coef_test(mixed, vcov = "CR2", cluster = ethnicitydata$dept, test = "Satterthwaite")$SE
  C3 <- coef_test(indian, vcov = "CR2", cluster = ethnicitydata$dept, test = "Satterthwaite")$SE
  C4 <- coef_test(bang, vcov = "CR2", cluster = ethnicitydata$dept, test = "Satterthwaite")$SE
  
  names(C1) <- names(black$coefficients)
  names(C2) <- names(mixed$coefficients)
  names(C3) <- names(indian$coefficients)
  names(C4) <- names(bang$coefficients)
  
  
  
  
  
  cm <- c("Lagged_Private_Procurement" = "For-profit Outsourcing (%)",
          "black" = "Black/Black British (%)",
          "mixed" = "Mixed ethnicity (%)",
          "indian" = "Indian (%)",
          "bangladeshi_pakistani" = "Bangladeshi/ Pakistani (%)")
  
  
  modelsummary(list("(1)"=black, "(2)" = mixed, "(3)" = indian, "(4)" = bang),
               statistic_override=list(C1, C2, C3, C4), coef_omit = "Intercept|dept|year",
               coef_map = cm,fmt=4,
               stars = T ,statistic = c("std.error", "conf.int"),
               output = "gt")%>%
    tab_spanner(label = 'ln(T. Mortality)', columns = 2:5) 
  
  
  
  
}

#Missing data plot
Create_S.21.1 <-  function(MyAnnualDataCCG) {
  
  missingdataprivate <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$Private_Sector_Procurement_Spend),]
  missingdatamort <- MyAnnualDataCCG[complete.cases(MyAnnualDataCCG$Treatable_Mortality_Rate),]
  
  plot <- ggplot(missingdatamort[which(missingdatamort$year!="2010"&missingdatamort$year!="2011"&missingdatamort$year!="2012"),],aes(x=year, y=dept))+
    geom_point()+
    theme_minimal()+
    labs(x="Year", y="CCG", title = "Treatable Mortality")
  
  plot2 <- ggplot(missingdataprivate,aes(x=year, y=dept))+
    geom_point()+
    theme_minimal()+
    labs(x="Year", y="CCG", title = "For-profit Outsourcing")
  
  yes <- cowplot::plot_grid(plot,plot2, ncol=2,labels = "AUTO")
  
  yes  
}

#model variable table
Create_S.22<- function(MyAnnualDataCCG){
  
  ####Table1 - variable description####
  table_data <- as.data.frame(MyAnnualDataCCG) %>% dplyr::select(Treatable_Mortality_Rate, Private_Sector_Procurement_Spend,total_spend_10millions, Local_Authority_Spend_per_pop,Claimant_percent, CCGpop ,Unemployment_percent ,BAME_percent,Qual_lvl4_percent ,GDHI_per_person, professional_and_managerial)
  
  names(table_data)[names(table_data)=="Treatable_Mortality_Rate"] <- "Treatable Mortality Rate"
  names(table_data)[names(table_data)=="Private_Sector_Procurement_Spend"] <- "For-Profit Outsourcing (%)"
  names(table_data)[names(table_data)=="Local_Authority_Spend_per_pop"] <- "Local Authority Spend (per Capita)"
  names(table_data)[names(table_data)=="Claimant_percent"] <- "Claimant Rate"
  names(table_data)[names(table_data)=="CCGpop"] <- "Population size"
  names(table_data)[names(table_data)=="Unemployment_percent"] <- "Unemployment Rate"
  names(table_data)[names(table_data)=="BAME_percent"] <- "Ethnic Minority (%)"
  names(table_data)[names(table_data)=="Qual_lvl4_percent"] <- "Degree Education (%)"
  names(table_data)[names(table_data)=="GDHI_per_person"] <- "Average Disposable H.hold Income"
  names(table_data)[names(table_data)=="total_spend_10millions"] <- "Total CCG Spend (£Ms)"
  names(table_data)[names(table_data)=="professional_and_managerial"] <- "Managerial or Professional Occupation (%)"
  
  #only summarise cases which will be used in model#
  table_data <- table_data[complete.cases(table_data),]
  
  #Calculate descriptive stats
  descriptivestats <- stat.desc(table_data)
  t_descriptivestats <- as.data.frame(t(descriptivestats))
  
  #manually create Source column
  t_descriptivestats$Source <- c("ONS", "Rahal & Mohan, (2022).", "Rahal & Mohan, (2022).", "MHCLG (RSX)" , "ONS (Claimant Count)", "ONS", "ONS (APS)", "ONS (APS)", "ONS (APS)", "ONS (GDHI)", "ONS (APS)")
  
  #manually Create IQR
  
  t_descriptivestats$IQR <-  c(IQR(table_data$`Treatable Mortality Rate`) ,IQR(table_data$`For-Profit Outsourcing (%)`),IQR(table_data$`Total CCG Spend (£Ms)`),IQR(table_data$`Local Authority Spend (per Capita)`),IQR(table_data$`Claimant Rate`),IQR(table_data$`Population size`),IQR(table_data$`Unemployment Rate`),IQR(table_data$`Ethnic Minority (%)`),IQR(table_data$`Degree Education (%)`),IQR(table_data$`Average Disposable H.hold Income`),IQR(table_data$`Managerial or Professional Occupation (%)`))
  
  #Create columns
  is.num <- sapply(t_descriptivestats, is.numeric)
  
  t_descriptivestats[is.num] <- lapply(t_descriptivestats[is.num], round, 2)
  
  
  t_descriptivestats$meanse <- with(t_descriptivestats,  paste0(mean ," (", SE.mean, ")")) 
  t_descriptivestats$mediqr <- with(t_descriptivestats,  paste0(median ," (", IQR, ")")) 
  
  #write table
  Table_1 <-    
    knitr::kable(t_descriptivestats[c("meanse","mediqr","Source")],
                 format = "latex", # default
                 digits = 2,        # specify decimal places
                 caption = "Study Variables",
                 col.names = c("Mean (SD)","Median (IQR)","Source"),
                 row.names = T,
                 # align = c("c","c","c","r")
                 # padding = 2     # inner spacing
    )%>%kable_styling(font_size = 3)
  
  Table_1  
}

#CCG data table
Create_S.25.1 <- function(MyAnnualDataCCG) {
  timedata <- MyAnnualDataCCG[which(MyAnnualDataCCG$year=="2013"|MyAnnualDataCCG$year=="2014"|MyAnnualDataCCG$year=="2015"|MyAnnualDataCCG$year=="2016"|MyAnnualDataCCG$year=="2017"|MyAnnualDataCCG$year=="2018"|MyAnnualDataCCG$year=="2019"|MyAnnualDataCCG$year=="2020"),]
  tabledata <-timedata[c("dept", "total_spend_10millions" ,"Total_Private_sector_spend", "treatable_mortality_deaths" )]
  tabledata2 <-timedata[c("dept", "Treatable_Mortality_Rate")]
  
  tabledata <- aggregate(.~ dept ,sum,data=tabledata, na.rm=T, na.action=NULL)
  tabledata2 <- aggregate(.~ dept ,mean,data=tabledata2, na.rm=T, na.action=NULL)
  
  tabledata$Total_Private_sector_spend <-   tabledata$Total_Private_sector_spend/10000000
  
  tabledata$outsourcing <- tabledata$Total_Private_sector_spend/tabledata$total_spend_10millions*100
  tabledata <-tabledata[c("dept", "total_spend_10millions" ,"Total_Private_sector_spend","outsourcing" ,"treatable_mortality_deaths" )]
  
  tabledata <- tabledata[which(tabledata$outsourcing>0),]
  tabledata <- merge(tabledata, tabledata2, by="dept", all.x=T)
  
  Table_1 <-    
    knitr::kable(tabledata,
                 format = "latex", # default
                 digits = 4,        # specify decimal places
                 caption = "CCG data",
                 col.names = c("CCG","Total Spend (£10Ms)","Private sector spend (£10Ms)","For-profit outsourcing (%)", "Total Treatable Deaths", "Average Treatable Mortality Rate"),
                 row.names = T,
                 # align = c("c","c","c","r")
                 # padding = 2     # inner spacing
    )%>%kable_styling(font_size = 2)
  
  Table_1
}

#CCG outsourcing stripe chart
Create_S.25.2 <- function(myDataCCG) {
  myDataCCGst <- myDataCCG
  myDataCCGst$date <- as.Date(myDataCCGst$date, format =  "%Y-%m-%d")
  
  #remove observations before April 2013
  
  myDataCCGst <- myDataCCGst[which(myDataCCGst$date>="2013-04-01"),]
  
  myDataCCGst <- myDataCCGst %>% mutate(PrivateSector = ifelse((myDataCCGst$match_type=="Companies House"&is.na(as.double(myDataCCGst$CharityRegNo)))|(!is.na(as.double(myDataCCGst$CompanyNumber))& is.na(as.double(myDataCCGst$CharityRegNo)))| (!is.na(myDataCCGst$CompanyCategory)&is.na(as.double(myDataCCGst$CharityRegNo)))|(myDataCCGst$audit_type=="3"&myDataCCGst$match_type=="No Match"&!is.na(as.double(myDataCCGst$CompanyNumber))),1,0))
  
  
  plotdata <- myDataCCGst[c("date", "dept","amount","PrivateSector")]
  
  plotdata$month <- format(plotdata$date,"%m/%y")
  
  plotdataPriv <- plotdata[which(plotdata$PrivateSector==1),]
  names(plotdataPriv)[names(plotdataPriv)=="amount"] <- "private_spend"
  
  plotdata <- aggregate(. ~month+dept, data=plotdata, sum)
  plotdataPriv <- aggregate(. ~month+dept, data=plotdataPriv, sum)
  
  plotdata <- merge(plotdata[c(1,2,4)], plotdataPriv[c(1,2,4)], by=c("dept","month"), all=T)
  
  library(lubridate)
  plotdata$month <- as.character(plotdata$month)
  plotdata$month <- my(plotdata$month)
  
  plotdata$outsourcing <- plotdata$private_spend/plotdata$amount*100
  
  
  
  theme_strip <- theme_minimal()+
    theme(axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
    )
  
  library(RColorBrewer)
  col_strip <- brewer.pal(11, "RdBu")
  
  plotdata2 <- plotdata
  
  plotdata2$outsourcing[is.na(plotdata2$outsourcing)] <- 0
  plotdata2$outsourcing[which(plotdata2$outsourcing>10)] <- 10
  plotdata2$outsourcing[which(plotdata2$outsourcing<0)] <- 0
  
  
  warmingstripes <- ggplot(plotdata2,
                           aes(x = month, y = 1, fill = outsourcing))+
    geom_tile()+
    scale_x_date(limits = as.Date(c("2013-07-01","2020-02-29")))+
    scale_y_continuous(expand = c(0, 0))+
    scale_fill_gradientn(colors = rev(col_strip), name = "For-profit\nOutsourcing (%)", 
                         breaks=c(0,2,4,6,8, 10),labels=c("0 (or less)",2,4,6,8,"10+"),
                         limits=c(0,10))+
    guides(fill = guide_colorbar(barwidth = 1))+
    theme_strip+
    labs(x="")+theme(text=element_text(size=7), legend.key.size = unit(0.4, "cm"))+
    facet_wrap(~dept, ncol=5)
  warmingstripes
  
}



































