# =======================================================================
# Title: payanalysis.R
# Author: Sofia Guo
# Date: 11/9/23
# Description: Script to analyze the CA wage data from publicpay.ca.org
# =======================================================================
#load libraries
library(dplyr)
library(plyr)
library(readxl)
library(stringr)
library(magrittr)
library(ggplot2)
library(janitor)
library(reshape)
library(forcats)
library(scales)
library(writexl)

#set wd
setwd("/Users/sofia/Library/Mobile Documents/com~apple~CloudDocs/Box Backup/QP/Data/Wages/CA")

#read in the county data
county_22 <- read.csv("2022_County.csv")

#read in the city data (since SF, SC is counted as a city)
city_22 <- read.csv("2022_City.csv")
SF <- subset(read.csv("2022_City.csv"), EmployerCounty == "San Francisco")

#append county_22 with SF data
all_counties <- rbind(county_22, SF) %>%
  dplyr::select(EmployerCounty, DepartmentOrSubdivision, Position, MinPositionSalary, MaxPositionSalary, RegularPay, OvertimePay, LumpSumPay, TotalWages, TotalRetirementAndHealthContribution) %>%
  dplyr::filter(TotalWages >=MinPositionSalary)

#factorize EmployerCounty
all_counties$EmployerCounty <- factor(all_counties$EmployerCounty)

#read in the DepartmentOrSubdivision names for the top 18 counties (missing Santa Clara and Contra Costa)
depts <- as.data.frame(read_excel("/Users/sofia/Library/Mobile Documents/com~apple~CloudDocs/Box Backup/QP/Data/Wages/exploratoryanalysis.xlsx", 
                                  sheet = "allcounties"))

# #read in position key
# poskey <- array(unlist(read_excel("/Users/sofia/Library/Mobile Documents/com~apple~CloudDocs/Box Backup/QP/Data/Wages/exploratoryanalysis.xlsx", sheet = "positionkey")[,1]))

# #list of departments (removing NA's)
# fulldepts <- na.omit(as.data.frame(read_excel("/Users/sofia/Library/Mobile Documents/com~apple~CloudDocs/Box Backup/QP/Data/Wages/exploratoryanalysis.xlsx", sheet = "crosswalk"))[,2])[1:54]

#factorize the child pop size
depts$ChildPopSize <- factor(depts$ChildPopSize , levels=c("Large", "Medium", "Small"))

#merge the depts and the original dataset
joined <- as.data.frame(full_join(all_counties, depts, by = "EmployerCounty")) 

# for diagnosing missing counties
# join_count <- unique(joined$EmployerCounty)
# rel_count <- unique(relevantcounties$EmployerCounty)
# missing_elements <- join_count[!join_count %in% rel_count]
# print(missing_elements)

#filter for related department and positions
relevantcounties <- joined %>%
  dplyr::filter(DepartmentOrSubdivision == "Children & Family Services" |
                  DepartmentOrSubdivision == "Health & Human Serv Agency" |
                  DepartmentOrSubdivision == "Social Services Agency" |
                  DepartmentOrSubdivision == "Dept. Of Public Social Services" |
                  DepartmentOrSubdivision == "Human Services" |
                  DepartmentOrSubdivision == "Child, Family, And Adult Services" |
                  DepartmentOrSubdivision == "Children And Family Services" |
                  DepartmentOrSubdivision == "Department of Social Services" |
                  DepartmentOrSubdivision == "Hsa - Services Staff" |
                  DepartmentOrSubdivision == "Human Services Agency" |
                  DepartmentOrSubdivision == "Child Welfare Services" |
                  DepartmentOrSubdivision == "Community Services Agency" |
                  DepartmentOrSubdivision == "Health And Human Services" |
                  DepartmentOrSubdivision == "Social Services" |
                  DepartmentOrSubdivision == "HSSD/Child Welfare Services" |
                  DepartmentOrSubdivision == "Human Services Department" |
                  DepartmentOrSubdivision == "Human Services-Social Services" |
                  DepartmentOrSubdivision == "Health and Human Services Department" |
                  DepartmentOrSubdivision == "Health Services Agency" |
                  DepartmentOrSubdivision == "HHSA" |
                  DepartmentOrSubdivision == "Department of Social Services" |
                  DepartmentOrSubdivision == "Employment and Social Services" |
                  DepartmentOrSubdivision == "Hhs Child Social Serv" |
                  DepartmentOrSubdivision == "Social Services & Ben Admin" |
                  DepartmentOrSubdivision == "Ss Social Service Programs" |
                  DepartmentOrSubdivision == "Child Welfare Services" |
                  DepartmentOrSubdivision == "Hhsa - Child Welfare Services" |
                  DepartmentOrSubdivision == "Welfare Administration" |
                  DepartmentOrSubdivision == "HHS APS And CPS" |
                  DepartmentOrSubdivision == "Department Social Services" |
                  DepartmentOrSubdivision == "Social Services Administration" |
                  DepartmentOrSubdivision == "Health And Human Services/Cps Emer Resp" |
                  DepartmentOrSubdivision == "Social Services Admin" |
                  DepartmentOrSubdivision == "Department Of Social Services" |
                  DepartmentOrSubdivision == "Human Services Administration" |
                  DepartmentOrSubdivision == "Calworks & Human Services" |
                  DepartmentOrSubdivision == "Welfare" |
                  DepartmentOrSubdivision == "H&Hs" |
                  DepartmentOrSubdivision == "Hhs" |
                  DepartmentOrSubdivision == "Health & Human Services" |
                  DepartmentOrSubdivision == "Health And Human Services Agency" |
                  DepartmentOrSubdivision == "Health and Social Services" |
                  DepartmentOrSubdivision == "Health And Social Services Agency" |
                  DepartmentOrSubdivision == "Health and Human Service Agency" |
                  DepartmentOrSubdivision == "Health & Human Services Agency" |
                  DepartmentOrSubdivision == "Hsa - Health Services Agency" |
                  DepartmentOrSubdivision == "Hsa Health Services Agency" |
                  DepartmentOrSubdivision == "Hhsa - Health & Human Services Agency" |
                  DepartmentOrSubdivision == "Hhsa Health and Human Services Agency" |
                  DepartmentOrSubdivision == "Hhsa - Health and Social Services Agency" |
                  DepartmentOrSubdivision == "Human Services Agency" |
                  DepartmentOrSubdivision == "Human Services-Social Service" |
                  DepartmentOrSubdivision == "Human Services - Health Services Agency" |
                  DepartmentOrSubdivision == "Social Services-Human Services Agency" |
                  DepartmentOrSubdivision == "Human Services-Health Services Agency"
  ) %>%
  dplyr::filter(Position == "Children'S Social Worker II" |
                  Position == "Children'S Social Worker III" |
                  Position == "Protective Services Worker" |
                  Position == "Sr. Social Worker" |
                  Position == "Social Services Practitioner III" |
                  Position == "Social Service Practitionr III" |
                  Position == "Human Svcs Soc Wkr Mstr Dgr" |
                  Position == "Child Welfare Worker II" |
                  Position == "Social Worker" |
                  Position == "Social Service Worker IV" |
                  Position == "Social Service Worker V" |
                  Position == "Social Worker III" |
                  Position == "Social Worker IV" |
                  Position == "Hs Child Welfare Social Worker II" |
                  Position == "Hs Child Welfare Social Worker III" |
                  Position == "Hs Child Welfare Social Worker IV" |
                  Position == "Children's Services Social Worker I" |
                  Position == "Children's Services Social Worker II" |
                  Position == "Children's Services Social Worker III" |
                  Position == "Social Worker V" |
                  Position == "Social Services Worker III-Cws" |
                  Position == "Social Services Practitioner" |
                  Position == "Social Worker II" |
                  Position == "Social Worker III" |
                  Position == "Child Protective Services Social Worker" |
                  Position == "Social Worker IV-A" |
                  Position == "Social Worker IV-B" |
                  Position == "Client Services Practitioner I" |
                  Position == "Client Services Practitioner II" |
                  Position == "Senior Social Worker" |
                  Position == "Social Worker Practitioner" |
                  Position == "Social Worker, Senior" |
                  Position == "Social Service Worker III-Cps" |
                  Position == "Child Welfare Worker II" |
                  Position == "Social Worker IV MSS" |
                  Position == "Social Worker IV A 40 Hours" |
                  Position == "Social Worker IV B 40 Hours" |
                  Position == "Social Worker IV C 40 Hours" |
                  Position == "Social Worker IV D 40 Hours" |
                  Position == "Child Protective Srvcs Wkr I" |
                  Position == "Child Protective Srvcs Wkr II" |
                  Position == "Social Worker Child Services IV" |
                  Position == "Social Worker III - Master's Level" |
                  Position == "Social Worker IV(B)" |
                  Position == "Social Worker IV - Cps" |
                  Position == "Social Worker IV - A" |
                  Position == "Social Worker II - IVa/Aps/Cps" |
                  Position == "Social Worker III &" |
                  Position == "Social Worker IV L1" |
                  Position == "Social Worker I" |
                  Position == "Social Worker IV" |
                  Position == "Integ Case Worker III" |
                  Position == "Sw III/Dep Pg" |
                  Position == "7205 Social Worker IV" |
                  Position == "Client Services Practitioner - II" |
                  Position == "Client Services Practitioner - I")

#remove NA rows for childpopsize
#add computed variables of interest
relevantcounties <- relevantcounties[!(is.na(relevantcounties$ChildPopSize)), ] %>%
  dplyr::mutate(class = fct_reorder(EmployerCounty, TotalWages, .fun='median'))%>%
  group_by(EmployerCounty) %>%
  dplyr::mutate(TotalWorkers = n(),
                Caseload = round(CCInvest/TotalWorkers),
                MedWage = median(TotalWages),
                LWratio = 100*(TotalWages/AnnualizedLW),
                InvAlleg = 100*(CCInvest/CCAlleg),
                SubInv = 100*(CCSub/CCInvest))%>%
  dplyr::mutate(workmean = mean(relevantcounties$TotalWorkers),
                worksd = sd(relevantcounties$TotalWorkers),
                scaledworker = (TotalWorkers - workmean)/worksd,
                casemean = mean(relevantcounties$Caseload),
                casesd = sd(relevantcounties$Caseload),
                scaledcase = (Caseload - casemean)/casesd,
                invmean = mean(relevantcounties$InvAlleg),
                invsd = sd(relevantcounties$InvAlleg),
                scaledinv= (InvAlleg-invmean)/invsd,
                submean = mean(relevantcounties$SubInv),
                subsd = sd(relevantcounties$SubInv),
                scaledsub = (SubInv-submean)/subsd,
                povmean=mean(relevantcounties$ChildPovRt),
                povsd = sd(relevantcounties$ChildPovRt),
                scaledpov=(ChildPovRt-povmean)/povsd,
                bidmean=mean(relevantcounties$PercBiden),
                bidsd=sd(relevantcounties$PercBiden),
                scaledbid=(PercBiden-bidmean)/bidsd)

#rescale poverty and biden
relevantcounties$ChildPovRt <- 100*relevantcounties$ChildPovRt
relevantcounties$PercBiden <- 100*relevantcounties$PercBiden
relevantcounties$AllegRt <- 100*relevantcounties$AllegRt

#write into xlsx
write_xlsx(relevantcounties, path = "/Users/sofia/Library/Mobile Documents/com~apple~CloudDocs/Box Backup/QP/Data/Wages/CA/relevantcounties.xlsx")

#========================================LIVING WAGE X InvAlleg and SubInv graphs======================
#======================================================================================================

# InvAlleg
wages_lwratio_invalleg <- ggplot(relevantcounties, aes(LWratio, fct_reorder(EmployerCounty, LWratio, .fun='median'), group = InvAlleg, color = InvAlleg)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  geom_vline(xintercept = 1, linetype = "dotted")+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "(Total wage/annualized living wage estimates)", y="County",
       title= "Ratios of total wage to living wage estimates (from MIT) - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(ChildPopSize~Region, scales = "free_y", dir = "v")+
  #facet_grid(ChildPopSize~CAlocation, scales = "free_y", switch = "both") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks = seq(0,2,0.2), limits = range(0,2))+
  scale_color_gradient2(midpoint = mean(relevantcounties$`InvAlleg`), low = "blue", mid = "orange", high = "red", space = "Lab" )

wages_lwratio_invalleg

#======================================================================================================

#SubInv
wages_lwratio_subinv <- ggplot(relevantcounties, aes(LWratio, fct_reorder(EmployerCounty, LWratio, .fun='median'), group = SubInv, color = SubInv)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  geom_vline(xintercept = 1, linetype = "dotted")+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "(Total wage/annualized living wage estimates)", y="County",
       title= "Ratios of total wage to living wage estimates (from MIT) - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(ChildPopSize~., scales = "free_y", dir = "v")+
  #facet_grid(ChildPopSize~CAlocation, scales = "free_y", switch = "both") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks = seq(0,2,0.1), limits = range(0,2))+
  scale_color_gradient2(midpoint = mean(relevantcounties$`SubInv`), low = "blue", mid = "orange", high = "red", space = "Lab" )

wages_lwratio_subinv

#========================================LIVING WAGE GRAPHS============================================
#======================================================================================================

#living wage x childpov estimates ratio
wages_lwratio <- ggplot(relevantcounties, aes(LWratio, fct_reorder(EmployerCounty, LWratio, .fun='median'), group = ChildPovRt, color = ChildPovRt)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  geom_vline(xintercept = 1, linetype = "dotted")+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "(Total wage/annualized living wage estimates)", y="County",
       title= "Ratios of total wage to living wage estimates (from MIT) - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(ChildPopSize~., scales = "free_y", dir = "v")+
  #facet_grid(ChildPopSize~CAlocation, scales = "free_y", switch = "both") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks = seq(0,2,0.1), limits = range(0,2))+
  scale_color_gradient2(midpoint = mean(relevantcounties$`ChildPovRt`), low = "blue", mid = "orange", high = "red", space = "Lab" )

wages_lwratio

#======================================================================================================

#living wage estimates ratio - biden
wages_lwratio_biden <- ggplot(relevantcounties, aes(LWratio, fct_reorder(EmployerCounty, LWratio, .fun='median'), group = PercBiden, color = PercBiden)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  geom_vline(xintercept = 1, linetype = "dotted")+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "(Total wage/annualized living wage estimates)", y="County",
       title= "Ratios of total wage to living wage estimates (from MIT) - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(ChildPopSize~., scales = "free_y", dir = "v")+
  #facet_grid(ChildPopSize~CAlocation, scales = "free_y", switch = "both") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks = seq(0,2,0.1), limits = range(0,2))+
  scale_color_gradient2(midpoint = mean(relevantcounties$`PercBiden`), low = "red", mid = "pink", high = "blue", space = "Lab" )

wages_lwratio_biden


#======================================================================================================

#living wage estimates ratio - allegrt
wages_lwratio_alleg <- ggplot(relevantcounties, aes(LWratio, fct_reorder(EmployerCounty, LWratio, .fun='median'), group = AllegRt, color = AllegRt)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  geom_vline(xintercept = 1, linetype = "dotted")+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "(Total wage/annualized living wage estimates)", y="County",
       title= "Ratios of total wage to living wage estimates (from MIT) - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(ChildPopSize~., scales = "free_y", dir = "v")+
  #facet_grid(ChildPopSize~CAlocation, scales = "free_y", switch = "both") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks = seq(0,2,0.1), limits = range(0,2))+
  scale_color_gradient2(midpoint = mean(relevantcounties$`AllegRt`), low = "blue", mid = "orange", high = "red", space = "Lab" )

wages_lwratio_alleg

#======================================================================================================

#living wage estimates ratio - entry
wages_lwratio_entry <- ggplot(relevantcounties, aes(LWratio, fct_reorder(EmployerCounty, LWratio, .fun='median'), group = EntryRt, color = EntryRt)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  geom_vline(xintercept = 1, linetype = "dotted")+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "(Total wage/annualized living wage estimates)", y="County",
       title= "Ratios of total wage to living wage estimates (from MIT) - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(ChildPopSize~., scales = "free_y", dir = "v")+
  #facet_grid(ChildPopSize~CAlocation, scales = "free_y", switch = "both") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks = seq(0,2,0.1), limits = range(0,2))+
  scale_color_gradient2(midpoint = mean(relevantcounties$`EntryRt`), low = "blue", mid = "orange", high = "red", space = "Lab" )

wages_lwratio_entry


#======================================================================================================

#table of county estimates of LWratio, AnnualizedLW, 

BA <- relevantcounties %>%
  dplyr::filter(Region == "Bay Area") %>%
  dplyr::group_by(EmployerCounty) %>%
  dplyr::summarize(Count = n())

dt_lw <- relevantcounties %>%
  dplyr::filter(Region == "Bay Area") %>%
  dplyr::group_by(EmployerCounty, Position) %>%
  dplyr::summarize(Count = n(),
                   AvgTotWage = mean(TotalWages),
                   MedTotWage = median(TotalWages),
                   AvgLW = mean(AnnualizedLW),
                   AvgLWratio = mean(LWratio),
                   MedLWratio = median(LWratio),
                   AvgInvAlleg = mean(InvAlleg),
                   AvgSubInv = mean(SubInv))
#write into file
write_xlsx(dt_lw, path = "/Users/sofia/Library/Mobile Documents/com~apple~CloudDocs/Box Backup/QP/Data/Wages/CA/livingwagesummarytable.xlsx")
#======================================================================================================

#living wage estimates
wages_lw <- ggplot(relevantcounties, aes(TotalWages, class, group = AnnualizedLW, color = AnnualizedLW)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "Total Wages (dollars)", y="County",
       title= "Comparison of total wages by child population size, opioid overdose death rate per 100k people, and county - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  #facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  facet_grid(.~ChildPopSize, scales = "free") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::dollar_format(),breaks = seq(0,300000,50000)) +
  scale_color_gradient2(midpoint = mean(relevantcounties$`AnnualizedLW`), low = "green", mid = "orange",
                        high = "red", space = "Lab" )

wages_lw

#======================================================================================================
#poverty rate x region x childpopsize

wages_pov_loc <- ggplot(relevantcounties, aes(TotalWages, class, group = ChildPovRt, color = ChildPovRt)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "Total Wages (dollars)", y="County",
       title= "Comparison of total wages by child population size, child poverty rate, and county - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  #facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  facet_grid(ChildPopSize~Region, scales = "free_y") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::dollar_format(),breaks = seq(0,300000,50000)) +
  scale_color_gradient2(midpoint = mean(relevantcounties$`ChildPovRt`), low = "blue", mid = "orange", high = "red", space = "Lab" )

wages_pov_loc
#=====================================================================================================

#percbiden x ca location x childpopsize

wages_biden_loc <- ggplot(relevantcounties, aes(TotalWages, class, group = PercBiden, color = PercBiden)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "Total Wages (dollars)", y="County",
       title= "Comparison of total wages by child population size, percent voting Biden, and county - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  #facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  facet_grid(ChildPopSize~Region, scales = "free_y") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::dollar_format(),breaks = seq(0,300000,50000)) +
  scale_color_gradient2(midpoint = mean(relevantcounties$`PercBiden`), low = "red", mid = "orange",
                        high = "blue", space = "Lab" )

wages_biden_loc

#=====================================================================================================

#allegrt x ca location x size

wages_alleg_loc <- ggplot(relevantcounties, aes(TotalWages, class, group = AllegRt, color = AllegRt)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "Total Wages (dollars)", y="County",
       title= "Comparison of total wages by child population size, allegation proportion, and county - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  #facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  facet_grid(ChildPopSize~Region, scales = "free_y") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::dollar_format(),breaks = seq(0,300000,50000)) +
  scale_color_gradient2(midpoint = mean(relevantcounties$`AllegRt`), low = "blue", mid = "orange",
                        high = "red", space = "Lab" )

wages_alleg_loc


#=====================================================================================================

#allegrt x ca location x size

wages_un_loc <- ggplot(relevantcounties, aes(TotalWages, class, group = Unemployment, color = Unemployment)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "Total Wages (dollars)", y="County",
       title= "Comparison of total wages by child population size, unemployment rates, and county - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  #facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  facet_grid(ChildPopSize~CAlocation, scales = "free_y") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::dollar_format(),breaks = seq(0,300000,50000)) +
  scale_color_gradient2(midpoint = mean(relevantcounties$`Unemployment`), low = "blue", mid = "orange",
                        high = "red", space = "Lab" )

wages_un_loc

##=====================================================================================================

#percent bideon and allegation ratio scatter

ggplot(depts, aes(x = ChildPovRt, y= PercBiden)) +
  geom_point(aes(color = AllegRt, group = AllegRt)) +
  geom_smooth(method = "lm") +
  labs(x = "Ratio of children in poverty", y="Percent voting Biden",
       title= "Relationship between child poverty ratio and percent voting Biden - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  #facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  #facet_grid(ChildPopSize~CAlocation, scales = "free_y") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1)) +
  scale_color_gradient2(midpoint = mean(depts$`AllegRt`), low = "blue", mid = "orange",
                        high = "red", space = "Lab" )

##=====================================================================================================

#poverty x unemployment x alleg

ggplot(depts, aes(x = ChildPovRt, y= Unemployment)) +
  geom_point(aes(color = AllegRt, group = AllegRt)) +
  geom_smooth(method = "lm") +
  labs(x = "Child poverty ratio", y="Unemployment rate",
       title= "Relationship between child poverty ratio and unemployment rate - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  #facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  #facet_grid(ChildPopSize~CAlocation, scales = "free_y") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1)) +
  scale_color_gradient2(midpoint = mean(depts$`AllegRt`), low = "blue", mid = "orange",
                        high = "red", space = "Lab" )


##=====================================================================================================

#medianwage x unemployment x alleg

ggplot(relevantcounties, aes(x = log(MedWage), y= AllegRt)) +
  geom_point(aes(color = ChildPovRt, group = ChildPovRt)) +
  geom_smooth(method = "lm") +
  labs(x = "log(median wage)", y="Allegation ratio",
       title= "Relationship between log(median wage) and allegation ratio - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  #facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  #facet_grid(ChildPopSize~CAlocation, scales = "free_y") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1)) +
  scale_color_gradient2(midpoint = mean(depts$`ChildPovRt`), low = "blue", mid = "orange",
                        high = "red", space = "Lab" )


##=====================================================================================================
## RAW WAGE INITIAL GRAPHS
#======================================================================================================
#visualize by county
#boxplot plot of wages
wages_percbiden <- ggplot(relevantcounties, aes(TotalWages, class, group = `PercBiden`, color = `PercBiden`)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "Total Wages (dollars)", y="County",
       title= "Comparison of total wages by child population size, percent voted for Biden in 2020, and county - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  #facet_grid(.~ChildPopSize, scales = "free") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::dollar_format(),breaks = seq(0,300000,50000)) +
  scale_y_discrete() +
  scale_color_gradient2(midpoint = mean(relevantcounties$PercBiden), low = "red", mid = "orange",
                        high = "blue", space = "Lab" )

wages_percbiden
#======================================================================================================

#wages and allegation ratio
wages_alleg <- ggplot(relevantcounties, aes(TotalWages, class, group = AllegRt, color = AllegRt)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "Total Wages (dollars)", y="County",
       title= "Comparison of total wages by child population size, allegation proportion, and county - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  #facet_grid(.~ChildPopSize, scales = "free") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::dollar_format(),breaks = seq(0,300000,50000)) +
  scale_color_gradient2(midpoint = mean(relevantcounties$AllegRt), low = "blue", mid = "orange",
                        high = "red", space = "Lab" )

wages_alleg


#======================================================================================================

#wages and sub ratio
wages_sub <- ggplot(relevantcounties, aes(TotalWages, class, group = SubRt, color = SubRt)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "Total Wages (dollars)", y="County",
       title= "Comparison of total wages by child population size, substantiation rate, and county - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  #facet_grid(.~ChildPopSize, scales = "free") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::dollar_format(),breaks = seq(0,300000,50000)) +
  scale_color_gradient2(midpoint = mean(relevantcounties$SubRt), low = "blue", mid = "orange",
                        high = "red", space = "Lab" )

wages_sub

#======================================================================================================

#wages and invest ratio
wages_inv <- ggplot(relevantcounties, aes(TotalWages, class, group = InvestRt, color = InvestRt)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "Total Wages (dollars)", y="County",
       title= "Comparison of total wages by child population size, substantiation rate, and county - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  #facet_grid(.~ChildPopSize, scales = "free") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::dollar_format(),breaks = seq(0,300000,50000)) +
  scale_color_gradient2(midpoint = mean(relevantcounties$InvestRt), low = "blue", mid = "orange",
                        high = "red", space = "Lab" )

wages_inv
#======================================================================================================

#wages and entry ratio
wages_ent <- ggplot(relevantcounties, aes(TotalWages, class, group = EntryRt, color = EntryRt)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5)+
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "Total Wages (dollars)", y="County",
       title= "Comparison of total wages by child population size, substantiation rate, and county - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  #facet_grid(.~ChildPopSize, scales = "free") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::dollar_format(),breaks = seq(0,300000,50000)) +
  scale_color_gradient2(midpoint = mean(relevantcounties$EntryRt), low = "blue", mid = "orange",
                        high = "red", space = "Lab" )

wages_ent

#======================================================================================================

#child poverty
wages_poverty <- ggplot(relevantcounties, aes(TotalWages, class, group = ChildPovRt, color = ChildPovRt)) +
  geom_boxplot(show.legend = T,
               outlier.size = 0.3,
               outlier.alpha = 0.5) + 
  #geom_violin(width=1.4)+
  #geom_jitter(color="grey", size=0.2, alpha=0.3) +
  labs(x = "Total Wages (dollars)", y="County",
       title= "Comparison of total wages by child population size, child poverty rate, and county - 2022",
       caption = "Wage data from publicpay.ca.gov; voting data from politico.com ; top 20 counties w/most children excluding Santa Clara, Contra Costa due to missing data.")+
  facet_wrap(.~ChildPopSize, dir = "v", scales = "free_y")+
  #facet_grid(.~ChildPopSize, scales = "free") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size = .1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::dollar_format(),breaks = seq(0,300000,50000)) +
  scale_color_gradient2(midpoint = mean(relevantcounties$`ChildPovRt`), low = "green", mid = "orange",
                        high = "red", space = "Lab" )

wages_poverty

