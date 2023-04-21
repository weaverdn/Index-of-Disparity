library(tidyverse)

# Load Data:

# Census baseline:
censusFULL <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Census Data - Raw/Finished Census 2016-2001/20220918completecensusprofile.csv")

# 2016 Census:
census16 <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Census Data - Raw/Finished Census 2016-2001/2016censusWide2.csv",
                     fileEncoding="latin1")
#National:
disp_indexc <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Disparity Index - Analysis and Writeup/April 2023 (Final) Output/NationalCSVOutput/Disparity_Index_ca.csv")
pop_indexc <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Disparity Index - Analysis and Writeup/April 2023 (Final) Output/NationalCSVOutput/PopulationDecline_ca.csv")
pdr_indexc <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Disparity Index - Analysis and Writeup/April 2023 (Final) Output/NationalCSVOutput/PopDepRatio_ca.csv")
lab_indexc <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Disparity Index - Analysis and Writeup/April 2023 (Final) Output/NationalCSVOutput/LabourDecline_ca.csv")
ind_indexc <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Disparity Index - Analysis and Writeup/April 2023 (Final) Output/NationalCSVOutput/IndustryComp_ca.csv")

#Provincial:
disp_indexp <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Disparity Index - Analysis and Writeup/April 2023 (Final) Output/ProvincialCSVOutput/Disparity_Index_pv.csv")
pop_indexp <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Disparity Index - Analysis and Writeup/April 2023 (Final) Output/ProvincialCSVOutput/PopulationDecline_pv.csv")
pdr_indexp <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Disparity Index - Analysis and Writeup/April 2023 (Final) Output/ProvincialCSVOutput/PopDepRatio_pv.csv")
lab_indexp <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Disparity Index - Analysis and Writeup/April 2023 (Final) Output/ProvincialCSVOutput/LabourDecline_pv.csv")
ind_indexp <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Disparity Index - Analysis and Writeup/April 2023 (Final) Output/ProvincialCSVOutput/IndustryComp_pv.csv")

# Remoteness:
rem <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Remoteness Index - Raw/Merged_Remoteness.csv")


# Let's figure out what percentile 28.7% Of Dominant Industry is among the CSDs in each year:
industry_scores <- ind_indexc %>%
  pivot_longer(cols=c(X2001Share, X2006Share, 
                      X2011Share, X2016Share),
               names_to="Year",
               values_to="Share") %>%
  select(csdcode, Year, Share) %>%
  mutate(Year = case_when(grepl("2001",Year) ~ 2001,
                          grepl("2006",Year) ~ 2006,
                          grepl("2011",Year) ~ 2011,
                          TRUE ~ 2016)) %>%
  left_join(ind_indexc %>% 
              pivot_longer(cols=c(X2001Dominant, X2006Dominant, 
                                  X2011Dominant, X2016Dominant),
                           names_to="Year",
                           values_to="Dominant") %>% 
              select(csdcode, Year, Dominant) %>%
              mutate(Year = case_when(grepl("2001",Year) ~ 2001,
                                      grepl("2006",Year) ~ 2006,
                                      grepl("2011",Year) ~ 2011,
                                      TRUE ~ 2016)),
            by = c("csdcode", "Year"))


rank_industry_year <- industry_scores %>%
  filter(!is.na(Share)) %>%
  mutate(Dominant = ifelse(Share >= (2/7), 1,0)) %>%
  group_by(Year) %>%
  summarize(Pct_below_28.7 = sum(Dominant)/n())

# Analysis of First Nations Communities:
cwi16 <- read.csv("/Users/DNW/Dropbox/Rural Data Project - RA/Census Data - Raw/Community Wellbeing Index/CWI_2016.csv")
fn_csd <- cwi16 %>%
  filter(type %in% c("First Nations Community / Communaut\xe9 des Premi\xe8res Nations",
                     "Inuit Community / Communaut\xe9 inuite")) %>%
  select(csdcode, pop16)

# Missing Communities:
missing_csd <- census16 %>%
  select(csdcode) %>%
  anti_join(disp_indexc %>% 
              filter(!is.na(DisparityScore)) %>%
              select(csdcode), by="csdcode")


# Missing FN communities
missing_csd_by_FN <- missing_csd %>% 
  left_join(fn_csd %>% 
              mutate(FN="FN") %>%
              select(csdcode, FN),
            by="csdcode") %>%
  group_by(FN) %>%
  summarize(n())

# Average population of FN communities with Index scores:
disp_indexc %>% 
  filter(!is.na(DisparityScore)) %>%
  filter(csdcode %in% fn_csd$csdcode) %>%
  select(csdcode) %>%
  left_join(censusFULL %>% select(csdcode, X2016.1.id.Population..2016) %>%
              mutate(pop16 = as.numeric(X2016.1.id.Population..2016)),
            by= "csdcode") %>%
  summarize(mean(pop16,na.rm=TRUE))

# 407 (47%) of the 861 missing CSDs are FN communities.

# Pop Dependency for FN:
pdr_analysis <- censusFULL %>%
  select(csdcode,
         youth = `X2016.9.id.0.to.14.years`,
         working = `X2016.13.id.15.to.64.years`,
         elder = `X2016.24.id.65.years.and.over`) %>%
  mutate(youth = as.numeric(youth),
         working = as.numeric(working),
         elder = as.numeric(elder)) %>%
  mutate(dependent = rowSums({.} %>% select(youth, working), na.rm=TRUE)) %>%
  left_join(fn_csd %>%
              mutate(FN = "FN") %>%
              select(csdcode, FN),
            by="csdcode") %>%
  mutate(PDR = (dependent/working),
         youth_over_elderly = (youth/elder)) %>%
  filter(!is.infinite(youth_over_elderly)) %>%
  group_by(FN) %>%
  summarize(mean(PDR, na.rm=TRUE), mean(youth_over_elderly, na.rm=TRUE))

# NHS Sensitivity
ind_2011_loss <- ind_indexc %>%
  select(csdcode, X2001Dominant, X2006Dominant,
         X2011Dominant, X2016Dominant) %>%
  pivot_longer(cols=c(X2001Dominant, X2006Dominant,
                      X2011Dominant, X2016Dominant),
               names_to="year") %>%
  group_by(year) %>%
  summarize(csds = sum(value != ""))

lab_2011_loss <- lab_indexc %>%
  select(csdcode, ue2001, ue2006, ue2011, ue2016) %>%
  mutate(ue2001=as.character(ue2001),
         ue2006=as.character(ue2006),
         ue2011=as.character(ue2011),
         ue2016=as.character(ue2016)) %>%
  pivot_longer(cols=c(ue2001, ue2006, ue2011, ue2016),
               names_to="year") %>%
  mutate(value = as.numeric(value)) %>%
  group_by(year) %>%
  summarize(csds = sum(!is.na(value)),na.rm=TRUE)

# Total CSDs by Province:
prov_csds <- census16 %>%
  select(csdcode) %>%
  mutate(Prov = substr(csdcode,1,2)) %>%
  group_by(Prov) %>%
  summarize(total=n())

total_csds16 <- census16 %>%
  select(csdcode) %>%
  summarize(CSDs = n_distinct(csdcode))

total_csdsFULL <- censusFULL %>% 
  select(csdcode) %>%
  summarize(CSDs = n_distinct(csdcode))

total_csdsIndexR <- n_distinct((rem %>% filter(!is.na(Most.Recent.Remoteness)))$csdcode)

total_csdsIndexDandR <- censusFULL %>% 
  left_join(rem %>%
              filter(!is.na(Most.Recent.Remoteness)),
            by="csdcode") %>%
  summarize(n_distinct(csdcode))

# CSD in 2016 with at least one other period:
total_csd_computed <- disp_indexc %>%
  summarize(n_distinct(csdcode))

# CSD with Index Score
total_csd_finalindex <- disp_indexc %>%
  filter(!is.na(DisparityScore)) %>%
  summarize(n_distinct(csdcode))

# Missing CSDs by Province:
missing_csd_prov <- missing_csd %>%
  mutate(Prov = substr(csdcode,1,2)) %>%
  group_by(Prov) %>%
  summarize(missing=n())

# Percent of Province Missing:
pct_missing_prov <- prov_csds %>%
  left_join(missing_csd_prov, by="Prov") %>%
  mutate(pct = scales::percent(missing/total))

# Missing because of Change in CSD Boundary: New to Census in 2016
new_csd <- census16 %>%
  select(csdcode) %>%
  anti_join(censusFULL,by="csdcode") %>%
  mutate(Prov = substr(csdcode, 1,2))

length(new_csd$csdcode)

# Missing because of insufficient data:
dropped_csd <- censusFULL %>%
  select(csdcode) %>%
  anti_join(disp_indexc %>%
              filter(!is.na(DisparityScore)),by="csdcode") %>%
  mutate(Prov = substr(csdcode,1,2))

length(dropped_csd$csdcode)

# Total Misisng CSD:
total_missing <-  bind_rows(new_csd, dropped_csd)

length(total_missing$csdcode)

# Breakdown of Missing type by Province:
dropped_csd %>%
  group_by(Prov) %>%
  summarize(new = n())

new_csd %>%
  group_by(Prov) %>%
  summarize(dropped=n())


