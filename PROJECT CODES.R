#Group 4 Project Script
#Codes by: Shannaya Raul
install.packages("tidyverse")
install.packages('Synth')
install.packages('devtools')
install_github("bcastanho/SCtools")
install.packages("haven")
install.packages("modelsummary")
install.packages("gt")
install.packages("gtable")
install.packages("gtsummary")
install.packages("ggplot")
install.packages("dagitty")
install.packages("ggdist")
install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggdist)
library(dagitty)
library(ggplot2)
library(dplyr)
library(SCtools)
library(haven)
library(tidyverse)
library(devtools)
library(Synth)
library(modelsummary)
library(gt)
library(gtable)
library(gtsummary)
library(ggplot2)
library(tidyverse)
library(dplyr)
getwd()
setwd("C:/Users/shann/OneDrive/Documents/MIA PROGRAM/1st Y-SPRING/Quant II- Bush 635/Project/Data")
getwd()

WDI_Data <- read.csv("WDI_popdata.csv")
GDP_Data <- read.csv("GDP.csv")
Agriculture_Data <- read.csv("Agriculture.csv")
Manufacturing_Data <- read.csv("Manufacturing.csv")
Services_Data <- read.csv("Services.csv")
Exports_Data <- read.csv("Exports.csv")
Imports_Data <- read.csv("Imports.csv")
Industry_Data <- read.csv("Industry.csv")
GNI_Data <- read.csv("GNIpc.csv")
Land_Data <- read.csv("Land Area.csv")
NL_Data <- read.csv("annualized_level_0.csv")

########DATA CLEANING####################

df_long <- pivot_longer(WDI_Data, 
                        cols = starts_with("X"),  # Columns containing years
                        names_to = "Year",         # Name of the new 'year' column
                        values_to = "Population")  # Name of the new 'population' column


GDP_Data <-pivot_longer(GDP_Data,
                        cols = starts_with("X"),  
                        names_to = "Year",        
                        values_to = "Population")
GDP_Data <- GDP_Data %>% 
  rename(Value = Population)

GDP_Data <- subset(GDP_Data, GDP_Data$Country.Name != "Africa Eastern and Southern")
GDP_Data <- subset(GDP_Data, GDP_Data$Country.Name != "Africa Western and Central")

Agriculture_Data <-pivot_longer(Agriculture_Data,
                       cols = starts_with("X"),  
                       names_to = "Year",        
                       values_to = "Population")
Agriculture_Data <- subset(Agriculture_Data, Agriculture_Data$Country.Name != "Africa Eastern and Southern")
Agriculture_Data <- Agriculture_Data %>% 
  rename(Value = Population)
Agriculture_Data <- subset(Agriculture_Data, Agriculture_Data$Country.Name != "Africa Western and Central")

Exports_Data <-pivot_longer(Exports_Data,
                                cols = starts_with("X"),  
                                names_to = "Year",        
                                values_to = "Value")
Exports_Data <- subset(Exports_Data, Exports_Data$Country.Name != "Africa Eastern and Southern")
Exports_Data <- subset(Exports_Data, Exports_Data$Country.Name != "Africa Western and Central")

Imports_Data <-pivot_longer(Imports_Data,
                            cols = starts_with("X"),  
                            names_to = "Year",        
                            values_to = "Value")
Imports_Data <- subset(Imports_Data, Imports_Data$Country.Name != "Africa Eastern and Southern")
Imports_Data <- subset(Imports_Data, Imports_Data$Country.Name != "Africa Western and Central")

Industry_Data <-pivot_longer(Industry_Data,
                            cols = starts_with("X"),  
                            names_to = "Year",        
                            values_to = "Value")
Industry_Data <- subset(Industry_Data, Industry_Data$Country.Name != "Africa Eastern and Southern")
Industry_Data <- subset(Industry_Data, Industry_Data$Country.Name != "Africa Western and Central")

Services_Data <-pivot_longer(Services_Data,
                            cols = starts_with("X"),  
                            names_to = "Year",        
                            values_to = "Value")
Services_Data <- subset(Services_Data, Services_Data$Country.Name != "Africa Eastern and Southern")
Services_Data <- subset(Services_Data, Services_Data$Country.Name != "Africa Western and Central")

GNI_Data <-pivot_longer(GNI_Data,
                             cols = starts_with("X"),  
                             names_to = "Year",        
                             values_to = "Value")
GNI_Data <- subset(GNI_Data, GNI_Data$Country.Name != "Africa Eastern and Southern")
GNI_Data <- subset(GNI_Data, GNI_Data$Country.Name != "Africa Western and Central")


Manufacturing_Data <-pivot_longer(Manufacturing_Data,
                             cols = starts_with("X"),  
                             names_to = "Year",        
                             values_to = "Value")
Manufacturing_Data <- subset(Manufacturing_Data, Manufacturing_Data$Country.Name != "Africa Eastern and Southern")
Manufacturing_Data <- subset(Manufacturing_Data, Manufacturing_Data$Country.Name != "Africa Western and Central")

##########################MERGING DATA################################
GDP_Data <- GDP_Data %>%
  filter(Year >= "X1992")
df_long$Year <- gsub("X|\\.\\.YR", "", df_long$Year)
df_long$Year <- substr(df_long$Year, 1, 4)
GDP_Data$Year <- gsub("X", "", GDP_Data$Year)

# Merge GDP_Data with Main_Data
General_data <- left_join(df_long, GDP_Data, by = c("Country.Name", "Country.Code", "Year"))
General_data <- General_data %>% 
  select(-starts_with("I"),
         -starts_with("S"))
General_data <- General_data %>% 
  rename(GDP = Value)
#   Merge GNI
GNI_Data <- GNI_Data %>%
  filter(Year >= "X1992")
GNI_Data$Year <- gsub("X", "", GNI_Data$Year)
General_data <- left_join(General_data, GNI_Data, by = c("Country.Name", "Country.Code", "Year"))
General_data <- General_data %>% 
  select(-starts_with("I"))
General_data <- General_data %>% 
  rename(GNIpc = Value)
#Merge Exports
Exports_Data <- Exports_Data %>%
  filter(Year >= "X1992")
Exports_Data$Year <- gsub("X", "", Exports_Data$Year)
General_data <- left_join(General_data, Exports_Data, by = c("Country.Name", "Country.Code", "Year"))
General_data <- General_data %>% 
  select(-starts_with("I"))
General_data <- General_data %>% 
  rename(Exports = Value)
#Merge Imports
Imports_Data <- Imports_Data %>%
  filter(Year >= "X1992")
Imports_Data$Year <- gsub("X", "", Imports_Data$Year)
General_data <- left_join(General_data, Imports_Data, by = c("Country.Name", "Country.Code", "Year"))
General_data <- General_data %>% 
  select(-starts_with("I"))
General_data <- General_data %>% 
  rename(Imports = Value)

#Merge Services
Services_Data <- Services_Data %>%
  filter(Year >= "X1992")
Services_Data$Year <- gsub("X", "", Services_Data$Year)
General_data <- left_join(General_data, Services_Data, by = c("Country.Name", "Country.Code", "Year"))
General_data <- General_data %>%
  select(-matches("^I") | starts_with("Imports"))
General_data <- General_data %>% 
  rename(Services = Value)

#Merge Agriculture
Agriculture_Data <- Agriculture_Data %>%
  filter(Year >= "X1992")
Agriculture_Data$Year <- gsub("X", "", Agriculture_Data$Year)
General_data <- left_join(General_data, Agriculture_Data, by = c("Country.Name", "Country.Code", "Year"))
General_data <- General_data %>%
  select(-matches("^I") | starts_with("Imports"))
General_data <- General_data %>% 
  rename(Agriculture = Value)
#Merge Industry
Industry_Data <- Industry_Data %>%
  filter(Year >= "X1992")
Industry_Data$Year <- gsub("X", "", Industry_Data$Year)
General_data <- left_join(General_data, Industry_Data, by = c("Country.Name", "Country.Code", "Year"))
General_data <- General_data %>%
  select(-matches("^I") | starts_with("Imports"))
General_data <- General_data %>% 
  rename(Industry = Value)

#Merge Manufacturing 
Manufacturing_Data <- Manufacturing_Data %>%
  filter(Year >= "X1992")
Manufacturing_Data$Year <- gsub("X", "", Manufacturing_Data$Year)
General_data <- left_join(General_data, Manufacturing_Data, by = c("Country.Name", "Country.Code", "Year"))
General_data <- General_data %>%
  select(-matches("^I") | starts_with("Imports"))
General_data <- General_data %>% 
  rename(Manufacturing = Value)

#Merge Land
Land_Data <-pivot_longer(Land_Data,
                        cols = starts_with("X"),  
                        names_to = "Year",        
                        values_to = "Area_Km2")
Land_Data <- subset(Land_Data, Land_Data$Country.Name != "Africa Eastern and Southern")
Land_Data <- subset(Land_Data, Land_Data$Country.Name != "Africa Western and Central")
Land_Data <- Land_Data %>%
  filter(Year >= "X1992")
Land_Data$Year <- gsub("X", "", Land_Data$Year)
General_data <- left_join(General_data, Land_Data, by = c("Country.Name", "Country.Code", "Year"))
General_data <- General_data %>%
  select(-matches("^I") | starts_with("Imports"))
General_data <- General_data %>% 
  select(-Indicator.Code)
General_data <- General_data %>% 
  select(-Indicator.Name)
General_data <- General_data %>% 
  rename(Area_km2 = Value)

#Merge NL Data
NL_Data <- NL_Data %>%
  filter(Year >= "1992")
NL_Data$Year <- as.character(NL_Data$Year)

General_data <- left_join(General_data, NL_Data, by = c( "Country.Code", "Year"))
General_data <- General_data %>% 
  select(-starts_with("l"))
General_data <- General_data %>% 
  select(-starts_with("u"))
General_data <- General_data %>% 
  select(-starts_with("v"))
General_data <- General_data %>% 
  select(-starts_with("f"))
General_data <- General_data %>% 
  select(-starts_with("t"))
General_data <- General_data %>% 
  select(-starts_with("w"))
General_data <- General_data %>% 
  select(-starts_with("r"))
General_data <- General_data %>%
  select(-matches("^s") | starts_with("Services"))
General_data <- General_data %>%
  select(-matches("^e") | starts_with("Exports"))
General_data <- General_data %>% 
  select(-precipitation_gpcp)
General_data <- General_data %>% 
  select(-gdelt_protest)
General_data <- General_data %>% 
  select(-ntl_dmsp_ext)
General_data <- General_data %>% 
  select(-population_count)
General_data <- General_data %>% 
  select(-Area_km2)
General_data <- General_data %>% 
  select(-precipitation_gpcc)
General_data <- General_data %>% 
  select(-precipitation_cru)
General_data <- General_data %>% 
  select(-ntl_dvnl)
General_data <- General_data %>% 
  select(-gdelt_coercion)
General_data <- General_data %>% 
  select(-dmsp_cloud_free_coverage)
General_data <- General_data %>% 
  select(-precipitation_gpcc)
General_data <- General_data %>% 
  select(-precipitation_cru)
General_data <- General_data %>% 
  select(-gdelt_coercion)
General_data <- General_data %>% 
  select(-ntl_dvnl)
General_data <- General_data %>% 
  select(-dmsp_cloud_free_coverage)

General_data <- subset(General_data, General_data$Country.Name != "Data from database: World Development Indicators")

##Calculating Night Lights Per Capita###

General_data$night_lightspc <- General_data$dmsp_stable_lights / General_data$Population
head(General_data)
General_data <- read.csv("General_Data.csv")

# Saving the general data
install.packages("openxlsx")
library(openxlsx)
write.xlsx(General_data, file = "General_Data.csv", rowNames = FALSE)


######################### SYNTHETIC CONTROLS####################################

#Ranking Data 

General_data$Rank <- seq_along(General_data$Country.Name)


#Creating a GGPLOT
Liberia_data <- General_data %>% mutate(
  Log_Nlights = log(night_lightspc), 
  Lib = if_else(Country.Name == "Liberia", 1, 0)
)

ggplot(Liberia_data, 
       aes(x = Year, y = Log_Nlights, 
           shape = factor(Lib, labels = 
                            c("Other Countries", "Liberia")))) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line")  +
  labs(x = "Year", y = "Night Lights Growth", shape = "") +
  theme_bw() + theme(legend.position = "bottom")

#Creating Synthetic Liberia
#Create a list of all city IDs in the dataset other than Liberia
GDbackup <- General_data

General_data <- GDbackup
General_data <- General_data %>%
  mutate(log_nl = log(night_lightspc)) %>%
  filter(!is.na(log_nl)) %>%
  mutate(country_code = 
           as.numeric(factor(Country.Code, levels = unique(Country.Code))))

#Converting dataset into a data.frame format
General_data <- as.data.frame(General_data)

#Getting 1992 data value for all countries
data92 <- General_data %>% filter(Year == 1992) %>% 
  select(country_code, dmsp_stable_lights) %>% 
  rename(dmsp_stable_lights_92 = dmsp_stable_lights)

General_data <- left_join(x = General_data, y = data92, 
                          by = c('country_code')) 
General_data <- General_data %>% 
  mutate(idx = dmsp_stable_lights / dmsp_stable_lights_92)

General_data <- General_data %>% filter(Year != 1992) %>%
  filter(country_code != 200)


### Filtering the General_data so that I only have LBR and 
### Plausible countries for my donor pool. They should be countries that:
### A) Are in SubSaharan Africa and did not experience conflict between 1992-2013.
### or
### B) Are not in SSA but produce Gold, Rubber or other LBR staples, and were did not experience conflict between 1992-2013.

# List of countries to include
Countries_to_include <- c("BWA", "NAM", "TZA", "MWI", "GHA", "THA", "MYS", "VNM", "BOL", "LBR", "LSO", "BRA")

# Filtering the dataset to include only the specified countries
Filtered_data <- General_data %>% 
  filter(Country.Code %in% Countries_to_include)

Filtered_data <- Filtered_data %>%
  mutate(country_code = 
           as.numeric(factor(Country.Code, levels = unique(Country.Code))))

List_countries <- unique(Filtered_data$country_code)
List_countries <- List_countries[-6] ### Check what the country_code for LBR is after filtering the data above, it will NOT be 111.

#Preparing data as an input for the synthetic control method using the dataprep function:
Filtered_data$Year <- as.numeric(Filtered_data$Year)

##Preparing my Donor Pool
dataprep.out <- dataprep( 
  foo = Filtered_data,
  dependent = "idx", 
  time.variable = "Year", 
  unit.variable = "country_code",
  special.predictors = list( 
    list("idx", 1993, "mean"),
    list("idx", 1994, "mean"),
    list("idx", 1995, "mean"),
    list("idx", 1996, "mean"),
    list("idx", 1997, "mean"),
    list("idx", 1998, "mean"),
    list("idx", 1999, "mean"),
    list("idx", 2000, "mean"),
    list("idx", 2001, "mean"),
    list("idx", 2002, "mean"),
    list("idx", 2003, "mean")
  ),
  treatment.identifier = 6,
  controls.identifier = List_countries, 
  time.predictors.prior = c(1993:2003), 
  time.optimize.ssr = c(1993:2003), 
  unit.names.variable = "Country.Name",
  time.plot = 1993:2013
)


#Passing the output of dataprep as an input for the synth function
synth.out <- synth(dataprep.out)
synth.out
#Producing the table of results with the synth.tab function and print it
synth.table <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.table)

## Producing the synthetic control plot and the gaps plot with the path.plot and the
#gaps.plot functions. 

#path.plot
path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

## gaps.plot
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)

#Producing the gaps plot with the placebo estimates for other cities using the
#generate.placebos and plot_placebos functions.
Placebo <- generate.placebos(dataprep.out, synth.out, Sigf.ipop = 2)

## Comparing differences with synthetic versions: Liberia vs. placebos
P1 <- plot_placebos(Placebo,discard.extreme=TRUE, 
                    mspe.limit=10, xlab='Year')
plot(P1)

Placebo <- generate.placebos(dataprep.out, synth.out, Sigf.ipop = 2)


# Manually setting the placebo year
Placebo$placebo.year <- 2000  

## Comparing differences with synthetic versions: Liberia vs. placebos
P1 <- plot_placebos(Placebo, discard.extreme = TRUE, mspe.limit = 10, xlab = 'Year')
plot(P1)


#Testing and plotting the change in predictability before and after the treatment for Liberia and for other countries using the mspe.test and mspe.plot functions.
Ratio <- mspe.test(Placebo)

Ratio$p.val

mspe.plot(Placebo, discard.extreme = FALSE)

###Summary Statistics####

summary_stats <- Filtered_data %>%
  group_by(Year) %>%
  summarise(
    Population_mean = mean(Population, na.rm = TRUE),
    Population_sd = sd(Population, na.rm = TRUE),
    GNIpc_mean = mean(GNIpc, na.rm = TRUE),
    GNIpc_sd = sd(GNIpc, na.rm = TRUE),
    GDP_mean = mean(GDP, na.rm = TRUE),
    GDP_sd = sd(GDP, na.rm = TRUE),
    Exports_mean = mean(Exports, na.rm = TRUE),
    Exports_sd = sd(Exports, na.rm = TRUE),
    Industry_mean = mean(Industry, na.rm = TRUE),
    Industry_sd = sd(Industry, na.rm = TRUE),
    Agriculture_mean = mean(Agriculture, na.rm = TRUE),
    Agriculture_sd = sd(Agriculture, na.rm = TRUE),
    Manufacturing_mean = mean(Manufacturing, na.rm = TRUE),
    Manufacturing_sd = sd(Manufacturing, na.rm = TRUE),
    Imports_mean = mean(Imports, na.rm = TRUE),
    Imports_sd = sd(Imports, na.rm = TRUE),
    Area_mean = mean(Area_Km2, na.rm = TRUE),
    Area_sd = sd(Area_Km2, na.rm = TRUE),
    Population_D_mean = mean(population_density, na.rm = TRUE),
    Population_D_sd = sd(population_density, na.rm = TRUE),
    Services_mean = mean(Services, na.rm = TRUE),
    Services_sd = sd(Services, na.rm = TRUE),
    Stable_lights_mean = mean(dmsp_stable_lights, na.rm = TRUE),
    Stable_lights_sd = sd(dmsp_stable_lights, na.rm = TRUE),
    Nightltspc_mean = mean(night_lightspc, na.rm = TRUE),
    Nightltspc_sd = sd(night_lightspc, na.rm = TRUE),
    log_nl_mean = mean(log_nl, na.rm = TRUE),
    log_nl_sd = sd(log_nl, na.rm = TRUE),
    idx_mean = mean(idx, na.rm = TRUE),
    idx_sd = sd(idx, na.rm = TRUE)
  )

# View summary statistics
print(summary_stats)
write.csv(summary_stats, "General_summary_statistics.csv", row.names = FALSE)

#Summary Statistics for Liberia

Liberia_Sum <- Filtered_data %>%
  filter(Country.Name == "Liberia")
Summary_Lib <- Liberia_Sum %>%
  group_by(Year) %>%
  summarise(
    Population_mean = mean(Population, na.rm = TRUE),
    Population_sd = sd(Population, na.rm = TRUE),
    GNIpc_mean = mean(GNIpc, na.rm = TRUE),
    GNIpc_sd = sd(GNIpc, na.rm = TRUE),
    GDP_mean = mean(GDP, na.rm = TRUE),
    GDP_sd = sd(GDP, na.rm = TRUE),
    Exports_mean = mean(Exports, na.rm = TRUE),
    Exports_sd = sd(Exports, na.rm = TRUE),
    Industry_mean = mean(Industry, na.rm = TRUE),
    Industry_sd = sd(Industry, na.rm = TRUE),
    Agriculture_mean = mean(Agriculture, na.rm = TRUE),
    Agriculture_sd = sd(Agriculture, na.rm = TRUE),
    Manufacturing_mean = mean(Manufacturing, na.rm = TRUE),
    Manufacturing_sd = sd(Manufacturing, na.rm = TRUE),
    Imports_mean = mean(Imports, na.rm = TRUE),
    Imports_sd = sd(Imports, na.rm = TRUE),
    Area_mean = mean(Area_Km2, na.rm = TRUE),
    Area_sd = sd(Area_Km2, na.rm = TRUE),
    Population_D_mean = mean(population_density, na.rm = TRUE),
    Population_D_sd = sd(population_density, na.rm = TRUE),
    Services_mean = mean(Services, na.rm = TRUE),
    Services_sd = sd(Services, na.rm = TRUE),
    Stable_lights_mean = mean(dmsp_stable_lights, na.rm = TRUE),
    Stable_lights_sd = sd(dmsp_stable_lights, na.rm = TRUE),
    Nightltspc_mean = mean(night_lightspc, na.rm = TRUE),
    Nightltspc_sd = sd(night_lightspc, na.rm = TRUE),
    log_nl_mean = mean(log_nl, na.rm = TRUE),
    log_nl_sd = sd(log_nl, na.rm = TRUE),
    idx_mean = mean(idx, na.rm = TRUE),
    idx_sd = sd(idx, na.rm = TRUE)
  )
print(Summary_Lib)


# Convert summary_stats to a formatted table using kable
# Save summary statistics as a CSV file
write.csv(Summary_Lib, "summary_statistics.csv", row.names = FALSE)


write.table(Summary_Lib, file = "summary_statistics.txt", sep = "\t", row.names = TRUE)

Summary_Liber <- kable(Summary_Lib, "latex", align = "c") %>%
  kable_styling()

# Save the formatted table as a PDF file
pdf("Summary_Liber.pdf")
print(Summary_Liber)
dev.off()
