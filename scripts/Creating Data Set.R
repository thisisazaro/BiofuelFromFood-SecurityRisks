#Загрузим различные библиотеки для проведения экономического анализа
library(WDI)
library(dplyr)
library(readr)
library(stringr)
library(writexl)
library(readxl)
library(tidyr)
#Определим путь сохранения статистических данных
setwd("C:\\Users\\User\\OneDrive - РАНХиГС\\Рабочий стол\\ВКР\\Статистика")
#Возьмем данные по ВВП из WDI (Всемирного банка)
gdp <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD", start = 2000, end = 2023)
wb_countries <- WDI_data$country %>%
  filter(region != "Aggregates") %>%
  select(iso2c, Country = country)
gdp_clean <- gdp %>%
  inner_join(wb_countries, by = "iso2c")
#Возьмем данные по Продовольственной безопасности из WDI (Всемирного банка)
food_prod_index <- WDI(country = "all", indicator = "AG.PRD.FOOD.XD", start = 2000, end = 2023)
food_prod_clean <- food_prod_index %>%
  inner_join(wb_countries, by = "iso2c") %>%
  rename(Food_Production_Index = AG.PRD.FOOD.XD)
#Выберем подходящие нам FSI из данных FAO, загруженных в папку "Статистика"
fsi <- read_csv("FSI(N).csv")
fsi_clean <- fsi %>%
  filter(Item == "Average dietary energy supply adequacy (percent) (3-year average)") %>%
  select(Country = Area, Year, Food_Supply_Adequacy = Value) %>%
  mutate(Food_Supply_Adequacy = as.numeric(Food_Supply_Adequacy),
         Year = str_sub(Year, 1, 4),                 
         Year = as.integer(Year))
#Добавим другие данные из WDI
indicators <- c("AG.YLD.CREL.KG", "NE.IMP.GNFS.ZS", "SL.AGR.EMPL.ZS", "SN.ITK.DEFC.ZS", "EG.FEC.RNEW.ZS")
macro_data <- WDI(country = "all", indicator = indicators, start = 2000, end = 2023)
macro_clean <- macro_data %>%
  inner_join(wb_countries, by = "iso2c") %>%
  rename(
    Cereal_Yield = AG.YLD.CREL.KG,
    Food_Imports_GDP = NE.IMP.GNFS.ZS,
    Agri_Employment = SL.AGR.EMPL.ZS,
    Undernourishment = SN.ITK.DEFC.ZS,
    Renewable_Energy_Share = EG.FEC.RNEW.ZS
  )
#Создадим таблицу full_data для унификации формата значений
full_data <- gdp_clean %>%
  select(Country, iso2c, year, GDP_per_capita = NY.GDP.PCAP.CD)
full_data <- full_data %>%
  left_join(food_prod_clean %>%
              select(iso2c, year, Food_Production_Index),
            by = c("iso2c", "year"))
full_data <- full_data %>%
  left_join(macro_clean %>%
              select(iso2c, year, Cereal_Yield, Food_Imports_GDP, Agri_Employment, Undernourishment, Renewable_Energy_Share),
            by = c("iso2c", "year"))
full_data <- full_data %>%
  left_join(fsi_clean, by = c("Country", "year" = "Year"))
#Cохраним наш полный набор данных full_data в документ формата excel
write_xlsx(full_data, path = "full_data.xlsx")
#Добавим данные по производству биотоплива (биоэтанол и биодизель) из OECD
ethanol_raw <- read_excel("Biofuel_OECD.xlsx", sheet = 3)
str(ethanol_raw)
biodiesel_raw <- read_excel("Biofuel_OECD.xlsx", sheet = 4)
str(biodiesel_raw)
ethanol_raw <- ethanol_raw %>%
  rename(Country = `Time period`) %>%
  mutate(Country = str_squish(str_remove_all(Country, "·")))
biodiesel_raw <- biodiesel_raw %>%
  rename(Country = `Time period`) %>%
  mutate(Country = str_squish(str_remove_all(Country, "·")))
ethanol_long <- ethanol_raw %>%
  pivot_longer(cols = -Country, names_to = "year", values_to = "Ethanol_Production") %>%
  mutate(year = as.integer(year),
         Ethanol_Production = as.numeric(Ethanol_Production))
biodiesel_long <- biodiesel_raw %>%
  pivot_longer(cols = -Country, names_to = "year", values_to = "Biodiesel_Production") %>%
  mutate(year = as.integer(year),
         Biodiesel_Production = as.numeric(Biodiesel_Production))
aggregates <- c("OECD", "European Union", "Total", "World")
ethanol_long_clean <- ethanol_long %>%
  filter(!Country %in% aggregates) %>%
  distinct(Country, year, .keep_all = TRUE)
biodiesel_long_clean <- biodiesel_long %>%
  filter(!Country %in% aggregates) %>%
  distinct(Country, year, .keep_all = TRUE)
biofuel_combined <- full_join(
  ethanol_long_clean, 
  biodiesel_long_clean, 
  by = c("Country", "year")
)
full_data <- full_data %>%
  left_join(biofuel_combined, by = c("Country", "year"))
#Добавим дополнительные данные из WDI, которые нам подходят
extra_indicators <- c("FP.CPI.TOTL.ZG", "EG.USE.PCAP.KG.OE", "AG.LND.ARBL.ZS", "SP.POP.TOTL", "NV.IND.TOTL.ZS")
extra_data <- WDI(
  country = "all",
  indicator = extra_indicators,
  start = 2000,
  end = 2023
)
extra_clean <- extra_data %>%
  inner_join(wb_countries, by = "iso2c") %>%
  rename(
    Food_Inflation = FP.CPI.TOTL.ZG,
    Energy_per_Capita = EG.USE.PCAP.KG.OE,
    Arable_Land_Pct = AG.LND.ARBL.ZS,
    Population_Total = SP.POP.TOTL,
    Industry_GDP_Share = NV.IND.TOTL.ZS
  )
full_data <- full_data %>%
  left_join(extra_clean, by = c("Country", "year"))

#Необходимые данные для экономической модели:
#1. Продовольственная безопасность
#1.0. Продовольственная безопасность состоит из 4 постулатов - availability, access, stability, utilization.
#-> (https://www.fao.org/faostat/en/#data/FS)
FSI_all <- read_csv("FSI(N).csv")
#1.1. Показатели по availability (физическое наличие продовольствия)
#1.1.1. Average dietary energy supply adequacy
FSI_adesa <- FSI_all %>%
  filter(Item == "Average dietary energy supply adequacy (percent) (3-year average)") %>%
  select(Country = Area, Year, Average_dietary_energy_supply_adequacy_percentage = Value)
#1.1.2. Share of dietary energy supply derived from cereals, roots and tubers
FSI_sodesdfc <- FSI_all %>%
  filter(Item == "Share of dietary energy supply derived from cereals, roots and tubers (percent) (3-year average)") %>%
  select(Country = Area, Year, Share_of_dietary_energy_supply_derived_from_cereals_roots_and_tubers_percentage = Value)
FSI_aps <- FSI_all %>%
#1.1.3. Average protein supply
  filter(Item == "Average protein supply (g/cap/day) (3-year average)") %>%
  select(Country = Area, Year, Average_protein_supply_grams_per_capita_per_day = Value)
FSI_asopoao <- FSI_all %>%
#1.1.4. Average supply of protein of animal origin
  filter(Item == "Average supply of protein of animal origin (g/cap/day) (3-year average)") %>%
  select(Country = Area, Year, Average_protein_supply_of_animal_origin_grams_per_capita_per_day = Value)
#1.1. Объединим все показатели по availability в отдельный Data Set (FSI_availability)
FSI_availability <- FSI_adesa %>%
  left_join(FSI_sodesdfc, by = c("Country", "Year")) %>%
  left_join(FSI_aps, by = c("Country", "Year")) %>%
  left_join(FSI_asopoao,by = c("Country", "Year"))
#1.2. Показатели по access (физический и экономический доступ к продовольствию)
#1.2.1. Rail lines density (X)
rail_lines_data <- read_excel("rail_lines_density.xlsx", sheet =1)
rail_long <- rail_lines_data %>%
  pivot_longer(cols = matches("^20\\d{2}"), 
               names_to = "year",
               values_to = "rail_km") %>%
  mutate(year = as.numeric(year))
rail_clean <- rail_long %>%
  filter(!grepl("Africa|World|Europe|Asia|Middle East|income", 
                'Country Name', ignore.case = TRUE)) %>%
  select(Country = 'Country Name', iso3c = 'Country Code', year, rail_km)
full_data <- full_data %>%
  left_join(rail_clean, by = c("iso3c", "year"))
#1.2.2. Gross domestic product per capita (in purchasing power equivalent)
FSI_gdppc <- FSI_all %>%
  filter(Item == "Gross domestic product per capita, PPP, (constant 2017 international $)") %>%
  select(Country = Area, Year, GDP_per_capita_PPP = Value)
#1.2.3. Prevalence of undernourishment, 3-year averages
FSI_pouavg <- FSI_all %>%
  filter(Item == "Prevalence of undernourishment (percent) (3-year average)") %>%
  select(Country = Area, Year, Prevalence_of_undernourishment_percentage = Value)
#1.2.4. Prevalence of undernourishment, yearly estimates
FSI_poua <- FSI_all %>%
  filter(Item == "Prevalence of undernourishment (percent) (annual value)") %>%
  select(Country = Area, Year, Prevalence_of_undernourishment_annual_percentage = Value)
#1.2.5. Prevalence of severe food insecurity in the total population, 3-year averages
FSI_posfiittp <- FSI_all %>%
  filter(Item == "Prevalence of severe food insecurity in the total population (percent) (3-year average)") %>%
  select(Country = Area, Year, Prevalence_of_severe_food_insecurity_in_total_population_percentage = Value)
#1.2.6. Prevalence of severe food insecurity in the total population, yearly estimates
FSI_posfiittpa <- FSI_all %>%
  filter(Item == "Prevalence of severe food insecurity in the total population (percent) (annual value)") %>%
  select(Country = Area, Year, Prevalence_of_severe_food_insecurity_in_total_population_percentage_annual = Value)
#1.2.7. Prevalence of moderate or severe food insecurity in the total population, 3-year averages
FSI_pomosfiittp <- FSI_all %>%
  filter(Item == "Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)") %>%
  select(Country = Area, Year, Prevalence_of_moderate_or_severe_food_insecurity_in_total_population_percentage = Value)
#1.2.7. Prevalence of moderate or severe food insecurity in the total population, yearly estimates
FSI_pomosfiittpa <- FSI_all %>%
  filter(Item == "Prevalence of moderate or severe food insecurity in the total population (percent) (annual value)") %>%
  select(Country = Area, Year, Prevalence_of_moderate_or_severe_food_insecurity_in_total_population_percentage = Value)
#1.2. Объединим все показатели по access в отдельный Data Set (FSI_access)
#FSI_access <- FSI_rld %>%
  #left_join(FSI_gdppc, by = c("Country", "Year")) %>%
  #left_join(FSI_pouavg, by = c("Country", "Year")) %>%
  #left_join(FSI_poua, by = c("Country", "Year")) %>%
  #left_join(FSI_posfiittp, by = c("Country", "Year")) %>%
  #left_join(FSI_posfiittpa, by = c("Country", "Year")) %>%
  #left_join(FSI_pomosfiittp, by = c("Country", "Year")) %>%
  #left_join(FSI_pomosfiittpa, by = c("Country", "Year"))
#1.3. Показатели по stability (постоянный и беспрерывный доступ к продовольствию)
#1.3.1. Cereals imports dependency ratio
FSI_cidr <- FSI_all %>%
  filter(Item == "Cereal import dependency ratio (percent) (3-year average)") %>%
  select(Country = Area, Year, Cereal_import_dependency_ratio_percentage = Value)
#1.3.2. Percent of arable land equipped for irrigation
FSI_poalefi <- FSI_all %>%
  filter(Item == "Percent of arable land equipped for irrigation (percent) (3-year average)") %>%
  select(Country = Area, Year, Percent_of_arable_land_equipped_for_irrigation_percentage = Value)
#1.3.3. Value of food imports over total merchandise exports
FSI_vofiotme <- FSI_all %>%
  filter(Item == "Value of food imports in total merchandise exports (percent) (3-year average)") %>%
  select(Country = Area, Year, Value_of_food_imports_in_total_merchandise_exports_percentage = Value)
#1.3.4. Political stability and absence of violence/terrorism
FSI_psaaovt <- FSI_all %>%
  filter(Item == "Political stability and absence of violence/terrorism (index)") %>%
  select(Country = Area, Year, Political_stability_and_absence_of_violence_or_terrorism_index = Value)
#1.3.5. Per capita food supply variability
FSI_pcfsv <- FSI_all %>%
  filter(Item == "Per capita food supply variability (kcal/cap/day)") %>%
  select(Country = Area, Year, Per_capita_food_supply_variability_kcal_per_capita_per_day = Value)
#1.3. Объединим все показатели по access в отдельный Data Set (FSI_stability)
FSI_stability <- FSI_cidr %>%
  left_join(FSI_poalefi, by = c("Country", "Year")) %>%
  left_join(FSI_vofiotme, by = c("Country", "Year")) %>%
  left_join(FSI_psaaovt, by = c("Country", "Year")) %>%
  left_join(FSI_pcfsv, by = c("Country", "Year"))
#1.4. Показатели по utilization (употребление продовольствия в пищу)
#1.4.1. People using at least basic drinking water services
FSI_pualbdws <- FSI_all %>%
  filter(Item == "Percentage of population using at least basic drinking water services (percent)") %>%
  select(Country = Area, Year, Percentage_of_population_using_at_least_basic_drinking_water_services = Value)
#1.4.2. People using safely managed drinking water services
FSI_pusmdws <- FSI_all %>%
  filter(Item == "Percentage of population using safely managed drinking water services (percent)") %>%
  select(Country = Area, Year, Percentage_of_population_using_safely_managed_drinking_water_services = Value)
#1.4.3. People using at least basic sanitation services
FSI_puabss <- FSI_all %>%
  filter(Item == "Percentage of population using at least basic sanitation services (percent)") %>%
  select(Country = Area, Year, Percentage_of_population_using_at_least_basic_sanitation_services = Value)
#1.4.4. People using safely managed sanitation services
FSI_pusmss <- FSI_all %>%
  filter(Item == "Percentage of population using safely managed sanitation services (percent)") %>%
  select(Country = Area, Year, Percentage_of_population_using_safely_managed_sanitation_services = Value)
#1.4.5. Percentage of children under 5 years of age affected by wasting
FSI_pocu5oaabw <- FSI_all %>%
  filter(Item == "Percentage of children under 5 years affected by wasting (percent)") %>%
  select(Country = Area, Year, Percentage_of_children_under_5_years_affected_by_wasting_percentage = Value)
#1.4.6. Percentage of children under 5 years of age who are stunted
FSI_pocu5oawas <- FSI_all %>%
  filter(Item == "Percentage of children under 5 years of age who are stunted (modelled estimates) (percent)") %>%
  select(Country = Area, Year, Percentage_of_children_under_5_years_who_are_stunted_as_modelled_estimates = Value)
#1.4.7. Percentage of children under 5 years of age who are overweight
FSI_pocu5oawao <- FSI_all %>%
  filter(Item == "Percentage of children under 5 years of age who are overweight (modelled estimates) (percent)") %>%
  select(Country = Area, Year, Percentage_of_children_under_5_years_who_are_overweight_as_modelled_estimates = Value)
#1.4.8. Prevalence of obesity in the adult population (18 years and older)
FSI_pooitap <- FSI_all %>%
  filter(Item == "Prevalence of obesity in the adult population (18 years and older) (percent)") %>%
  select(Country = Area, Year, Prevalence_of_obesity_in_adult_population_18_years_and_older_percentage = Value)
#1.4.9. Prevalence of anemia among women of reproductive age (15-49 years)
FSI_poaawora <- FSI_all %>%
  filter(Item == "Prevalence of anemia among women of reproductive age (15-49 years) (percent)") %>%
  select(Country = Area, Year, Prevalence_of_anemia_among_women_of_reproductive_age_from_15_to_49_percentage = Value)
#1.4.10.Prevalence of exclusive breastfeeding among infants 0-5 months of age
FSI_poebaiofa <- FSI_all %>%
  filter(Item == "Prevalence of exclusive breastfeeding among infants 0-5 months of age (percent)") %>%
  select(Country = Area, Year, Prevalence_of_exclusive_breastfeeding_among_infants_0_to_5_months_of_age_percentage = Value)
#1.4.11.Prevalence of low birthweight
FSI_polb <- FSI_all %>%
  filter(Item == "Prevalence of low birthweight (percent)") %>%
  select(Country = Area, Year, Prevalence_of_low_birthweight_percentage = Value)
#1.4. Объединим все показатели по access в отдельный Data Set (FSI_utilization)
FSI_utilization <- FSI_pualbdws %>%
  left_join(FSI_poalefi, by = c("Country", "Year")) %>%
  
#1.5. Other additional useful statistics
#1.5.1. Number of people undernourished, 3-year averages
FSI_nopu <- FSI_all %>%
  filter(Item == "Number of people undernourished (million) (3-year average)") %>%
  select(Country = Area, Year, Number_of_people_undernourished_in_millions = Value)
#1.5.2. Number of people undernourished, yearly estimates
FSI_nopua <- FSI_all %>%
  filter(Item == "Number of people undernourished (million) (annual value)") %>%
  select(Country = Area, Year, Number_of_people_undernourished_in_millions_annual = Value)
#1.5.3. Number of severely food insecure people, 3-year averages
FSI_nosfip <- FSI_all %>%
  filter(Item == "Number of severely food insecure people (million) (3-year average)") %>%
  select(Country = Area, Year, Number_of_severely_food_insecure_people = Value)
#1.5.4. Number of severely food insecure people, yearly estimates
FSI_nosfipa <- FSI_all %>%
  filter(Item == "Number of severely food insecure people (million) (annual value)") %>%
  select(Country = Area, Year, Number_of_severely_food_insecure_people_annual = Value)
#1.5.5. Number of moderately or severely food insecure people, 3-year averages
FSI_nomosfip <- FSI_all %>%
  filter(Item == "Number of moderately or severely food insecure people (million) (3-year average)") %>%
  select(Country = Area, Year, Number_of_moderately_or_severely_food_insecure_people = Value)
#1.5.6. Number of moderately or severely food insecure people, yearly estimates
FSI_nomosfipa <- FSI_all %>%
  filter(Item == "Number of moderately or severely food insecure people (million) (annual value)") %>%
  select(Country = Area, Year, Number_of_moderately_or_severely_food_insecure_people_annual = Value)
#1.5.7. Minimum Dietary Energy Requirement (MDER)
FSI_mder <- FSI_all %>%
  filter(Item == "Minimum dietary energy requirement  (kcal/cap/day)") %>%
  select(Country = Area, Year, Minimum_dietary_energy_requirement_kcal_per_capita_per_day = Value)
#1.5.8. Average Dietary Energy Requirement (ADER)
FSI_ader <- FSI_all %>%
  filter(Item == "Average dietary energy requirement (kcal/cap/day)") %>%
  select(Country = Area, Year, Average_dietary_energy_requirement_kcal_per_capita_per_day = Value)
#1.5.9. Coefficient of variation of habitual caloric consumption distribution
FSI_covohccd <- FSI_all %>%
  filter(Item == "Coefficient of variation of habitual caloric consumption distribution (real number)") %>%
  select(Country = Area, Year, Coefficient_of_variation_of_havitual_caloric_consumption_distribution_real = Value)
#1.5.10.Incidence of caloric losses at retail distribution level
FSI_ioclardl <- FSI_all %>%
  filter(Item == "Incidence of caloric losses at retail distribution level (percent)") %>%
  select(Country = Area, Year, Icidence_of_caloric_losses_at_retail_distribution_level_percentage = Value)
#1.5.11.Coefficient of variation of habitual caloric consumption distribution
FSI_covhccd <- FSI_all %>%
  filter(Item == "Coefficient of variation of habitual caloric consumption distribution (real number)") %>%
  select(Country = Area, Year, Coefficient_of_variation_of_habitual_caloric_consumption_distribution_real = Value)
#1.5.12.Incidence of caloric losses at retail distribution level
FSI_ioclardl <- FSI_all %>%
  filter(Item == "Incidence of caloric losses at retail distribution level (percent)") %>%
  select(Country = Area, Year, Incidence_of_caloric_losses_at_retail_distribution_level_percentage = Value)
#1.5.13.Dietary energy supply used in the estimation of prevalence of undernourishment, yearly
FSI_desuiteopfu <- FSI_all %>%
  filter(Item == "Dietary energy supply used in the estimation of prevalence of undernourishment (kcal/cap/day)") %>%
  select(Country = Area, Year, Dietary_energy_supply_used_in_estimation_of_prevalence_of_undernourishment_kcal_per_capita_per_day = Value)
#1.5.14.Dietary energy supply used in the estimation of prevalence of undernourishment, 3-year average
FSI_desuiteopfu <- FSI_all %>%
  filter(Item == "Dietary energy supply used in the estimation of prevalence of undernourishment (kcal/cap/day) (3-year average)") %>%
  select(Country = Area, Year, Dietary_energy_supply_used_in_estimation_of_prevalence_of_undernourishment_kcal_per_capita_per_day_3_year_average = Value)
#1.5.15.Average fat supply
FSI_afs <- FSI_all %>%
  filter(Item == "Average fat supply (g/cap/day) (3-year average)") %>%
  select(Country = Area, Year, Dietary_fat_supply_in_grams_per_capita_per_day_3_year_average = Value)
#1.5.16.Number of children under 5 years affected by wasting
FSI_nocu5abw <- FSI_all %>%
  filter(Item == "Number of children under 5 years affected by wasting (million)") %>%
  select(Country = Area, Year, Number_of_children_under_5_years_affected_by_wasting_million = Value)
#1.5.17.Number of children under 5 years of age who are stunted
FSI_nocu5oawas <- FSI_all %>%
  filter(Item == "Number of children under 5 years of age who are stunted (modeled estimates) (million)") %>%
  select(Country = Area, Year, Number_of_children_under_5_years_of_age_who_are_stunted_as_modelled_estimates = Value)
#1.5.18.Number of children under 5 years of age who are overweight
FSI_nocu5oawao <- FSI_all %>%
  filter(Item == "Number of children under 5 years of age who are overweight (modeled estimates)") %>%
  select(Country = Area, Year, Number_of_children_under_5_years_of_age_who_are_overweight_as_modelled_estimates = Value)
#1.5.19.Number of obese adults (18 years and older)
FSI_nooa <- FSI_all %>%
  filter(Item == "Number of obese adults (18 years and older) (million)") %>%
  select(Country = Area, Year, Number_of_obese_adults_18_and_older_million = Value)
#1.5.20.Number of women of reproductive age (15-49 years) affected by anemia
FSI_noworaaba <- FSI_all %>%
  filter(Item == "Number of women of reproductive age (15-49 years) affected by anemia (million)") %>%
  select(Country = Area, Year, Number_of_women_of_reproductive_age_15_to_49_affected_by_anemia_million = Value)
#1.5.21.Number of exclusively breastfed infants (0-5 months of age)
# (X)
#1.5.21.Number of newborns with low birthweight
FSI_nonwlb <- FSI_all %>%
  filter(Item == "Number of newborns with low birthweight (million)") %>%
  select(Country = Area, Year, Number_of_newborns_with_low_birthweight_million = Value)
#1.5. Объединим все показатели по access в отдельный Data Set (FSI_additional)


#2. Производство биотоплива (based on feedstock)
#-> (https://data-explorer.oecd.org/vis?tm=Ethanol%20production&pg=0&snb=5&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_AGR%40DF_OUTLOOK_2024_2033&df[ag]=OECD.TAD.ATM&df[vs]=1.1&pd=2000%2C2024&dq=.A.CPC_35491%2BCPC_EX_BIO%2BCPC_EX_35492.QP..&ly[rw]=MEASURE&ly[cl]=TIME_PERIOD&ly[rs]=COMMODITY&to[TIME_PERIOD]=false&isAvailabilityDisabled=false&hc[Measure]=&fc=Reference%20area)
BP_feedstock <- read_excel("Biofuels by commodity", sheet =1)
#3. Дополнительные данные:
#3.1. Данные по рынку агрохозяйственной промышленности
#-> (https://app.amis-outlook.org/#/market-database/supply-and-demand-overview)
#3.2. Данные по рынку энергии
#-> (https://www.energyinst.org/statistical-review/resources-and-data-downloads)
#Данные по рынку животного корма (включено в 3.1?)
#-> (?)
#4. Другие независимые экономические показатели (ВВП, прибыль, др.)
#-> (https://data.worldbank.org/indicator, ...)

x <- unique(FSI_all$Item)
grepl("Number of exclusively breastfed infants", x)
