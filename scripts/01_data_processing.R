

library(WDI)
library(countrycode)
library(dplyr)
library(stringr)
library(readr)
library(writexl)

gdp <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD", start = 2000, end = 2023)
head(gdp)

# Подготовим справочник стран с нужным названием
wb_countries <- WDI_data$country %>%
        filter(region != "Aggregates") %>%
        select(iso2c, Country = country)  # <- здесь переименовываем сразу

# Присоединим к GDP
gdp_clean <- gdp %>%
        inner_join(wb_countries, by = "iso2c")
head(gdp_clean)


# Food Security Index (из OurWorldInData)
# Индикатор: Food Production Index (2004–2006 = 100)
food_prod_index <- WDI(country = "all", indicator = "AG.PRD.FOOD.XD", start = 2000, end = 2023)
food_prod_clean <- food_prod_index %>%
        inner_join(wb_countries, by = "iso2c") %>%
        rename(Food_Production_Index = AG.PRD.FOOD.XD)
head(food_prod_clean)


# Добавим еще переменных
#| Переменная                        | Индикатор WDI             | Назначение                                  |
#|----------------------------------|----------------------------|---------------------------------------------|
#| 🌾 Урожайность зерновых          | `AG.YLD.CREL.KG`           | Эффективность продовольственного производства |
#| 🚚 Импорт продовольствия (% ВВП) | `NE.IMP.GNFS.ZS`           | Импортная зависимость                        |
#| 🧑‍🌾 Занятость в сельском хоз-ве  | `SL.AGR.EMPL.ZS`           | Доля сельхоз-занятых                         |
#| 👨‍👩‍👧‍👦 Доля недоедающих            | `SN.ITK.DEFC.ZS`           | Прямой индикатор продовольственной недостаточности |
#| 🔌 Возобновляемая энергия (% от потребления) | `EG.FEC.RNEW.ZS` | Контекст биоэнергетики                      |

indicators <- c(
        "AG.YLD.CREL.KG",    # Cereal yield (kg per hectare)
        "NE.IMP.GNFS.ZS",    # Imports of goods and services (% of GDP)
        "SL.AGR.EMPL.ZS",    # Employment in agriculture (% of total employment)
        "SN.ITK.DEFC.ZS",    # Prevalence of undernourishment (% of population)
        "EG.FEC.RNEW.ZS"     # Renewable energy consumption (% of total)
        )

macro_data <- WDI(country = "all", indicator = indicators, start = 2000, end = 2023)

# Фильтрация стран
macro_clean <- macro_data %>%
        inner_join(wb_countries, by = "iso2c") %>%
        rename(
                Cereal_Yield = AG.YLD.CREL.KG,
                Food_Imports_GDP = NE.IMP.GNFS.ZS,
                Agri_Employment = SL.AGR.EMPL.ZS,
                Undernourishment = SN.ITK.DEFC.ZS,
                Renewable_Energy_Share = EG.FEC.RNEW.ZS
        )

head(macro_clean)


## индекс прод безопасности


fsi <- read_csv("data/FSI(N).csv")
str(fsi)
glimpse(fsi)       
colnames(fsi)      



# Оставим только нужный индикатор
# Оставляем только строки, где в колонке Item указан нужный нам показатель “Средняя 
# энергетическая обеспеченность продовольствием (в % от нормы, за 3 года)”
# Это ключевой прокси-индикатор продовольственной безопасности, часто используемый в ООН и FAO.

# Берёт только три нужные колонки: Area → переименовывается в Country, 
# Year → как есть (например, "2000-2002"), Value → переименовывается в Food_Supply_Adequacy.
# Получаем минимальный, чистый датафрейм с нужными столбцами.

fsi_clean <- fsi %>%
        filter(Item == "Average dietary energy supply adequacy (percent) (3-year average)") %>%
        select(Country = Area, Year, Food_Supply_Adequacy = Value) %>%
        mutate(
                Food_Supply_Adequacy = as.numeric(Food_Supply_Adequacy),
                Year = str_sub(Year, 1, 4),                 # Преобразуем "2000–2002" → "2000"
                Year = as.integer(Year)
        ) 

head(fsi_clean)



# объёмы производства биоэтанола и биодизеля по странам (в миллионах литров), а значит:
# Непосредственно отражают влияние биотоплива из продовольственного сырья
# Позволяют построить переменные: Bioethanol_Production, Biodiesel_Production
# Идеальны для регрессий влияния биотоплива на продовольственную безопасность
# источник OECD-FAO Agricultural Outlook 2024-2033

library(readxl)
library(dplyr)
library(tidyr)


# Загрузка листа с Ethanol (лист 3)
ethanol_raw <- read_excel("data/biofuel_oecd.xlsx", sheet = 3)
str(ethanol_raw)
# Загрузка листа с Biodiesel (лист 4)
biodiesel_raw <- read_excel("data/biofuel_oecd.xlsx", sheet = 4)
str(biodiesel_raw)

library(stringr)

ethanol_raw <- ethanol_raw %>%
        rename(Country = `Time period`) %>%
        mutate(Country = str_squish(str_remove_all(Country, "·")))  

biodiesel_raw <- biodiesel_raw %>%
        rename(Country = `Time period`) %>%
        mutate(Country = str_squish(str_remove_all(Country, "·")))
library(tidyr)
library(dplyr)

ethanol_long <- ethanol_raw %>%
        pivot_longer(cols = -Country, names_to = "year", values_to = "Ethanol_Production") %>%
        mutate(
                year = as.integer(year),
                Ethanol_Production = as.numeric(Ethanol_Production)
        )
str(ethanol_long)

biodiesel_long <- biodiesel_raw %>%
        pivot_longer(cols = -Country, names_to = "year", values_to = "Biodiesel_Production") %>%
        mutate(
                year = as.integer(year),
                Biodiesel_Production = as.numeric(Biodiesel_Production)
        )
str(biodiesel_long)

# Список агрегатов, которые можно исключить
aggregates <- c("OECD", "European Union", "Total", "World")

# Очистка ethanol
ethanol_long_clean <- ethanol_long %>%
        filter(!Country %in% aggregates) %>%
        distinct(Country, year, .keep_all = TRUE)

# Очистка biodiesel
biodiesel_long_clean <- biodiesel_long %>%
        filter(!Country %in% aggregates) %>%
        distinct(Country, year, .keep_all = TRUE)


biofuel_combined <- full_join(
        ethanol_long_clean, 
        biodiesel_long_clean, 
        by = c("Country", "year")
)

biofuel_combined









## объединяем в финальный набор данных

# Начинаем с GDP
full_data <- gdp_clean %>%
        select(iso2c, year, GDP_per_capita = NY.GDP.PCAP.CD, Country)

# Добавляем Food Production Index
full_data <- full_data %>%
        left_join(food_prod_clean %>%
                          select(iso2c, year, Food_Production_Index),
                  by = c("iso2c", "year"))

# Добавляем Macro Indicators
full_data <- full_data %>%
        left_join(macro_clean %>%
                          select(iso2c, year, Cereal_Yield, Food_Imports_GDP,
                                 Agri_Employment, Undernourishment, Renewable_Energy_Share),
                  by = c("iso2c", "year"))

# Добавляем Food Security Adequacy      
full_data <- full_data %>%
        left_join(fsi_clean, by = c("Country", "year" = "Year"))


# Добавляем Ethanol_Production и Biodiesel_Production

full_data <- full_data %>%
        left_join(biofuel_combined, by = c("Country", "year"))


glimpse(full_data)
head(full_data)
summary(full_data)

# сохраняем в excel
write_xlsx(full_data, path = "data/full_data.xlsx")

str(full_data)


