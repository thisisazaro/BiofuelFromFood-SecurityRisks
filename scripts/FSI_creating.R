
# Cоздадим показатель продовольственной безопасности (Food Security Index, FSI) по методологии FAO

# Нормализуем каждую переменную в диапазоне от 0 до 100:

# FS_element = ((Country Value - World Minimum) / (World Maximum - World Minimum)) × 100

# Рассчитываем подиндексы:
# FSAVA (Availability)
# FSACC (Accessibility)
# FSUTI (Utilization)
# FSSTA (Stability)
# Итоговый FSI = среднее из этих четырёх индексов.

install.packages("WDI")
library(WDI)

## FSACC
# Определяем нужные переменные
indicators_FSACC <- c(
        "NY.GDP.PCAP.PP.KD",   # GDP per capita, PPP (constant)
        "SN.ITK.DEFC.ZS",      # Prevalence of undernourishment
        "SN.ITK.SVFI.ZS"       # Severe food insecurity (если доступно)
)

# Загружаем данные
fsacc_data <- WDI(country = "all", indicator = indicators_FSACC, start = 2000, end = 2022)

# Посмотрим на названия переменных
head(fsacc_data)

# Переименуем для удобства
colnames(fsacc_data) <- c("country","iso2c", "iso3c", "year", "gdp_pc", "undernourishment", "food_insecurity")

# Функция нормализации
normalize <- function(x, reverse = FALSE) {
        if (reverse) {
                (max(x, na.rm = TRUE) - x) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
        } else {
                (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
        }
}

# Применим нормализацию
fsacc_data$gdp_index <- normalize(fsacc_data$gdp_pc)
fsacc_data$undern_index <- normalize(fsacc_data$undernourishment, reverse = TRUE)
fsacc_data$foodsec_index <- normalize(fsacc_data$food_insecurity, reverse = TRUE)

# Считаем FSACC (доступность еды)
fsacc_data$FSACC <- rowMeans(fsacc_data[, c("gdp_index", "undern_index", "foodsec_index")], na.rm = TRUE)

# Просмотр результата
head(fsacc_data[, c("country", "FSACC")])


## FSUTI — Food Utilization Index

indicators_FSUTI <- c(
        "SH.H2O.BASW.ZS",     # Access to basic drinking water
        "SH.STA.SMSS.ZS",     # Access to sanitation
        "SH.STA.OB18.MA.ZS",  # Obesity (males, замена если общий нет)
        "SH.ANM.ALLW.ZS"      # Anemia (women)
)

fsuti_data <- WDI(country = "all", indicator = indicators_FSUTI, start = 2000, end = 2022)
head(fsuti_data)

# Переименуем
colnames(fsuti_data) <- c("country", "iso2c", "iso3c", "year", "water", "sanitation", "obesity", "anemia")

# Нормализация
normalize <- function(x, reverse = FALSE) {
        if (reverse) {
                (max(x, na.rm = TRUE) - x) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
        } else {
                (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
        }
}

# Применим
fsuti_data$water_index     <- normalize(fsuti_data$water)
fsuti_data$sanitation_index<- normalize(fsuti_data$sanitation)
fsuti_data$obesity_index   <- normalize(fsuti_data$obesity, reverse = TRUE)
fsuti_data$anemia_index    <- normalize(fsuti_data$anemia, reverse = TRUE)

# Соберём FSUTI
fsuti_data$FSUTI <- rowMeans(fsuti_data[, c("water_index", "sanitation_index", 
                                            "obesity_index", "anemia_index")], na.rm = TRUE)

# Посмотрим
head(fsuti_data[, c("country", "FSUTI")])



## FSSTA — Food Stability Index

indicators_FSSTA <- c(
        "AG.LND.IRIG.AG.ZS",  # Arable land equipped for irrigation
        "PV.EST"              # Political stability
)

fssta_data <- WDI(country = "all", indicator = indicators_FSSTA, start = 2000, end = 2022)
head(fssta_data)

# Переименуем
colnames(fssta_data) <- c("country", "iso2c", "iso3c", "year", "irrigation", "stability")

# Функция нормализации
normalize <- function(x, reverse = FALSE) {
        if (reverse) {
                (max(x, na.rm = TRUE) - x) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
        } else {
                (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
        }
}

# Применим
fssta_data$irrigation_index <- normalize(fssta_data$irrigation)
fssta_data$stability_index  <- normalize(fssta_data$stability)

# Рассчитаем FSSTA
fssta_data$FSSTA <- rowMeans(fssta_data[, c("irrigation_index", "stability_index")], na.rm = TRUE)

# Просмотр результата
head(fssta_data[, c("country", "FSSTA")])

## FSAVA — Food Availability Index

indicators_FSAVA <- c(
        "AG.PRD.FOOD.XD",    # Food production index
        "AG.YLD.CREL.KG",    # Cereal yield (kg per hectare)
        "AG.LND.ARBL.ZS"     # Arable land (% of land area)
)

fsava_data <- WDI(country = "all", indicator = indicators_FSAVA, start = 2000, end = 2022)
head(fsava_data)


# Переименуем
colnames(fsava_data) <- c("country", "iso2c", "iso3c", "year", "food_prod", "cereal_yield", "arable_land")

# Нормализация
normalize <- function(x, reverse = FALSE) {
        if (reverse) {
                (max(x, na.rm = TRUE) - x) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
        } else {
                (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
        }
}

# Применим
fsava_data$prod_index   <- normalize(fsava_data$food_prod)
fsava_data$yield_index  <- normalize(fsava_data$cereal_yield)
fsava_data$arable_index <- normalize(fsava_data$arable_land)

# Рассчитаем FSAVA
fsava_data$FSAVA <- rowMeans(fsava_data[, c("prod_index", "yield_index", "arable_index")], na.rm = TRUE)

# Посмотрим результат
head(fsava_data[, c("country", "FSAVA")])

# Теперь нужно объединить все подиндексы (FSACC, FSUTI, FSSTA, FSAVA) в итоговый FSI

library(dplyr)

# подготовим подиндексы (оставим нужные колонки)
fsacc_sub <- fsacc_data[, c("country", "year", "FSACC")]
fsuti_sub <- fsuti_data[, c("country", "year", "FSUTI")]
fssta_sub <- fssta_data[, c("country", "year", "FSSTA")]
fsava_sub <- fsava_data[, c("country", "year", "FSAVA")]

str(fsacc_sub)
str(fsuti_sub)
str(fssta_sub)
str(fsava_sub)

# объединяем
fsi_panel <- fsacc_sub %>%
        full_join(fsuti_sub, by = c("country", "year")) %>%
        full_join(fssta_sub, by = c("country", "year")) %>%
        full_join(fsava_sub, by = c("country", "year"))

# считаем общий FSI
fsi_panel$FSI <- rowMeans(fsi_panel[, c("FSACC", "FSUTI", "FSSTA", "FSAVA")], na.rm = TRUE)
head(fsi_panel)


library(ggplot2)

fsi_panel %>% 
        filter(country == "Afghanistan") %>%
        ggplot(aes(x = year, y = FSI)) +
        geom_line(color = "steelblue", linewidth = 1.2) +
        geom_point(color = "darkred") +
        labs(title = "Food Security Index for Afghanistan",
             x = "Year", y = "FSI (0–100)") +
        theme_minimal()


# Построим карту мира по FSI в 2026 г.

library(ggplot2)
library(rnaturalearth)
library(sf)

fsi_2016 <- fsi_panel %>% filter(year == 2016)

# Карта стран
world <- ne_countries(scale = "medium", returnclass = "sf")

# Объединяем с FSI
map_data <- left_join(world, fsi_2016, by = c("name" = "country"))
ggplot(map_data) +
        geom_sf(aes(fill = FSI)) +
        scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
        theme_minimal() +
        labs(title = "Global Food Security Index, 2016",
             fill = "FSI")







