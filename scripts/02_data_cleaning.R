
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

full_data <- read_excel("data/full_data.xlsx")

str(full_data)
summary(full_data)

# заполним NA для rail_km
full_data <- full_data %>%
        group_by(iso3c) %>%
        arrange(year) %>%
        fill(rail_km, .direction = "downup") %>%
        ungroup()
summary(full_data$rail_km)

full_data <- full_data %>%
        mutate(rail_km = ifelse(is.na(rail_km), 0, rail_km))


vars_to_fill <- c("GDP_per_capita", "Cereal_Yield", "Agri_Employment", "Undernourishment",
                  "Renewable_Energy_Share", "Food_Supply_Adequacy", "Food_Inflation",
                  "Energy_per_Capita", "Industry_GDP_Share")

full_data <- full_data %>%
        group_by(iso3c) %>%
        arrange(year) %>%
        fill(all_of(vars_to_fill), .direction = "downup") %>%
        ungroup()
sapply(full_data[vars_to_fill], function(x) sum(is.na(x)))

full_data <- full_data %>%
        group_by(iso3c) %>%
        mutate(across(all_of(vars_to_fill), ~ if_else(
                is.na(.), 
                coalesce(median(., na.rm = TRUE), median(full_data[[cur_column()]], na.rm = TRUE)), 
                .
        ))) %>%
        ungroup()

# Food_Production_Index и Food_Imports_GDP

vars_fill_median <- c("Food_Production_Index", "Food_Imports_GDP")

# 1. Сперва fill по времени внутри страны
full_data <- full_data %>%
        group_by(iso3c) %>%
        arrange(year) %>%
        fill(all_of(vars_fill_median), .direction = "downup") %>%
        ungroup()

# 2. Потом медиана по стране, если еще есть NA
full_data <- full_data %>%
        group_by(iso3c) %>%
        mutate(across(all_of(vars_fill_median), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
        ungroup()
sapply(full_data[vars_fill_median], function(x) sum(is.na(x)))
full_data <- full_data %>%
        mutate(across(all_of(vars_fill_median), ~ ifelse(is.na(.), 0, .)))

# Ethanol_Production и Biodiesel_Production
full_data <- full_data %>%
        mutate(
                Ethanol_Production = ifelse(is.na(Ethanol_Production), 0, Ethanol_Production),
                Biodiesel_Production = ifelse(is.na(Biodiesel_Production), 0, Biodiesel_Production)
        )


# Arable_Land_Pct
full_data <- full_data %>%
        group_by(iso3c) %>%
        arrange(year) %>%
        fill(Arable_Land_Pct, .direction = "downup") %>%
        ungroup()

full_data <- full_data %>%
        group_by(iso3c) %>%
        mutate(Arable_Land_Pct = ifelse(is.na(Arable_Land_Pct),
                                        median(Arable_Land_Pct, na.rm = TRUE),
                                        Arable_Land_Pct)) %>%
        ungroup()

full_data <- full_data %>%
        mutate(Arable_Land_Pct = ifelse(is.na(Arable_Land_Pct), 0, Arable_Land_Pct))


## проверка на выбросы


vars_to_check <- c("GDP_per_capita", "Cereal_Yield", "Food_Imports_GDP",
                   "Arable_Land_Pct", "Food_Production_Index", "Energy_per_Capita")
detect_outliers_iqr <- function(x) {
        q1 <- quantile(x, 0.25, na.rm = TRUE)
        q3 <- quantile(x, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower <- q1 - 1.5 * iqr
        upper <- q3 + 1.5 * iqr
        return(x < lower | x > upper)
}

outlier_data <- full_data %>%
        select(iso3c, Country.x, year, all_of(vars_to_check)) %>%
        pivot_longer(cols = all_of(vars_to_check), names_to = "variable", values_to = "value") %>%
        group_by(iso3c, variable) %>%
        mutate(is_outlier = detect_outliers_iqr(value)) %>%
        ungroup() %>%
        filter(is_outlier == TRUE)

ggplot(outlier_data %>% filter(value > 0), aes(x = variable, y = value)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(color = "red", alpha = 0.3, width = 0.2) +
        scale_y_continuous(trans = "log10") +
        labs(title = "Выбросы по переменным", y = "Значение (log10)") +
        theme_minimal()





## винзоризация

vars_to_winsorize <- c("GDP_per_capita", "Cereal_Yield", "Food_Imports_GDP",
                       "Arable_Land_Pct", "Food_Production_Index", "Energy_per_Capita",
                       "Agri_Employment", "Undernourishment", "Renewable_Energy_Share", 
                       "Food_Supply_Adequacy", "Ethanol_Production", "Biodiesel_Production",
                       "Food_Inflation", "Population_Total", "Industry_GDP_Share",
                       "rail_km")

winsorize_vec <- function(x, lower = 0.05, upper = 0.95) {
        qnt <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
        x[x < qnt[1]] <- qnt[1]
        x[x > qnt[2]] <- qnt[2]
        return(x)
}

full_data_clean <- full_data %>%
        mutate(across(all_of(vars_to_winsorize), winsorize_vec))


# Выбираем винзоризованные переменные и переводим в long-формат
full_data_clean %>%
        select(all_of(vars_to_winsorize)) %>%
        pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
        ggplot(aes(x = value)) +
        geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
        facet_wrap(~ variable, scales = "free") +
        theme_minimal() +
        labs(title = "Распределение переменных после винзоризации", x = "", y = "Частота")


# Логарифмическое преобразование для скошенных переменных:
full_data_clean <- full_data_clean %>%
        mutate(across(c("GDP_per_capita", "Energy_per_Capita", "rail_km", "Population_Total"), 
                      ~ log1p(.), .names = "{.col}_log"))

vars_to_log <- c("GDP_per_capita", "Cereal_Yield", "Food_Imports_GDP",
                 "Arable_Land_Pct", "Food_Production_Index", "Energy_per_Capita")

full_data_clean <- full_data_clean %>%
        mutate(across(all_of(vars_to_log), ~ log1p(.), .names = "{.col}_log"))

full_data_clean %>%
        select(ends_with("_log")) %>%
        pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
        ggplot(aes(x = value)) +
        geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
        facet_wrap(~ variable, scales = "free") +
        theme_minimal() +
        labs(title = "Гистограммы логарифмированных переменных", x = "", y = "Частота")












