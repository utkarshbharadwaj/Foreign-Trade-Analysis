# Load the required package
library(tidyverse)

# 1. Set Working Directory
setwd("/home/diazonium1109/Downloads/Assignments/Semester - II/DARP/Project - Group 24")

# 2. Read and Clean RBI Data
rbi_data <- read.csv("RBI_Foreign_Trade_data.csv", skip = 2, header = FALSE)
colnames(rbi_data) <- c("Year", "Month", 
                        "Exports_INR", "Exports_USD", "Exports_Oil_INR", "Exports_Oil_USD", 
                        "Exports_Nonoil_INR", "Exports_Nonoil_USD", 
                        "Imports_INR", "Imports_USD", "Imports_Oil_INR", "Imports_Oil_USD", 
                        "Imports_Nonoil_INR", "Imports_Nonoil_USD", 
                        "TradeBalance_INR", "TradeBalance_USD", "TradeBalance_Oil_INR", "TradeBalance_Oil_USD", 
                        "TradeBalance_Nonoil_INR", "TradeBalance_Nonoil_USD")

df_clean <- rbi_data %>%
  fill(Year, .direction = "down") %>%
  mutate(across(-c(Year, Month), ~ as.numeric(gsub(",", "", .))))

# 3. Load CPI and Calculate
cpi_data <- read.csv("CPIAUCSL.csv") %>%
  mutate(Date = as.Date(observation_date)) %>%
  select(Date, CPI_Value = CPIAUCSL)

base_cpi <- 326.031 # Dec 2025 Base

df_idea1 <- df_clean %>%
  mutate(Date_String = paste("01", Month, Year, sep = "-")) %>%
  mutate(Date = as.Date(Date_String, format = "%d-%B-%Y")) %>%
  left_join(cpi_data, by = "Date") %>%
  mutate(
    Exchange_Rate = (Exports_INR * 10) / Exports_USD,
    TradeBalance_Constant_USD = TradeBalance_USD * (base_cpi / CPI_Value)
  ) %>%
  drop_na(TradeBalance_Constant_USD, Exchange_Rate) %>%
  arrange(Date)

# 4. Clean Visualization 1
coeff <- max(abs(df_idea1$TradeBalance_Constant_USD), na.rm = TRUE) / max(df_idea1$Exchange_Rate, na.rm = TRUE)

plot_1 <- ggplot(df_idea1, aes(x = Date)) +
  geom_col(aes(y = TradeBalance_Constant_USD, fill = "Trade Balance"), alpha = 0.75) +
  geom_line(aes(y = Exchange_Rate * -(coeff/1.5), color = "Implied Exchange Rate"), linewidth = 1.2) +
  
  scale_y_continuous(
    name = "Trade Balance (Dec 2025 USD Millions)",
    sec.axis = sec_axis(~ . / -(coeff/1.5), name = "Implied Exchange Rate (INR per USD)")
  ) +
  scale_fill_manual(name = "", values = c("Trade Balance" = "#EF476F")) +
  scale_color_manual(name = "", values = c("Implied Exchange Rate" = "#118AB2")) +
  theme_minimal() +
  labs(
    title = "Trade Balance vs. Rupee Depreciation (1990-2025)",
    subtitle = "Does a weaker Rupee correlate with a narrowing trade deficit?",
    x = "Year"
  ) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14))

print(plot_1)
