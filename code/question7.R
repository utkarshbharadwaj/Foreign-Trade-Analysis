# Load the required package
library(tidyverse)

# 1. Set Working Directory
setwd("/home/diazonium1109/Downloads/Assignments/Semester - II/DARP/Project - Group 24")

# 2. Read and Clean Data
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

# 3. Load CPI and Calculate Base
cpi_data <- read.csv("CPIAUCSL.csv") %>%
  mutate(Date = as.Date(observation_date)) %>%
  select(Date, CPI_Value = CPIAUCSL)

base_cpi <- 326.031 # Dec 2025 Base

# 4. Prepare Data for Idea 7
df_idea7 <- df_clean %>%
  filter(Year >= 1991) %>%
  mutate(Date_String = paste("01", Month, Year, sep = "-")) %>%
  mutate(Date = as.Date(Date_String, format = "%d-%B-%Y")) %>%
  left_join(cpi_data, by = "Date") %>%
  
  # Group by Year to get the Annual Totals
  group_by(Year) %>%
  summarise(
    # Nominal INR Deficit
    Annual_Deficit_INR_Nominal = abs(sum(TradeBalance_INR, na.rm = TRUE)),
    # Constant USD Deficit (Inflation-Adjusted)
    Annual_Deficit_USD_Constant = abs(sum(TradeBalance_USD * (base_cpi / CPI_Value), na.rm = TRUE))
  ) %>%
  
  # Create a Growth Index (Base Year 1991 = 100)
  mutate(
    Index_INR = (Annual_Deficit_INR_Nominal / Annual_Deficit_INR_Nominal[Year == 1991]) * 100,
    Index_USD_Constant = (Annual_Deficit_USD_Constant / Annual_Deficit_USD_Constant[Year == 1991]) * 100
  )

# 5. Visualization 7: The Divergence Plot
df_idea7_long <- df_idea7 %>%
  select(Year, Index_INR, Index_USD_Constant) %>%
  pivot_longer(cols = c(Index_INR, Index_USD_Constant), names_to = "Metric", values_to = "Growth_Index")

plot_7 <- ggplot(df_idea7_long, aes(x = Year, y = Growth_Index, color = Metric)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5, alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Currency Valuation Divergence: The Illusion of the Deficit",
    subtitle = "Comparing Nominal INR Deficit Growth against the Constant USD Deficit (Base 1991 = 100)",
    x = "Year",
    y = "Deficit Growth Index (1991 = 100)"
  ) +
  scale_color_manual(name = "Denomination:", 
                     labels = c("Nominal INR (Not Adjusted)", "Constant USD (Inflation-Adjusted)"),
                     values = c("Index_INR" = "#EF476F", "Index_USD_Constant" = "#118AB2")) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(plot_7)