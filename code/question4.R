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

# 3. Load CPI Data
cpi_data <- read.csv("CPIAUCSL.csv") %>%
  mutate(Date = as.Date(observation_date)) %>%
  select(Date, CPI_Value = CPIAUCSL)

base_cpi <- 326.031 # Dec 2025 Base

# 4. Prepare Data & Calculate Constant Averages
month_levels <- c("January", "February", "March", "April", "May", "June", 
                  "July", "August", "September", "October", "November", "December")

df_idea4 <- df_clean %>%
  filter(Year >= 2011) %>%
  mutate(Date_String = paste("01", Month, Year, sep = "-")) %>%
  mutate(Date = as.Date(Date_String, format = "%d-%B-%Y")) %>%
  left_join(cpi_data, by = "Date") %>%
  
  # Convert to Constant USD FIRST
  mutate(
    Exp_Oil_Const = Exports_Oil_USD * (base_cpi / CPI_Value),
    Exp_Nonoil_Const = Exports_Nonoil_USD * (base_cpi / CPI_Value),
    Imp_Oil_Const = Imports_Oil_USD * (base_cpi / CPI_Value),
    Imp_Nonoil_Const = Imports_Nonoil_USD * (base_cpi / CPI_Value)
  ) %>%
  drop_na() %>%
  mutate(Month = factor(Month, levels = month_levels)) %>%
  
  # Group and Average the Constant Values
  group_by(Month) %>%
  summarise(
    Avg_Exp_Oil = mean(Exp_Oil_Const, na.rm = TRUE),
    Avg_Exp_Nonoil = mean(Exp_Nonoil_Const, na.rm = TRUE),
    Avg_Imp_Oil = mean(Imp_Oil_Const, na.rm = TRUE),
    Avg_Imp_Nonoil = mean(Imp_Nonoil_Const, na.rm = TRUE)
  )

# 5. Visualization 4: Seasonal Patterns
df_idea4_long <- df_idea4 %>%
  pivot_longer(cols = -Month, names_to = "Trade_Type", values_to = "Value")

plot_4 <- ggplot(df_idea4_long, aes(x = Month, y = Value, color = Trade_Type, group = Trade_Type)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3.5) +
  theme_minimal() +
  labs(
    title = "Seasonal Patterns: Oil vs. Non-Oil Trade (2011-2025)",
    subtitle = "Historical monthly averages isolating cyclical spikes from baseline energy flows",
    x = "Month",
    y = "Average Volume (Dec 2025 USD Millions)"
  ) +
  scale_color_manual(name = "Trade Component:", 
                     labels = c("Non-Oil Exports", "Oil Exports", "Non-Oil Imports", "Oil Imports"),
                     values = c(
                       "Avg_Exp_Nonoil" = "#118AB2",  
                       "Avg_Exp_Oil"    = "#06D6A0",  
                       "Avg_Imp_Nonoil" = "#EF476F",  
                       "Avg_Imp_Oil"    = "#FFD166"   
                     )) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(plot_4)