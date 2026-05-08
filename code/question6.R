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

# 3. Load CPI and Calculate Base
cpi_data <- read.csv("CPIAUCSL.csv") %>%
  mutate(Date = as.Date(observation_date)) %>%
  select(Date, CPI_Value = CPIAUCSL)

base_cpi <- 326.031 # Dec 2025 Base

# 4. Prepare Data & Adjust for Inflation
df_idea6 <- df_clean %>%
  # Filter from 2011 to 2024 (Full Years)
  filter(Year >= 2011 & Year <= 2024) %>%
  mutate(Date_String = paste("01", Month, Year, sep = "-")) %>%
  mutate(Date = as.Date(Date_String, format = "%d-%B-%Y")) %>%
  left_join(cpi_data, by = "Date") %>%
  
  # Convert to Constant Dec 2025 USD
  mutate(
    Exp_Nonoil_Constant = Exports_Nonoil_USD * (base_cpi / CPI_Value),
    Imp_Nonoil_Constant = Imports_Nonoil_USD * (base_cpi / CPI_Value)
  ) %>%
  select(Date, Year, Exp_Nonoil_Constant, Imp_Nonoil_Constant) %>%
  drop_na() %>%
  arrange(Date)

# 5. CAGR Calculation on Constant Dollars
annual_idea6 <- df_idea6 %>%
  group_by(Year) %>%
  summarise(
    Total_Exp = sum(Exp_Nonoil_Constant, na.rm = TRUE),
    Total_Imp = sum(Imp_Nonoil_Constant, na.rm = TRUE)
  )

val_exp_2011 <- annual_idea6$Total_Exp[annual_idea6$Year == 2011]
val_exp_2024 <- annual_idea6$Total_Exp[annual_idea6$Year == 2024]
cagr_exp <- ((val_exp_2024 / val_exp_2011)^(1/13) - 1) * 100

val_imp_2011 <- annual_idea6$Total_Imp[annual_idea6$Year == 2011]
val_imp_2024 <- annual_idea6$Total_Imp[annual_idea6$Year == 2024]
cagr_imp <- ((val_imp_2024 / val_imp_2011)^(1/13) - 1) * 100

gap_2011 <- val_imp_2011 - val_exp_2011
gap_2024 <- val_imp_2024 - val_exp_2024
gap_growth <- ((gap_2024 / gap_2011) - 1) * 100

cat("\n--- CAGR ANALYSIS (CONSTANT USD) ---\n")
cat(sprintf("Non-Oil Exports CAGR (2011-2024): %.2f%%\n", cagr_exp))
cat(sprintf("Non-Oil Imports CAGR (2011-2024): %.2f%%\n", cagr_imp))
cat(sprintf("Physical Gap Expansion: %.2f%%\n", gap_growth))

# 6. Visualization 6: The Widening Gap
plot_6 <- ggplot(df_idea6, aes(x = Date)) +
  geom_ribbon(aes(ymin = Exp_Nonoil_Constant, ymax = Imp_Nonoil_Constant), 
              fill = "#EF476F", alpha = 0.25) +
  geom_line(aes(y = Imp_Nonoil_Constant, color = "Non-Oil Imports"), linewidth = 1.2) +
  geom_line(aes(y = Exp_Nonoil_Constant, color = "Non-Oil Exports"), linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "Manufacturing Self-Reliance vs. Import Dependency (2011-2024)",
    subtitle = "Analyzing the widening structural gap in core manufacturing (Dec 2025 USD)",
    x = "Year",
    y = "Trade Volume (Dec 2025 USD Millions)"
  ) +
  scale_color_manual(name = "Trade Flow:", 
                     values = c("Non-Oil Imports" = "#EF476F", 
                                "Non-Oil Exports" = "#118AB2")) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(plot_6)