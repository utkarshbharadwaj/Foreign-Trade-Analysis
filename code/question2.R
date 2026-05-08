df_idea2 <- df_clean %>%
  filter(Year >= 2011) %>%
  mutate(Date_String = paste("01", Month, Year, sep = "-")) %>%
  mutate(Date = as.Date(Date_String, format = "%d-%B-%Y")) %>%
  left_join(cpi_data, by = "Date") %>%
  mutate(
    TB_Overall_Constant = TradeBalance_USD * (base_cpi / CPI_Value),
    TB_Nonoil_Constant = TradeBalance_Nonoil_USD * (base_cpi / CPI_Value)
  ) %>%
  select(Date, TB_Overall_Constant, TB_Nonoil_Constant) %>%
  drop_na() %>%
  arrange(Date)

df_idea2_long <- df_idea2 %>%
  pivot_longer(cols = c(TB_Overall_Constant, TB_Nonoil_Constant), 
               names_to = "Balance_Type", values_to = "Value")

plot_2 <- ggplot(df_idea2_long, aes(x = Date, y = Value, color = Balance_Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8, alpha = 0.6) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "The Energy Gap: Overall vs. Non-Oil Trade Balance (2011-2025)",
    subtitle = "Isolating the physical burden of crude oil imports",
    x = "Year",
    y = "Trade Balance (Dec 2025 USD Millions)"
  ) +
  scale_color_manual(name = "Trade Component:", 
                     labels = c("Non-Oil Balance", "Overall Balance (Incl. Oil)"),
                     values = c("TB_Nonoil_Constant" = "#06D6A0", 
                                "TB_Overall_Constant" = "#EF476F")) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14))

print(plot_2)