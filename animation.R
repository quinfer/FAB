library(tidyverse)
library(gganimate)
library(viridis)
fixed_inc <- read.table("slide_material/fixed_income.tsv", sep = "\t", header = TRUE)
equity <-read.table("slide_material/equity.tsv", sep = "\t", header = TRUE)
derivatives <-read.table("slide_material/derivatives.tsv", sep = "\t", header = TRUE)
# using R create a bar chart of the global capital markets using the data in the files fixed_income.tsv, equity.tsv, derivatives.tsv
fixed_inc |>
  select(Year,Total) |>
  rename("Fixed Income"=Total) |>
  left_join(
    equity |>
      select(Year,Total) |>
      rename("Equities"=Total), by="Year") |>
  left_join(derivatives,by="Year") |>
  select(!Total) ->totals
names(totals)[4:5]<-c("Exchange Traded Derivatives","OTC Derivatives")
totals |> 
  map_df(~as.numeric(str_remove_all(.x,","))) |>
  pivot_longer(!Year,values_to = "Value",names_to = "Asset Class") ->totals
totals <- totals %>%
  group_by(Year) %>%
  mutate(Rank = rank(-Value)) %>%
  ungroup()

# Assuming totals is your final data frame
totals <- totals %>%
  arrange(Year, `Asset Class`, Rank) %>%
  mutate(interval = ceiling(Value / 100*1000)) # Create intervals of 100

anim <- totals %>%
  filter(Year == 2022) %>%
  mutate(new_value = Value / 1000) %>%
  ggplot(aes(x = `Asset Class`, y = new_value, fill = `Asset Class`)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "USD$ Trillions", caption = "Source: Bank of International Settlements") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_states(
    interval, # Use the interval column for transitions
    transition_length = 2, # Adjust as needed
    state_length = 2       # Adjust as needed
  ) + ease_aes('linear') # Changed to linear for consistent animation speed
anim_save("animated_plot.gif", anim)
