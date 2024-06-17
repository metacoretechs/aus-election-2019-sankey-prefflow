library(plotly)
library(dplyr)
library(tibble)
library(stringr)
library(shiny)
library(networkD3)

div_dop <- read.csv("HouseDopByPPDownload-24310-VIC/HouseDopByPPDownload-24310-VIC-ASTO.csv", skip = 1)
colours <- read.csv("party_colours.csv")

# ======================================
# The AEC raw preference files are huge!
# The following code can be used to concatenate and summarise the files
# Download the files from https://results.aec.gov.au/24310/Website/HouseDownloadsMenu-24310-Csv.htm
# Unzip the files, then uncomment the code below and run it
# ======================================
#div_enrol <- read.csv("divenrol2019.csv")
#states <- div_enrol %>%
#  select(state) %>%
#  distinct()
#divs <- div_enrol %>%
#  select(state, division, aec_abbrev) %>%
#  distinct()

#div <- 1
#filename <- paste0("HouseDopByPPDownload-24310-", divs[div, 1],
#                   "/HouseDopByPPDownload-24310-", divs[div, 1], "-",
#                   divs[div, 3], ".csv")
#tempfile <- read.csv(filename, skip = 1) %>%
#  filter(CalculationType == "Preference Count" | CalculationType == "Transfer Count") %>%
#  select(DivisionNm, CountNum, Surname, GivenNm, PartyAb, PartyNm, CalculationType, CalculationValue) %>%
#  group_by(DivisionNm, CountNum, Surname, GivenNm, PartyAb, PartyNm, CalculationType) %>%
#  summarise(Votes = sum(CalculationValue))

#dop_summary <- tempfile

#for (div in 2:151) {
#
#  filename <- paste0("HouseDopByPPDownload-24310-", divs[div, 1],
#                     "/HouseDopByPPDownload-24310-", divs[div, 1], "-",
#                     divs[div, 3], ".csv")
#  tempfile <- read.csv(filename, skip = 1) %>%
#    filter(CalculationType == "Preference Count" | CalculationType == "Transfer Count") %>%
#    select(DivisionNm, CountNum, Surname, GivenNm, PartyAb, PartyNm, CalculationType, CalculationValue) %>%
#    group_by(DivisionNm, CountNum, Surname, GivenNm, PartyAb, PartyNm, CalculationType) %>%
#    summarise(Votes = sum(CalculationValue))
#  dop_summary <- bind_rows(dop_summary, tempfile)
#}
# write.csv(dop_summary, "dop_summary.csv", row.names = FALSE)
# ======================================

divs_dop <- read.csv("dop_summary.csv")

div_dop <- divs_dop %>%
  filter(DivisionNm == "Fraser")

dop_nodes <- div_dop %>%
  filter(CalculationType == "Preference Count") %>%
  filter(Votes > 0) %>%
  left_join(colours, by = c("PartyAb" = "aec_abbrev")) %>%
  arrange(CountNum, desc(Votes)) %>%
  rowid_to_column(var = "node_id") %>%
  mutate(node_id = node_id - 1) %>%
  mutate(label = paste0(str_to_title(Surname), " (", PartyAb, ")"))

dop_values <- div_dop %>%
  filter(CalculationType == "Transfer Count") %>%
  filter(Votes > 0)
  
values <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(values) <- c("source", "value", "target")



for (countnum in 0:(max(dop_nodes$CountNum))) {
  this_count <- dop_nodes %>%
    filter(CountNum == countnum)
  next_count <- dop_nodes %>%
    filter(CountNum == countnum + 1)
  transfer <- this_count %>%
    filter(node_id < max(node_id)) %>%
    inner_join(next_count, by = c("PartyAb", "Surname")) %>%
    select(source = node_id.x, value = Votes.x, target = node_id.y)
  excluded <- max(this_count$node_id)
  excluded_dist <- dop_values %>%
    ungroup() %>%
    filter(CountNum == countnum + 1) %>%
    inner_join(next_count, by = c("PartyAb", "Surname")) %>%
    arrange(node_id) %>%
    mutate(source = excluded, value = Votes.x) %>%
    select(source, value, target = node_id)
  values <- bind_rows(values, transfer, excluded_dist)
  }

# Plotly
# fig <- plot_ly(
#   type = "sankey",
#   orientation = "h",
#   valueformat = ".0f",
#   
#   node = list(
#     label = dop_nodes$label,
#     color = dop_nodes$colour_code,
#     #x = dop_nodes$CountNum,
#     y = dop_nodes$node_id,
#     pad = 15,
#     thickness = 20,
#     line = list(
#       color = "black",
#       width = 0.5
#     )
#   ),
#   
#   link = list(
#     source = values$source,
#     target = values$target,
#     value =  values$value
#   )
# )
# 
# fig

# networkD3
colrange = paste(dop_nodes$colour_code, collapse='","')
colscale = paste('d3.scaleOrdinal() .range(["',colrange,'"])')

pref_flows <- sankeyNetwork(Links = values, Nodes = dop_nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "label",
                    fontSize= 12, fontFamily = 'sans-serif', nodeWidth = 20, iterations = 0, sinksRight = TRUE,
                    colourScale = colscale,
                    height = 600, width = 1800)
pref_flows

fp <- dop_nodes %>%
  filter(CountNum == 0) %>%
  select(colour_code, label, Votes) %>%
  arrange(desc(Votes)) %>%
  mutate()

ggplot(fp, aes(x=reorder(label, Votes), y=Votes, fill = reorder(colour_code, -Votes))) +
  geom_col() +
  scale_fill_manual(name = "",
                     labels = fp$label,
                     values = fp$colour_code) +
  coord_flip() +
  labs(x="Candidate", y="Votes", title="First preference votes")