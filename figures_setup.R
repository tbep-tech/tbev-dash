library(sf)
library(tidyverse)
library(mapview)
library(RColorBrewer)
library(plotly)
library(leafpop)
library(gridExtra)
library(leafsync)


# Define color palette

tbepcols10 <- c("#00806E","#004F7E","#5C4A42","#958984","#D69C4E","#962D14","#352849","#9C974A","#78B7C5","#D8A499","#427355","#D67336","#7394D4","#F2AD00","#C93211","#E6A0C4","#090909")



#### Economic Impact Evaluation ####

EI_sector <- read.csv("data/EI_sector.csv")
EI_industry <- read.csv("data/EI_industry.csv")
EI_contribution <- read.csv("data/EI_contribution.csv")


#### * Total #####

# IMPACT: Output | CONTRIBUTION: Total
OutputTotal <- EI_sector %>%
  filter(Contribution == "Total") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
OutputTotal

# IMPACT: Output | CONTRIBUTION: Direct
OutputDirect <- EI_sector %>%
  filter(Contribution == "Direct") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
OutputDirect

# IMPACT: Output | CONTRIBUTION: Indirect
OutputIndirect <- EI_sector %>%
  filter(Contribution == "Indirect") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
OutputIndirect

# IMPACT: Output | CONTRIBUTION: Induced
OutputInduced <- EI_sector %>%
  filter(Contribution == "Induced") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
OutputInduced

# IMPACT: Income | CONTRIBUTION: Total
IncomeTotal <- EI_sector %>%
  filter(Contribution == "Total") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
IncomeTotal

# IMPACT: Income | CONTRIBUTION: Direct
IncomeDirect <- EI_sector %>%
  filter(Contribution == "Direct") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
IncomeDirect

# IMPACT: Income | CONTRIBUTION: Indirect
IncomeIndirect <- EI_sector %>%
  filter(Contribution == "Indirect") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
IncomeIndirect

# IMPACT: Income | CONTRIBUTION: Induced
IncomeInduced <- EI_sector %>%
  filter(Contribution == "Induced") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
IncomeInduced


# IMPACT: Employment | CONTRIBUTION: Total
EmploymentTotal <- EI_sector %>%
  filter(Contribution == "Total") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
EmploymentTotal

# IMPACT: Employment | CONTRIBUTION: Direct
EmploymentDirect <- EI_sector %>%
  filter(Contribution == "Direct") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
EmploymentDirect

# IMPACT: Employment | CONTRIBUTION: Indirect
EmploymentIndirect <- EI_sector %>%
  filter(Contribution == "Indirect") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
EmploymentIndirect

# IMPACT: Employment | CONTRIBUTION: Induced
EmploymentInduced <- EI_sector %>%
  filter(Contribution == "Induced") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Sector,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Sectors</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
EmploymentInduced



#### * Defense #####

# IMPACT: Output | ATTRIBUTE: Industry
Defense_OutputIndustry <- EI_industry %>%
  filter(Sector == "Defense") %>%
    plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Defense_OutputIndustry

# IMPACT: Output | ATTRIBUTE: Contribution
Defense_OutputContribution <- EI_sector %>%
  filter(Sector == "Defense" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Defense_OutputContribution

# IMPACT: Income | ATTRIBUTE: Industry
Defense_IncomeIndustry <- EI_industry %>%
  filter(Sector == "Defense") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Defense_IncomeIndustry

# IMPACT: Income | ATTRIBUTE: Contribution
Defense_IncomeContribution <- EI_sector %>%
  filter(Sector == "Defense" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Defense_IncomeContribution

# IMPACT: Employment | ATTRIBUTE: Industry
Defense_EmploymentIndustry <- EI_industry %>%
  filter(Sector == "Defense") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Defense_EmploymentIndustry

# IMPACT: Employment | ATTRIBUTE: Contribution
Defense_EmploymentContribution <- EI_sector %>%
  filter(Sector == "Defense" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Defense_EmploymentContribution

#### * Living Resources #####

# IMPACT: Output | ATTRIBUTE: Industry
Living.Resources_OutputIndustry <- EI_industry %>%
  filter(Sector == "Living Resources") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Living.Resources_OutputIndustry


# IMPACT: Output | ATTRIBUTE: Contribution
Living.Resources_OutputContribution <- EI_sector %>%
  filter(Sector == "Living Resources" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Living.Resources_OutputContribution

# IMPACT: Income | ATTRIBUTE: Industry
Living.Resources_IncomeIndustry <- EI_industry %>%
  filter(Sector == "Living Resources") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Living.Resources_IncomeIndustry

# IMPACT: Income | ATTRIBUTE: Contribution
Living.Resources_IncomeContribution <- EI_sector %>%
  filter(Sector == "Living Resources" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Living.Resources_IncomeContribution

# IMPACT: Employment | ATTRIBUTE: Industry
Living.Resources_EmploymentIndustry <- EI_industry %>%
  filter(Sector == "Living Resources") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Living.Resources_EmploymentIndustry

# IMPACT: Employment | ATTRIBUTE: Contribution
Living.Resources_EmploymentContribution <- EI_sector %>%
  filter(Sector == "Living Resources" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Living.Resources_EmploymentContribution

#### * Marine Construction #####

# IMPACT: Output | ATTRIBUTE: Industry
Marine.Construction_OutputIndustry <- EI_industry %>%
  filter(Sector == "Marine Construction") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Construction_OutputIndustry

# IMPACT: Output | ATTRIBUTE: Contribution
Marine.Construction_OutputContribution <- EI_sector %>%
  filter(Sector == "Marine Construction" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Construction_OutputContribution

# IMPACT: Income | ATTRIBUTE: Industry
Marine.Construction_IncomeIndustry <- EI_industry %>%
  filter(Sector == "Marine Construction") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Construction_IncomeIndustry

# IMPACT: Income | ATTRIBUTE: Contribution
Marine.Construction_IncomeContribution <- EI_sector %>%
  filter(Sector == "Marine Construction" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Construction_IncomeContribution

# IMPACT: Employment | ATTRIBUTE: Industry
Marine.Construction_EmploymentIndustry <- EI_industry %>%
  filter(Sector == "Marine Construction") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Construction_EmploymentIndustry

# IMPACT: Employment | ATTRIBUTE: Contribution
Marine.Construction_EmploymentContribution <- EI_sector %>%
  filter(Sector == "Marine Construction" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Construction_EmploymentContribution

#### * Marine Transportation #####

# IMPACT: Output | ATTRIBUTE: Industry
Marine.Transportation_OutputIndustry <- EI_industry %>%
  filter(Sector == "Marine Transportation") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Transportation_OutputIndustry

# IMPACT: Output | ATTRIBUTE: Contribution
Marine.Transportation_OutputContribution <- EI_sector %>%
  filter(Sector == "Marine Transportation" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Transportation_OutputContribution

# IMPACT: Income | ATTRIBUTE: Industry
Marine.Transportation_IncomeIndustry <- EI_industry %>%
  filter(Sector == "Marine Transportation") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Transportation_IncomeIndustry

# IMPACT: Income | ATTRIBUTE: Contribution
Marine.Transportation_IncomeContribution <- EI_sector %>%
  filter(Sector == "Marine Transportation" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Transportation_IncomeContribution

# IMPACT: Employment | ATTRIBUTE: Industry
Marine.Transportation_EmploymentIndustry <- EI_industry %>%
  filter(Sector == "Marine Transportation") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Transportation_EmploymentIndustry

# IMPACT: Employment | ATTRIBUTE: Contribution
Marine.Transportation_EmploymentContribution <- EI_sector %>%
  filter(Sector == "Marine Transportation" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Marine.Transportation_EmploymentContribution

#### * Mineral Extraction #####

# IMPACT: Output | ATTRIBUTE: Industry
Mineral.Extraction_OutputIndustry <- EI_industry %>%
  filter(Sector == "Mineral Extraction") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Mineral.Extraction_OutputIndustry

# IMPACT: Output | ATTRIBUTE: Contribution
Mineral.Extraction_OutputContribution <- EI_sector %>%
  filter(Sector == "Mineral Extraction" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Mineral.Extraction_OutputContribution

# IMPACT: Income | ATTRIBUTE: Industry
Mineral.Extraction_IncomeIndustry <- EI_industry %>%
  filter(Sector == "Mineral Extraction") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Mineral.Extraction_IncomeIndustry

# IMPACT: Income | ATTRIBUTE: Contribution
Mineral.Extraction_IncomeContribution <- EI_sector %>%
  filter(Sector == "Mineral Extraction" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Mineral.Extraction_IncomeContribution

# IMPACT: Employment | ATTRIBUTE: Industry
Mineral.Extraction_EmploymentIndustry <- EI_industry %>%
  filter(Sector == "Mineral Extraction") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Mineral.Extraction_EmploymentIndustry

# IMPACT: Employment | ATTRIBUTE: Contribution
Mineral.Extraction_EmploymentContribution <- EI_sector %>%
  filter(Sector == "Mineral Extraction" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Mineral.Extraction_EmploymentContribution

#### * Research #####

# IMPACT: Output | ATTRIBUTE: Industry
Research_OutputIndustry <- EI_industry %>%
  filter(Sector == "Research") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Research_OutputIndustry

# IMPACT: Output | ATTRIBUTE: Contribution
Research_OutputContribution <- EI_sector %>%
  filter(Sector == "Research" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Research_OutputContribution

# IMPACT: Income | ATTRIBUTE: Industry
Research_IncomeIndustry <- EI_industry %>%
  filter(Sector == "Research") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Research_IncomeIndustry

# IMPACT: Income | ATTRIBUTE: Contribution
Research_IncomeContribution <- EI_sector %>%
  filter(Sector == "Research" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Research_IncomeContribution

# IMPACT: Employment | ATTRIBUTE: Industry
Research_EmploymentIndustry <- EI_industry %>%
  filter(Sector == "Research") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Research_EmploymentIndustry

# IMPACT: Employment | ATTRIBUTE: Contribution
Research_EmploymentContribution <- EI_sector %>%
  filter(Sector == "Research" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Research_EmploymentContribution

#### * Ship and Boat Building #####

# IMPACT: Output | ATTRIBUTE: Industry
Ship.and.Boat.Building_OutputIndustry <- EI_industry %>%
  filter(Sector == "Shipbuilding") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Ship.and.Boat.Building_OutputIndustry

# IMPACT: Output | ATTRIBUTE: Contribution
Ship.and.Boat.Building_OutputContribution <- EI_sector %>%
  filter(Sector == "Ship and Boat Building" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Ship.and.Boat.Building_OutputContribution

# IMPACT: Income | ATTRIBUTE: Industry
Ship.and.Boat.Building_IncomeIndustry <- EI_industry %>%
  filter(Sector == "Shipbuilding") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Ship.and.Boat.Building_IncomeIndustry

# IMPACT: Income | ATTRIBUTE: Contribution
Ship.and.Boat.Building_IncomeContribution <- EI_sector %>%
  filter(Sector == "Ship and Boat Building" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Ship.and.Boat.Building_IncomeContribution

# IMPACT: Employment | ATTRIBUTE: Industry
Ship.and.Boat.Building_EmploymentIndustry <- EI_industry %>%
  filter(Sector == "Shipbuilding") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Ship.and.Boat.Building_EmploymentIndustry

# IMPACT: Employment | ATTRIBUTE: Contribution
Ship.and.Boat.Building_EmploymentContribution <- EI_sector %>%
  filter(Sector == "Ship and Boat Building" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Ship.and.Boat.Building_EmploymentContribution

#### * Tourism and Recreation #####

# IMPACT: Output | ATTRIBUTE: Industry
Tourism.and.Recreation_OutputIndustry <- EI_industry %>%
  filter(Sector == "Tourism & Recreation") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Tourism.and.Recreation_OutputIndustry

# IMPACT: Output | ATTRIBUTE: Contribution
Tourism.and.Recreation_OutputContribution <- EI_sector %>%
  filter(Sector == "Tourism and Recreation" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Output,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Output/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Tourism.and.Recreation_OutputContribution

# IMPACT: Income | ATTRIBUTE: Industry
Tourism.and.Recreation_IncomeIndustry <- EI_industry %>%
  filter(Sector == "Tourism & Recreation") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Tourism.and.Recreation_IncomeIndustry

# IMPACT: Income | ATTRIBUTE: Contribution
Tourism.and.Recreation_IncomeContribution <- EI_sector %>%
  filter(Sector == "Tourism and Recreation" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Personal.Income,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text = ~paste("$", round(Personal.Income/1000000000, digits = 2), "billion")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Tourism.and.Recreation_IncomeContribution

# IMPACT: Employment | ATTRIBUTE: Industry
Tourism.and.Recreation_EmploymentIndustry <- EI_industry %>%
  filter(Sector == "Tourism & Recreation") %>%
  plot_ly(width = 800, height = 350,
    labels = ~Industry,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Industries</b>", side = "top"),
      orientation = "v"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Tourism.and.Recreation_EmploymentIndustry

# IMPACT: Employment | ATTRIBUTE: Contribution
Tourism.and.Recreation_EmploymentContribution <- EI_sector %>%
  filter(Sector == "Tourism and Recreation" & Contribution %in% c("Direct","Indirect","Induced")) %>%
  plot_ly(width = 800, height = 350,
    labels = ~Contribution,
    values = ~Employment,
    type = "pie",
    sort = FALSE,
    direction = "clockwise",
    marker = list(colors = tbepcols10),
    textinfo = "percent",
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = "text",
    text =~paste(prettyNum(round(Employment, digits = 0), big.mark = ","), "jobs")) %>%
  layout(
    legend = list(
      title = list(text = "<b>Contributions</b>", side = "top"),
      orientation = "v",
      x = 0.3,
      y = -100),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Tourism.and.Recreation_EmploymentContribution


layout(
  legend = list(
    title = list(text = "<b>Contributions</b>", side = "top"),
    orientation = "v",
    valign = "top"),
  hoverlabel = list(
    font = list(family = "Arial", size = 16, color = "white")))


#### Property Values ####


load(file = "data/shoreline.RData")

mapview(watershed, color = "black", lwd = 2, legend = FALSE, label = "Name", popup = FALSE) +
  mapview(shoreline, col.regions = "#00806E", layer.name = "Shoreline (0.25 mile buffer)", label = FALSE, popup = FALSE)

PV_values <- read.csv("data/PV_values.csv")

PropertyValues1 <- PV_values %>%
  plot_ly(
    width = 900,
    height = 410,
    x = ~Indicator,
    y = ~Number.of.properties,
    type = "bar",
    marker = list(color = c("#00806E","#004F7E")),
    hoverinfo = "text",
    text = ~Properties) %>%
  layout(
    #title = "Properties Included in Model",
    yaxis = list(title = "Number of properties close to coast"),
    xaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

PropertyValues2 <- PV_values %>%
  plot_ly(
    width = 900,
    height = 410,
    x = ~Indicator,
    y = ~Value.added.to.each.nearby.home,
    type = "bar",
    marker = list(color = c("#00806E","#004F7E")),
    hoverinfo = "text",
    text = ~Value) %>%
  layout(
    #title = "Benefit per Property",
    yaxis = list(title = "Value added to each home ($)", tick0 = 0, dtick = 20000),
    xaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

PropertyValues3 <- PV_values %>%
  plot_ly(
    width = 900,
    height = 410,
    x = ~Indicator,
    y = ~Added.regional.value..billion.,
    type = "bar",
    marker = list(color = c("#00806E","#004F7E")),
    hoverinfo = "text",
    text = ~Benefit) %>%
  layout(
    #title = "Total Regional Benefit",
    yaxis = list(title = "Value to all homes near coast ($ billion)"),
    xaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

PropertyValues <- subplot(PropertyValues1, PropertyValues2, PropertyValues3, nrows = 1,
                          titleY = TRUE, titleX = FALSE, margin = 0.06) %>%
  layout(showlegend = FALSE,
         annotations = list(
           list(x = 0.05, y = 1.09, text = "Properties Included", showarrow = F, xref='paper', yref='paper', font = list(size = 16)),
           list(x = 0.5, y = 1.09, text = "Benefit per Property", showarrow = F, xref='paper', yref='paper', font = list(size = 16)),
           list(x = 0.98, y = 1.09, text = "Total Regional Benefit", showarrow = F, xref='paper', yref='paper', font = list(size = 16))))
PropertyValues

#### Ecosystem Service Evaluation ####


#### * All #####


load(file = "data/habitats.RData")
load(file = "data/watershed.RData")

#habitats <- habitats %>%
#  mutate(acres = prettyNum(round(as.numeric(Acreage),digits = 0),big.mark = ","),
#         Area = paste(as.character(acres), " acres"),
#         Sequestration = paste(C_value, " in annual carbon sequestration services"),
#         Denitrification = paste(N_value, " in annual denitrification services"))

#save(habitats, file = "data/habitats.RData")


mapview(watershed, zcol = "Name", color = "black", lwd = 2, legend = FALSE, popup = FALSE) +
  mapview(habitats, zcol = "Habitat", col.regions = c("#004F7E","#9C974A","#962D14","#5C4A42","#78B7C5","#00806E","#F2AD00"),
        alpha.regions = 1.0, lwd = 0, layer.name = "Habitats", popup = popupTable(habitats,
                                                                                  zcol = c("Habitat","Area","Sequestration","Denitrification"),
                                                                                  feature.id = FALSE,
                                                                                  row.numbers = FALSE))


#### * Carbon Sequestration #####

ES_carbon <- read.csv("data/ES_carbon.csv")

# SCALE: Per-acre | FLOW: Sequestration

CarbonSeq_PerAcre1 <- ES_carbon %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Per.acre.Sequestration..tonnes.CO2.ac.yr.,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#5C4A42","#00806E","#004F7E","#F2AD00")),
    hoverinfo = "text",
    text = ~paste(Per.acre.Sequestration..tonnes.CO2.ac.yr., "tonnes CO2 per acre annually")) %>%
  layout(
    xaxis = list(title = "tonnes CO2/ac/yr"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

CarbonSeq_PerAcre2 <- ES_carbon %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Per.acre.Sequestration.Value....ac.,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#5C4A42","#00806E","#004F7E","#F2AD00")),
    hoverinfo = "text",
    text = ~paste("$", prettyNum(round(Per.acre.Sequestration.Value....ac., digits = 0)), "per acre annually")) %>%
  layout(
    xaxis = list(title = "$/ac/yr"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

CarbonSeq_PerAcre <- subplot(CarbonSeq_PerAcre1, CarbonSeq_PerAcre2, nrows = 1,
                             titleX = TRUE, titleY = FALSE, margin = 0.09) %>%
  layout(showlegend = FALSE,
         annotations = list(
           list(x = 0.05, y = 1.1, text = "Carbon Sequestration Rate", showarrow = F, xref='paper', yref='paper', font = list(size = 16)),
           list(x = 0.94, y = 1.1, text = "Value of Carbon Sequestered", showarrow = F, xref='paper', yref='paper', font = list(size = 16))))
CarbonSeq_PerAcre

# SCALE: Per-acre | FLOW: Stock

CarbonStock_PerAcre1 <- ES_carbon %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Per.acre.Stock..tonnes.CO2.ac.,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#5C4A42","#00806E","#004F7E","#F2AD00")),
    hoverinfo = "text",
    text = ~paste(prettyNum(round(Per.acre.Stock..tonnes.CO2.ac., digits = 0)), "tonnes CO2 per acre")) %>%
  layout(
    xaxis = list(title = "tonnes CO2/ac"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

CarbonStock_PerAcre2 <- ES_carbon %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Per.acre.Stock.Value....ac.,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#5C4A42","#00806E","#004F7E","#F2AD00")),
    hoverinfo = "text",
    text = ~paste("$", prettyNum(round(Per.acre.Stock.Value....ac., digits = 0), big.mark = ","), "per acre")) %>%
  layout(
    xaxis = list(title = "$/ac"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

CarbonStock_PerAcre <- subplot(CarbonStock_PerAcre1, CarbonStock_PerAcre2, nrows = 1,
                             titleX = TRUE, titleY = FALSE, margin = 0.09) %>%
  layout(showlegend = FALSE,
         annotations = list(
           list(x = 0.05, y = 1.1, text = "Current Carbon Stock", showarrow = F, xref='paper', yref='paper', font = list(size = 16)),
           list(x = 0.94, y = 1.1, text = "Value of Current Carbon Stock", showarrow = F, xref='paper', yref='paper', font = list(size = 16))))
CarbonStock_PerAcre


# SCALE: Total | FLOW: Sequestration

CarbonSeq_Total1 <- ES_carbon %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Total.Sequestration..tonnes.CO2.yr.,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#5C4A42","#00806E","#004F7E","#F2AD00")),
    hoverinfo = "text",
    text = ~paste(prettyNum(round(Total.Sequestration..tonnes.CO2.yr., digits = 0), big.mark = ","), "tonnes CO2 annually")) %>%
  layout(
    xaxis = list(title = "tonnes CO2/yr"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

CarbonSeq_Total2 <- ES_carbon %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Total.Sequestration.Value....,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#5C4A42","#00806E","#004F7E","#F2AD00")),
    hoverinfo = "text",
    text = ~paste("$", prettyNum(round(Total.Sequestration.Value...., digits = 0), big.mark = ","), "annually")) %>%
  layout(
    xaxis = list(title = "$/yr"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

CarbonSeq_Total <- subplot(CarbonSeq_Total1, CarbonSeq_Total2, nrows = 1,
                           titleX = TRUE, titleY = FALSE, margin = 0.09) %>%
  layout(showlegend = FALSE,
         annotations = list(
           list(x = 0.05, y = 1.1, text = "Carbon Sequestration Rate", showarrow = F, xref='paper', yref='paper', font = list(size = 16)),
           list(x = 0.94, y = 1.1, text = "Value of Carbon Sequestered", showarrow = F, xref='paper', yref='paper', font = list(size = 16))))
CarbonSeq_Total


# SCALE: Total | FLOW: Stock

CarbonStock_Total1 <- ES_carbon %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Total.Stock..tonnes.CO2.,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#5C4A42","#00806E","#004F7E","#F2AD00")),
    hoverinfo = "text",
    text = ~paste(prettyNum(round(Total.Stock..tonnes.CO2., digits = 0), big.mark = ","), "tonnes CO2")) %>%
  layout(
    xaxis = list(title = "tonnes CO2"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

CarbonStock_Total2 <- ES_carbon %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Total.Stock.Value....,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#5C4A42","#00806E","#004F7E","#F2AD00")),
    hoverinfo = "text",
    text = ~paste("$",prettyNum(round(Total.Stock.Value...., digits = 0), big.mark = ","))) %>%
  layout(
    xaxis = list(title = "$"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

CarbonStock_Total <- subplot(CarbonStock_Total1, CarbonStock_Total2, nrows = 1,
                               titleX = TRUE, titleY = FALSE, margin = 0.09) %>%
  layout(showlegend = FALSE,
         annotations = list(
           list(x = 0.05, y = 1.1, text = "Current Carbon Stock", showarrow = F, xref='paper', yref='paper', font = list(size = 16)),
           list(x = 0.94, y = 1.1, text = "Value of Current Carbon Stock", showarrow = F, xref='paper', yref='paper', font = list(size = 16))))
CarbonStock_Total





#### * Denitrification #####

ES_denitrification <- read.csv("data/ES_denitrification.csv")


# SCALE: Per-acre

Denitrification_PerAcre1 <- ES_denitrification %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Per.acre.Denitrification..kg.N.ac.yr.,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#962D14","#00806E","#004F7E")),
    hoverinfo = "text",
    text = ~paste(Per.acre.Denitrification..kg.N.ac.yr., "kg N per acre annually")) %>%
  layout(
    xaxis = list(title = "kg N/ac/yr"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Denitrification_PerAcre1

Denitrification_PerAcre2 <- ES_denitrification %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Per.acre.Denitrification.Value....ac.,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#962D14","#00806E","#004F7E")),
    hoverinfo = "text",
    text = ~paste("$", prettyNum(round(Per.acre.Denitrification.Value....ac., digits = 0), big.mark = ","), "per acre annually")) %>%
  layout(
    xaxis = list(title = "$/ac/yr"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Denitrification_PerAcre2

Denitrification_PerAcre <- subplot(Denitrification_PerAcre1, Denitrification_PerAcre2, nrows = 1,
                             titleX = TRUE, titleY = FALSE, margin = 0.09) %>%
  layout(showlegend = FALSE,
         annotations = list(
           list(x = 0.05, y = 1.1, text = "Denitrification Rate", showarrow = F, xref='paper', yref='paper', font = list(size = 16)),
           list(x = 0.94, y = 1.1, text = "Value of Denitrification Services", showarrow = F, xref='paper', yref='paper', font = list(size = 16))))
Denitrification_PerAcre



# SCALE: Total

Denitrification_Total1 <- ES_denitrification %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Total.Denitrification..kg.N.yr.,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#962D14","#00806E","#004F7E")),
    hoverinfo = "text",
    text = ~paste(prettyNum(round(Total.Denitrification..kg.N.yr., digits = 0), big.mark = ","), "kg N annually")) %>%
  layout(
    xaxis = list(title = "kg N/yr"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Denitrification_Total1

Denitrification_Total2 <- ES_denitrification %>%
  plot_ly(
    width = 900,
    height = 325,
    y = ~Habitat,
    x = ~Total.Denitrification.Value....,
    type = "bar",
    orientation = "h",
    marker = list(color = c("#9C974A","#78B7C5","#962D14","#00806E","#004F7E")),
    hoverinfo = "text",
    text = ~paste("$",prettyNum(round(Total.Denitrification.Value...., digits = 0), big.mark = ","), "annually")) %>%
  layout(
    xaxis = list(title = "$/yr"),
    yaxis = list(title = ""),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))
Denitrification_Total2

Denitrification_Total <- subplot(Denitrification_Total1, Denitrification_Total2, nrows = 1,
                                   titleX = TRUE, titleY = FALSE, margin = 0.09) %>%
  layout(showlegend = FALSE,
         annotations = list(
           list(x = 0.05, y = 1.1, text = "Denitrification Rate", showarrow = F, xref='paper', yref='paper', font = list(size = 16)),
           list(x = 0.96, y = 1.1, text = "Value of Denitrification Services", showarrow = F, xref='paper', yref='paper', font = list(size = 16))))
Denitrification_Total


#### * Flood Protection #####

ES_flood <- read.csv("data/ES_flood.csv")

Flood_Parcels <- ES_flood %>%
  plot_ly(
    width = 900,
    height = 400,
    x = ~County,
    y = ~Number.of.Protected.Parcels,
    type = "bar",
    #marker = list(color = c("#00806E","#004F7E")),
    transforms = list(list(type = "groupby", groups = ~Habitat, styles = list(
      list(target = "Wetlands", value = list(marker = list(color = "#004F7E"))),
      list(target = "Mangroves", value = list(marker = list(color = "#9C974A"))),
      list(target = "All habitats", value = list(marker = list(color = "#000000")))
    ))),
    hoverinfo = "text",
    text = paste(ES_flood$Habitat, ":<br>",
                 prettyNum(round(ES_flood$Number.of.Protected.Parcels, digits = 0), big.mark = ","),
                 "parcels protected")) %>%
  layout(
    #title = "Properties Included in Model",
    yaxis = list(title = "Number of parcels"),
    xaxis = list(title = "County"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

Flood_Values <- ES_flood %>%
  plot_ly(
    width = 900,
    height = 400,
    x = ~County,
    y = ~Property.Values.in.Flood.Zone,
    type = "bar",
    #marker = list(color = c("#00806E","#004F7E")),
    transforms = list(list(type = "groupby", groups = ~Habitat, styles = list(
      list(target = "Wetlands", value = list(marker = list(color = "#004F7E"))),
      list(target = "Mangroves", value = list(marker = list(color = "#9C974A"))),
      list(target = "All habitats", value = list(marker = list(color = "#000000")))
    ))),
    hoverinfo = "text",
    text = paste(ES_flood$Habitat, ":<br>",
                 ES_flood$Values, "total property value")) %>%
  layout(
    #title = "Properties Included in Model",
    yaxis = list(title = "Property values ($)"),
    xaxis = list(title = "County"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

Flood_Benefits <- ES_flood %>%
  plot_ly(
    width = 900,
    height = 400,
    x = ~County,
    y = ~Total.Flood.Protection.Benefits,
    type = "bar",
    #marker = list(color = c("#00806E","#004F7E")),
    transforms = list(list(type = "groupby", groups = ~Habitat, styles = list(
      list(target = "Wetlands", value = list(marker = list(color = "#004F7E"))),
      list(target = "Mangroves", value = list(marker = list(color = "#9C974A"))),
      list(target = "All habitats", value = list(marker = list(color = "#000000")))
    ))),
    hoverinfo = "text",
    text = paste(ES_flood$Habitat, ":<br>",
                 ES_flood$Benefits, "in benefits")) %>%
  layout(
    #title = "Properties Included in Model",
    yaxis = list(title = "Total benefits ($)"),
    xaxis = list(title = "County"),
    hoverlabel = list(
      font = list(family = "Arial", size = 16, color = "white")))

FloodProtection <- subplot(Flood_Parcels, Flood_Values, Flood_Benefits, nrows = 1,
                           titleY = TRUE, titleX = TRUE, margin = 0.06) %>%
  layout(showlegend = FALSE,
         annotations = list(
           list(x = 0.04, y = 1.09, text = "Protected Properties", showarrow = F, xref='paper', yref='paper', font = list(size = 16)),
           list(x = 0.5, y = 1.09, text = "Property Values in Adjacent Flood Zone", showarrow = F, xref='paper', yref='paper', font = list(size = 16)),
           list(x = 0.99, y = 1.09, text = "Flood Protection Benefit", showarrow = F, xref='paper', yref='paper', font = list(size = 16))))
FloodProtection


# Map

load(file = "data/wetlands.RData")
load(file = "data/mangroves.RData")
load(file = "data/parcels.RData")


parcelmap <- parcels %>%
  filter(FloodZone != "No intersect project Flood Zones") %>%
  mapview(zcol = "FloodZone", col.regions = c("#D69C4E","#5C4A42"), alpha.regions = 1.0, layer.name = "Protected Parcels", lwd = 0,
                     popup = popupTable(parcels,
                                        zcol = "Habitat",
                                        feature.id = FALSE,
                                        row.numbers = FALSE))

habitatmaps <- mapview(wetlands, col.regions = "#004F7E", lwd = 0, alpha.regions = 1.0, layer.name = "Wetlands", label = "FLUCCSDESC", popup = FALSE) +
  mapview(mangroves, col.regions = "#9C974A", lwd = 0, alpha.regions = 1.0, layer.name = "Mangroves", label = "FLUCCSDESC", popup = FALSE)

sync(habitatmaps, parcelmap)



