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


# SCALE: Per-acre
Carbon_PerAcre1 <- ggplot(ES_carbon, aes(x = Habitat, y = Per.acre.Sequestration..tonnes.CO2.ac.yr., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Annual carbon sequestration rate (tonnes CO2/ac/yr)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,2.5)) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#5C4A42","#78B7C5","#00806E","#F2AD00"))
Carbon_PerAcre1

Carbon_PerAcre2 <- ggplot(ES_carbon, aes(x = Habitat, y = Per.acre.Sequestration.Value....ac., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Annual value of carbon sequestered ($/ac/yr)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,250)) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#5C4A42","#78B7C5","#00806E","#F2AD00"))
Carbon_PerAcre2

Carbon_PerAcre3 <- ggplot(ES_carbon, aes(x = Habitat, y = Per.acre.Stock..tonnes.CO2.ac., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Current carbon stock (tonnes CO2/ac)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,350)) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#5C4A42","#78B7C5","#00806E","#F2AD00"))
Carbon_PerAcre3

Carbon_PerAcre4 <- ggplot(ES_carbon, aes(x = Habitat, y = Per.acre.Stock.Value....ac., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Value of current carbon stock ($/ac)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,30000), labels = scales::comma) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#5C4A42","#78B7C5","#00806E","#F2AD00"))
Carbon_PerAcre4

grid.arrange(Carbon_PerAcre1, Carbon_PerAcre2, Carbon_PerAcre3, Carbon_PerAcre4, ncol = 2, nrow = 2)


# SCALE: Total
Carbon_Total1 <- ggplot(ES_carbon, aes(x = Habitat, y = Total.Sequestration..tonnes.CO2.yr., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Annual carbon sequestration rate (tonnes CO2/yr)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,500000), labels = scales::comma) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#5C4A42","#78B7C5","#00806E","#F2AD00"))
Carbon_Total1

Carbon_Total2 <- ggplot(ES_carbon, aes(x = Habitat, y = Total.Sequestration.Value...., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Annual value of carbon sequestered ($ million/yr)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,50000000), labels = function(x)x/1000000) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#5C4A42","#78B7C5","#00806E","#F2AD00"))
Carbon_Total2

Carbon_Total3 <- ggplot(ES_carbon, aes(x = Habitat, y = Total.Stock..tonnes.CO2., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Current carbon stock (million tonnes CO2)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,40000000), labels = function(x)x/1000000) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#5C4A42","#78B7C5","#00806E","#F2AD00"))
Carbon_Total3

Carbon_Total4 <- ggplot(ES_carbon, aes(x = Habitat, y = Total.Stock.Value...., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Value of current carbon stock ($ billion)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,4000000000), labels = function(x)x/1000000000) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#5C4A42","#78B7C5","#00806E","#F2AD00"))
Carbon_Total4

grid.arrange(Carbon_Total1, Carbon_Total2, Carbon_Total3, Carbon_Total4, ncol = 2, nrow = 2)



#### * Denitrification #####

ES_denitrification <- read.csv("data/ES_denitrification.csv")


# SCALE: Per-acre
Denitrification_PerAcre1 <- ggplot(ES_denitrification, aes(x = Habitat, y = Per.acre.Denitrification..kg.N.ac.yr., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Annual denitrification rate (kg N/ac/yr)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,40)) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#962D14","#78B7C5","#00806E"))
Denitrification_PerAcre1


Denitrification_PerAcre2 <- ggplot(ES_denitrification, aes(x = Habitat, y = Per.acre.Denitrification.Value....ac., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Annual value of denitrification services ($/ac/yr)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,4000), labels = scales::comma) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#962D14","#78B7C5","#00806E"))
Denitrification_PerAcre2

grid.arrange(Denitrification_PerAcre1, Denitrification_PerAcre2, ncol = 2, nrow = 1)


# SCALE: Total
Denitrification_Total1 <- ggplot(ES_denitrification, aes(x = Habitat, y = Total.Denitrification..kg.N.yr., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Annual denitrification rate (million kg N/yr)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,6000000), labels = function(x)x/1000000) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#962D14","#78B7C5","#00806E"))
Denitrification_Total1

Denitrification_Total2 <- ggplot(ES_denitrification, aes(x = Habitat, y = Total.Denitrification.Value...., fill = Habitat)) +
  geom_bar(stat = "identity") +
  ylab("Annual value of denitrification services ($ million/yr)\n") +
  theme(legend.position = "none",
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "gray85")) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(position = "right", limits = c(0,600000000), labels = function(x)x/1000000) +
  scale_fill_manual(values = c("#004F7E","#9C974A","#962D14","#78B7C5","#00806E"))
Denitrification_Total2

grid.arrange(Denitrification_Total1, Denitrification_Total2, ncol = 2, nrow = 1)



#### * Flood Protection #####

ES_flood <- read.csv("data/ES_flood.csv")

Flood_Parcels <-
  ggplot(ES_flood, aes(x = County, y = Number.of.Protected.Parcels, fill = Habitat)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Protected Parcels") +
  ylab("Number of parcels\n") +
  xlab("\nCounty") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.y = unit(.3, "cm"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "gray85")) +
  scale_y_continuous(limits = c(0,9000), labels = scales::comma) +
  scale_fill_manual(values = c("#000000","#9C974A","#004F7E"))
Flood_Parcels

Flood_Values <-
  ggplot(ES_flood, aes(x = County, y = Property.Values.in.Flood.Zone, fill = Habitat)) +
    geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Property Values in Adjacent Flood Zone") +
  ylab("Property values ($ billion)\n") +
    xlab("\nCounty") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin (.5,.5,.5,.5, "cm"),
        axis.title.y = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.length.y = unit(.3, "cm"),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour = "gray85")) +
    scale_y_continuous(limits = c(0,7500000000), labels = function(x)x/1000000000) +
    scale_fill_manual(values = c("#000000","#9C974A","#004F7E"))
Flood_Values

Flood_Benefits <-
  ggplot(ES_flood, aes(x = County, y = Total.Flood.Protection.Benefits, fill = Habitat)) +
    geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Flood Protection Benefits") +
  ylab("Total benefits ($ million)\n") +
    xlab("\nCounty") +
    theme(legend.position = c(0.85,0.65),
          legend.title = element_blank(),
          legend.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.margin = margin (.5,.5,.5,.5, "cm"),
          axis.title.y = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.length.y = unit(.3, "cm"),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour = "gray85")) +
    scale_y_continuous(limits = c(0,950000000), labels = function(x)x/1000000) +
    scale_fill_manual(values = c("#000000","#9C974A","#004F7E"))
Flood_Benefits

grid.arrange(Flood_Parcels, Flood_Values, Flood_Benefits, ncol = 3, nrow = 1)

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



