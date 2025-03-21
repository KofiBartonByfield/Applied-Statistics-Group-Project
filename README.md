# README: The Hard Science of Soft Targets -- What Makes Terrorist Attacks Lethal in Europe

## Overview
This repository contains the code, data, and analysis for the Applied Statistical Analysis II group assignment investigating the factors influencing the lethality of terrorist attacks in Europe. Our research applies the Attack Contextual and Tactical InteractiON (ACTION) framework, which explores how environmental context, tactical decisions, and group experience contribute to variations in casualties and fatalities.

## Authors
- **Kofi Barton-Byfield** (24375742)
- **Ombeline Mussat** (24346050)
- **Eimhin O’Neill** (20332107)
- **Fionn O’Sullivan** (17326750)

## Research Abstract
While some terrorist attacks result in mass casualties, others lead to minimal harm. This study explores key factors influencing the lethality of terrorist attacks in Europe using data from the Global Terrorism Database. We test six hypotheses examining:
- Target type
- Attack location
- Seasonality
- Group experience
- Weapon selection

Our findings largely align with our predictions, with some results challenging aspects of our ACTION framework. These insights contribute to a deeper understanding of attack lethality and suggest areas for further research.

## Repository Structure
```
|-- data/                     # Contains datasets used for analysis
|   |-- global_terrorism.csv   # Extracted subset from the Global Terrorism Database
|   |-- Codebook.pdf
|-- code/                   # R scripts:
|   |-- Data Cleaning.py       # Prepares and cleans the dataset
|   |-- Regressions.R             # Statistical modelling 
|   |-- Visualisations.R       # Generates plots and tables 
|-- plots/               
|-- tables/    
|-- final_report.pdf       # Full write-up of the study
|-- README.md                  # Project documentation

```

## Data Source
The primary dataset used in this study is an extracted subset of the **Global Terrorism Database (GTD)**, which records incidents of terrorism worldwide. The dataset was filtered to focus on European attacks.

- **Download the dataset here**: [Global Terrorism Database](https://www.start.umd.edu/download-global-terrorism-database)

## Acknowledgements
We thank the providers of the **Global Terrorism Database (GTD)** for making their dataset publicly available, enabling this research.


