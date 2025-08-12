## Theme:
 "The Impacts of Government Stability in Post-Conflict Settings: A Study of Economic Growth Using Nightime Lights Data in Liberia (1989-2013)"

 ## Featuring: 
 Alexis Rangel, Shannaya Raul, Mallory Sailer and Crystal Vinal

 ## 
 This is a Quasi-experimental project aiming to understand the effects of government stability and the election of President Ellen Johnson Sirleaf in 2005, after almost 14 years of civil war in Liberia (1989-2003), to the country's economic growth post-war.  

 ## Overview
 The project uses nightlights data from Harvard University's Growth Lab as an indicator of socioeconomic development in Liberia, given the absence of economic indicator's data, to perform a synthetic control analysis. 
 The analysis aimed at building a Synthetic Liberia without the war, by using a donor pool of countries with similar economic characteristics that had not gone through a war in the same timeline of interest, to infer Liberia's economic behavior absent the war compared to the real Liberia, with the war, before and after the treatment (governmental elections).
 Our results show that from 2003 to 2013 (Post-treatment period), real Liberia under a new government does better off economically, compared to synthetic Liberia meaning that a stable government had a positive effect on the country's economic growth.

 ## Tools & Libraries
 - R-Studio:
    - tidyverse
    - Synth
    - devtools
    - bcastanho/SCtools
    - haven
    - modelsummary
    - gt
    - gtable
    - gtsummary
    - ggplot
    - daggity
    - ggdist
    - knitr
    - kableExtra

## Project Structure

│
├── README.md         # Project description
├── scripts/           # Code file (.R )
├── data/              # Data files (Sources: World Bank Data, Harvard Growth Lab, UNDP)
└── output/            # Results/graphs

## How to Run
1. Clone this repository:
   ```bash
   git clone https://github.com/Ushannayaraul/Synthetic_Controls_Liberias_Economic_Growth_Post_War.git

Thank you for your time!
If you have any questions, feel free to contact me @ shannaya_raul@tamu.edu (Email) or https://www.linkedin.com/in/shannaya-raul/ (LinkedIn).
