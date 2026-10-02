# NHL Player Performance & Tactical Archetypes Analysis 🏒

An end-to-end sports analytics project uncovering modern tactical player roles in the National Hockey League (2018–2019 season) using multivariate statistical techniques and an interactive R Shiny dashboard.

## 📌 Project Overview
Traditional player positions (Center, Winger, Defenseman) often fail to capture the true functional role of modern NHL skaters. This project leverages unsupervised learning to define data-driven tactical archetypes and analyze roster construction across franchises:
- **Data Pipeline & Preprocessing:** Aggregated game-by-game statistics to per-game skater averages, filtering out small sample sizes and treating missing values.
- **Dimensionality Reduction (PCA):** Reduced collinear performance metrics into 4 interpretable tactical axes explaining over 81% of total variance:
  - *PC1:* Star Status / Overall Volume (39.03%)
  - *PC2:* Tactical Specialization: Offensive vs. Defensive Sacrifice (19.68%)
  - *PC3:* Physical Aggressiveness & Discipline (13.16%)
  - *PC4:* Collective Impact Factor (Plus/Minus) (9.55%)
- **Clustering:** Applied K-Means ($k=6$) validated via silhouette analysis to establish 6 functional archetypes: *Benchwarmers*, *Blocking Specialists*, *Superstars*, *Elite Defensive Walls*, *Physical Specialists*, and *Rotation / Two-Way Players*.
- **Interactive Dashboard:** Built a customized R Shiny application with bespoke NHL styling to explore player radars, PCA biplots, roster archetype compositions, and a market scouting tool.

## 🚀 Interactive Shiny App
The dashboard allows users to:
1. **Explore Individual Radars:** Visualize standardized profiles against archetype benchmarks.
2. **Project onto PCA Space:** Analyze where skaters sit along tactical components.
3. **Compare Rosters:** Benchmark roster distributions (e.g., Presidents' Trophy winners vs. lower-tier teams).
4. **Market Finder:** Filter available players based on custom statistical thresholds.

### Running Locally
To launch the Shiny app locally:
```r
# Install required packages
install.packages(c("shiny", "shinydashboard", "tidyverse", "plotly", "DT", "FactoMineR", "factoextra", "scales"))

# Run the app
shiny::runApp("app.R")


├── app.R               # Main interactive Shiny application
├── datos_app.RData     # Lightweight precomputed models and datasets
├── www/                # Web assets (branding, styles, logos)
├── report/             # Full technical markdown report, HTML export, and figures
└── README.md           # Project documentation


🛠️ Built With

    Language: R

    Web App: R Shiny, shinydashboard, Plotly, DT

    Modeling & Statistics: FactoMineR, factoextra, cluster

    Visualization: ggplot2, scales