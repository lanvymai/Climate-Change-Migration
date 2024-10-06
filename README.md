# Climate Change and Extreme Weather Effects on Internal Migration: A Global Perspective

This project investigates the impact of climate change and extreme weather events on internal migration patterns across the globe.

## Tools Used

- RStudio
- Excel
- Panel linear regression
- Generalized Least Square
- Fixed effects
- Random effects

## Project Overiview

This data science project explores the relationships between climate change, specifically extreme weather events, and the rate of internal migration. 

### The dataset
> Merged from 3 different sources:
> - Internal Displacement Monitoring Center  --> Internal Displacement Rate
> - Germanwatch --> Climate Risk Index (CRI) 
> - Bündnis Entwicklung Hilft --> World Risk Index ('Exposure' element)

### The analysis includes 
> - Merging 3 seperate dataset on country
> - Constructing econometric models
> - Applying diagonistic tests (Random effects, Hausman Test, Tests for time-fixed effect, serial correlation, and heteroskedasticity)
> - Handling missing values

🏅 This research was presented at the Midwest Economics Association (MEA) Conference.

## Data Analysis

The project includes several key visualizations:

1. Data Distribution
2. Correlation plot
3. Scatterplot: Displacement to Exposure by Region
4. Scatterplot: Displacement to Exposure by Income Group

## Getting Started

To replicate this analysis:

1. Clone this repository
2. Install R and RStudio
3. Open the R project file
4. Run the analysis scripts

## Results

- Climate Risk Index negatively relates internal displacement rates (MigRate)
-     Counterintuitive
-     A possible explanation is the inability to migrate, which might be part of the construction of this index in terms
- Exposure rate to weather-related events is positively associated with internal displacement rates (MigRate).
- Internal displacement rates (MigRate) in high income countries is lower when exposure increase by one point.


## Future Work

### Limitation

> ### Data Limitation
> - The indices are complicated and it is hard to replicate their calculations due to the limitations on our knowledge of geological statistics.
> - The climate events being measured can be inconsistent.
> - Lots of missing values.
> - Data collection methods can be inconsistent across countries. Harmonizing data always leads to some loss of information.  

> ### Econometrics Limitation
> - Endogeneity
> - Omitted variable bias
> - Dependent variable measures displacement specifically because of disasters ⇒ in some ways it is like regressing Y against Y

### Further Research Direction
- Understand migration as a coping response to climate change among other coping mechanisms.
- Proxies to measure climate-related impact. (For example: Possibly studying an increase in climate change-resistant infrastructure → might signal less migration)
- Grouping countries together based on their vulnerabilities (social, weather-related, economic, political, etc.)
- Study specific disasters in isolation - allows for teasing out coping mechanisms.



## Acknowledgments

Special thanks to the Midwest Economics Association for the opportunity to present this research.

The full presentation of this research can be found [here](https://docs.google.com/presentation/d/1sKTUUIy_kA80zLxevyjVPrJ7maUXksZA/edit?usp=sharing&ouid=102529998437857684808&rtpof=true&sd=true)
