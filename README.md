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

I ran panel linear regression from merged dataset on RStudio, using Generalized Least Square and random effects to control for serial correlation and unobserved heterogeneity, respectively.
 
### The dataset
> #### Create an original country-level dataset from merging Internal Displacement Monitoring Center (IDMC), World Risk Index (WRI) Report, and World Bank.
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

### 1. Data Distribution

<img width="692" alt="distribution" src="https://github.com/user-attachments/assets/b31f3997-a75b-4bea-9262-6b2b2a790300">

  
### 2. Correlation plot
   
![correplot](https://github.com/user-attachments/assets/ecf9d43d-4e72-4de9-8461-c56d83e8a15a)


### 3. Scatterplot: Displacement to Exposure by Region

![migrate region](https://github.com/user-attachments/assets/49cb4c15-9e96-4f69-8a4f-06de979d0a61)


### 4. Scatterplot: Displacement to Exposure by Income Group

![migrate income](https://github.com/user-attachments/assets/9e736b38-d7f7-4e33-932c-7c5b38cdbac3)


## Getting Started

To replicate this analysis:

1. Clone this repository
2. Install R and RStudio
3. Open the R project file
4. Run the analysis scripts


## Results

- Climate Risk Index negatively relates internal displacement rates (MigRate)
  - Counterintuitive
  - A possible explanation is the inability to migrate, which might be part of the construction of this index in terms
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

This project was completed as a part of ECON 380: Senior Capstone @Beloit College 2021. Special thanks to the course instructor Prof Diep Phan.

Special thanks to the Midwest Economics Association for the opportunity to present this research.

The full presentation of this research can be found [here](https://docs.google.com/presentation/d/1sKTUUIy_kA80zLxevyjVPrJ7maUXksZA/edit?usp=sharing&ouid=102529998437857684808&rtpof=true&sd=true)
The full paper on this research can be found [here](https://drive.google.com/file/d/1GeLBRd9y7Mhj0aI2iwSC8F4fxtJVIjs7/view?usp=sharing)
