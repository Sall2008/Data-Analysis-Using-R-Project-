## Folie 1 

# Title: Exploring the Social Index Impact on House Prices 

# **Key Finding from Distance Analysis:**
  - We’ve already established that **households are willing to pay a premium** for proximity to schools. 

# **Next Question:**
  - Now, we investigate **whether this price premium is driven by school quality**, measured through the **Social Index**
    
# Research Focus 
    Hypothesis 2: 
    We hypothesize that the **Social Index** moderates the effect of **school proximity** on house prices, with better schools leading to stronger price premiums.
  
  
  
## Folie 2
  
#  Title: What is the Social Index? 
  
  Definition: The Social Index is a measure that shows the socio-economic and demographic background of a schools students.
  
  Components of the social index are: 
    - Parental Education 
    - Socio-economic Status of the Family 
    - Migration Background
    - Single-Parent Household
    - Unemployment in the Family 
    - Housing Situation 
  
  --> It provides insights into the **social environment** that might influence housing prices in that area.

  
## Folie 3 
  
# Title: Categorization of the Social Index 
  
  **Social Index Classification:**
    - **Good Schools**: Social Index 1-2
  - **Average Schools**: Social Index 3-4
  - **Bad Schools**: Social Index 5-8
  
  - **Skewed Distribution**: The Social Index classes are heavily skewed, with many schools falling into classes 1, 2 & 3. We excluded class 9 for our analysis because of too less observations
  
  **Graph**: Distribution of Social Index values across schools showing the skewed nature of the data.
  # Balkendiagramm Sozialindex
  
  df_school_meta %>%
    ggplot(aes(x = factor(social_index))) +
    geom_bar() +
    labs(x = "Social Index (1–9)", y = "Number of schools")
  
  
  ## Folie 4
  
# Title: Spatial Distribution of School Social Index in NRW 
  
  ggplot(df_grid, aes(x = lon, y = lat)) +
    geom_point(aes(color = social_index), size = 0.8, alpha = 0.8) +
    scale_color_viridis_c(name = "Social Index") +
    coord_equal() +
    theme_minimal() +
    labs(
      title = "Spatial Distribution of School Social Index in NRW",
      subtitle = "Each grid cell colored by the social index of the nearest school"
    )
  
  
  
  ## Folie 5
  
# Title: Hedonic Regression Model for Social Index 
  
  ## Model Specification:
  
  \[
    \text{log\_price} = \beta_0 + \beta_1 \cdot \text{dist\_primary\_km} + \beta_2 \cdot \text{dist\_secondary\_km} + \beta_3 \cdot \text{school\_quality} + \beta_4 \cdot (\text{dist\_primary\_km} \times \text{school\_quality}) + \beta_5 \cdot (\text{dist\_secondary\_km} \times \text{school\_quality}) + \beta_6 \cdot \text{log\_area} + \beta_7 \cdot \text{log\_plot\_area} + \beta_8 \cdot \text{zimmeranzahl} + \beta_9 \cdot \text{house\_age} + \epsilon
    \]
  
  Where:
    
    - **log_price**: Natural logarithm of the housing price.
  - **dist_primary_km**: Distance to the nearest primary school (in kilometers).
  - **dist_secondary_km**: Distance to the nearest secondary school (in kilometers).
  - **school_quality**: Categorical variable representing school quality (good, average, bad).
  - **log_area**: Natural logarithm of the living area of the house.
  - **log_plot_area**: Natural logarithm of the plot area of the house.
  - **zimmeranzahl**: Number of rooms in the house.
  - **house_age**: Age of the house (in years).
  - **\(\epsilon\)**: Error term.
  
  
  - **School quality** is treated as a categorical variable with three levels: "good", "average", and "bad".
  - The interaction terms \( \text{dist\_primary\_km} \times \text{school\_quality} \) and \( \text{dist\_secondary\_km} \times \text{school\_quality} \) are included to assess whether the effect of distance on housing prices varies by the quality of the nearby school.
  
  
  
  ## Folie 6 
  
# Title: Regression Results 
  
  
  
  # Ergebnistabelle mit Good als Referenzkategorie 
  # Regression mit Interaktionseffekten
  m_good_ref <- lm(base_formula, data = df_reg1)
  
  # Anzeige der Ergebnisse in einer Tabelle
  modelsummary(
    m_good_ref,
    stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),  # Signifikanzniveaus
    title = "Table 1: Hedonic Regression Results - Effect of Distance on Housing Prices (Good as Reference)",
    output = "gt"  # Ausgabeformat für den Viewer
  )
  
  
  
  
  
  ## Folie 7
  
# Title: Interpretaition of the Results 
  
  ### **Primary School Proximity**:
  - **Distance to primary school** is still **significant** and negatively affects house prices.
  - Interaction Effect: The effect is **stronger for average schools** (Estimate = -0.041, p = 0.009), suggesting that the price drop is greater when the school quality is average.
  - **No significant effect** for bad schools (p = 0.2996).
  
  ### **Secondary School Proximity**:
  - **Distance to secondary school** is also still **significant** and negatively impacts house prices.
  - The effect is **stronger for average schools** (Estimate = 0.0307, p = 0.0096), meaning proximity to secondary schools is more impactful in areas with average school quality.
  - **No significant effect** for bad schools (p = 0.7414).
  