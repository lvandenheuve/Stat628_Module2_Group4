# Stat628 Module2 ~ Group4
Luke VandenHeuvel, Shuguang Chen, Xinyue Zhu

The contents of our GitHub repository includes a file containing our code, a file containing our data, our two-page summary report, as well as the rest of our files. The code folder contains the code that we used to check correlations, create models, and compare their measurables such as R^2 values. This folder also contains the contents need to run the Shiny App with our formula. The data folder contains two csv files, one called BodyFat.csv which contains the original data, and BodyFat_Cleaned.csv which contains our cleaned data, after conversions, imputations, and deletions. This data was cleaned in excel and not in RStudio, so there is no corresponding code for this process. This cleaned data is what we used for our calculations and model formulations.

In order to run our Shiny App that contains our Body Fat Calculator, you must download the 'App.R' file within the Code/ShinyApp/ directory as well as download the 'www' folder within that same directory. These two downloads must be located within the same folder when running the ShinyApp as the 'www' folder contains images that are rendered in the app. After this is done you can open the 'App.R' file in RStudio and click "Run App" which prompts the application to appear on the screen. Then for app use, first the user selects an age and weight using the top two sliders. Once a weight is selected, the minimum and maximum options for abdomen circumference change to keep the results robust. Then an abdomen circumference should be selected and then the 'Submit' button can be clicked. This results in the user's metrics being brought up in a table, the user's body fat percentage to be printed, and an image of a table to be brought up that contains body fat health categories that are based on the age entered.
