# Kaggle-House-Prices-Competition

I took part in the Kaggle house prices competition, where the goal is to predict the sale prices of houses as accurate as possible using regression techniques.
In this project I used R and made my first experiences with the tidymodels framework for machine learning in R, which I first discovered after I had already done the data cleansing and feature engineering, so in this regard the code might look a little bit inconsistent.

I could achieve a decent result with a RMSE of 0.1246 (log-transformed prices) and made it under the best 22% of all competitors (September 2020).
Some of the main keys in my opinion were:

* focusing on the most relevant features
* thinking about which features should be sumarized into one and which contain useful additional information
* transformation of numeric features (especially those related to real size metrics like feet or square feet)
  * many other Kaggler's approached this using Box-Cox transformation. I used Tukey's ladder of power instead
* removing only the most extreme and obvious outliers from the training data
  * in my case I only removed two observations
* stacking up / averaging the best models improved my predictions a little bit

Also I noticed that using the tidymodels framework works better than using machine learning techniques (such as cross-validation) which are built in in the single packages' functions.

