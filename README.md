# OECD Country Recession Classification

### 1. Folder Structure

```python
Repository
 |-- recession_prediction_code.R	# Main code notebook
 |    |-- input               # input csv data
 |    |    |-- OECD_data.csv        # Country level economic indicators
 |    |-- images              # for ReadMe file             
```

### 2. Overview

For this project the main questions that will be explored are:

1. For deep recessions (<-5% GDP decrease in a quarter) what are the lagging economic indicators with the highest variable importance?
2. Can we create an accurate classifier to predict the probability of negative economic growth in a country?

### 3. Data Source

The data I will be using is from OECD (Organization for Economic Cooperation and Development). This dataset contains quarterly data from 49 countries on 211 financial and socioeconomic variables ranging from household debt to taxes and oil market conditions

The raw data was structured vertically with one row for country, time period, variable name and variable value. In order to be ready for model input, the data had to be transformed so each variable was a column, and where each row represented a country’s data for a specific year and quarter.

### 4. Methodology

The three classification models that will be tested are support vector machine, the tree-based method random forest and multinomial regression

The response variable REC_FLAG_2 had to be created from the GDP Growth variable within the data, with a GDP growth < -5% indicating recession class 2 <1% for recession class 1 otherwise positive GDP values are 0. Some of the numeric variables are currency dependent, for example some are in USD and not AUD, therefore are highly skewed. To resolve this skewness a log transformation was performed on variables that were not ratios or small values.

Highly correlated predictors with correlation >90% were removed which left 71 predictors. Removing correlated variables was also necessary for pre-processing the data for model input, as multinomial regression assumed iid inputs. Two additional ratios were created for the self-employed to total employment and household to government savings.

### 5. Results

For question 1, the top 5 predictors for deeper recessions are listed below.

![result1](/images/result-1.png)

This result could guide governments as to where to provide relief when these values are trending downwards.

As for question 2 classification, the class (0/1 recession indicator) imbalance combined with the log transformations likely produced variables with near-zero variance which I believe made classification difficult. The log transformations could have diluted or masked some relationships with variables that had sparse or granular data therefore causing selection bias.

The best model was the SVM with cubic kernel which generated a test accuracy of 73%, however with a high false positive rate.

![result2](/images/result-2.png)

The prediction could have been improved by creating more ratios from the data. For example, social security benefits interacted with unemployment or ratio of short term to long term bond rates.
