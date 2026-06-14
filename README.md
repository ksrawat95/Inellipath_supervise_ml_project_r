# Inellipath_supervise_ml_project_r

## Project Overview
This repository contains an end-to-end data science and supervised machine learning project implemented in R. Using the Census Income dataset (`census-income_.csv`), the project covers data preprocessing, exploratory data analysis (EDA), data visualization, and the implementation of multiple supervised learning models to predict yearly income classes (greater than $50k vs. less than or equal to $50k).

## Tech Stack
- **Programming Language**: R
- **Key Packages**:
  - Data Wrangling: `dplyr`, `stringr`
  - Visualization: `ggplot2`, `plotly`
  - Machine Learning & Utilities: `caTools`, `rpart`, `randomForest`, `ROCR`

## Key Implementation Details
- **Data Preprocessing**: Replaces placeholders (e.g. `" ?"`) with `NA`, removes missing values using `na.omit`, and cleans up leading/trailing whitespaces using `str_trim`.
- **Exploratory Data Analysis & Visualization**: Utilizes `ggplot2` and `plotly` to build complex visualizations including stacked/grouped bar charts, histograms, scatter plots, and box plots.
- **Model Implementations**:
  - **Simple Linear Regression**: Models `hours.per.week` using `education.num`.
  - **Logistic Regression**: Predicts the binary income class (`X`) using demographic features, and evaluates performance using ROC curves and AUC.
  - **Decision Tree**: Builds classification trees using `rpart` to find key decision split parameters.
  - **Random Forest**: Fits ensemble trees using `randomForest` to obtain high-accuracy classifications.
