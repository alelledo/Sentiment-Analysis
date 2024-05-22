# NFT Sentiment Analysis

## Introduction

This project performs sentiment analysis on a series of tweets related to different NFT assets. It links OpenSea and Twitter parameters with tweet valence, and utilizes an ordered logistic regression to assess the impact of not only tweet sentiment but also different Twitter and OpenSea metrics on the value of NFTs. Multiple models are studied for this project,linear regression, binomial, poisson, ultimately an Ordered Logistic Model is considered the most adequate. 

## Scraping OpenSea

To collect data from OpenSea, the following steps were undertaken:

- Installation of the OpenSea library using pip.
- Utilization of the OpenSea API to scrape data, including asset events such as sales.
- Cleaning of the dataset to ensure consistency and usability for analysis.

## Scraping Twitter

Twitter data was collected using the snscrape library. The process involved:

- Installation of snscrape via pip.
- Scraping tweets related to NFT assets using relevant hashtags.
- Cleaning the Twitter dataset for further analysis.

## Data Cleaning

Both OpenSea and Twitter datasets underwent cleaning procedures to enhance their suitability for analysis. This included:

- Standardization of asset names.
- Conversion of currency values to USD.
- Removal of unnecessary characters and symbols.
- Handling of missing values and data type conversions.

## Sentiment Analysis

Sentiment analysis was conducted using a pre-trained neural network model based on RoBERTa. The process involved:

- Installation of the transformers library via pip.
- Application of the sentiment classification model to classify tweets into positive, neutral, and negative sentiments.
- Analysis of sentiment intensity for each tweet.

## Final Dataset

The cleaned and analyzed datasets from OpenSea and Twitter were merged to create a final dataset. This dataset includes:

- NFT asset details.
- OpenSea transaction data.
- Tweet content and sentiment analysis results.

## Files

- `asset_events.csv`: Dataset containing OpenSea transaction data.
- `eth_events_2.csv`: Processed OpenSea data focusing on Ethereum transactions.
- `only_twitter_df.csv`: Raw Twitter data.
- `sentiment_events2.csv`: Twitter data with sentiment analysis results.
- `almost_final_df.csv`: Merged dataset combining OpenSea and Twitter data.
- `linear_regression.R`: R code file for performing linear regression analysis.

## Usage

This project can serve as a basis for further research into the relationship between social media sentiment and the value of NFT assets. Users can explore various analyses and visualizations using the provided datasets.

## Dependencies

- Python 3.8
- Libraries: opensea, pandas, numpy, requests, snscrape, transformers, emoji, nltk, scikit-learn, imbalanced-learn
- R
- Libraries: readr, ggplot2, dplyr, lubridate, Hmisc, corrplot, MASS


