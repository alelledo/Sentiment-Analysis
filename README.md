# NFT Sentiment Analysis

## Introduction

This project performs sentiment analysis on a series of tweets related to different NFT assets. It links OpenSea and Twitter parameters with tweet valence, and utilizes an ordered logistic regression to assess the impact of not only tweet sentiment but also different Twitter and OpenSea metrics on the value of NFTs.

## Scraping OpenSea

### To collect data from OpenSea, the following steps were undertaken:

1. Installation of the OpenSea library using pip.
2. Utilization of the OpenSea API to scrape data, including asset events such as sales.
3. Cleaning of the dataset to ensure consistency and usability for analysis.

## Scraping Twitter

### Twitter data was collected using the snscrape library. The process involved:

1. Installation of snscrape via pip.
2. Scraping tweets related to NFT assets using relevant hashtags.
3. Cleaning the Twitter dataset for further analysis.

## Data Cleaning

Both OpenSea and Twitter datasets underwent cleaning procedures to enhance their suitability for analysis. This included:

1. Standardization of asset names.
2. Conversion of currency values to USD.
3. Removal of unnecessary characters and symbols.
4. Handling of missing values and data type conversions.

## Sentiment Analysis

### Sentiment analysis was conducted using a pre-trained neural network model based on RoBERTa. The process involved:

1. Installation of the transformers library via pip.
2. Application of the sentiment classification model to classify tweets into positive, neutral, and negative sentiments.
3. Analysis of sentiment intensity for each tweet.

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

## Usage

This project can serve as a basis for further research into the relationship between social media sentiment and the value of NFT assets. Users can explore various analyses and visualizations using the provided datasets.

## Dependencies

- Python 3.8
- Libraries: opensea, pandas, numpy, requests, snscrape, transformers, emoji, nltk, scikit-learn, imbalanced-learn

## Contributors

- [Your Name]
- [Other Contributors]

## License

This project is licensed under the [License Name] License - see the `LICENSE.md` file for details.
