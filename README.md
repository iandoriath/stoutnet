# stoutnet
2023 Big Data Bowl Submission
https://www.kaggle.com/code/iandelorey/stoutnet

## Python Notebooks
KLC.ipynb - Using trained networks, calculates Clean AUC for use in KLC calculation.

train.ipynb - Trains a clean play classifier using simple CNN.

## R Code
data_preprocess.R - Prepares raw tabular data for use in 2D+1 encoding.

klc.R - Calculates and visualizes KLC.

## Trained Networks
cnn_feanor_4.pt - StoutNet trained for 4 epochs on randomly truncated plays.

cnn_feanor_fullplay_4.pt - StoutNet trained for 4 epochs on full plays.

## Tabular Data
klc.csv - Contains calculated Clean AUC for every play in the dataset.
