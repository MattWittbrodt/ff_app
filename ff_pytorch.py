#%%
import pandas as pd
import numpy as np

#%%
wk = 11

#%% Read in data
data = pd.read_excel('/mnt/c/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/rb_deep_test.xlsx')
data.head()
# %% Splitting data into training and test
train = data.loc[data.proj_week <= (wk-2)]
test = data.loc[data.proj_week == (wk - 1)]

# %% Replace NA's with median
train = train.fillna(train.median())
test = test.fillna(test.median())

# %% Getting top 10 standing
train_y = train['top_ten']
test_y = test['top_ten']

# Removing from training set
train = train.drop(columns = 'top_ten')
test = test.drop(columns = 'top_ten')

# %% Create dataset

class rb_dataset(dataset)
