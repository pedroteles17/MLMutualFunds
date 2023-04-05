#%%
import pandas as pd
import numpy as np

import datetime
import time

from IPython.display import clear_output # clear jupyter notebook
import os

from sklearn.preprocessing import StandardScaler

from sklearn.linear_model import LinearRegression, Lasso, Ridge, ElasticNet
from sklearn.svm import SVR
from sklearn.neighbors import KNeighborsRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.ensemble import RandomForestRegressor, ExtraTreesRegressor, AdaBoostRegressor, GradientBoostingRegressor
from sklearn.dummy import DummyRegressor

from lightgbm import LGBMRegressor
from xgboost import XGBRegressor

# %%
model_path = 'model'
predictions_path = f'{model_path}/predictions'
os.makedirs(predictions_path, exist_ok=True)

# Read data cleaned in R
data = pd.read_csv(
    f"{model_path}/model_data.csv", dtype={'fund_code': str}
)
    
print(f'Shape Before Drop: {data.shape}')

data = data\
    .drop(['minimum_first_investment', 'qualified_investor'], axis = 1)\
    .dropna()

print(f'Shape After Drop: {data.shape}')

data['date'] = data['date'].apply(pd.to_datetime)

# %%
# Separate train and test dataset into X_train, X_test, y_train, y_test
def prepare_train_test(data):
    # Separate in dependent and independent variables
    y, X = data[['abnormal_return']], data.drop('abnormal_return', axis = 1)

    # Reshape the dependente variable
    y = y.values.ravel()
    
    return X, y

def pre_processing(X_train, X_test):
    # Because some models use distances, we scale our features
    scaler = StandardScaler()
    scaler.fit(X_train)
    
    X_train_scaled = scaler.transform(X_train)
    X_test_scaled = scaler.transform(X_test)

    return X_train_scaled, X_test_scaled

# Make any pre-processing needed fit model and predict
def fit_pred_model(model_class, X_train, X_test, y_train):
    # Call model
    regressor = model_class
    
    # Fit
    regressor.fit(X_train, y_train)
    
    # Predict
    y_pred = regressor.predict(X_test)
    
    return y_pred

#%%
all_dates = list(set(data['date'].to_list()))
all_dates = [date for date in all_dates if date >= datetime.datetime(2010, 1, 1)]
all_dates.sort()

models = [
    XGBRegressor(random_state=42), 
    ExtraTreesRegressor(random_state=42), 
    DummyRegressor(), 
    LGBMRegressor(random_state=42),
    ElasticNet(random_state=42),
    LinearRegression(), 
    Lasso(random_state=42),  
    Ridge(random_state=42),
    SVR(), 
    KNeighborsRegressor(), 
    RandomForestRegressor(random_state=42)
]

#%%
pred_df = pd.DataFrame(
    columns = [
        'fund_code', 'prediction', 'true_value', 
        'date', 'execution_time', 'model'
    ]
)
for date in all_dates:
    # Train = all data before test
    data_train = data.loc[data['date'] < date]\
        .drop(columns=['fund_code', 'date'])

    # Test = specific month
    data_test = data.loc[data['date'] == date]

    funds_code_test = data_test['fund_code'].to_list()
    
    data_test = data_test\
        .drop(columns=['fund_code', 'date'])

    # X, y split
    X_train, y_train = prepare_train_test(data_train)
    X_test, y_test = prepare_train_test(data_test)

    X_train, X_test = pre_processing(X_train, X_test)

    for model in models:
        st = time.time() # Start stopwatch

        y_pred = fit_pred_model(model, X_train, X_test, y_train)

        et = time.time() # Stop stopwatch

        model_pred = pd.DataFrame(
            list(zip(funds_code_test, y_pred, y_test)), 
            columns = ['fund_code', 'prediction', 'true_value']
        )

        model_pred['date'] = date
        model_pred['execution_time'] = et - st
        model_pred['model'] = model.__class__.__name__

        pred_df = pd.concat([pred_df, model_pred])

        pred_df.to_csv(f'{predictions_path}/predictions.csv', index = False)

        print(f'{date} - {model.__class__.__name__}')
    
    clear_output(wait=True)

# %%
