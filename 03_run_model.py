#%%
import pandas as pd
from utils import DataProcessor, Model
from tqdm import tqdm
import datetime

from sklearn.neural_network import MLPRegressor
from sklearn.preprocessing import MinMaxScaler
from lightgbm import LGBMRegressor

# %%
model_path = 'data/model/'

# Read data cleaned in R
data = pd.read_parquet(f"{model_path}model_data.parquet")
    
data = (
    data
    .drop([
        'minimum_first_investment', 'qualified_investor',
        'initial_lockup_period', 'minimum_additional_investment',
        'minimum_redemption', 'pension_fund', 'charges_performance_fee'
    ], axis = 1)
    .assign(
        date = lambda x: pd.to_datetime(x['date']),
        foreing_investment = lambda x: x['foreing_investment'].astype('float64'),
        performance_fee = lambda x: x['performance_fee'].fillna(20),
    )
)

#%%
dates = list(set(data['date'].to_list()))
dates = [date for date in dates if date >= datetime.datetime(2010, 1, 1)]
dates.sort()

#%%
predictions = []
for date in tqdm(dates, total=len(dates)):
    data_processor = DataProcessor(data, 1, MinMaxScaler(), 'mean')

    # Get train and test sets
    X_train, y_train, X_test, y_test, funds_code_test = data_processor.get_train_test_processed(date)

    # Do hyperparameter optimization considering the previous month as validation set
    date_validation = date - pd.DateOffset(months=1)
    X_train, y_train, X_val, y_val, _ = data_processor.get_train_test_processed(date_validation)

    model = LGBMRegressor(random_state=42)
    model_processor = Model(model, X_train, y_train, X_val, y_val)

    study = model_processor.run_optuna_study(model_processor.lightgbm_objective, 'minimize', 100)

    # Run model with the best hyperparameters found on data from the current month as test set
    X_train, y_train, X_test, y_test, funds_code_test = data_processor.get_train_test_processed(date)

    model = LGBMRegressor(**study.best_params, random_state=42, verbose=-1)
    model_processor = Model(model, X_train, y_train, X_test, y_test)

    # Get predictions
    prediction_df = model_processor.get_predictions_df(funds_code_test, date)

    predictions.append(prediction_df)

predictions = pd.concat(predictions)

#%%
predictions = []
for date in tqdm(dates, total=len(dates)):
    data_processor = DataProcessor(data, 1, MinMaxScaler(), 'mean')

    # Run model
    X_train, y_train, X_test, y_test, funds_code_test = data_processor.get_train_test_processed(date)

    #model = LGBMRegressor(random_state=42, verbose=-1)
    model = MLPRegressor(random_state=42, hidden_layer_sizes=(128, 64, 32, 16))
    model_processor = Model(model, X_train, y_train, X_test, y_test)

    # Get predictions
    prediction_df = model_processor.get_predictions_df(funds_code_test, date)

    predictions.append(prediction_df)

predictions = pd.concat(predictions)

# %%
