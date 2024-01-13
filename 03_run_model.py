#%%
import pandas as pd
from utils import DataProcessor, Model
from tqdm import tqdm
import datetime

from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression, Lasso, Ridge, ElasticNet
from sklearn.svm import SVR
from sklearn.neighbors import KNeighborsRegressor
from sklearn.ensemble import RandomForestRegressor, ExtraTreesRegressor
from sklearn.dummy import DummyRegressor

from lightgbm import LGBMRegressor
from xgboost import XGBRegressor

# %%
model_path = 'model'

# Read data cleaned in R
data = pd.read_csv(
    f"{model_path}/model_data.csv", dtype={'fund_code': str}
)
    
data = (
    data
    .drop([
        'minimum_first_investment', 'qualified_investor',
        'initial_lockup_period', 'minimum_additional_investment',
        'minimum_redemption', 'pension_fund', 'charges_performance_fee'
    ], axis = 1)
    .assign(
        date = lambda x: pd.to_datetime(x['date']),
        performance_fee = lambda x: x['performance_fee'].fillna(20),
    )
)

#%%
all_dates = list(set(data['date'].to_list()))
all_dates = [date for date in all_dates if date >= datetime.datetime(2010, 1, 1)]
all_dates.sort()

models = [
    DummyRegressor(),
    ElasticNet(random_state=42),
    ExtraTreesRegressor(random_state=42), 
    KNeighborsRegressor(), 
    Lasso(random_state=42), 
    LGBMRegressor(random_state=42),
    LinearRegression(), 
    Ridge(random_state=42),
    RandomForestRegressor(random_state=42),
    SVR(), 
    XGBRegressor(random_state=42),
]

#%%
predictions = []
for date in tqdm(all_dates, total=len(all_dates)):
    data_processor = DataProcessor(data, 1, StandardScaler(), 'mean')
    X_train, y_train, X_test, y_test, funds_code_test = data_processor.process_date(date)

    for model in models[:1]:
        model_processor = Model(model, X_train, y_train, X_test, y_test)

        prediction_df = model_processor.get_predictions_df(funds_code_test, date)

        predictions.append(prediction_df)

predictions = pd.concat(predictions)

#%%
