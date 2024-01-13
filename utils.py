import pandas as pd
import time

from sklearn.impute import SimpleImputer
from sklearn.pipeline import Pipeline

class DataProcessor:
    """
    A class to process financial data for predictive modeling.
    """

    def __init__(self, data, look_ahead_months, scaler, imputer_strategy):
        """
        Initializes the DataProcessor with data, configuration for look-ahead months, scaler, and imputer strategy.
        """
        self.data = data
        self.look_ahead_months = look_ahead_months
        self.scaler = scaler
        self.imputer_strategy = imputer_strategy

    def process_date(self, split_date):
        """
        Processes the data by splitting, scaling, and imputing, returning the train and test sets.
        """
        train, test = self._train_test_on_date_split(split_date)

        funds_code_test = test['fund_code'].to_list()

        info_cols = ['fund_code', 'date']
        train, test = train.drop(columns=info_cols), test.drop(columns=info_cols)

        X_train, y_train = self._target_features_split(train)
        X_test, y_test = self._target_features_split(test)

        X_train, X_test = self._pre_processing(X_train, X_test)

        return X_train, y_train, X_test, y_test, funds_code_test

    def _target_features_split(self, data):
        """
        Splits the data into target and features based on the look-ahead months.
        """
        target_variable = f'abnormal_return_{self.look_ahead_months}m'
        data = data.dropna(subset=[target_variable])
        y = data[[target_variable]].values.ravel()
        X = data.drop(columns=[col for col in data.columns if col.startswith('abnormal_return')])
        return X, y

    def _train_test_on_date_split(self, split_date, add_validation=False):
        """
        Splits the data into training and testing sets based on a given date.
        """
        train = self.data[self.data['date'] < split_date]
        test = self.data[self.data['date'] == split_date]

        if add_validation:
            validation_date = train['date'].max()
            return self._train_validation_split(train, validation_date) + (test,)
        
        return train, test
    
    @staticmethod
    def _train_validation_split(train, validation_date):
        """
        Splits the training data into training and validation sets based on a validation date.
        """
        validation = train[train['date'] >= validation_date]
        train = train[train['date'] < validation_date]
        return train, validation

    def _pre_processing(self, X_train, X_test):
        """
        Applies preprocessing steps like imputing and scaling to the training and test data.
        """
        pipeline = Pipeline([
            ('imputer', SimpleImputer(strategy=self.imputer_strategy)),
            ('scaler', self.scaler)
        ])

        X_train_scaled = pipeline.fit_transform(X_train)
        X_train_scaled = pd.DataFrame(X_train_scaled, columns=X_train.columns)

        X_test_scaled = pipeline.transform(X_test)
        X_test_scaled = pd.DataFrame(X_test_scaled, columns=X_test.columns)

        return X_train_scaled, X_test_scaled
    
class Model:
    def __init__(self, model, X_train, y_train, X_test, y_test):
        self.model = model
        self.X_train = X_train
        self.y_train = y_train
        self.X_test = X_test
        self.y_test = y_test

    def get_predictions(self):
        st = time.time()

        self.model.fit(self.X_train, self.y_train)

        y_pred = self.model.predict(self.X_test)

        et = time.time()

        return y_pred, et - st
    
    def get_predictions_df(self, funds_code_test, date):
        y_pred, execution_time = self.get_predictions()

        prediction_df = pd.DataFrame(
            list(zip(funds_code_test, y_pred, self.y_test)), 
            columns = ['fund_code', 'prediction', 'true_value']
        ) \
            .assign(
                date = date,
                execution_time = execution_time,
                model = self.model.__class__.__name__
            )
        
        return prediction_df



    
