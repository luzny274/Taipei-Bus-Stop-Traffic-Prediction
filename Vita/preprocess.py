import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import copy
import sklearn.model_selection
import sklearn.pipeline
import sklearn.preprocessing
import sklearn.compose
import torch

try:
    import google.colab
    from google.colab import drive
    drive.mount('/content/drive')

    IN_COLAB = True
except:
    IN_COLAB = False
    
#CHANGE THIS
colab_path = "/content/drive/MyDrive/Colab Notebooks/NTU_DA/"

class TimeSeries:
    def __init__(self, timestampsData, timeSeriesData):
        self.timestampsData = timestampsData # 2D tensor, additional data corresponding to the current timestamp (weather and so on)
        self.timeSeriesData = timeSeriesData # 3D tensor, time series (mrt flows + hours)

        self.shape = timeSeriesData.shape

    def __getitem__(self, arg):
        return TimeSeries(self.timestampsData[arg], self.timeSeriesData[arg])

    def to_pytorch(self, use_cuda, dtype):
        timestampsData_t = torch.from_numpy(self.timestampsData).to(dtype = dtype)
        timeSeriesData_t = torch.from_numpy(self.timeSeriesData).to(dtype = dtype)

        if use_cuda:
            timestampsData_t = timestampsData_t.cuda()
            timeSeriesData_t = timeSeriesData_t.cuda()

        return TimeSeries(timestampsData_t, timeSeriesData_t)

class DatasetPreprocess:
    def __init__(self, path_to_dataset, sequence_length, is_dataset_reshaped):
        do_a_time_series = sequence_length > 0

        ###############################################
        ############### Load the dataset
        ###############################################

        if IN_COLAB:
            path_to_dataset = colab_path + path_to_dataset
        df = pd.read_csv(path_to_dataset, parse_dates=True)

        if is_dataset_reshaped:
            total_sample_cnt = df.to_numpy().shape[0]
            ## Remove incomplete rows
            incomplete_Y_rows = np.nonzero(pd.isna((df.iloc[:, 0])).to_numpy())[0]
            other_incomplete_rows = [(i+1) for i in incomplete_Y_rows if ((i+1) not in incomplete_Y_rows and (i+1) < total_sample_cnt)] #We have to exclude the following rows as well (since previous_mrt_flow features will be incomplete too)
            other_incomplete_rows += [0]
            
            df_clean = df.drop(incomplete_Y_rows.tolist() + other_incomplete_rows)
            df_clean = df_clean.reset_index(drop=True)
            df = df_clean
        else:
            df["day_in_a_week"] = pd.to_datetime(df["datetime"]).dt.dayofweek #Get the day in a week
            df["month"] = pd.to_datetime(df["datetime"]).dt.month #Get month

            ## Removing collinear features

            df = df.drop('datetime', axis=1) 
            df = df.drop("day", axis = 1) 
            df = df.drop("next_dist", axis = 1)
            df = df.drop("bus_cnt", axis = 1)
            df = df.drop("bike_cnt", axis = 1)
            df = df.drop("school_cnt", axis = 1)

        ###############################################
        ############### Dataset information
        ###############################################
        
        categorical_names = ["mrt_station", "hour", "status", "day_in_a_week", "month"]
        categorical_variables = []
        
        for column in df.columns:
            for name in categorical_names:
                if name in column:
                    categorical_variables.append(column)

        print("categorical_variables:", categorical_variables)

        Y_variables = []
        for column in df.columns:
            if "mrt_flow" in column and "previous_" not in column:
                Y_variables.append(column)

        y_indices = [df.columns.get_loc(Y_variables[i]) for i in range(len(Y_variables))]
        cat_indices = [df.columns.get_loc(categorical_variables[i]) for i in range(len(categorical_variables))]
        real_indices = [i for i in list(range(0, len(df.columns))) if (i not in cat_indices and i not in y_indices)]
        print("metro_flow index: ", y_indices, "\ncategorical indices: ", cat_indices, "\nnumerical indices: ", real_indices)

        def get_unique_values(values):
            return list(dict.fromkeys(values))

        unique_string_names = []
        print("\n")
        category_cnt = 0
        for var in categorical_variables:
            unique_vals = get_unique_values([df[var][i] for i in range(len(df))])
            print(str(len(unique_vals)) + " unique " + var + ":\t", unique_vals)
            category_cnt += len(unique_vals)

            is_string = np.max(np.array([isinstance(s, str) for s in unique_vals]))
            if is_string:
                print(var + " is a string!")
                unique_string_names = get_unique_values(unique_string_names + unique_vals)

        ###############################################
        ############### X Preprocessing 
        ###############################################

        data = df.to_numpy()

        #Convert string names to ints
        print("Processing string categories")
        for i in range(len(unique_string_names)):
            occurances = np.nonzero(data == unique_string_names[i])[0]
            if len(occurances) > 0:
                print("\tFirst occurance of " + str(unique_string_names[i]) + ":", occurances[0])
            data[data == unique_string_names[i]] = i

        #Replace missing values with NaNs
        data[data == "-"] = np.nan

        ##Train test split
        self.train_split_ratio = 0.7
        self.val_split_ratio = 0.1
        self.test_split_ratio = 0.2

        self.sample_cnt = data.shape[0]
        self.train_sz = int(self.train_split_ratio * self.sample_cnt)
        self.val_sz = int(self.val_split_ratio * self.sample_cnt)

        val_ind = self.train_sz + self.val_sz
        self.test_sz = self.sample_cnt - val_ind

        print("\nTrain-val-test split")
        print("\tTrain start index:", 0)
        print("\tVal start index:", self.train_sz)
        print("\tTest start index:", val_ind)

        self.Y_train_raw = data[:self.train_sz, y_indices]
        self.X_train_c_raw = data[:self.train_sz, cat_indices + real_indices]

        self.Y_val_raw = data[self.train_sz:val_ind, y_indices]
        self.X_val_c_raw = data[self.train_sz:val_ind, cat_indices + real_indices]

        self.Y_test_raw = data[val_ind:, y_indices]
        self.X_test_c_raw = data[val_ind:, cat_indices + real_indices]

        if do_a_time_series:
            hour_ind = df.columns.get_loc("hour")
            timeSeries_train_hour = data[:self.train_sz, hour_ind]
            timeSeries_val_hour   = data[self.train_sz:val_ind, hour_ind]
            timeSeries_test_hour  = data[val_ind:, hour_ind]

        ##Preprocessing
        new_cat_indices = list(range(len(cat_indices)))
        new_real_indices = list(range(len(cat_indices), len(cat_indices) + len(real_indices)))

        #One hot encoding for categories, Robust scaling for numerical values
        self.X_preprocessor = sklearn.compose.ColumnTransformer(
            transformers=[
                ("cat", sklearn.preprocessing.OneHotEncoder(categories='auto', handle_unknown="ignore"), new_cat_indices),
                ("num", sklearn.preprocessing.RobustScaler(), new_real_indices)
            ]
        )
        self.X_preprocessor.fit(self.X_train_c_raw)
        self.X_train_c = self.X_preprocessor.transform(self.X_train_c_raw)
        self.X_val_c   = self.X_preprocessor.transform(self.X_val_c_raw)
        self.X_test_c  = self.X_preprocessor.transform(self.X_test_c_raw)

        ###############################################
        ############### Y Preprocessing
        ###############################################

        # Transforming Y
        self.Y_scaler = sklearn.preprocessing.RobustScaler().fit(self.Y_train_raw)
        self.Y_train = self.Y_scaler.transform(self.Y_train_raw)
        self.Y_val   = self.Y_scaler.transform(self.Y_val_raw)
        self.Y_test  = self.Y_scaler.transform(self.Y_test_raw)

        # Replace NaNs with zeros
        self.X_train_c[np.isnan(self.X_train_c)] = 0
        self.X_val_c  [np.isnan(self.X_val_c)]   = 0
        self.X_test_c [np.isnan(self.X_test_c)]  = 0

        print("Y_test_raw mean:  \t", np.mean  (self.Y_test_raw))
        print("Y_test_raw median:\t", np.median(self.Y_test_raw))
        print("Y_test_raw std:   \t", np.std   (self.Y_test_raw))

        ###############################################
        ############### Time Series Preprocessing
        ###############################################

        if do_a_time_series:
            hour_ind = df.columns.get_loc("hour")

            self.hour_onehot = sklearn.preprocessing.OneHotEncoder(categories='auto', handle_unknown="ignore", sparse_output=False)
            self.hour_onehot.fit(timeSeries_train_hour[:, None])
            self.timeSeries_train = np.concatenate([self.Y_train, self.hour_onehot.transform(timeSeries_train_hour[:, None])], axis = 1)
            self.timeSeries_val   = np.concatenate([self.Y_val  , self.hour_onehot.transform(timeSeries_val_hour  [:, None])], axis = 1)
            self.timeSeries_test  = np.concatenate([self.Y_test , self.hour_onehot.transform(timeSeries_test_hour [:, None])], axis = 1)

            #Turn time series into sequences
            def timeSeriesReshape(timeSeries):
                reshapedTimeSeries = np.zeros((timeSeries.shape[0] - sequence_length, sequence_length, timeSeries.shape[1]))
                for i in range(sequence_length, timeSeries.shape[0]):
                    reshapedTimeSeries[i-sequence_length, :, :] = timeSeries[i-sequence_length:i, :]
                return reshapedTimeSeries

            self.Y_train = self.Y_train[sequence_length:, :]
            self.Y_val   = self.Y_val  [sequence_length:, :]
            self.Y_test  = self.Y_test [sequence_length:, :]

            self.X_train_c = self.X_train_c[sequence_length:, :]
            self.X_val_c   = self.X_val_c  [sequence_length:, :]
            self.X_test_c  = self.X_test_c [sequence_length:, :]

            self.timeSeries_train = timeSeriesReshape(self.timeSeries_train)
            self.timeSeries_val   = timeSeriesReshape(self.timeSeries_val  )
            self.timeSeries_test  = timeSeriesReshape(self.timeSeries_test )

            self.X_train = TimeSeries(self.X_train_c, self.timeSeries_train)
            self.X_val   = TimeSeries(self.X_val_c  , self.timeSeries_val  )
            self.X_test  = TimeSeries(self.X_test_c , self.timeSeries_test )
        else:
            self.X_train = self.X_train_c
            self.X_val   = self.X_val_c
            self.X_test  = self.X_test_c

    def Y_inverse_transform(self, Y):
        return self.Y_scaler.inverse_transform(Y)

