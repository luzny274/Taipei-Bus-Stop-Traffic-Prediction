import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import copy
import datetime

### Load the dataset

path_to_dataset = 'FinalData.csv'
try:
    import google.colab
    from google.colab import drive
    drive.mount('/content/drive')

    #CHANGE THIS
    colab_path = "/content/drive/MyDrive/Colab Notebooks/NTU_DA/"
    path_to_dataset = colab_path + path_to_dataset

    IN_COLAB = True
except:
    IN_COLAB = False

df = pd.read_csv(path_to_dataset, parse_dates=True)
df['day_in_a_week'] = pd.to_datetime(df["datetime"]).dt.dayofweek
df['month'] = pd.to_datetime(df["datetime"]).dt.month

#Remove collinear features
df = df.drop("day", axis = 1)
df = df.drop("next_dist", axis = 1)
df = df.drop("bus_cnt", axis = 1)
df = df.drop("bike_cnt", axis = 1)
df = df.drop("school_cnt", axis = 1)

## Transformation
unique_timestamps = list(dict.fromkeys([df['datetime'][i] for i in range(len(df))]))

dt_unique_timestamps = [datetime.datetime.strptime(ts, "%Y-%m-%d %H:%M:%S") for ts in unique_timestamps]
dt_unique_timestamps.sort()

#variables independent of station
station_variables = ['day_in_a_week', 'month', 'hour', 'air_pressure', 'air_temperature', 'relative_humidity', 'precipitation', 'sunshine_duration']

#variables that will not be used for inference
index_variables = ['datetime', 'mrt_station', 'mrt_flow']

#variables that will be used for inference and are dependent on station
other_variables = [v for v in df.columns if (v not in station_variables and v not in index_variables)]

sample_size = len(dt_unique_timestamps)
unique_station_names = list(dict.fromkeys([df["mrt_station"][i] for i in range(len(df))]))
station_cnt = len(unique_station_names)

df_new = pd.DataFrame() #Transformed dataset
Y = np.full((sample_size, station_cnt), np.nan) #mrt flows of all stations, incomplete rows will contain NaNs

#Get mrt flows for each station in a timestamp
def get_mrt_flow(timestamp_df):
    mrt_flow = np.full((station_cnt), np.nan)
    
    for ii in range(station_cnt):
        station_mrt_flow = timestamp_df[timestamp_df['mrt_station'] == unique_station_names[ii]]["mrt_flow"]
        station_mrt_flow = station_mrt_flow.reset_index(drop=True)
        if len(station_mrt_flow) != 1:
            print("ERROR::mrt flow ambiguous")
            continue
        mrt_flow[ii] = station_mrt_flow[0]
        
    return mrt_flow

#Create row with only NaNs
def create_dummy_row(columns):
    dummy_row = dict()
    for column in columns:
        dummy_row[column] = np.nan
    return pd.DataFrame([dummy_row])  
    
#Get first row Y
Y[0, :] = get_mrt_flow(df[df['datetime'] == str(dt_unique_timestamps[0])])

#We ignore the first sample
df_new = pd.concat([df_new, create_dummy_row(df_new.columns)], ignore_index=True)

#Transform all other rows
for i in range(1, sample_size):
    print("{}/{}".format(i, sample_size), end="\r")
    timestamp = dt_unique_timestamps[i]
    
    timestamp_df = df[df['datetime'] == str(timestamp)]
    timestamp_df = timestamp_df.reset_index(drop=True)

    #Check whether data is complete
    if len(timestamp_df['mrt_station']) != station_cnt:
        print("ERROR::incomplete row")
        df_new = pd.concat([df_new, create_dummy_row(df_new.columns)], ignore_index=True)
        continue
        
    #Get Y (mrt flows)
    Y[i, :] = get_mrt_flow(timestamp_df)

    #Get new row 
    timestamp_df_row = dict()  

    #Get variables that are the same for all stations
    for station_var in station_variables:
        timestamp_df_row[station_var] = timestamp_df[station_var][0]
        
    #Get previous timestamp mrt flows
    for ii in range(station_cnt):
        timestamp_df_row["previous_" + unique_station_names[ii] + "__mrt_flow"] = Y[i-1, ii]
    
        
    #Get variables that are unique for each station
    for var in other_variables:
        for row in range(len(timestamp_df[var])):
            timestamp_df_row[timestamp_df['mrt_station'][row] + "_" + var] = timestamp_df[var][row]

    timestamp_df_row = pd.DataFrame([timestamp_df_row])     
    df_new = pd.concat([df_new, timestamp_df_row], ignore_index=True)

print("Saving the dataset...")
df_new.to_csv("FinalData_reshaped.csv")
np.savetxt("FinalData_reshaped_Y.csv", Y, delimiter=",")