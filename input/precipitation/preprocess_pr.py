import xarray as xr
import glob
import os

# Set path to your NetCDF files
data_dir = "/home/dmercado/Documents/balance-particioning"
file_pattern = os.path.join(data_dir, "input/precipitation/GPCC_total_precipitation_mon_0.25x0.25_global_*.nc")
file_list = sorted(glob.glob(file_pattern))

# Open all files and combine by coordinates
datasets = []
for file in file_list:
    print("Opening", file)
    ds = xr.open_dataset(file, decode_times=True)
    datasets.append(ds)

# Concatenate manually along time
ds = xr.concat(datasets, dim="time")

# Inspect the variable
print(ds)

# Optional: replace fill values with NaN
#ds['pr'] = ds['pr'].where(ds['pr'] < 1e10)

# Convert time to datetime (if not already)
ds['time'] = xr.decode_cf(ds).time

# Group by year and compute ANNUAL TOTAL or MEAN
# Option 1: Annual total (more typical for precipitation)
pr_annual = ds['pr'].resample(time='YS').sum(dim='time')

# Option 2: Annual mean (less common for precipitation)
# pr_annual = ds['pr'].resample(time='YS').mean(dim='time')

# Create dataset to save
ds_annual = pr_annual.to_dataset(name='pr')

# Export to NetCDF: GPCC_total_precipitation_annual_0.25x0.25_global_1950-2019
output_path = os.path.join(data_dir, "input/precipitation.nc")
ds_annual.to_netcdf(output_path)

print("Annual file saved to:", output_path)
