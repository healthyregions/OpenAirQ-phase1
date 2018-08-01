import numpy as np
import xarray as xr
import rasterio
#from matplotlib.pyplot import *
import os
import datetime
import glob2
#import pandas as pd
#import netCDF4
#from rasterio_to_xarray import rasterio_to_xarray, xarray_to_rasterio
#import rasterio_to_xarray

def rasterio_to_xarray(fname):
    with rasterio.Env():
        with rasterio.open(fname) as src:
            data = src.read(1)

            # Set values to nan wherever they are equal to the nodata
            # value defined in the input file
            data = np.where(data == src.nodata, np.nan, data)

            # Get coords
            nx, ny = src.width, src.height
            x0, y0 = src.bounds.left, src.bounds.top
            dx, dy = src.res[0], -src.res[1]

            coords = {'y': np.arange(start=y0, stop=(y0 + ny * dy), step=dy),
                      'x': np.arange(start=x0, stop=(x0 + nx * dx), step=dx)}

            dims = ('y', 'x')

            attrs = {}

            try:
                aff = src.affine
                attrs['affine'] = aff.to_gdal()
            except AttributeError:
                pass

            try:
                c = src.crs
                attrs['crs'] = c.to_string()
            except AttributeError:
                pass

    return xr.DataArray(data, dims=dims, coords=coords, attrs=attrs)



def maiac_file_to_da(filename):
    da = rasterio_to_xarray(filename)

    time_str = os.path.basename(filename)[17:-13]
    time_obj = datetime.datetime.strptime(time_str, '%Y%j%H%M')
    da.coords['time'] = time_obj

    return da

folder = r'C:/Users/arpan/Documents/MAIAC/2016/Projected'

files = glob2.glob(r'C:/Users/arpan/Documents/MAIAC/2016/Projected/*')
print(files)
list_of_das = map(maiac_file_to_da, files)
MAIAC_AOT = xr.concat(list_of_das, 'time')
#print(MAIAC_AOT)
reordered_MAIAC_AOT = MAIAC_AOT.isel(time=np.argsort(MAIAC_AOT.time))
Daily_AOT = reordered_MAIAC_AOT.resample(time="D").median('time')
Daily_AOT = Daily_AOT.dropna(dim='time', how='all')
print('Loaded all data for folder: {folder}'.format(folder=folder))
g = Daily_AOT.groupby('time.month')

print(g.groups.items())

for month, indices in g.groups.items():
    print('Processing month: {month}'.format(month=month))
    subset = Daily_AOT.isel(time=indices)
    sub_ds = subset.to_dataset(name = 'data')

    filename = r'C:/Users/arpan/Documents/MAIAC/nc_monthly/Projected_{month}_AOT.nc'.format(fol_name=os.path.basename(folder), month=month)
    #print(filename)
    sub_ds.to_netcdf(filename, mode = 'w', format = "NETCDF4")

    print('Processed {fname}'.format(fname=folder))
