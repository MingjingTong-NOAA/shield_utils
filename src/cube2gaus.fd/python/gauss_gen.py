#!/usr/bin/env python

import netCDF4
import numpy as np

from gauss_grid import *

if __name__=='__main__':
  N = 192
  lat, bnds = gaussian_latitudes(N)
  nlat = N*2
  nlon = N*4
  dlon = 360./nlon
  lon = np.arange(0.,360.,dlon)
  print lon

  rootgrp = netCDF4.Dataset('gaus_N'+str(N)+'.nc', 'w', format='NETCDF4')
  dim_lat = rootgrp.createDimension('lat', nlat)
  dim_lon = rootgrp.createDimension('lon', nlon)
  var_lat = rootgrp.createVariable('lat','f8',('lat',))
  var_lon = rootgrp.createVariable('lon','f8',('lon',))
  var_lat.units = 'degrees south'
  var_lon.units = 'degrees east'
  var_lat[:] = lat
  var_lon[:] = lon
  rootgrp.close()

