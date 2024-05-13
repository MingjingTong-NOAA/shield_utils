#!/usr/bin/env python

import netCDF4
import numpy as np

from gauss_grid import *

def make_norm_dist_data(center,radius,amp,lon,lat,var):
  nlon = np.size(lon)
  nlat = np.size(lat)
  lon2d, lat2d = np.meshgrid(lon,lat)
  nlev = np.shape(var)[0]

  for k in np.arange(nlev-1,nlev):
    for i in np.arange(nlon):
      for j in np.arange(nlat):
        dist = np.sqrt(np.square(lon2d[j,i]-center[0])+np.square(lat2d[j,i]-center[1]))
        var[k,j,i] = amp*np.exp(-dist*dist/2./radius/radius)


def gen_da_in_data(N,data_name):
  lat, bnds = gaussian_latitudes(N)
  nlat = N*2
  nlon = N*4
  dlon = 360./nlon
  lon = np.arange(0.,360.,dlon)
  lev = 63
  ilev = lev+1
  lvl = np.arange(lev)
  ilvl = np.arange(ilev)

  rootgrp = netCDF4.Dataset(data_name, 'w', format='NETCDF4')

  # dims
  dim_lat = rootgrp.createDimension('lat', nlat)
  var_lat = rootgrp.createVariable('lat','f4',('lat',))
  var_lat.units = 'degrees north'
  var_lat[:] = lat

  dim_lon = rootgrp.createDimension('lon', nlon)
  var_lon = rootgrp.createVariable('lon','f4',('lon',))
  var_lon.units = 'degrees east'
  var_lon[:] = lon

  dim_lvl  = rootgrp.createDimension('lev', lev)
  var_lvl  = rootgrp.createVariable('lev','f4',('lev',))
  var_lvl[:] = lvl

  dim_ilvl = rootgrp.createDimension('ilev', ilev)
  var_ilvl = rootgrp.createVariable('ilev','f4',('ilev',))
  var_ilvl[:] = ilvl

  # vars
  var_pf      = rootgrp.createVariable('pfull','f4',('lev',))
  var_hyai    = rootgrp.createVariable('hyai','f4',('ilev',))
  var_hybi    = rootgrp.createVariable('hybi','f4',('ilev',))
  var_u_inc   = rootgrp.createVariable('u_inc','f4',('lev','lat','lon',))
  var_v_inc   = rootgrp.createVariable('v_inc','f4',('lev','lat','lon',))
  var_dp_inc  = rootgrp.createVariable('delp_inc','f4',('lev','lat','lon',))
  var_pt_inc  = rootgrp.createVariable('T_inc','f4',('lev','lat','lon',))
  var_q_inc   = rootgrp.createVariable('sphum_inc','f4',('lev','lat','lon',))
  var_wat_inc = rootgrp.createVariable('liq_wat_inc','f4',('lev','lat','lon',))
  var_o3_inc  = rootgrp.createVariable('o3mr_inc','f4',('lev','lat','lon',))

  # put data to var
  var_pf[:]      = lvl
  var_hyai[:]    = ilvl
  var_hybi[:]    = ilvl
  var_u_inc[:]   = 0.
  var_v_inc[:]   = 0.
  var_dp_inc[:]  = 0.
  var_pt_inc[:]  = 0.
  var_q_inc[:]   = 0.
  var_wat_inc[:] = 0.
  var_o3_inc[:]  = 0.

  # put perturbation
  var = np.copy(var_pt_inc[:])
  make_norm_dist_data((90.,0.),70.,1,lon,lat,var)
  var_u_inc[:] = var

  rootgrp.close()

if __name__=='__main__':
  gen_da_in_data(384,'fake_ncep_inc_u.nc')

