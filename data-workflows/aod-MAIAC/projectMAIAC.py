import os
import gdal
import glob2
input_folder= r'C:/Users/arpan/Documents/MAIAC/2016'
output_folder = r'C:/Users/arpan/Documents/MAIAC/2016/Projected'

latlon_info = gdal.Info(r'C:/Users/arpan/Documents/MAIAC/MAIACLatlon.h03v04.hdf')

#print(latlon_info)

gdal.Translate(r"C:/Users/arpan/Documents/MAIAC/h03v04_lat.vrt",
               r'HDF4_EOS:EOS_GRID:"C:/Users/arpan/Documents/MAIAC/MAIACLatlon.h03v04.hdf":latlon:lat', format = 'VRT' )

gdal.Translate(r"C:/Users/arpan/Documents/MAIAC/h03v04_lon.vrt",
               r'HDF4_EOS:EOS_GRID:"C:/Users/arpan/Documents/MAIAC/MAIACLatlon.h03v04.hdf":latlon:lon', format = 'VRT' )

filenames = glob2.glob(r'C:/Users/arpan/Documents/MAIAC/2016/*.hdf')
#print(filenames)

geoloc_xml = """<VRTDataset rasterXSize="1200" rasterYSize="1200">
  <Metadata domain="GEOLOCATION">
     <mdi key="X_DATASET">h03v04_lon.vrt</mdi>
     <mdi key="X_BAND">1</mdi>
     <mdi key="Y_DATASET">h03v04_lat.vrt</mdi>
     <mdi key="Y_BAND">1</mdi>
     <mdi key="PIXEL_OFFSET">0</mdi>
     <mdi key="LINE_OFFSET">0</mdi>
     <mdi key="PIXEL_STEP">1</mdi>
     <mdi key="LINE_STEP">1</mdi>
  </Metadata>
  <VRTRasterBand dataType="Float32" band="1">
    <ColorInterp>Gray</ColorInterp>
    <SimpleSource>
      <SourceFilename relativeToVRT="0">HDF4_EOS:EOS_GRID:"{fname}":grid1km:Optical_Depth_047</SourceFilename>
      <SourceBand>1</SourceBand>
      <SourceProperties RasterXSize="1200" RasterYSize="1200" DataType="Float32" BlockXSize="1200" BlockYSize="1200" />
      <SrcRect xOff="0" yOff="0" xSize="1200" ySize="1200" />
      <DstRect xOff="0" yOff="0" xSize="1200" ySize="1200" />
    </SimpleSource>
  </VRTRasterBand>
</VRTDataset>
"""

def geolocate_and_warp(input_filename, output_filename, output_crs='EPSG:4326'):
    with open(r'geoloc_test.vrt', 'w') as myfile:
        myfile.write(geoloc_xml.format(fname=input_filename))
    gdal.Translate(output_filename, r'geoloc_test.vrt', format = 'GTiff')



for filename in filenames:
    print ('Processing {fname}'.format(fname=filename))
    output_basename = os.path.join(output_folder,
                                  os.path.relpath(filename,
                                                 start = input_folder))
    output_dir = os.path.dirname(output_basename)
    AOT_filename = output_basename + "_proj.tif"

    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    geolocate_and_warp(filename, AOT_filename)
