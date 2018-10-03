'From Cuis 5.0 of 7 November 2016 [latest update: #3464] on 3 October 2018 at 9:15:48 am'!
'Description Please enter a description for this package'!
!provides: 'GeographicDatasets' 1 13!
!requires: 'ImageProcessing' 1 nil nil!
!requires: 'WebClient' 1 16 nil!
!requires: 'Cartography' 1 1 nil!
SystemOrganization addCategory: #'GeographicDatasets-Tests'!
SystemOrganization addCategory: #GeographicDatasets!


!classDefinition: #EGM96Geoid5Test category: #'GeographicDatasets-Tests'!
TestCase subclass: #EGM96Geoid5Test
	instanceVariableNames: 'instance'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets-Tests'!
!classDefinition: 'EGM96Geoid5Test class' category: #'GeographicDatasets-Tests'!
EGM96Geoid5Test class
	instanceVariableNames: ''!

!classDefinition: #ShuttleRadarTopographyMission3Test category: #'GeographicDatasets-Tests'!
TestCase subclass: #ShuttleRadarTopographyMission3Test
	instanceVariableNames: 'instance'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets-Tests'!
!classDefinition: 'ShuttleRadarTopographyMission3Test class' category: #'GeographicDatasets-Tests'!
ShuttleRadarTopographyMission3Test class
	instanceVariableNames: ''!

!classDefinition: #TiledECEFSampledWorldTest category: #'GeographicDatasets-Tests'!
TestCase subclass: #TiledECEFSampledWorldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets-Tests'!
!classDefinition: 'TiledECEFSampledWorldTest class' category: #'GeographicDatasets-Tests'!
TiledECEFSampledWorldTest class
	instanceVariableNames: ''!

!classDefinition: #TiledECEFVariableSampledDEMTest category: #'GeographicDatasets-Tests'!
TestCase subclass: #TiledECEFVariableSampledDEMTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets-Tests'!
!classDefinition: 'TiledECEFVariableSampledDEMTest class' category: #'GeographicDatasets-Tests'!
TiledECEFVariableSampledDEMTest class
	instanceVariableNames: ''!

!classDefinition: #TrueMarbleTest category: #'GeographicDatasets-Tests'!
TestCase subclass: #TrueMarbleTest
	instanceVariableNames: 'instance'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets-Tests'!
!classDefinition: 'TrueMarbleTest class' category: #'GeographicDatasets-Tests'!
TrueMarbleTest class
	instanceVariableNames: ''!

!classDefinition: #MeridiansAndParallelsSampledWorld category: #GeographicDatasets!
Object subclass: #MeridiansAndParallelsSampledWorld
	instanceVariableNames: 'nominalGSD'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'MeridiansAndParallelsSampledWorld class' category: #GeographicDatasets!
MeridiansAndParallelsSampledWorld class
	instanceVariableNames: ''!

!classDefinition: #SampledTile category: #GeographicDatasets!
Object subclass: #SampledTile
	instanceVariableNames: 'samples'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'SampledTile class' category: #GeographicDatasets!
SampledTile class
	instanceVariableNames: 'nullInstance'!

!classDefinition: #ValidableTile category: #GeographicDatasets!
SampledTile subclass: #ValidableTile
	instanceVariableNames: 'needsValidation'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'ValidableTile class' category: #GeographicDatasets!
ValidableTile class
	instanceVariableNames: ''!

!classDefinition: #TiledSampledWorld category: #GeographicDatasets!
Object subclass: #TiledSampledWorld
	instanceVariableNames: 'name allTiles cachedTiles datasetPath'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'TiledSampledWorld class' category: #GeographicDatasets!
TiledSampledWorld class
	instanceVariableNames: 'instance'!

!classDefinition: #TiledECEFSampledWorld category: #GeographicDatasets!
TiledSampledWorld subclass: #TiledECEFSampledWorld
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'TiledECEFSampledWorld class' category: #GeographicDatasets!
TiledECEFSampledWorld class
	instanceVariableNames: ''!

!classDefinition: #TiledECEFUniformlySampledWorld category: #GeographicDatasets!
TiledECEFSampledWorld subclass: #TiledECEFUniformlySampledWorld
	instanceVariableNames: 'voxelSide samplesPerTileSide'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'TiledECEFUniformlySampledWorld class' category: #GeographicDatasets!
TiledECEFUniformlySampledWorld class
	instanceVariableNames: ''!

!classDefinition: #TiledECEFNaturalEarth category: #GeographicDatasets!
TiledECEFUniformlySampledWorld subclass: #TiledECEFNaturalEarth
	instanceVariableNames: 'sourceDataset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'TiledECEFNaturalEarth class' category: #GeographicDatasets!
TiledECEFNaturalEarth class
	instanceVariableNames: ''!

!classDefinition: #TiledECEFVariableSampledWorld category: #GeographicDatasets!
TiledECEFSampledWorld subclass: #TiledECEFVariableSampledWorld
	instanceVariableNames: 'tilesPerFaceSide maxSamplesPerTileSide'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'TiledECEFVariableSampledWorld class' category: #GeographicDatasets!
TiledECEFVariableSampledWorld class
	instanceVariableNames: ''!

!classDefinition: #TiledECEFVariableSampledDEM category: #GeographicDatasets!
TiledECEFVariableSampledWorld subclass: #TiledECEFVariableSampledDEM
	instanceVariableNames: 'sourceGeoid sourceDEM'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'TiledECEFVariableSampledDEM class' category: #GeographicDatasets!
TiledECEFVariableSampledDEM class
	instanceVariableNames: ''!

!classDefinition: #TiledECEFVariableSampledNaturalEarth category: #GeographicDatasets!
TiledECEFVariableSampledWorld subclass: #TiledECEFVariableSampledNaturalEarth
	instanceVariableNames: 'sourceDataset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'TiledECEFVariableSampledNaturalEarth class' category: #GeographicDatasets!
TiledECEFVariableSampledNaturalEarth class
	instanceVariableNames: ''!

!classDefinition: #TiledLatitudeLongitudeSampledWorld category: #GeographicDatasets!
TiledSampledWorld subclass: #TiledLatitudeLongitudeSampledWorld
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'TiledLatitudeLongitudeSampledWorld class' category: #GeographicDatasets!
TiledLatitudeLongitudeSampledWorld class
	instanceVariableNames: ''!

!classDefinition: #EGM96Geoid5 category: #GeographicDatasets!
TiledLatitudeLongitudeSampledWorld subclass: #EGM96Geoid5
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'EGM96Geoid5 class' category: #GeographicDatasets!
EGM96Geoid5 class
	instanceVariableNames: ''!

!classDefinition: #NaturalEarth category: #GeographicDatasets!
TiledLatitudeLongitudeSampledWorld subclass: #NaturalEarth
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'NaturalEarth class' category: #GeographicDatasets!
NaturalEarth class
	instanceVariableNames: ''!

!classDefinition: #ShuttleRadarTopographyMission category: #GeographicDatasets!
TiledLatitudeLongitudeSampledWorld subclass: #ShuttleRadarTopographyMission
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'ShuttleRadarTopographyMission class' category: #GeographicDatasets!
ShuttleRadarTopographyMission class
	instanceVariableNames: ''!

!classDefinition: #ShuttleRadarTopographyMission3 category: #GeographicDatasets!
ShuttleRadarTopographyMission subclass: #ShuttleRadarTopographyMission3
	instanceVariableNames: 'cacheSize logFilename'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'ShuttleRadarTopographyMission3 class' category: #GeographicDatasets!
ShuttleRadarTopographyMission3 class
	instanceVariableNames: ''!

!classDefinition: #ShuttleRadarTopographyMission30 category: #GeographicDatasets!
ShuttleRadarTopographyMission subclass: #ShuttleRadarTopographyMission30
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'ShuttleRadarTopographyMission30 class' category: #GeographicDatasets!
ShuttleRadarTopographyMission30 class
	instanceVariableNames: ''!

!classDefinition: #TrueMarble category: #GeographicDatasets!
TiledLatitudeLongitudeSampledWorld subclass: #TrueMarble
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'TrueMarble class' category: #GeographicDatasets!
TrueMarble class
	instanceVariableNames: ''!

!classDefinition: #TiledWebmercatorSampledWorld category: #GeographicDatasets!
TiledSampledWorld subclass: #TiledWebmercatorSampledWorld
	instanceVariableNames: 'projection zoomLevel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'TiledWebmercatorSampledWorld class' category: #GeographicDatasets!
TiledWebmercatorSampledWorld class
	instanceVariableNames: ''!

!classDefinition: #GoogleSampledWorld category: #GeographicDatasets!
TiledWebmercatorSampledWorld subclass: #GoogleSampledWorld
	instanceVariableNames: 'loPassSigma'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GeographicDatasets'!
!classDefinition: 'GoogleSampledWorld class' category: #GeographicDatasets!
GoogleSampledWorld class
	instanceVariableNames: ''!


!MeridiansAndParallelsSampledWorld commentStamp: '<historical>' prior: 0!
A synthetic dataset that simply plots a latitude and longitude grid.!

!SampledTile commentStamp: '<historical>' prior: 0!
A sampled tile for any subclass of TiledSampledWorld. Holds 'samples'. If nil, the tile is null (no data available).!

!ValidableTile commentStamp: '<historical>' prior: 0!
Tiles that require a validation before being queried for values.
In particular, for SRTM 3 datasets, to enforce the invariant that the last column and last row must equal first column or row of nieghbor.!

!TiledSampledWorld commentStamp: '<historical>' prior: 0!
A TiledSampledWorld holds a sampled image or magnitude of the whole world. Useful for DEM (like STRM) or global images (like TrueMarble or BlueMarble).

Sampled data can be, for example, 16bit ints (like STRM) or 16, 24 or 32 bit RGB (like trueMarble). Float32 and Float64 values are possible too.

Fine sampling of the whole world would result in too high memory usage.

Ivar allTiles is an Array2D with size to hold all possible tiles. Each tile is a (sub)instance of SampledTile. At each position, it can hold:
	- nil: Have no information. Must build the tile from the tile file if needed.
	- SampledTile with samples = nil. All samples have null value (0 if numbers, Black if colors, etc.). No need to hold data. The data file does not exist (because of being missing, or being intentionally not included, i.e. SRTM3 at the oceans)
	- SampledTile with samples notNil. Might be a FloatImage, Form, etc.
	
Ivar cachedTiles is a FIFO holding the positions of cached regular tiles.

When a missing tile is requested, it is built and stored at allTiles. The position is added as first to cachedTiles. If cache is already full, then the last element of cachedTiles is removed and removed from allTiles (replaced by nil), to be garbage collected.

Several interpolation approaches are possible (but must be hooked in!!).

General references on projections
http://www.remotesensing.org/geotiff/spec/geotiff2.5.html
https://en.wikipedia.org/wiki/List_of_map_projections

Note: everywhere in this hierarchy, the following coordinates are zero-based:
- position of a tile
- position of a sample inside a tile
- position of a sample inside the "global" sample space (or face for ECEF subclasses)!

!TiledECEFSampledWorld commentStamp: '<historical>' prior: 0!
Provide an alternative and convenient sampling of the world surface.

Here, we want a sampling that is cheap to evalate for ECEF coordinates (quite as if converted to LLA gps coordinates, and altitude was discarded). 
We use a cube, and each face is in "Earth from space" projection: https://en.wikipedia.org/wiki/Orthographic_projection_in_cartography.

This makes conversion from ECEF trivial.

Could also be called 'OrthographicSphericalCubeWorld' or such.
!

!TiledECEFUniformlySampledWorld commentStamp: '<historical>' prior: 0!
All our tiles are sampled uniformly and have the same size.!

!TiledECEFNaturalEarth commentStamp: '<historical>' prior: 0!
For comfortably experimenting with ECEF sampled datasets.
A nice looking, low resolution, NaturalEarth.

Note: TiledECEFSampledWorlds are Spheres. So, TiledECEFNaturalEarth is most likely not geometrically precise if you want really good geometric accuracy.!

!TiledECEFVariableSampledWorld commentStamp: '<historical>' prior: 0!
Each tile can have different size and resolution. Sampling is done with normalized coordinates, [0.0 .. 1.0] x [0.0 .. 1.0] in the OpenCL style. See FloatImage>>bilinearInterpolatedValueAtOpenCLClampToEdgeNormalizedX:y: .

- tilesPerFaceSide is the number of tiles each face is divided in, both horizontally and vertically.
- maxSamplesPerTileSide is a hint for building tiles. Do not use tiles larger than this.!

!TiledECEFVariableSampledDEM commentStamp: 'jmv 8/13/2018 13:35:30' prior: 0!
ECEF sampled DEM with tiles of uniform extent in metres, but with varying resolution (and sample count).

TiledECEFVariableSampledDEM loRes exampleSanFrancisco displayAutoRangeAt: 0@0 zoom: 1.
TiledECEFVariableSampledDEM hiRes exampleSanFrancisco displayAutoRangeAt: 0@0 zoom: 1.

TiledECEFVariableSampledDEM hiRes exampleBuenosAires displayAutoRangeAt: 0@0 zoom: 1.
TiledECEFVariableSampledDEM loRes exampleArgentina displayAutoRangeAt: 0@0 zoom: 1.
TiledECEFVariableSampledDEM loRes exampleWorld displayAutoRangeAt: 0@0 zoom: 1.!

!TiledECEFVariableSampledNaturalEarth commentStamp: '<historical>' prior: 0!
Quite like TiledECEFNaturalEarth, but with tiles of non-equal size. Also, tiles are instances of FloatMatrix (i.e. monochrome).!

!TiledLatitudeLongitudeSampledWorld commentStamp: '<historical>' prior: 0!
Sampling is done in an Equirrectangular projection, in particular Plate Carrée. This uniformly sampled geodetic Latitude and Longitude. See https://en.wikipedia.org/wiki/Equirectangular_projection
This is the same projection used by most global raster datasets (BlueMarble, TrueMarble, DEMs, etc).

Fine sampling of the whole world would result in too high memory usage. Tiles include an integer number of degrees of latitude and longitude. There is a cache that forgets LRU tiles.!

!EGM96Geoid5 commentStamp: 'jmv 8/13/2018 13:35:48' prior: 0!
This class implements the EGM96 Geoid Dataset with a sample grid of 5 minutes. It is based on the data file obtained from: http://geographiclib.sourceforge.net/html/geoid.html#geoidinst. 

Aconcagua:
EGM96Geoid5 getInstance atLatitude: -32.6534 longitude: -70.0111

EGM96Geoid5 getInstance exampleArgentina displayAutoRangeAt: 0@0 zoom: 1.
EGM96Geoid5 getInstance exampleWorld displayAutoRangeAt: 0@0 zoom: 1.!

!NaturalEarth commentStamp: 'jmv 8/13/2018 13:36:01' prior: 0!
Cool RGB images of the whole world.

NaturalEarth getInstance exampleArgentina displayAutoRangeAt: 0@0 zoom: 1.
NaturalEarth getInstance exampleWorld displayAutoRangeAt: 0@0 zoom: 1.
!

!ShuttleRadarTopographyMission commentStamp: '<historical>' prior: 0!
Abstract superclass for SRTM and SRTM-like DEMs.!

!ShuttleRadarTopographyMission3 commentStamp: 'jmv 8/13/2018 13:36:15' prior: 0!
3 arc sec. 16bit signed data. Horizontal resolution is 3 arc sec (lat/lon) or about 90 meters. Vertical resolution is 1 m. Vertical accuracy is about 17m. Relative to WGS84 / EGM96 geoid (see http://en.wikipedia.org/wiki/EGM96 ).

Aconcagua:

(ShuttleRadarTopographyMission3 getInstance atLatitude: -32.653333 longitude: -70.010833) + (EGM96Geoid5 getInstance atLatitude: -32.653333 longitude: -70.010833). 6961.112276686436.
Official (https://en.wikipedia.org/wiki/Aconcagua) : 6961 :)

ShuttleRadarTopographyMission3 getInstance atLatitude: -80 longitude: 180
ShuttleRadarTopographyMission3 getInstance atLatitude: -30 longitude: 180
ShuttleRadarTopographyMission3 getInstance atLatitude: -90 longitude: 180

ShuttleRadarTopographyMission3 getInstance exampleSanFrancisco displayAutoRangeAt: 0@0 zoom: 1.
ShuttleRadarTopographyMission3 getInstance exampleBuenosAires displayAutoRangeAt: 0@0 zoom: 1.!

!ShuttleRadarTopographyMission30 commentStamp: 'jmv 8/13/2018 13:36:48' prior: 0!
30 arc sec. 16bit signed data. Horizontal resolution is 30 arc sec (lat/lon) or about 900 meters. Vertical resolution is also of approximately 900m. Relative to WGS84 / EGM96 geoid (see http://en.wikipedia.org/wiki/EGM96 ).

Aconcagua (slightly off, this answers the highest sample around):
	ShuttleRadarTopographyMission30 getInstance atLatitude: -32.6625 longitude: -70.02083333.

ShuttleRadarTopographyMission30 getInstance atLatitude: -80 longitude: 180
ShuttleRadarTopographyMission30 getInstance atLatitude: -30 longitude: 180
ShuttleRadarTopographyMission30 getInstance atLatitude: -90 longitude: 180
ShuttleRadarTopographyMission30 getInstance atLatitude: -80 longitude: 179.99
ShuttleRadarTopographyMission30 getInstance atLatitude: -30 longitude: 179.99
ShuttleRadarTopographyMission30 getInstance atLatitude: -90 longitude: 179.99

ShuttleRadarTopographyMission30 getInstance exampleSanFrancisco displayAutoRangeAt: 0@0 zoom: 1.
ShuttleRadarTopographyMission30 getInstance exampleBuenosAires displayAutoRangeAt: 0@0 zoom: 1.
ShuttleRadarTopographyMission30 getInstance exampleArgentina displayAutoRangeAt: 0@0 zoom: 1.!

!TrueMarble commentStamp: 'jmv 8/13/2018 13:37:27' prior: 0!
Cool RGB images of the whole world. Resolution is 7.5 arc sec (lat/lon), or about 250 meters.

TrueMarble getInstance exampleSanFrancisco displayAutoRangeAt: 0@0 zoom: 1.
TrueMarble getInstance exampleBuenosAires displayAutoRangeAt: 0@0 zoom: 1.

For example, for original tile A1, geotiff data (read with listgeo.exe):
----------------------------------------------------------------------------------------------------
Geotiff_Information:
   Version: 1
   Key_Revision: 1.0
   Tagged_Information:
      ModelTiepointTag (2,3):
         0                0                0                
         -180             90               0                
      ModelPixelScaleTag (1,3):
         0.00208333333    0.00208333333    0                
      End_Of_Tags.
   Keyed_Information:
      GTModelTypeGeoKey (Short,1): ModelTypeGeographic
      GTRasterTypeGeoKey (Short,1): RasterPixelIsArea
      GeographicTypeGeoKey (Short,1): GCS_WGS_84
      GeogCitationGeoKey (Ascii,7): "WGS 84"
      GeogAngularUnitsGeoKey (Short,1): Angular_Degree
      End_Of_Keys.
   End_Of_Geotiff.

GCS: 4326/WGS 84
Datum: 6326/World Geodetic System 1984
Ellipsoid: 7030/WGS 84 (6378137.00,6356752.31)
Prime Meridian: 8901/Greenwich (0.000000/  0d 0' 0.00"E)

Corner Coordinates:
Upper Left    (180d 0' 0.00"W, 90d 0' 0.00"N)
Lower Left    (180d 0' 0.00"W, 45d 0' 0.00"N)
Upper Right   (135d 0' 0.00"W, 90d 0' 0.00"N)
Lower Right   (135d 0' 0.00"W, 45d 0' 0.00"N)
Center        (157d30' 0.00"W, 67d30' 0.00"N)
-----------------------------------------------------------------------------------------------

But as each pixel represents an area of 7.5x7.5 arc sec, the center of topLeft pixel is:
179d 59' 56.25"W  or 179.9989583333333W
89d 59' 56.25"N  or 89.9989583333333N

http://www.remotesensing.org/geotiff/faq.html#Who%20owns%20GeoTIFF%20Format?

What is the effect of PixelIsArea vs. PixelIsPoint?

Setting the GTRasterTypeGeoKey value to RasterPixelIsPoint or RasterPixelIsArea alters how the raster coordinate space is to be interpreted. This is defined in section 2.5.2.2 of the GeoTIFF specification. In the case of PixelIsArea (default) a pixel is treated as an area and the raster coordinate (0,0) is the top left corner of the top left pixel. PixelIsPoint treats pixels as point samples with empty space between the "pixel" samples. In this case raster (0,0) is the location of the top left raster pixel.
The net effect is a half pixel offset when interpreting PixelIsPoint vs. PixelIsArea images. There has been some confusion on the intepretation of this value in the GeoTIFF community for some time and some held the opinion that this parameter should have no effect on the georeferencing of the image. While this matter has been settled, the result is that some software packages, particularly those built on GDAL 1.7.x or older incorrectly interprete the georeferenced location of PixelIsPoint images - placing them offset by half a pixel. One way of avoiding compatability problems is to produce GeoTIFF files with the default PixelIsArea interpretation.

For us, I (jmv) divided each TrueMarble tile into 5x5 smaller tiles, and saved each of these in a file. Each smaller tile is 9x9 degrees of long/lat (original are 45x45 degrees) and 4320x4320 pixels (original are 21600x21600 pxels). This is to save memory, and better caching behavior.!

!TiledWebmercatorSampledWorld commentStamp: '<historical>' prior: 0!
Tiles are web mercator projection, with integer scale, as used by most popular mapping apps.

zoomLevel is an integer greater or equal to zero. Tiles are always 256@256 in extent. The world is divided in (2 ^ zoomLevel) tiles in each direction. Total tiles is (2^(2*zoomLevel)). Tiles have no overlap. The origin is the 'topLeft' corner of the topLeft pixel. This means that the value for the topLeft pixel is actually for coordinates 0.5@0.5.

See https://wiki.openstreetmap.org/wiki/Zoom_levels!

!GoogleSampledWorld commentStamp: 'jmv 8/13/2018 13:37:38' prior: 0!
Google maps tiles.

(GoogleSampledWorld zoomLevel: 0) exampleArgentina displayAutoRangeAt: 0@0 zoom: 1.!

!EGM96Geoid5Test methodsFor: 'setUp - tearDown' stamp: 'jmv 3/4/2016 10:39'!
setUp
	instance _ EGM96Geoid5 getInstance! !

!EGM96Geoid5Test methodsFor: 'testing' stamp: 'jmv 3/23/2016 12:23'!
testGlobaPixelPosition
	"As specified at http://geographiclib.sourceforge.net/html/geoid.html
	# Origin 90N 0E
	"
	self assert: (instance globalPixelPositionForLatitude: 90 longitude: 0) = (0@0).
	self assert: (instance globalPixelPositionForLatitude: -90 longitude: 359.999999) rounded = ((instance tileAt: 0@0) samples extent-1)! !

!EGM96Geoid5Test methodsFor: 'testing' stamp: 'jmv 11/10/2015 12:57'!
testInterpolationBeyondEnds

	"Test preconditions. If not true, can not do test."
	self assert: (instance atLatitude: -37 longitude: 359) > 16 description: 'Test assumption failed'.
	self assert: (instance atLatitude: -37 longitude: 360) > 16 description: 'Test assumption failed'.

	"Real test. Stuff we want to assert."
	self assert: (instance atLatitude: -37 longitude: 359.95) > 16 description: 'Looks like trying to interpolate outside bounds'.
	self assert: (instance atLatitude: -37 longitude: 359.995) > 16 description: 'Looks like trying to interpolate outside bounds'.! !

!ShuttleRadarTopographyMission3Test methodsFor: 'setUp - tearDown' stamp: 'jmv 3/4/2016 10:40'!
setUp
	instance _ ShuttleRadarTopographyMission3 getInstance! !

!ShuttleRadarTopographyMission3Test methodsFor: 'testing' stamp: 'jmv 1/22/2016 16:21'!
testFilenames

	| tileExtentSamples |
	self assert: (instance filenameForTileAt: 0@0) = 'N89W180.hgt'.
	self assert: (instance filenameForTileAt: 0@1) = 'N88W180.hgt'.
	self assert: (instance filenameForTileAt: 0@88) = 'N01W180.hgt'.
	self assert: (instance filenameForTileAt: 0@89) = 'N00W180.hgt'.
	self assert: (instance filenameForTileAt: 0@90) = 'S01W180.hgt'.
	self assert: (instance filenameForTileAt: 0@91) = 'S02W180.hgt'.
	self assert: (instance filenameForTileAt: 0@178) = 'S89W180.hgt'.
	self assert: (instance filenameForTileAt: 0@179) = 'S90W180.hgt'.

	self assert: (instance filenameForTileAt: 0@89) = 'N00W180.hgt'.
	self assert: (instance filenameForTileAt: 1@89) = 'N00W179.hgt'.
	self assert: (instance filenameForTileAt: 2@89) = 'N00W178.hgt'.
	self assert: (instance filenameForTileAt: 102@89) = 'N00W078.hgt'.
	self assert: (instance filenameForTileAt: 172@89) = 'N00W008.hgt'.
	self assert: (instance filenameForTileAt: 178@89) = 'N00W002.hgt'.
	self assert: (instance filenameForTileAt: 179@89) = 'N00W001.hgt'.
	self assert: (instance filenameForTileAt: 180@89) = 'N00E000.hgt'.
	self assert: (instance filenameForTileAt: 181@89) = 'N00E001.hgt'.
	self assert: (instance filenameForTileAt: 280@89) = 'N00E100.hgt'.
	self assert: (instance filenameForTileAt: 350@89) = 'N00E170.hgt'.
	self assert: (instance filenameForTileAt: 358@89) = 'N00E178.hgt'.
	self assert: (instance filenameForTileAt: 359@89) = 'N00E179.hgt'.

	tileExtentSamples _ instance class tileWidthInSamples @ instance class tileHeightInSamples.
	
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: -0.9 longitude: -100) // tileExtentSamples) = 'S01W100.hgt'.
	
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: -90 longitude: -180) // tileExtentSamples) = 'S90W180.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: -90 longitude: -100) // tileExtentSamples) = 'S90W100.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: -90 longitude: -99) // tileExtentSamples) = 'S90W099.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: -90 longitude: -1) // tileExtentSamples) = 'S90W001.hgt'.

	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: -89.5 longitude: -180) // tileExtentSamples) = 'S90W180.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: -89.5 longitude: -100) // tileExtentSamples) = 'S90W100.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: -89.5 longitude: -99) // tileExtentSamples) = 'S90W099.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: -89.5 longitude: -1) // tileExtentSamples) = 'S90W001.hgt'.

	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: 90 longitude: -180) // tileExtentSamples) = 'N89W180.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: 90 longitude: -100) // tileExtentSamples) = 'N89W100.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: 90 longitude: -99) // tileExtentSamples) = 'N89W099.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: 90 longitude: -1) // tileExtentSamples) = 'N89W001.hgt'.

	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: 89.5 longitude: -180) // tileExtentSamples) = 'N89W180.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: 89.5 longitude: -100) // tileExtentSamples) = 'N89W100.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: 89.5 longitude: -99) // tileExtentSamples) = 'N89W099.hgt'.
	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: 89.5 longitude: -1) // tileExtentSamples) = 'N89W001.hgt'.

	self assert: (instance filenameForTileAt: (instance globalPixelPositionForLatitude: 0.0 longitude: 179.99999) // tileExtentSamples) = 'S01E179.hgt'.! !

!ShuttleRadarTopographyMission3Test methodsFor: 'testing' stamp: 'jmv 1/22/2016 16:21'!
testGlobaPixelPosition
	"As specified in http://www2.jpl.nasa.gov/srtm/faq.html"
	| tileExtentSamples |
	self assert: (instance globalPixelPositionForLatitude: 90 longitude: -180) = (0@0).
	tileExtentSamples _ instance class tileWidthInSamples @ instance class tileHeightInSamples.
	self assert: (instance globalPixelPositionForLatitude: -90 longitude: 179.999999) rounded = (ShuttleRadarTopographyMission3 tileCount * tileExtentSamples)! !

!TiledECEFSampledWorldTest methodsFor: 'testing' stamp: 'jmv 3/23/2016 16:41'!
test01
	"
	TiledECEFSampledWorldTest new test01
	"
	| ne r tile posInTile faceIndex |
	r _ GPSPosition wgs84SemiMajorAxis.
	ne _ TiledECEFVariableSampledNaturalEarth tilesPerFaceSide: 3 samplesPerTileSide: 101.
	{
		(1@0@0) normalized * r.
		(0@1@0) normalized * r.
		(0@0@1) normalized * r.
		(1@1@1) normalized * r.
		(1@1@1.01) normalized * r.
		(1@1@0.99) normalized * r } do: [ :ecef |
			faceIndex _ ne bestFaceFor: ecef.
			ne withTileAndPositionInTileOf: ecef atFace: faceIndex evaluate: [ :tilePos :openCLNormalizedPositionInTile |
				tile _ ne newTileSamples.
				posInTile _ (tile oneBasedCoordinatesForOpenCLNormalized: openCLNormalizedPositionInTile) - 1.
				ne 
					ecefOfFace: faceIndex 
					tilePosition: tilePos 
					positionInTile: posInTile
					samples: tile
					into: [ :recoveredECEF :pointBelongsInFace :pointBelongsInExtendedFace |
						pointBelongsInFace
							ifTrue: [ self assert: (recoveredECEF - ecef) length < 1e-6 ]
							ifFalse: [ self halt ]]]]! !

!TiledECEFSampledWorldTest methodsFor: 'testing' stamp: 'jmv 3/23/2016 16:41'!
test02
	"
	TiledECEFSampledWorldTest new test02
	"
	| ne tile |
	ne _ TiledECEFVariableSampledNaturalEarth tilesPerFaceSide: 1 samplesPerTileSide: 101.
	tile _ ne newTileSamples.
	
	"North pole"
	ne 
		ecefOfFace: 1
		tilePosition: 0@0 
		positionInTile: 50@50 
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (0@0@1) ]
				ifFalse: [ self halt ]].
	
	"South pole"
	ne 
		ecefOfFace: 6
		tilePosition: 0@0 
		positionInTile: 50@50
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (0@0@-1) ]
				ifFalse: [ self halt ]].
			
	"Points around Equator"
	ne 
		ecefOfFace: 2
		tilePosition: 0@0 
		positionInTile: 50@50
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (0@-1@0) ]
				ifFalse: [ self halt ]].
	ne 
		ecefOfFace: 3
		tilePosition: 0@0 
		positionInTile: 50@50
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (1@0@0) ]
				ifFalse: [ self halt ]].
	ne 
		ecefOfFace: 4
		tilePosition: 0@0 
		positionInTile: 50@50
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (0@1@0) ]
				ifFalse: [ self halt ]].
	ne 
		ecefOfFace: 5
		tilePosition: 0@0 
		positionInTile: 50@50
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (-1@0@0) ]
				ifFalse: [ self halt ]].! !

!TiledECEFSampledWorldTest methodsFor: 'testing' stamp: 'jmv 3/23/2016 16:41'!
test03
	"
	TiledECEFSampledWorldTest new test03
	"
	| ne tile |
	ne _ TiledECEFVariableSampledNaturalEarth tilesPerFaceSide: 3 samplesPerTileSide: 101.
	tile _ ne newTileSamples.
		"North pole"
	ne 
		ecefOfFace: 1
		tilePosition: 1@1 
		positionInTile: 50@50 
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (0@0@1) ]
				ifFalse: [ self halt ]].
	
	"South pole"
	ne 
		ecefOfFace: 6
		tilePosition: 1@1 
		positionInTile: 50@50
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (0@0@-1) ]
				ifFalse: [ self halt ]].
			
	"Points around Equator"
	ne 
		ecefOfFace: 2
		tilePosition: 1@1 
		positionInTile: 50@50
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (0@-1@0) ]
				ifFalse: [ self halt ]].
	ne 
		ecefOfFace: 3
		tilePosition: 1@1 
		positionInTile: 50@50
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (1@0@0) ]
				ifFalse: [ self halt ]].
	ne 
		ecefOfFace: 4
		tilePosition: 1@1 
		positionInTile: 50@50
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (0@1@0) ]
				ifFalse: [ self halt ]].
	ne 
		ecefOfFace: 5
		tilePosition: 1@1 
		positionInTile: 50@50
		samples: tile
		into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
			pointBelongsInFace 
				ifTrue: [ self assert: ecef normalized = (-1@0@0) ]
				ifFalse: [ self halt ]].! !

!TiledECEFSampledWorldTest methodsFor: 'testing' stamp: 'jmv 3/23/2016 16:41'!
test04
	"
	TiledECEFSampledWorldTest new test04
	"
	| ne tile |
	ne _ TiledECEFNaturalEarth tilesPerFaceSide: 3 samplesPerTileSide: 101.
	tile _ ne newTileSamples.
	"Last position of a tile is equivalent to first position in next tile"
	1 to: 6 do: [ :faceIndex |
		0 to: 1 do: [ :i |
			0 to: 1 do: [ :j |
				ne 
					ecefOfFace: faceIndex
					tilePosition: j@i
					positionInTile: 101@50
					samples: tile
					into: [ :ecef1 :pointBelongsInFace1 :pointBelongsInExtendedFace1 |
						ne 
							ecefOfFace: faceIndex
							tilePosition: j+1@i 
							positionInTile: 1@50 
							samples: tile
							into: [ :ecef2 :pointBelongsInFace2 :pointBelongsInExtendedFace2 |
								(pointBelongsInFace1 and: pointBelongsInFace2) 
									ifTrue: [ self assert: ecef1 = ecef2 ]
									ifFalse: [ self halt ]]].
				
				ne 
					ecefOfFace: faceIndex
					tilePosition: j@i 
					positionInTile: 50@101
					samples: tile
					into: [ :ecef3 :pointBelongsInFace3 :pointBelongsInExtendedFace3 |
						ne 
							ecefOfFace: faceIndex
							tilePosition: j@(i+1)
							positionInTile: 50@1
							samples: tile
							into: [ :ecef4 :pointBelongsInFace4 :pointBelongsInExtendedFace4 |
								(pointBelongsInFace3 and: pointBelongsInFace4) 
									ifTrue: [ self assert: ecef3 = ecef4 ]
									ifFalse: [ self halt ]]]]]]! !

!TiledECEFSampledWorldTest methodsFor: 'testing' stamp: 'jmv 3/23/2016 16:42'!
test05
	"
	TiledECEFSampledWorldTest new test05
	"
	| cornerEcef ne tile faceIndex |
	cornerEcef _ (1@-1@1) normalized * GPSPosition wgs84SemiMajorAxis.
	ne _ TiledECEFNaturalEarth tilesPerFaceSide: 3 samplesPerTileSide: 101.
	tile _ ne newTileSamples.
	faceIndex _ ne bestFaceFor: cornerEcef.
	ne withTileAndPositionInTileOf: cornerEcef atFace: faceIndex evaluate: [ :tilePos :pos |
		"test preconditions. If fail, rethink test."
		self assert: faceIndex = 1.
		self assert: tilePos = (2@2).
		self assert: (pos - (66.5352128002918@66.5352128002918)) r < 1.0e-12.
		"Now the actual test. The ecef for the corner is the same coming from any of the three faces"
		ne 
			ecefOfFace: 1
			tilePosition: 2@2 
			positionInTile: pos 
			samples: tile
			into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
				pointBelongsInFace 
					ifTrue: [ self assert: (ecef - cornerEcef) length < 1e-6 ]
					ifFalse: [ self halt ]].
		ne 
			ecefOfFace: 2
			tilePosition: 2@0 
			positionInTile: pos x @ (100-pos y)
			samples: tile
			into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
				pointBelongsInFace
					ifTrue:  [ self assert: (ecef - cornerEcef) length < 1e-6 ]
					ifFalse: [ self halt ]].
		
		ne 
			ecefOfFace: 3
			tilePosition: 0@0 
			positionInTile: (100-pos x) @ (100-pos y)
			samples: tile
			into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
				pointBelongsInFace
					ifTrue: [ self assert: (ecef - cornerEcef) length < 1e-6 ]
					ifFalse: [ self halt ]]]! !

!TiledECEFVariableSampledDEMTest methodsFor: 'testing' stamp: 'jmv 3/3/2016 11:51'!
test01
	"
	TiledECEFVariableSampledDEMTest new test01
	"

	| dem demH distanceToSphere1 distanceToSphere3 geoid geoidH lat0 lat1 long0 long1 sphereECEF sphericCubeDEM surfaceECEF |
	dem _ ShuttleRadarTopographyMission3 getInstance.
	geoid _ EGM96Geoid5 getInstance.
	sphericCubeDEM _ TiledECEFVariableSampledDEM getInstance.
	sphericCubeDEM sourceDEM: dem geoid: geoid.

	lat0 _ -34.7. lat1 _ -34.3.		"South"
	long0 _ -58.7. long1 _ -58.3. "West"

	long0 to: long1 count: 100 do: [ :long |
		lat0 to: lat1 count: 100 do: [ :lat |
			geoidH _ geoid atLatitude: lat longitude: long.
			demH _ dem atLatitude: lat longitude: long.
			surfaceECEF _ (GPSPosition latitude: lat longitude: long altitude: geoidH+demH) asECEF.
			sphereECEF _ surfaceECEF normalized * GPSPosition wgs84SemiMajorAxis.
			distanceToSphere1 _ surfaceECEF length - sphereECEF length.

			self assert: (distanceToSphere1 abs - (surfaceECEF - sphereECEF) length) abs < 1e-8 description: 'surfaceECEF and sphereECEF must be colinear'.

			distanceToSphere3 _ sphericCubeDEM atECEF: sphereECEF.
			
			self assert: (distanceToSphere1 - distanceToSphere3) abs < 10 description: 'Spheric ECEF DEM not equal source dataset'
			]]! !

!TrueMarbleTest methodsFor: 'setUp - tearDown' stamp: 'jmv 3/4/2016 10:39'!
setUp
	instance _ TrueMarble getInstance! !

!TrueMarbleTest methodsFor: 'testing' stamp: 'jmv 1/22/2016 16:18'!
testGlobaPixelPosition
	"See TrueMarble class comment"
	| latLongForHalfAPixel lastX lastY |
	latLongForHalfAPixel _ TrueMarble sampleWidthSeconds/3600*0.5.
	lastX _ TrueMarble tileWidthInSamples * TrueMarble tileCount x - 1.
	lastY _ TrueMarble tileHeightInSamples * TrueMarble tileCount y - 1.
	self assert: (instance globalPixelPositionForLatitude: 90-latLongForHalfAPixel longitude: -180+latLongForHalfAPixel) r < 1.0e-10.
	self assert: ((instance globalPixelPositionForLatitude: 90-latLongForHalfAPixel longitude: 180-latLongForHalfAPixel) - (lastX@0)) r < 1.0e-10.
	self assert: ((instance globalPixelPositionForLatitude: -90+latLongForHalfAPixel longitude: -180+latLongForHalfAPixel) - (0@lastY)) r < 1.0e-10.
	self assert: ((instance globalPixelPositionForLatitude: -90+latLongForHalfAPixel longitude: 180-latLongForHalfAPixel) - (lastX@lastY)) r < 1.0e-10.
	
	self shouldnt: [ instance atLatitude: 90 longitude: -180 ] raise: Error.
	self shouldnt: [ instance atLatitude: -90 longitude: -180 ] raise: Error.
	self shouldnt: [ instance atLatitude: 90 longitude: 180 ] raise: Error.
	self shouldnt: [ instance atLatitude: -90 longitude: 180 ] raise: Error.
	self shouldnt: [ instance atLatitude: 90 longitude: 179.999999 ] raise: Error.
	self shouldnt: [ instance atLatitude: -90 longitude: 179.999999 ] raise: Error.! !

!MeridiansAndParallelsSampledWorld methodsFor: 'accessing' stamp: 'jmv 5/3/2016 15:15'!
atGPSPosition: aGPSPosition
	^self atLatitude: aGPSPosition latitude longitude: aGPSPosition longitude! !

!MeridiansAndParallelsSampledWorld methodsFor: 'accessing' stamp: 'jmv 7/13/2016 15:08:39'!
atLatitude: lat longitude: lon
	"latitude in -90.0 (south pole) to 90.0 (north pole)
	longitude in -180.0 (west) to 180.0 (east)"

	| b g r c1 c2 c3 k k2 k3 k1 |
	"
(lon + 69) abs < 0.001 ifTrue: [ ^ Color gray: 0.3 ].
(lat + 41) abs < 0.001 ifTrue: [ ^ Color gray: 0.4 ].
"
	"ok para AgMacro and AgMicro"
	k _ nominalGSD / 10000 * 2.
	k _ 10.0 raisedToInteger: k log rounded.
	k1 _ nominalGSD / 10000 * 1.5.

	c1 _ (lat-(lat roundTo: k)) abs min: (lon-(lon roundTo: k)) abs.
	c2 _ (lat-(lat roundTo: k*10)) abs min: (lon-(lon roundTo: k*10)) abs.
	c3 _ (lat-(lat roundTo: k*50)) abs min: (lon-(lon roundTo: k*50)) abs.

"	{c1. c2. c2 } print."
"	(c2 / c3 between: 0.5 and: 2.1) ifTrue: [ ^Color black ].
	(c1 / c2 between: 0.5 and: 2.1) ifTrue: [ ^Color gray: 0.2].
	(c1 / c3 between: 0.5 and: 2.1) ifTrue: [ ^Color gray: 0.2]." 
"	(c3 /k < 5) ifTrue: [ ^Color black ].
	(c2 /k < 1) ifTrue: [ ^Color gray: 0.3].
	(c1 /k < 0.1) ifTrue: [ ^Color gray: 0.6]."

	k3 _ 0.05*1.3 * 1.5.		"larger => wider AA (softer)"
	k2 _ 0.05*0.3. 				"larger => wider lines"
	c1 _ c1 /k1 - k2 / k3.

	k3 _ 0.05*1.3 * 2.		"larger => wider AA (softer)"
	k2 _ 0.05*1.0. 			"larger => wider lines"
	c2 _ c2 /k1 - k2 / k3.

	k3 _ 0.05*1.3 * 2.		"larger => wider AA (softer)"
	k2 _ 0.05*2. 			"larger => wider lines"
	c3 _ c3 /k1 - k2 / k3.

	r _ c1 min: 1 max: 0.
	g _ c2 min: 1 max: 0.
	b _ c3 min: 1 max: 0.
	^ Color gray: (r*0.15) + (g*0.25) + (b*0.6)! !

!MeridiansAndParallelsSampledWorld methodsFor: 'accessing' stamp: 'jmv 7/12/2016 14:40:35'!
nominalGSD: meters
	nominalGSD _ meters! !

!SampledTile methodsFor: 'testing' stamp: 'jmv 3/23/2016 12:00'!
isNull
	^samples isNil! !

!SampledTile methodsFor: 'accessing' stamp: 'jmv 3/23/2016 13:18'!
sampleCount
	^ samples ifNil: [ 0 ] ifNotNil: [ :s | s scalarSize ]! !

!SampledTile methodsFor: 'accessing' stamp: 'jmv 3/23/2016 11:46'!
samples
	^samples! !

!SampledTile methodsFor: 'accessing' stamp: 'jmv 3/23/2016 11:46'!
samples: data
	samples _ data! !

!SampledTile class methodsFor: 'instance creation' stamp: 'jmv 3/23/2016 11:47'!
null
	nullInstance ifNil: [
		nullInstance _ self samples: nil ].
	^ nullInstance! !

!SampledTile class methodsFor: 'instance creation' stamp: 'jmv 3/23/2016 11:46'!
samples: data
	^self new samples: data! !

!ValidableTile methodsFor: 'testing' stamp: 'jmv 3/23/2016 12:41'!
needsValidation
	^needsValidation! !

!ValidableTile methodsFor: 'accessing' stamp: 'jmv 3/23/2016 12:38'!
samples: data
	needsValidation _ data notNil.
	super samples: data! !

!ValidableTile methodsFor: 'accessing' stamp: 'jmv 3/23/2016 12:42'!
validated
	needsValidation _ false! !

!TiledSampledWorld methodsFor: 'accessing' stamp: 'jmv 8/17/2018 10:01:07'!
atLatitude: lat longitude: lon sampleArea: scalarSampleSizeMeters
	"scalarSampleSizeMeters is a number that usually the gsd in meters of the destination.
	It might be used to apply some low pass filtering for certain datasets.
	For example, GoogleSampledWorld supports using source tiles with higher resolution than destination, and apply a low pass, for maximum quality.
	Default implementation ignores the extra parameter."
	^ self atLatitude: lat longitude: lon! !

!TiledSampledWorld methodsFor: 'accessing' stamp: 'lpc 4/26/2016 15:25'!
datasetPath
	^ datasetPath! !

!TiledSampledWorld methodsFor: 'accessing' stamp: 'jmv 9/11/2015 15:44'!
datasetPath: aString
	datasetPath _ aString! !

!TiledSampledWorld methodsFor: 'initialization' stamp: 'jmv 11/10/2015 13:05'!
initialize
	allTiles _ Array2D extent: self class tileCount.
	cachedTiles _ OrderedCollection new! !

!TiledSampledWorld methodsFor: 'private' stamp: 'jmv 11/9/2015 16:23'!
sampleTile: tile at: zeroBasedPosition
	"Subclasses must implement this method.
	
	zeroBasedPosition = 0@0 means topLeft.
	
	Subclasses using Forms should answer:
		^tile colorAt: zeroBasedPosition

	Subclasses using Matrices (FloatMatrix, ByteMatrix) or any Array2D should answer:
		^tile at: zeroBasedPosition+1
	because Matrices (and Array2D) are 1-based.
	
	
	Something else. If using NearestNeighbor, just round argument. But if using bilinear interpolation:
	Bilinear Interpolation requires adding (and repeating):
		- as a last row, the first row of the tile below
		- as a last column, the first column of the tile to the right
	
	As examples of this, ShuttleRadarTopographyMission3 DEM format files are 1201x1201, and they already include
	these repeated row and column.
	
	But EGM96Geoid5 files doesn't. The file is 4320 x 2161, so in #loadFromFile, and extra column is added, repeating first column. No need to do it for the rows!!
	"

	self subclassResponsibility! !

!TiledSampledWorld methodsFor: 'examples' stamp: 'jmv 8/9/2018 17:34:38'!
demoResult: extent
	^ FloatImage extent: extent! !

!TiledSampledWorld methodsFor: 'examples' stamp: 'jmv 8/17/2018 17:03:11'!
example: aString  lat0: lat0 long0: long0 pixelExtent: pixelExtent gsdArcSeconds: gsdArcSeconds
	"Result is in an equal spaced latLong grid
	gsdArcSeconds gives that spacing"

	| demo lat1 long1 filename pixelSizeOnGroundMeters |
	filename _ name, '-', aString.
	lat1 _ lat0 - (pixelExtent y-1  * gsdArcSeconds / 3600.0).
	long1 _ long0 + (pixelExtent x-1  * gsdArcSeconds / 3600.0).
	demo _ self demoResult: pixelExtent.
	"On a latLong grid, gsd in the vertical direction almost doesn't change. Compute it and use it for sample filtering.
	Truth is that on a latLong grid vertical resolution is not the same as horizontal resolution, especially at high latitudes.
	We'd need an area filter that (in meters) gets horizontally narrower as we get closer to the poles.
	Here we just use the correct vertical direction filter height for both direcions. The effect is an oversmoothing in
	the horizontal direction.
	Well, after all this method is just a demo.
	"
	pixelSizeOnGroundMeters _ (gsdArcSeconds / 3600.0) "degrees " / 360 "fraction of diameter" * 40e6. "meters"
	self fillExample: demo lat0: lat0 lat1: lat1 long0: long0 long1: long1 sampleArea: pixelSizeOnGroundMeters.
	demo asFormAutoRange writePNGfileNamed: filename, '.png'.
	(filename, '.wld') asFileEntry forceWriteStreamDo: [ :strm |
		gsdArcSeconds / 3600.0 printOn: strm. strm newLine.
		0 printOn: strm. strm newLine.
		0 printOn: strm. strm newLine.
		gsdArcSeconds / 3600.0 negated printOn: strm. strm newLine.
		long0 printOn: strm. strm newLine.
		lat0 printOn: strm. strm newLine ].
	(filename, '-toTiff.sh') asFileEntry forceWriteStreamDo: [ :stream |
		stream
			nextPutAll: 'gdalwarp -s_srs "EPSG:4326" ';
			nextPutAll: filename, '.png ';
			nextPutAll: filename, '.tiff';
			newLine ].
	^ demo! !

!TiledSampledWorld methodsFor: 'examples' stamp: 'jmv 8/17/2018 16:49:50'!
fillExample: demo lat0: lat0 lat1: lat1 long0: long0 long1: long1 sampleArea: pixelSizeOnGroundMeters
	| lat long v |
	0 to: demo height-1 do: [ :y |
		lat _ y / (demo height-1.0) * (lat1-lat0) + lat0.
		0 to: demo width-1 do: [ :x |
			long _ x / (demo width-1.0) * (long1-long0) + long0.
			v _ self atLatitude: lat longitude: long sampleArea: pixelSizeOnGroundMeters.
			demo i: y+1 j: x+1  put: v ]]! !

!TiledSampledWorld class methodsFor: 'singleton' stamp: 'jmv 3/2/2016 14:18'!
clearInstance
	"
	TiledSampledWorld withAllSubclassesDo: [ :each | each clearInstance ]
	"
	instance _ nil! !

!TiledSampledWorld class methodsFor: 'singleton' stamp: 'jmv 8/8/2018 14:55:20'!
getInstance
	instance ifNil: [
		instance _ self new.
		instance datasetPath asDirectoryEntry exists ifFalse: [
			instance datasetPath asFileEntry exists ifFalse: [
				self error: 'Whoops, there''s no dataset you can use (or it''s stored in the wrong place)'.
				instance _ nil ]]].
	^ instance! !

!TiledSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/17/2018 11:13:19'!
example: aString  lat0: lat0 long0: long0 pixelExtent: pixelExtent gsdArcSeconds: gsdArcSeconds

	^ self getInstance
		example: aString
		lat0: lat0
		long0: long0
		pixelExtent: pixelExtent
		gsdArcSeconds: gsdArcSeconds! !

!TiledSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/17/2018 14:17:51'!
exampleAconcagua
	"
	ShuttleRadarTopographyMission30 exampleAconcagua displayAutoRangeAt: 0@0 zoom: 2
	ShuttleRadarTopographyMission3 exampleAconcagua displayAutoRangeAt: 0@0 zoom: 2
	TiledECEFVariableSampledDEM exampleAconcagua displayAutoRangeAt: 0@0 zoom: 2
	TrueMarble exampleAconcagua displayAutoRangeAt: 0@0 zoom: 2
	GoogleSampledWorld exampleAconcagua displayAutoRangeAt: 0@0 zoom: 2
	"
	^ self
		example: 'Aconcagua'
		lat0: -32.575
		long0: -70.095
		pixelExtent:  400@400
		gsdArcSeconds: 1.5.			"Same for latitude and longitude"! !

!TiledSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/17/2018 14:45:20'!
exampleArgentina
	"
	EGM96Geoid5 exampleArgentina displayAutoRangeAt: 0@0 zoom: 1
	ShuttleRadarTopographyMission30 exampleArgentina displayAutoRangeAt: 0@0 zoom: 1
	NaturalEarth exampleArgentina displayAutoRangeAt: 0@0 zoom: 1
	GoogleSampledWorld exampleArgentina displayAutoRangeAt: 0@0 zoom: 1
	"
	^ self
		example: 'Argentina'
		lat0: -19
		long0: -76
		pixelExtent:  440@700
		gsdArcSeconds: 200.			"Same for latitude and longitude"! !

!TiledSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/17/2018 11:31:02'!
exampleBuenosAires
	"
	ShuttleRadarTopographyMission30 exampleBuenosAires displayAutoRangeAt: 0@0 zoom: 1
	ShuttleRadarTopographyMission3 exampleBuenosAires displayAutoRangeAt: 0@0 zoom: 1
	TiledECEFVariableSampledDEM exampleBuenosAires displayAutoRangeAt: 0@0 zoom: 1
	TrueMarble exampleBuenosAires displayAutoRangeAt: 0@0 zoom: 1
	GoogleSampledWorld exampleBuenosAires displayAutoRangeAt: 0@0 zoom: 1.
	"
	^ self
		example: 'BuenosAires'
		lat0: -34.0
		long0: -58.9
		pixelExtent:  800@800
		gsdArcSeconds: 4.			"Same for latitude and longitude"! !

!TiledSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/17/2018 11:31:19'!
exampleMaunaLoa
	"
	ShuttleRadarTopographyMission30 exampleMaunaLoa displayAutoRangeAt: 0@0 zoom: 1.
	ShuttleRadarTopographyMission3 exampleMaunaLoa displayAutoRangeAt: 0@0 zoom: 1.
	TiledECEFVariableSampledDEM exampleMaunaLoa displayAutoRangeAt: 0@0 zoom: 1.
	TrueMarble exampleMaunaLoa displayAutoRangeAt: 0@0 zoom: 1.
	GoogleSampledWorld exampleMaunaLoa displayAutoRangeAt: 0@0 zoom: 1.
	"
	^ self
		example: 'MaunaLoa'
		lat0: 19.95
		long0: -155.95
		pixelExtent:  1600@1600
		gsdArcSeconds: 1.5.			"Same for latitude and longitude"! !

!TiledSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/17/2018 14:47:45'!
exampleNauru
	"
	ShuttleRadarTopographyMission30 exampleNauru displayAutoRangeAt: 0@0 zoom: 2
	ShuttleRadarTopographyMission3 exampleNauru displayAutoRangeAt: 0@0 zoom: 2
	TiledECEFVariableSampledDEM exampleNauru displayAutoRangeAt: 0@0 zoom: 2
	TiledECEFVariableSampledDEM exampleNauru displayAutoRangeAt: 0@0 zoom: 2
	TrueMarble exampleNauru displayAutoRangeAt: 0@0 zoom: 2
	GoogleSampledWorld exampleNauru displayAutoRangeAt: 0@0 zoom: 2
	"
	^ self
		example: 'Nauru'
		lat0: -0.47
		long0: 166.88
		pixelExtent:  400@400
		gsdArcSeconds: 1.			"Same for latitude and longitude"! !

!TiledSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/17/2018 11:31:49'!
exampleSanFrancisco
	"
	ShuttleRadarTopographyMission30 exampleSanFrancisco displayAutoRangeAt: 0@0 zoom: 2
	ShuttleRadarTopographyMission3 exampleSanFrancisco displayAutoRangeAt: 0@0 zoom: 2
	TiledECEFVariableSampledDEM exampleSanFrancisco displayAutoRangeAt: 0@0 zoom: 2
	TrueMarble exampleSanFrancisco displayAutoRangeAt: 0@0 zoom: 2
	GoogleSampledWorld exampleSanFrancisco displayAutoRangeAt: 0@0 zoom: 2
	"
	^ self
		example: 'SanFrancisco'
		lat0: 3.65/60+9/60+37
		long0: (50.51/60+13/60+121) negated
		pixelExtent:  600@400
		gsdArcSeconds: 1.			"Same for latitude and longitude"! !

!TiledSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/17/2018 11:32:00'!
exampleVicenteLopez
	"
	ShuttleRadarTopographyMission30 exampleVicenteLopez displayAutoRangeAt: 0@0 zoom: 1
	ShuttleRadarTopographyMission3 exampleVicenteLopez displayAutoRangeAt: 0@0 zoom: 1
	TiledECEFVariableSampledDEM exampleVicenteLopez displayAutoRangeAt: 0@0 zoom: 1
	GoogleSampledWorld exampleVicenteLopez displayAutoRangeAt: 0@0 zoom: 1.
	"
	^ self
		example: 'VicenteLopez'
		lat0: -34.524
		long0: -58.469
		pixelExtent:  800@800
		gsdArcSeconds: 0.03.			"Same for latitude and longitude"! !

!TiledSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/17/2018 11:32:19'!
exampleWorld
	"
	EGM96Geoid5 exampleWorld displayAutoRangeAt: 0@0 zoom: 1
	NaturalEarth exampleWorld displayAutoRangeAt: 0@0 zoom: 1
	GoogleSampledWorld exampleWorld displayAutoRangeAt: 0@0 zoom: 1
	"
	^ self
		example: 'World'
		lat0: 90
		long0: -180
		pixelExtent:  800@400
		gsdArcSeconds: 1620.			"Same for latitude and longitude"! !

!TiledECEFSampledWorld methodsFor: 'iterating' stamp: 'jmv 4/7/2016 11:23'!
allFaceTilesInto: aBlock
	"For all tiles into some face. Not specific to a particular face!!
	Evaluate aBlock. Block arguments are:
	- tilePosition, 0-based, i.e. 0@0 is the topLeft tile
	- pointBelongsInFace
	- pointBelongsInExtendedFace"

	| faceNumber intersectsExtendedFace intersectsFace last positionInFace tilePosition |
	faceNumber _ 1.		"It could be any face, it doesn't matter."
	last _ self tilesPerFaceSide - 1.
	0 to: last do: [ :tilei |
		0 to: last do: [ :tilej |
			tilePosition _ tilej@tilei.
			intersectsFace _ false.
			intersectsExtendedFace _ false.
			#(0.0 0.5 1.0) do: [ :y |
				#(0.0 0.5 1.0) do: [ :x |
					positionInFace _ self facePositionOfNormalized: x@y tilePosition: tilePosition.
					self ecefOfFace: faceNumber position: positionInFace into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
						intersectsFace _ intersectsFace | pointBelongsInFace.
						intersectsExtendedFace _ intersectsExtendedFace | pointBelongsInExtendedFace ]]].
			aBlock value: tilePosition value: intersectsFace value: intersectsExtendedFace ]]! !

!TiledECEFSampledWorld methodsFor: 'accessing' stamp: 'jmv 3/10/2016 12:37'!
atECEF: ecefOnTheSphere
	"posInTile is
		for TiledECEFUniformlySampledWorld: 			[0 .. width-1] x [0 .. height-1]
		for TiledECEFVariableSampledWorld: 			[0.0 .. 1.0] x [0.0 .. 1.0] (OpenCL style Normalized coordinates)
		"
	| faceIndex |
	faceIndex _ self bestFaceFor: ecefOnTheSphere.
	self withTileAndPositionInTileOf: ecefOnTheSphere atFace: faceIndex evaluate: [ :tilePos :posInTile |
		^ self sampleTile: (self tileAt: faceIndex at: tilePos) at: posInTile ]! !

!TiledECEFSampledWorld methodsFor: 'accessing' stamp: 'jmv 3/10/2016 12:40'!
atECEF: ecef fromFace: faceNumber
	"It's the sender's responsibility to determine from which face he wants to take the sample. If the face isn't chosen correctly, it will sample the projection of the ecef vector onto the chosen face.
	An alternative is to call #atECEF: that picks the appropriate face."
	self withTileAndPositionInTileOf: ecef atFace: faceNumber evaluate: [ :tilePos :posInTile |
		^ self sampleTile: (self tileAt: faceNumber at: tilePos) at: posInTile ]! !

!TiledECEFSampledWorld methodsFor: 'accessing' stamp: 'jmv 8/13/2018 15:00:38'!
tileAt: faceNumber at: zeroBasedTilePosition
	"faceNumber in #(1 2 3 4 5 6)
	aPoint is zero-based.
	allTiles coordinates are 1-based, i.e. they go from 1@1 to tileHorizontalCount@tileVerticalCount
	cachedTiles coordinates are 1-based
	"

	| tile oneBasedTilePosition tileCoordinates tileData |
	oneBasedTilePosition _ zeroBasedTilePosition+1.
	tile _ (allTiles at: faceNumber) at: oneBasedTilePosition.
	tile ifNil: [
		[ self needsToFreeSomeCache ] whileTrue: [
			"free oldest cache element.
			This is not the LRU element!!
			Freeing the LRU element requires moving the cached element to the first position if found. Worth it?"
			tileCoordinates _ cachedTiles removeLast.
			(allTiles at: tileCoordinates first) at: tileCoordinates second put: nil ].

		tileData _ self loadFace: faceNumber at: zeroBasedTilePosition. 		"Might answer nil"
		(tileData isNil and: [ self hasSourceDataset ]) ifTrue: [
			tileData _ self buildFace: faceNumber at: zeroBasedTilePosition ]. 	"Might answer nil"

		tile _ tileData
			ifNil: [ SampledTile null ]
			ifNotNil: [
				"Null tiles don't take space in the cache"
				cachedTiles addFirst: {faceNumber. oneBasedTilePosition}.
				SampledTile samples: tileData ].

		(allTiles at: faceNumber) at: oneBasedTilePosition put: tile ].
	^tile! !

!TiledECEFSampledWorld methodsFor: 'accessing' stamp: 'jmv 4/7/2016 11:10'!
tileSideLength
	"in metres"
	^ self class faceSideLengthInMeters / self tilesPerFaceSide! !

!TiledECEFSampledWorld methodsFor: 'conversion' stamp: 'jmv 3/10/2016 12:28'!
bestFaceFor: ecefOnTheSphere
	"An ECEF point might be present in more than one face.
	But most lie in just one face.
	In any case the face where it belongs is the face where it is the closest to the center of the face"

	ecefOnTheSphere z >= (ecefOnTheSphere x abs max: ecefOnTheSphere y abs)
		ifTrue: [ ^ 1 ].
	ecefOnTheSphere y negated >= (ecefOnTheSphere x abs max: ecefOnTheSphere z abs)
		ifTrue: [ ^ 2 ].
	ecefOnTheSphere x >= (ecefOnTheSphere y abs max: ecefOnTheSphere z abs)
		ifTrue: [ ^ 3 ].
	ecefOnTheSphere y >= (ecefOnTheSphere x abs max: ecefOnTheSphere z abs)
		ifTrue: [ ^ 4 ].
	ecefOnTheSphere x negated >= (ecefOnTheSphere y abs max: ecefOnTheSphere z abs)
		ifTrue: [ ^ 5 ].
	ecefOnTheSphere z negated >= (ecefOnTheSphere x abs max: ecefOnTheSphere y abs)
		ifTrue: [ ^ 6 ].
	self halt! !

!TiledECEFSampledWorld methodsFor: 'conversion' stamp: 'jmv 3/15/2016 10:27'!
ecefOfFace: faceNumber position: positionInFace into: aBlock
	"
	Answer an ECEF on the sphere.
	positionInFace is in metres. Center of the face is 0.0@0.0
	
	Evaluate aBlock. Block arguments are:
	- ecef
	- pointBelongsInFace
	- pointBelongsInExtendedFace
	
	The block is evaluated if ecef exists, even if not in face or extended face.

	| ww ecef |
	ww := TiledECEFSampledWorld tilesPerFaceSide: 1 samplesPerTileSide: 300 .
	{ 3000@-2000. 200@150. -2000@ -2500. -100@1000} do: [ :position |
		1 to: 6 do: [ :face |
			ecef := ww ecefOfFace: face position: position.
			ww withFaceAndPositionOf: ecef evaluate: [ :f :p |
				{ face. position. ' ---> '. ecef. ' ---> '. f. p } print ]]]
	"
	| missingCoordinate missingCoordinateSquared normalizedPointInTheFace radius ecef pointBelongsInFace pointBelongsInExtendedFace |
	radius _  GPSPosition wgs84SemiMajorAxis.
	normalizedPointInTheFace _ positionInFace / radius.
	missingCoordinateSquared _ 1.0 - (normalizedPointInTheFace dotProduct: normalizedPointInTheFace).
	missingCoordinateSquared < -1.0e-12
		ifTrue: [ ^ nil ].		"Requested point doesn't belong in the sphere surface"
	"Use a small constant to account por possible rounding errors in the border of the active face area.
	It is safe to use a specific value because coordinates are normalized"

	"Only ban points that should better go in another face if requested."
	missingCoordinate _ (missingCoordinateSquared max: 0.0) sqrt.

	pointBelongsInFace _ missingCoordinate + 1.0e-12 >= (normalizedPointInTheFace x abs max: normalizedPointInTheFace y abs).

	"If #faceLatitudeLongitudeExtent was 45, then this condition would be equivalent to the one above.
	I (jmv) know, this is tricky. See comment at #faceLatitudeLongitudeExtent "
	pointBelongsInExtendedFace _ ((normalizedPointInTheFace x abs max: normalizedPointInTheFace y abs) arcTan: missingCoordinate) radiansToDegrees < self class faceLatitudeLongitudeExtent.

	missingCoordinate _ missingCoordinate * radius.

	ecef _ faceNumber caseOf: {
		[ 1 ] -> [ positionInFace x @ positionInFace y negated @ missingCoordinate ].
		[ 2 ] -> [ positionInFace x @ missingCoordinate negated @ positionInFace y negated].
		[ 3 ] -> [ missingCoordinate @ positionInFace x @ positionInFace y negated].
		[ 4 ] -> [ positionInFace x negated @ missingCoordinate @ positionInFace y negated].
		[ 5 ] -> [ missingCoordinate negated @ positionInFace x negated @ positionInFace y negated].
		[ 6 ] -> [ positionInFace x @ positionInFace y @ missingCoordinate negated]
	}.
	
	aBlock value: ecef value: pointBelongsInFace value: pointBelongsInExtendedFace! !

!TiledECEFSampledWorld methodsFor: 'conversion' stamp: 'jmv 3/23/2016 16:40'!
ecefOfFace: faceNumber tilePosition: positionOfTile positionInTile: positionInTile samples: samples into: aBlock
	"
	Answer an ECEF on the sphere.
	positionOfTile is 0-based, i.e. 0@0 is the topLeft tile
	positionInTile is 0-based, i.e. 0@0 is the topLeft sample of the tile

	| ww ecef |
	ww := TiledECEFSampledWorld tilesPerFaceSide: 1 samplesPerTileSide: 300 .
	{ 3000@-2000. 200@150. -2000@ -2500. -100@1000} do: [ :position |
		1 to: 6 do: [ :face |
			ecef := ww ecefOfFace: face position: position.
			ww withFaceAndPositionOf: ecef evaluate: [ :f :p |
				{ face. position. ' ---> '. ecef. ' ---> '. f. p } print ]]]
	"

	^self
		ecefOfFace: faceNumber
		position: (self facePositionOf: positionInTile tilePosition: positionOfTile samples: samples)
		into: aBlock! !

!TiledECEFSampledWorld methodsFor: 'conversion' stamp: 'jmv 4/7/2016 11:11'!
facePositionOfNormalized: normalizedPositionInTile tilePosition: positionOfTile
	"normalizedPositionInTile is in [0.0 .. 1.0] @ [0.0 .. 1.0]
	positionOfTile is 0-based, i.e. 0@0 is the topLeft tile
	
	answer is center-based, i.e. 0@0 is the geometrical center of the face, and in metres."
	
	| tileSideLength |
	tileSideLength _ self tileSideLength.
	^(
		(((positionOfTile x + normalizedPositionInTile x) * tileSideLength)
			- (self class faceSideLengthInMeters /2.0))
			@
		(((positionOfTile y + normalizedPositionInTile y) * tileSideLength)
			- (self class faceSideLengthInMeters /2.0))
	)
	! !

!TiledECEFSampledWorld methodsFor: 'conversion' stamp: 'jmv 3/14/2016 17:10'!
positionInFaceOf: ecefOnTheSphere atFace: faceNumber

	faceNumber = 1
		ifTrue: [ ^ ecefOnTheSphere x@ecefOnTheSphere y negated ].
	faceNumber = 2
		ifTrue: [ ^ ecefOnTheSphere x@ecefOnTheSphere z negated ].
	faceNumber = 3
		ifTrue: [ ^ ecefOnTheSphere y@ecefOnTheSphere z negated ].
	faceNumber = 4
		ifTrue: [ ^ ecefOnTheSphere x negated @ecefOnTheSphere z negated ].
	faceNumber = 5
		ifTrue: [ ^ ecefOnTheSphere y negated @ ecefOnTheSphere z negated ].
	faceNumber = 6
		ifTrue: [ ^ ecefOnTheSphere x@ecefOnTheSphere y ].
	self halt! !

!TiledECEFSampledWorld methodsFor: 'building' stamp: 'jmv 4/7/2016 12:38'!
buildAllTiles
	"Does not store the tiles in the cache, just build them and save them to disk
	Builds also tiles that shouldn't be built. The corners of the face usually include only
	ecef that should be better sampled at another face.
	
	See (and prefer) 
		#buildMissingTiles
			Generates all missing files
		TiledECEFVariableSampledDEM >> buildAll:
			Generates 1/24 of all tiles. Does not generate tiles at the border of the faces. 
			Good for splitting the initial generation of very large datasets"
	| face |
	self halt: 'You usually dont want to run this method!!'.
	1 to: 6 do: [ :faceNumber |
		face _ allTiles at: faceNumber.
		1 to: face height do: [ :i |
			1 to: face width do: [ :j |
				self buildFace: faceNumber at: j@i-1 ]]]! !

!TiledECEFSampledWorld methodsFor: 'building' stamp: 'jmv 3/23/2016 16:39'!
buildFace: faceNumber
	"Does not store the tiles in the cache, just build them and save them to disk."
	| face |
	face _ allTiles at: faceNumber.
	1 to: face height do: [ :i |
		1 to: face width do: [ :j |
			self buildFace: faceNumber at: j@i-1 ]]! !

!TiledECEFSampledWorld methodsFor: 'building' stamp: 'jmv 3/23/2016 16:44'!
buildFace: faceNumber at: tilePosition
	"Does not store the tiles in the cache, just build them and save them to disk.
	tilePosition is zero-based"
	| tileSamples tileHasValidData h w |
	tileSamples _ self newTileSamples.
	h _ tileSamples height.
	w _ tileSamples width.
	tileHasValidData _ false.
	0 to: h-1 do: [ :y |
"		(tileHasValidData and: [y \\ 50 = 49]) ifTrue: [ self display: tile at: 0@0 ]."
		0 to: w-1 do: [ :x |
			self ecefOfFace: faceNumber tilePosition: tilePosition positionInTile: x@y samples: tileSamples
				into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
					self fillTileSamples: tileSamples at: x@y withDataForECEF: ecef pointBelongsInFace: pointBelongsInFace pointBelongsInExtendedFace: pointBelongsInExtendedFace.
					pointBelongsInExtendedFace ifTrue: [
							tileHasValidData _ true ]
					
				]]].
		tileHasValidData ifTrue: [
			tileSamples _ self subsample: tileSamples.
"			self display: tile at: 0@0."
			self writeToDisk: tileSamples face: faceNumber tilePosition: tilePosition ]
		ifFalse: [ 
			tileSamples _ nil ].
	^ tileSamples! !

!TiledECEFSampledWorld methodsFor: 'building' stamp: 'lpc 4/11/2016 14:29'!
buildMissingTiles
	"
	Appropriate for very large datasets, where peeking several positions for every tile would be extremely expensive.

	ne _ NaturalEarth getInstance.
	sphericCubeNE _ TiledECEFNaturalEarth tilesPerFaceSide: 30 samplesPerTileSide: 8.
	sphericCubeNE sourceDataset: ne.
	sphericCubeNE buildMissingTiles.
	"
	self missingTilesDo: [ :face :tilePosition | self buildFace: face at: tilePosition ]! !

!TiledECEFSampledWorld methodsFor: 'building' stamp: 'jmv 3/23/2016 16:26'!
subsample: samples
	"Hook for allowing subsampled tiles.
	This is done in specific subclasses. NOP here."
	^ samples! !

!TiledECEFSampledWorld methodsFor: 'displaying' stamp: 'jmv 3/23/2016 12:29'!
display: tile at: aPoint

	tile samples ifNotNil: [ :samples | samples displayAt: aPoint ]! !

!TiledECEFSampledWorld methodsFor: 'private' stamp: 'jmv 4/18/2016 16:31'!
filenameForTileAtFace: faceNumber tilePosition: positionOfTile

	| tilesPerFaceStringSize |
	tilesPerFaceStringSize _ self tilesPerFaceSide printString size.
	^ String streamContents: [ :strm |
		strm
			nextPutAll: self filenamePrefix;
			nextPut: $_.
		faceNumber printOn: strm base: 10.
		strm nextPut: $-.
		positionOfTile x printOn: strm base: 10 length: tilesPerFaceStringSize padded: true.
		strm nextPut: $-.
		positionOfTile y printOn: strm base: 10 length: tilesPerFaceStringSize padded: true.
		strm nextPut: $.; nextPutAll: self class tileFilenameExtension ]! !

!TiledECEFSampledWorld methodsFor: 'private' stamp: 'jmv 4/18/2016 09:47'!
loadFace: faceNumber at: tilePosition
	"tilePosition is zero-based"
	| filename path |
	path _ self pathForTileAtFace: faceNumber tilePosition: tilePosition.
	filename _ self filenameForTileAtFace: faceNumber tilePosition: tilePosition.
	^ self loadFromFile: filename in: path! !

!TiledECEFSampledWorld methodsFor: 'private' stamp: 'jmv 4/18/2016 09:44'!
loadFromFile: aFilename in: path
	self subclassResponsibility! !

!TiledECEFSampledWorld methodsFor: 'private' stamp: 'jmv 4/18/2016 09:41'!
pathForTileAtFace: faceNumber tilePosition: positionOfTile
	"Subclasses might use a folder tree to avoid too many entries (>400k) in on folder,
	as this seems to make filesystems like ext4 extremely slow"

	^ datasetPath! !

!TiledECEFSampledWorld methodsFor: 'testing' stamp: 'jmv 3/2/2016 18:30'!
hasSourceDataset
	^ false! !

!TiledECEFSampledWorld methodsFor: 'initialization' stamp: 'jmv 2/23/2016 12:30'!
initializeGSD: desiredGSD samplesPerTileSide: defaultSamplesPerTileSide
	"
	Tiles and Faces are square. width also specifies height.
	Typical values are
		desiredGSD = 90.0.
		samplesPerTileSide = 512.
	Then, tiles cover an area of 32.6km x 32.6km
	
	desiredGSD is a hint. Actual GSD for each tile might be slightly higher (to allow for desiredGSD in the borders of the face)
	"

	| maxVoxelSide approxTileSide tilesPerFaceSideNeeded |
	maxVoxelSide _ desiredGSD * 45 degreeCos.			"Voxel side must be no larger than this"
	approxTileSide _ maxVoxelSide * defaultSamplesPerTileSide.
	tilesPerFaceSideNeeded _ (self class faceSideLengthInMeters / approxTileSide) rounded.
	self
		initializeTilesPerFaceSide: tilesPerFaceSideNeeded
		samplesPerTileSide: defaultSamplesPerTileSide! !

!TiledECEFSampledWorld methodsFor: 'services' stamp: 'jmv 4/18/2016 10:06'!
missingTilesDo: aBlock
	| thisPath existingFilenamesForThisPath existingFilenamesPerPath path existing |

	thisPath _ nil.
	existingFilenamesForThisPath _ nil.
	existingFilenamesPerPath _ Dictionary new.

	self allFaceTilesInto: [ :tilePosition :intersectsFace :intersectsExtendedFace |
		intersectsExtendedFace ifTrue: [
			1 to: 6 do: [ :face |
				path _ self pathForTileAtFace: face tilePosition: tilePosition.
				path = thisPath ifFalse: [
					thisPath _ path.
					existingFilenamesForThisPath _ existingFilenamesPerPath at: thisPath ifAbsentPut: [
						existing _ Set new.
						thisPath asDirectoryEntry filesDo: [ :file | existing add: file name ].
						existing ]].
				(existingFilenamesForThisPath includes: (self filenameForTileAtFace: face tilePosition: tilePosition)) ifFalse: [ 
					aBlock value: face value: tilePosition ]]]].! !

!TiledECEFSampledWorld class methodsFor: 'constants' stamp: 'jmv 3/15/2016 10:29'!
faceLatitudeLongitudeExtent
	"In degrees.
	- If we answered 90, each face would include the whole orthographic projection
		(Earth seen from infinite distance, each face is a circle)

	- If we answered 45, there would be no missing areas and no overlap.
		We can say that, for example, for the face that is centered at the Equator and Greenwich meridian
		(face 3, the one including Africa), the lateral borders are meridians 45° East and 45° West.
		Face2, including America is laterally bounded by meridians 45° West and 135° West.

	- We answer a bit more than 45, to allow for a bit of overlap between faces:
		Face 3 is laterally bounded by meridians 48° East and 48° West
		Face 2 is laterally bounded by meridians 42° West and 138° West.
		This allows for the rectification of a frame, using just one face, even in the case the frame spans 2 faces
		
	Specifying the bound of faces 1 and 6 (containing the poles) seems harder, but that is just for the convention used
	parallels. As they are parallel circles (meridians are not!!) they don't help us here. Just rotate your sphere and think
	meridians!!
	"
	^48! !

!TiledECEFSampledWorld class methodsFor: 'constants' stamp: 'jmv 3/14/2016 16:48'!
faceSideLengthInMeters
	"See comment at #faceLatitudeLongitudeExtent "

	^self faceLatitudeLongitudeExtent degreeSin * 2 * GPSPosition wgs84SemiMajorAxis! !

!TiledECEFSampledWorld class methodsFor: 'instance creation' stamp: 'jmv 3/10/2016 12:43'!
gsd: desiredGSD samplesPerTileSide: samplesPerTileSide
	"
	Used for building real world (i.e. large) datasets.

	TiledECEFVariableSampledDEM gsd: 90 samplesPerTileSide: 512
	"
	^ self basicNew initializeGSD: desiredGSD samplesPerTileSide: samplesPerTileSide! !

!TiledECEFSampledWorld class methodsFor: 'instance creation' stamp: 'jmv 2/23/2016 12:20'!
tilesPerFaceSide: tilesPerFaceSide samplesPerTileSide: samplesPerTileSide
	"
	Used for experimenting with small instances

	TiledECEFDEM tilesPerFaceSide: 1 samplesPerTileSide: 300
	"
	^ self basicNew initializeTilesPerFaceSide: tilesPerFaceSide samplesPerTileSide: samplesPerTileSide! !

!TiledECEFUniformlySampledWorld methodsFor: 'displaying' stamp: 'jmv 2/23/2016 11:05'!
displayAll
	"Display all tiles. Load from disk as needed.
	Do not display missing or null tiles"
	| face facePosition |
	1 to: 6 do: [ :faceIndex |
		facePosition _ { 0@0. 0@1. 1@1. 2@1. 3@1. 0@2 } at: faceIndex.
		face _ allTiles at: faceIndex.
		1 to: face height do: [ :j |
			1 to: face width do: [ :i |
				self
					display: (self tileAt: faceIndex at: (j-1)@(i-1))
					at: facePosition * self faceSideLengthInSamples + (j@i -1 * self tileSideLengthInSamples) ]]]! !

!TiledECEFUniformlySampledWorld methodsFor: 'displaying' stamp: 'jmv 2/23/2016 11:05'!
displayAll3
	"Display all tiles. Load from disk as needed.
	Do not display missing or null tiles"
	| face facePosition |
	1 to: 6 do: [ :faceIndex |
		facePosition _ { 0@0. 0@1. 1@1. 2@1. 3@1. 0@2 } at: faceIndex.
		face _ allTiles at: faceIndex.
		1 to: face height by: 3do: [ :j |
			1 to: face width by: 3 do: [ :i |
				self
					display: (self tileAt: faceIndex at: (j-1)@(i-1))
					at: facePosition * self faceSideLengthInSamples + (j@i -1 * self tileSideLengthInSamples) ]]]! !

!TiledECEFUniformlySampledWorld methodsFor: 'displaying' stamp: 'jmv 2/23/2016 11:05'!
displayLoaded
	"Display just currently loaded (cached) tiles"
	| face facePosition |
	1 to: 6 do: [ :faceIndex |
		facePosition _ { 0@0. 0@1. 1@1. 2@1. 3@1. 0@2 } at: faceIndex.
		face _ allTiles at: faceIndex.
		1 to: face height do: [ :j |
			1 to: face width do: [ :i |
				((allTiles at: faceIndex) at: j@i) ifNotNil: [ :tile |
					self
						display: tile
						at: facePosition * self faceSideLengthInSamples + (j@i -1 * self tileSideLengthInSamples) ]]]]! !

!TiledECEFUniformlySampledWorld methodsFor: 'conversion' stamp: 'jmv 3/23/2016 16:22'!
facePositionOf: positionInTile tilePosition: positionOfTile samples: samples
	"positionOfTile is 0-based, i.e. 0@0 is the topLeft tile
	positionInTile is 0-based, i.e. 0@0 is the topLeft sample of the tile
	
	answer is center-based, i.e. 0@0 is the geometrical center of the face, and in metres."

	^(
		((positionOfTile x * self tileSideLengthInSamples) + positionInTile x - (self faceSideLengthInSamples/2.0))
			@
		((positionOfTile y * self tileSideLengthInSamples) + positionInTile y - (self faceSideLengthInSamples/2.0))
	)
	* voxelSide! !

!TiledECEFUniformlySampledWorld methodsFor: 'conversion' stamp: 'jmv 3/14/2016 17:11'!
withTileAndPositionInTileOf: ecefOnTheSphere atFace: faceNumber evaluate: aBlock
	"ecef must be on the sphere. This means ecef length = GPSPosition wgs84SemiMajorAxis"
	| distanceFromTopLeftInSamples positionInTile tilePosition positionInFace |
	positionInFace _ self positionInFaceOf: ecefOnTheSphere atFace: faceNumber.
	distanceFromTopLeftInSamples _ positionInFace / (voxelSide@voxelSide) + ((self faceSideLengthInSamples/2.0)@(self faceSideLengthInSamples/2.0)).
	tilePosition _ distanceFromTopLeftInSamples // (self tileSideLengthInSamples @ self tileSideLengthInSamples).
	positionInTile _ distanceFromTopLeftInSamples \\ (self tileSideLengthInSamples @ self tileSideLengthInSamples).
	aBlock value: tilePosition value: positionInTile! !

!TiledECEFUniformlySampledWorld methodsFor: 'constants' stamp: 'jmv 2/23/2016 11:05'!
faceSideLengthInSamples
	"Not the count of samples, but the total length"
	^ self tilesPerFaceSide * self tileSideLengthInSamples! !

!TiledECEFUniformlySampledWorld methodsFor: 'constants' stamp: 'jmv 2/23/2016 11:05'!
tileSideLengthInSamples
	"Not the count of samples, but the total length"
	^ samplesPerTileSide - 1! !

!TiledECEFUniformlySampledWorld methodsFor: 'constants' stamp: 'jmv 2/23/2016 11:05'!
tilesPerFaceSide
	"Answer must be integer. Call to #rounded is needed only because we deal with Floats and there might be a slight rounding error."
	^(self class faceSideLengthInMeters / voxelSide / self tileSideLengthInSamples) rounded! !

!TiledECEFUniformlySampledWorld methodsFor: 'private' stamp: 'jmv 4/18/2016 12:16'!
filenamePrefix

	^ String streamContents: [ :strm |
		strm nextPutAll: self class tileFilenamePrefix.
		self tilesPerFaceSide printOn: strm base: 10.
		strm nextPut: $-.
		self tileSideLengthInSamples printOn: strm base: 10 ]! !

!TiledECEFUniformlySampledWorld methodsFor: 'private' stamp: 'jmv 2/23/2016 11:34'!
needsToFreeSomeCache
	^ cachedTiles size >= self cacheSize! !

!TiledECEFUniformlySampledWorld methodsFor: 'initialization' stamp: 'jmv 4/18/2016 16:25'!
initializeTilesPerFaceSide: tilesPerFaceSide samplesPerTileSide: aSamplesPerTileSide
	"
	Tiles and Faces are square. width also specifies height."

	"Each face of the cube has length (in the largest part):"
	samplesPerTileSide _ aSamplesPerTileSide.
	voxelSide _ self class faceSideLengthInMeters / tilesPerFaceSide / self tileSideLengthInSamples. "meters"

	allTiles _ #(1 2 3 4 5 6) collect: [ :i | Array2D extent: tilesPerFaceSide@tilesPerFaceSide ].
	cachedTiles _ OrderedCollection new.

	datasetPath _ 'payload/datasets/', self filenamePrefix! !

!TiledECEFNaturalEarth methodsFor: 'constants' stamp: 'jmv 2/23/2016 11:05'!
cacheSize
	^ 5! !

!TiledECEFNaturalEarth methodsFor: 'filling' stamp: 'lpc 3/23/2016 15:38'!
fillTileSamples: samples at: aZeroBasedPoint withDataForECEF: ecefOnTheSphere pointBelongsInFace: pointBelongsInFace pointBelongsInExtendedFace: pointBelongsInExtendedFace
	| llh value |
	"This conversion from ecef to llh is rather arbitrary.
	See class comment"
	llh _ GPSPosition fromECEFSpherical: ecefOnTheSphere.
	value _ sourceDataset atLatitude: llh phi longitude: llh lambda.
	pointBelongsInFace ifFalse: [ value _ value * 0.8 ].
	pointBelongsInExtendedFace ifFalse: [ value _ value * 0.8 ].
	samples colorAt: aZeroBasedPoint put: value! !

!TiledECEFNaturalEarth methodsFor: 'filling' stamp: 'jmv 3/23/2016 12:17'!
newTileSamples
	| tileSamples |
	tileSamples _ Form extent: samplesPerTileSide@samplesPerTileSide depth: 16.
	tileSamples fillColor: Color red.
	^ tileSamples! !

!TiledECEFNaturalEarth methodsFor: 'filling' stamp: 'jmv 4/18/2016 09:47'!
writeToDisk: samples face: faceNumber tilePosition: positionOfTile
	| filename path |
	path _ self pathForTileAtFace: faceNumber tilePosition: positionOfTile.
	filename _ self filenameForTileAtFace: faceNumber tilePosition: positionOfTile.
	samples writePNGfileNamed: (path, '/', filename)! !

!TiledECEFNaturalEarth methodsFor: 'testing' stamp: 'jmv 3/2/2016 18:30'!
hasSourceDataset
	^ sourceDataset notNil! !

!TiledECEFNaturalEarth methodsFor: 'private' stamp: 'jmv 4/18/2016 09:45'!
loadFromFile: aFilename in: path
	| pathName |
	pathName _ path, '/', aFilename.
	pathName asFileEntry exists ifFalse: [ ^ nil "Allow for null tiles" ].
	^ Form fromFileNamed: pathName! !

!TiledECEFNaturalEarth methodsFor: 'private' stamp: 'jmv 3/23/2016 12:27'!
sampleTile: tile at: zeroBasedPosition
	"zeroBasedPosition = 0@0 means topLeft.
	Forms are zero based, meaning topLeft element is at 0@0.
	Argument is integer point"

	^tile samples colorAt: zeroBasedPosition rounded! !

!TiledECEFNaturalEarth methodsFor: 'accessing' stamp: 'jmv 2/23/2016 11:05'!
sourceDataset: aNaturalEarth
	sourceDataset _ aNaturalEarth! !

!TiledECEFNaturalEarth methodsFor: 'initialization' stamp: 'jmv 8/10/2018 14:59:45'!
initialize
	super initialize.
	name _ 'ECEF-NaturalEarth'.! !

!TiledECEFNaturalEarth class methodsFor: 'examples' stamp: 'lpc 3/28/2016 14:49'!
example1
	"
	TiledECEFNaturalEarth example1
	"
	| ne sphericCubeNE |
	ne _ NaturalEarth getInstance.
	sphericCubeNE _ TiledECEFNaturalEarth tilesPerFaceSide: 3 samplesPerTileSide: 115.
	sphericCubeNE sourceDataset: ne.
	sphericCubeNE buildAllTiles.
	sphericCubeNE sourceDataset: nil.
	sphericCubeNE displayAll! !

!TiledECEFNaturalEarth class methodsFor: 'examples' stamp: 'jmv 3/15/2016 09:00'!
example2
	"
	TiledECEFNaturalEarth example2
	"
	| ne sphericCubeNE |
	ne _ NaturalEarth getInstance.
	sphericCubeNE _ TiledECEFNaturalEarth tilesPerFaceSide: 30 samplesPerTileSide: 8.
	sphericCubeNE sourceDataset: ne.
	Display fillColor: Color red. sphericCubeNE displayAll. Display writePNGfileNamed: 'Display.png'! !

!TiledECEFNaturalEarth class methodsFor: 'constants' stamp: 'jmv 3/1/2016 10:44'!
tileFilenameExtension
	^ 'png'! !

!TiledECEFNaturalEarth class methodsFor: 'constants' stamp: 'jmv 3/1/2016 10:45'!
tileFilenamePrefix
	^ 'ECEF-NatEarth-'! !

!TiledECEFVariableSampledWorld methodsFor: 'displaying' stamp: 'jmv 3/1/2016 10:41'!
displayAll
	"Display all tiles. Load from disk as needed.
	Do not display missing or null tiles"
	
	| displayWH facePosition |
	displayWH _ 114*3+1.
	1 to: 6 do: [ :faceIndex |
		facePosition _ { 0@0. 0@1. 1@1. 2@1. 3@1. 0@2 } at: faceIndex.
		self displayFace: faceIndex at: facePosition pixelHeightAndWidth: displayWH ]! !

!TiledECEFVariableSampledWorld methodsFor: 'displaying' stamp: 'jmv 8/9/2018 13:44:33'!
displayFace: faceIndex at: facePosition pixelHeightAndWidth: displayWH
"repasar. probar"

	"Display all tiles in the requested face. Load from disk as needed.
	Do not display missing or null tiles
	facePosition is an integer point, usuallt in { 0@0. 0@1. 1@1. 2@1. 3@1. 0@2 }"
	
	| face value |
	face _ FloatImage extent: displayWH@displayWH.
	1 to: displayWH do: [ :y |
		1 to: displayWH do: [ :x |
			self ecefOfFace: faceIndex
				position: x@y - 0.5 / displayWH - 0.5 * self class faceSideLengthInMeters
				into: [ :ecef :pointBelongsInFace :pointBelongsInExtendedFace |
					pointBelongsInExtendedFace
						ifTrue: [
							value _ self atECEF: ecef fromFace: faceIndex.
							pointBelongsInFace ifFalse: [ value _ value * 0.8 ].
							face x: x y: y put: value ]
						ifFalse: [
							ecef ifNotNil: [
								value _ self atECEF: ecef fromFace: faceIndex.
								value _ value * 0.6.
								face x: x y: y put: value
								]
							ifNil: [
								"Some value that is easy to spot as different from actual data,
								but not far from possible values, so we don't ruin the visible dynamic range!!"
								face x: x y: y put: self class outOfFaceValue ]]
			]]].
"	face displayAt: facePosition * displayWH"
	face displayAutoRangeAt: facePosition * displayWH! !

!TiledECEFVariableSampledWorld methodsFor: 'conversion' stamp: 'jmv 4/7/2016 11:13'!
facePositionOf: positionInTile tilePosition: positionOfTile samples: samples
	"positionOfTile is 0-based, i.e. 0@0 is the topLeft tile
	positionInTile is 0-based, i.e. 0@0 is the topLeft sample of the tile
	
	answer is center-based, i.e. 0@0 is the geometrical center of the face, and in metres."
	
	| normalizedPositionInTile |
	normalizedPositionInTile _ samples openCLNormalizedCoordinatesForOneBased: positionInTile+1.
	^self facePositionOfNormalized: normalizedPositionInTile tilePosition: positionOfTile! !

!TiledECEFVariableSampledWorld methodsFor: 'conversion' stamp: 'jmv 3/14/2016 17:11'!
withTileAndPositionInTileOf: ecefOnTheSphere atFace: faceNumber evaluate: aBlock
	"ecef must be on the sphere. This means ecef length = GPSPosition wgs84SemiMajorAxis"
	| distanceFromTopLeftInTiles tilePosition openCLNormalizedPositionInTile positionInFace |
	positionInFace _ self positionInFaceOf: ecefOnTheSphere atFace: faceNumber.
	distanceFromTopLeftInTiles _ (self class faceSideLengthInMeters / 2.0 + positionInFace) / self tileSideLength.
	tilePosition _ distanceFromTopLeftInTiles floor.
	openCLNormalizedPositionInTile _ distanceFromTopLeftInTiles - tilePosition.
	aBlock value: tilePosition value: openCLNormalizedPositionInTile! !

!TiledECEFVariableSampledWorld methodsFor: 'private' stamp: 'jmv 4/18/2016 12:17'!
filenamePrefix

	^ String streamContents: [ :strm |
		strm nextPutAll: self class tileFilenamePrefix.
		self tilesPerFaceSide printOn: strm base: 10.
		strm nextPut: $-.
		maxSamplesPerTileSide printOn: strm base: 10 ]! !

!TiledECEFVariableSampledWorld methodsFor: 'private' stamp: 'jmv 5/16/2016 10:48'!
needsToFreeSomeCache

	cachedTiles size > 1 ifFalse: [
		^ false ].

	"100 tiles of biggest extent (512*512*4bytesPerFloat). 100 MiB. Reasonable? AgMicro should be happy with less than 20 tiles. For AgMacro, maybe 100 are needed."
	^ self totalCachedSamples > (100*maxSamplesPerTileSide *maxSamplesPerTileSide)! !

!TiledECEFVariableSampledWorld methodsFor: 'private' stamp: 'jmv 3/23/2016 12:17'!
newTileSamples

	^FloatImage extent: maxSamplesPerTileSide@maxSamplesPerTileSide! !

!TiledECEFVariableSampledWorld methodsFor: 'private' stamp: 'lpc 3/28/2016 11:47'!
sampleTile: tile at: normalizedPosition
	"normalizedPosition in [0.0 .. 1.0] x [0.0 .. 1.0], with OpenCL semantics"

	| tileSamples |
	tileSamples _ tile samples.
	^tileSamples
		ifNil: [ 0.0 ]
		ifNotNil: [ :samples | 
			| oneBased |
			oneBased _ samples oneBasedCoordinatesForOpenCLNormalized: normalizedPosition.
			samples bilinearInterpolatedValueAtX: oneBased x y: oneBased y ]! !

!TiledECEFVariableSampledWorld methodsFor: 'private' stamp: 'jmv 5/16/2016 10:33'!
totalCachedSamples
	"
	TiledECEFVariableSampledDEM getInstance totalCachedSamples * 4 / 1024.0 / 1024
	"
	^ (cachedTiles collect: [ :tileCoordinates | 
		((allTiles at: tileCoordinates first) at: tileCoordinates second) sampleCount ]) 
	sum! !

!TiledECEFVariableSampledWorld methodsFor: 'initialization' stamp: 'jmv 4/18/2016 16:25'!
initializeTilesPerFaceSide: aTilesPerFaceSide samplesPerTileSide: defaultSamplesPerTileSide
	"
	Tiles and Faces are square. width also specifies height.
	
	tilesPerFaceSide is to be taken as is.
	defaultSamplesPerTileSide is the maximum to be used. For each tile, a lower resolution might be chosen.
	"

	tilesPerFaceSide _ aTilesPerFaceSide.
	maxSamplesPerTileSide _ defaultSamplesPerTileSide.

	allTiles _ #(1 2 3 4 5 6) collect: [ :i | Array2D extent: tilesPerFaceSide@tilesPerFaceSide ].
	cachedTiles _ OrderedCollection new.

	datasetPath _ 'payload/datasets/', self filenamePrefix! !

!TiledECEFVariableSampledWorld methodsFor: 'constants' stamp: 'jmv 3/1/2016 11:03'!
maxSubsamplingRatio
	"Maximum ratio to be used when subsampling tiles.
	By default, use tiles of at least 2x2 samples."

	^ maxSamplesPerTileSide // 2! !

!TiledECEFVariableSampledWorld methodsFor: 'constants' stamp: 'jmv 3/1/2016 10:58'!
subsamplingTolerance
	"Maximum allowable error when subsampling tiles"

	^ self subclassResponsibility! !

!TiledECEFVariableSampledWorld methodsFor: 'constants' stamp: 'jmv 2/23/2016 12:34'!
tilesPerFaceSide

	^tilesPerFaceSide! !

!TiledECEFVariableSampledWorld methodsFor: 'building' stamp: 'jmv 3/23/2016 16:28'!
subsample: samples

	^  self subsample: samples maxRatio: self maxSubsamplingRatio tolerance: self subsamplingTolerance! !

!TiledECEFVariableSampledWorld methodsFor: 'building' stamp: 'jmv 3/23/2016 16:29'!
subsample: fullSizeSamples maxRatio: aNumber tolerance: tolerance
	| compressedTile reconstructedTile differenceTile valuesAreWithinToleratedRange compressedTileCoords fullSizeCoords openCLNormalized value |
	
	"Recursion end: no  subsampling possible."
	aNumber <= 1.0 ifTrue: [ ^ fullSizeSamples ].
		
	compressedTile _ FloatImage extent: (fullSizeSamples extent / aNumber) rounded.
	
	1 to: compressedTile width do: [ :x |
		1 to: compressedTile height do: [ :y |
			openCLNormalized _ compressedTile openCLNormalizedCoordinatesForOneBased: x@y.
			fullSizeCoords _ fullSizeSamples oneBasedCoordinatesForOpenCLNormalized: openCLNormalized.
			value _ fullSizeSamples bilinearInterpolatedValueAtX: fullSizeCoords x y: fullSizeCoords y.
			compressedTile x: x y: y put: value.
	]].
	
	reconstructedTile _ FloatImage extent: fullSizeSamples extent.
	1 to: reconstructedTile width do: [ :x |
		1 to: reconstructedTile height do: [ :y |
			openCLNormalized _ reconstructedTile openCLNormalizedCoordinatesForOneBased: x@y.
			compressedTileCoords _ compressedTile oneBasedCoordinatesForOpenCLNormalized: openCLNormalized.
			reconstructedTile x: x y: y put: (compressedTile bilinearInterpolatedValueAtX: compressedTileCoords x y: compressedTileCoords y)
	]].

	differenceTile _ fullSizeSamples - reconstructedTile.
	
	valuesAreWithinToleratedRange _ true.

	valuesAreWithinToleratedRange ifTrue: [
		valuesAreWithinToleratedRange _ differenceTile max abs < tolerance and: [ differenceTile min abs < tolerance ]].

	^ valuesAreWithinToleratedRange 
		ifFalse: [
			"Subsampling was too agressive. Try a bit less (i.e. a bigger tile)."
			self subsample: fullSizeSamples maxRatio: aNumber / 2 tolerance: tolerance ] 
		ifTrue: [
			"Ok"
			compressedTile ]! !

!TiledECEFVariableSampledWorld class methodsFor: 'constants' stamp: 'jmv 3/1/2016 12:37'!
outOfFaceValue
	"Value to be used when displaying, for the area outside the 'proper' part of the face.
	Meaning 'this point should be sampled at another face'"

	self subclassResponsibility! !

!TiledECEFVariableSampledDEM methodsFor: 'building' stamp: 'jmv 3/16/2016 11:54'!
addSourceDatasets
	"Only to be used when the full dataset is not yet built, but LL datasets are available"
	| dem geoid |
	dem _ ShuttleRadarTopographyMission3 getInstance.
	geoid _ EGM96Geoid5 getInstance.
	self sourceDEM: dem geoid: geoid! !

!TiledECEFVariableSampledDEM methodsFor: 'building' stamp: 'lpc 3/23/2016 15:38'!
fillTileSamples: samples at: aZeroBasedPoint withDataForECEF: ecefOnTheSphere pointBelongsInFace: pointBelongsInFace pointBelongsInExtendedFace: pointBelongsInExtendedFace
	| fraction distanceFromSphereToSurface |
	
	fraction _ sourceDEM fractionForIntersectionOfRadiusWith: ecefOnTheSphere geoid: sourceGeoid.

	"Usually negative: we move downwards from the Sphere to hit Earth surface"
	distanceFromSphereToSurface _ ((fraction - 1) * ecefOnTheSphere length) rounded.
	samples at: aZeroBasedPoint+1 put: distanceFromSphereToSurface! !

!TiledECEFVariableSampledDEM methodsFor: 'building' stamp: 'jmv 4/18/2016 09:53'!
writeToDisk: samples face: faceNumber tilePosition: positionOfTile

	| path |
	path _ self pathForTileAtFace: faceNumber tilePosition: positionOfTile.
	self writeToDisk: samples face: faceNumber tilePosition: positionOfTile in: path.
	
	"Enable this to duplicate new tiles. Useful when filling missing tiles or fixing some detail,
	for example when calling something like
		TiledECEFVariableSampledDEM getInstance addSourceDatasets buildMissingTiles
	"
	"
	self writeToDisk: samples face: faceNumber tilePosition: positionOfTile in: 'new'
	"! !

!TiledECEFVariableSampledDEM methodsFor: 'building' stamp: 'jmv 5/31/2016 16:09'!
writeToDisk: samples face: faceNumber tilePosition: positionOfTile in: path
	| w h value |
	w _ samples width.
	h _ samples height.
	path asDirectoryEntry // (self filenameForTileAtFace: faceNumber tilePosition: positionOfTile)
		forceWriteStreamDo: [ :stream |
			stream binary.
			1 to: h do: [ :i |
				1 to: w do: [ :j |
					value _ samples i: i j: j.
					stream nextSignedInt16Put: value asInteger bigEndian: false]]]! !

!TiledECEFVariableSampledDEM methodsFor: 'querying' stamp: 'jmv 2/17/2017 10:56:58'!
ecefOnTerrainFor: ecef

	| ecefOnTheSphere metersDownwards towardsTerrain |
	ecefOnTheSphere _ ecef normalized * GPSPosition wgs84SemiMajorAxis.
	metersDownwards _ self atECEF: ecefOnTheSphere.
	towardsTerrain _ ecefOnTheSphere * (metersDownwards / GPSPosition wgs84SemiMajorAxis).
	^ ecefOnTheSphere + towardsTerrain! !

!TiledECEFVariableSampledDEM methodsFor: 'querying' stamp: 'jmv 7/20/2017 13:17:51'!
gpsPositionOnTerrainFor: gpsPos

	| approximateAnswer |
	"This is not the approximateAnswer answer, it has different latitude (because it keeps geometric latitude, not geodetic latitude). Over 1 meter error"
	approximateAnswer _ GPSPosition fromECEF: (self ecefOnTerrainFor: gpsPos asECEF).

	"True (almost) geodetic altitude for same lat and lon. Sub centimeter error."
	^gpsPos withHeight: approximateAnswer height! !

!TiledECEFVariableSampledDEM methodsFor: 'testing' stamp: 'jmv 3/2/2016 18:31'!
hasSourceDataset
	^ sourceGeoid notNil and: [ sourceDEM notNil ]! !

!TiledECEFVariableSampledDEM methodsFor: 'private' stamp: 'jmv 5/31/2016 16:09'!
loadFromFile: aFilename in: path
	| pathName fileEntry fileSize wh tile |
	pathName _ path, '/', aFilename.
	fileEntry _ pathName asFileEntry.
	fileEntry exists ifFalse: [ ^ nil "Allow for null tiles" ].
	fileSize _ fileEntry fileSize.
	wh _ (fileSize / 2) sqrt.
	wh isInteger ifFalse: [
		self error: 'File has invalid size' ].
	tile _ FloatImage extent: wh@wh.
	fileEntry readStreamDo: [ :stream |
		stream binary.
		1 to: wh do: [ :i |
			1 to: wh do: [ :j |
				tile i: i j: j put: (stream nextSignedInt16BigEndian: false)]]].
	^ tile! !

!TiledECEFVariableSampledDEM methodsFor: 'private' stamp: 'jmv 4/18/2016 09:59'!
pathForTileAtFace: faceNumber tilePosition: positionOfTile
	"too many entries (>400k) in on folder, makes filesystems like ext4 extremely slow.
	So, if we are over 100x100 tiles per face, split in two levels."

	| tilesPerFaceString base |
	base _ super pathForTileAtFace: faceNumber tilePosition: positionOfTile.

	tilesPerFaceSide > 99 ifTrue: [
		tilesPerFaceString _ tilesPerFaceSide printString.
		^ base, '/',
			faceNumber printString, '-',
			(positionOfTile x printStringBase: 10 length: tilesPerFaceString size padded: true) ].
	^ base! !

!TiledECEFVariableSampledDEM methodsFor: 'constants' stamp: 'jmv 3/10/2016 13:41'!
maxSubsamplingRatio
	"rather arbitrary example: don't subsample too much..."

	^ super maxSubsamplingRatio min: 8! !

!TiledECEFVariableSampledDEM methodsFor: 'constants' stamp: 'jmv 3/18/2016 16:57'!
subsamplingTolerance
	"Maximum allowable error when subsampling tiles"

	^ 10 		"metres. Consistent with original dataset vertical resolution.
				Gives 16x16 subsampling (wrt original 90 metres)
				for all-water tiles (at least near Buenos Aires and Montevideo)
				
				Still allows viewing trees!!
				See example4"! !

!TiledECEFVariableSampledDEM methodsFor: 'accessing' stamp: 'jmv 8/10/2018 15:17:31'!
name: aString
	name _ aString! !

!TiledECEFVariableSampledDEM methodsFor: 'accessing' stamp: 'lpc 2/29/2016 14:00'!
sourceDEM: aDEM geoid: aGeoid
	sourceDEM _ aDEM.
	sourceGeoid _ aGeoid.! !

!TiledECEFVariableSampledDEM methodsFor: 'examples' stamp: 'jmv 8/17/2018 14:28:02'!
fillExample: demo lat0: lat0 lat1: lat1 long0: long0 long1: long1 sampleArea: pixelSizeOnGroundMeters
	| lat long v |
	0 to: demo height-1 do: [ :y |
		lat _ y / (demo height-1.0) * (lat1-lat0) + lat0.
		0 to: demo width-1 do: [ :x |
			long _ x / (demo width-1.0) * (long1-long0) + long0.
			v _ (self gpsPositionOnTerrainFor: (GPSPosition latitude: lat longitude: long altitude: 0)) altitude.
			demo i: y+1 j: x+1  put: v ]]! !

!TiledECEFVariableSampledDEM methodsFor: 'initialization' stamp: 'jmv 8/10/2018 15:00:47'!
initialize
	super initialize.
	name _ 'ECEFvar-DEM'.! !

!TiledECEFVariableSampledDEM class methodsFor: 'standalone application' stamp: 'jmv 4/5/2016 11:26'!
build: columnNumberString
	"columnNumberString asNumber between 0 and 23"

	self buildAll: columnNumberString asNumber * 15 - 180.
	Smalltalk snapshot: false andQuit: true embedded: false clearAllClassState: false! !

!TiledECEFVariableSampledDEM class methodsFor: 'standalone application' stamp: 'jmv 4/5/2016 11:44'!
buildECEFDEMBuilderApp
	"
	TiledECEFVariableSampledDEM buildECEFDEMBuilderApp
	"
	AppLauncher appGlobalName: self name appSelector: #build:.
	Smalltalk saveAs: 'ECEFDEMBuilder' andQuit: true clearAllClassState: false! !

!TiledECEFVariableSampledDEM class methodsFor: 'building' stamp: 'jmv 4/5/2016 09:59'!
buildAll
	"
	Use #buildAll:
	Tweak!!"
	| dem ecef geoid sphericCubeDEM step |
	ShuttleRadarTopographyMission3 clearInstance.
	dem _ ShuttleRadarTopographyMission3 getInstance.
	dem cacheSize: 100. "5.6MByte in memory each (2.8MByte on disk each)"

	EGM96Geoid5 clearInstance.
	geoid _ EGM96Geoid5 getInstance.

	TiledECEFVariableSampledDEM clearInstance.
	sphericCubeDEM _ TiledECEFVariableSampledDEM getInstance.
	sphericCubeDEM sourceDEM: dem geoid: geoid.

	step _ 90.0 / sphericCubeDEM tilesPerFaceSide * 0.95.
	"Iterate integer degrees: whole source tiles"
	-18 to: -18 do: [ :sourceTileLatitude |
		-50 to: -48 do: [ :sourceTileLongitude |
			('lat: ', sourceTileLatitude printString, ' - lon: ', sourceTileLongitude printString) print.
			0.0 to: 1.0 by: step do: [ :latFraction |
				0.0 to: 1.0 by: step do: [ :lonFraction |
					ecef _ (GPSPosition latitude: sourceTileLatitude+latFraction longitude: sourceTileLongitude+lonFraction altitude: 0) asECEF.
					sphericCubeDEM atECEF: ecef ]]]]! !

!TiledECEFVariableSampledDEM class methodsFor: 'building' stamp: 'jmv 4/5/2016 12:31'!
buildAll: longMajor
	"Tweak!!
	
	-180 to: 165 by: 15 do: [ :longMajor |
		TiledECEFVariableSampledDEM buildAll: longMajor ]
	"
	| dem ecef geoid sphericCubeDEM step |
	ShuttleRadarTopographyMission3 clearInstance.
	dem _ ShuttleRadarTopographyMission3 getInstance.
	dem cacheSize: 100. "5.6MByte in memory each (2.8MByte on disk each)"
	dem logFilename: 'VewFinderPanoramas3Validation', longMajor printString, '.txt'.

	EGM96Geoid5 clearInstance.
	geoid _ EGM96Geoid5 getInstance.

	TiledECEFVariableSampledDEM clearInstance.
	sphericCubeDEM _ TiledECEFVariableSampledDEM getInstance.
	sphericCubeDEM sourceDEM: dem geoid: geoid.

	step _ 90.0 / sphericCubeDEM tilesPerFaceSide * 0.95.

	(0.0 to: -90 by: step negated), (0.0 to: 90 by: step) do: [ :lat |
		0.0 to: 15 by: step do: [ :longMinor |
			(longMajor + longMinor @ lat) print.
			ecef _ (GPSPosition latitude: lat longitude: longMajor+longMinor altitude: 0) asECEF.
			sphericCubeDEM atECEF: ecef
			 ]]! !

!TiledECEFVariableSampledDEM class methodsFor: 'building' stamp: 'jmv 4/19/2016 15:43'!
buildSmall
	"
	TiledECEFVariableSampledDEM gsd: 90*64 samplesPerTileSide: 512
	(TiledECEFVariableSampledDEM gsd: 90*64 samplesPerTileSide: 512) displayAll
	"
	| dem |
	dem _ TiledECEFVariableSampledDEM gsd: 90*64 samplesPerTileSide: 512.
	dem addSourceDatasets.
	0 to: 4 do: [ :i |
		0 to: 4 do: [ :j |
			dem buildFace: 2"1..6" at: i@j ]]! !

!TiledECEFVariableSampledDEM class methodsFor: 'examples' stamp: 'jmv 3/3/2016 11:59'!
example1
	"
	TiledECEFVariableSampledDEM example1
	"
	| dem geoid sphericCubeDEM |
	dem _ ShuttleRadarTopographyMission30 getInstance.	"Low res"
	geoid _ EGM96Geoid5 getInstance.
	sphericCubeDEM _ TiledECEFVariableSampledDEM tilesPerFaceSide: 4 samplesPerTileSide: 128. "Low res"
	sphericCubeDEM sourceDEM: dem geoid: geoid.
	sphericCubeDEM buildFace: 2.
	sphericCubeDEM displayFace: 2 at: 0@0 pixelHeightAndWidth: 100! !

!TiledECEFVariableSampledDEM class methodsFor: 'examples' stamp: 'jmv 3/3/2016 11:32'!
example2
	"
	TiledECEFVariableSampledDEM example2
	"
	| dem geoid sphericCubeDEM demo ecefOnTheSphere h lat lat0 lat1 long long0 long1 |
	dem _ ShuttleRadarTopographyMission3 getInstance.
	geoid _ EGM96Geoid5 getInstance.
	sphericCubeDEM _ TiledECEFVariableSampledDEM getInstance.
	sphericCubeDEM sourceDEM: dem geoid: geoid.

	"city, delta, river, countryside"
	lat0 _ -35.7. lat1 _ -33.3.		"South"
	long0 _ -59.7. long1 _ -57.3. "West"
	"city"
	lat0 _ -34.7. lat1 _ -34.3.		"South"
	long0 _ -58.7. long1 _ -58.3. "West"
	"water"
	lat0 _ -35.7. lat1 _ -33.3.		"South"
	long0 _ -57.7. long1 _ -55.3. "West"

	demo _ FloatImage extent: 800@800.
	0 to: demo height-1 do: [ :y |
		lat _ y / (demo height-1) * (lat0-lat1) + lat1.
		0 to: demo width-1 do: [ :x |
			long _ x / (demo width-1) * (long1-long0) + long0.
			ecefOnTheSphere _ (GPSPosition latitude: lat longitude: long altitude: 0) 
											asECEF normalized * GPSPosition wgs84SemiMajorAxis.
			h _ sphericCubeDEM atECEF: ecefOnTheSphere.
			demo x: x+1 y: y+1 put: h
			]].
	(demo asFormMin: -6880 max: -6700) display.
	(demo asFormMin: -6880 max: -6700) writePNGfileNamed: 'VarDEM.png'! !

!TiledECEFVariableSampledDEM class methodsFor: 'examples' stamp: 'jmv 3/3/2016 11:02'!
example3
	"
	Perfect registration between us and original DEM
	TiledECEFVariableSampledDEM example3
	"

	| color d dem ecefOnTheSphere fromCubeDEM fromLLDEM geoid h lat lat0 lat1 long long0 long1 sphericCubeDEM |
	dem _ ShuttleRadarTopographyMission3 getInstance.
	geoid _ EGM96Geoid5 getInstance.
	sphericCubeDEM _ TiledECEFVariableSampledDEM getInstance.
	sphericCubeDEM sourceDEM: dem geoid: geoid.

	lat0 _ -35.7. lat1 _ -33.3.		"South"
	long0 _ -59.7. long1 _ -57.3. "West"
	lat0 _ -34.7. lat1 _ -34.3.		"South"
	long0 _ -58.7. long1 _ -58.3. "West"
	fromCubeDEM _ FloatImage extent: 800@800.
	fromLLDEM _ FloatImage extent: 800@800.
	0 to: fromCubeDEM height-1 do: [ :y |
		lat _ y / (fromCubeDEM height-1) * (lat0-lat1) + lat1.
		0 to: fromCubeDEM width-1 do: [ :x |
			long _ x / (fromCubeDEM width-1) * (long1-long0) + long0.

			h _ (geoid atLatitude: lat longitude: long) + (dem atLatitude: lat longitude: long).
			fromLLDEM x: x+1 y: y+1 put: (h min: 50 max: -10)+11/62.
			
			ecefOnTheSphere _ (GPSPosition latitude: lat longitude: long altitude: h) 
											asECEF normalized * GPSPosition wgs84SemiMajorAxis.
			d _ sphericCubeDEM atECEF: ecefOnTheSphere.
			fromCubeDEM x: x+1 y: y+1 put: d.
			]].
	(fromCubeDEM asFormMin: -6880 max: -6700) display.
	fromLLDEM display.
	color _ fromCubeDEM + 6880 / 180 asFormG: fromLLDEM b: fromLLDEM.
	color display! !

!TiledECEFVariableSampledDEM class methodsFor: 'examples' stamp: 'jmv 3/18/2016 16:55'!
example4
	"
	Perfect registration between us and original DEM
	TiledECEFVariableSampledDEM example4
	"

	| color d dem ecefOnTheSphere fromCubeDEM fromLLDEM geoid h lat lat0 lat1 long long0 long1 sphericCubeDEM |
	dem _ ShuttleRadarTopographyMission3 getInstance.
	geoid _ EGM96Geoid5 getInstance.
	sphericCubeDEM _ TiledECEFVariableSampledDEM getInstance.
	sphericCubeDEM sourceDEM: dem geoid: geoid.

	lat0 _ -35.7. lat1 _ -33.3.		"South"
	long0 _ -59.7. long1 _ -57.3. "West"
	lat0 _ -35.7. lat1 _ -35.3.		"South"
	long0 _ -58.7. long1 _ -58.3. "West"
	fromCubeDEM _ FloatImage extent: 800@800.
	fromLLDEM _ FloatImage extent: 800@800.
	0 to: fromCubeDEM height-1 do: [ :y |
		lat _ y / (fromCubeDEM height-1) * (lat0-lat1) + lat1.
		0 to: fromCubeDEM width-1 do: [ :x |
			long _ x / (fromCubeDEM width-1) * (long1-long0) + long0.

			h _ (geoid atLatitude: lat longitude: long) + (dem atLatitude: lat longitude: long).
			fromLLDEM x: x+1 y: y+1 put: h.
			
			ecefOnTheSphere _ (GPSPosition latitude: lat longitude: long altitude: h) 
											asECEF normalized * GPSPosition wgs84SemiMajorAxis.
			d _ sphericCubeDEM atECEF: ecefOnTheSphere.
			fromCubeDEM x: x+1 y: y+1 put: d.
			]].
	(fromCubeDEM asFormMin: -7220 max: -7020) display.
	fromLLDEM display.
	color _ fromCubeDEM + 7220 / 200 asFormG: fromLLDEM -28/34 b: fromLLDEM-28/34.
	color display! !

!TiledECEFVariableSampledDEM class methodsFor: 'singleton' stamp: 'jmv 8/9/2018 17:40:27'!
getInstance
	"
	TiledECEFVariableSampledDEM getInstance missingTilesDo: [ :face :tilePosition | {'DEM: '. face . tilePosition } print ].
	TiledECEFVariableSampledDEM clearInstance
	TiledECEFVariableSampledDEM getInstance
	"
	instance ifNil: [
		"512 might be a reasonabla balance between file size and file count"
		instance _ self hiRes ifNil: [ self loRes ].
		instance ifNil: [
			self error: 'Whoops, there''s no dataset you can use (or it''s stored in the wrong place)' ].
		].
	^ instance! !

!TiledECEFVariableSampledDEM class methodsFor: 'singleton' stamp: 'jmv 8/10/2018 15:17:52'!
hiRes
	"
	TiledECEFVariableSampledDEM hiRes
	"
	"512 might be a reasonabla balance between file size and file count"
	| answer |
	answer _ self gsd: 90 samplesPerTileSide: 512.
	answer name: 'ECEFvar-DEM'.
	^ answer datasetPath asDirectoryEntry exists
		ifTrue: [ answer ]! !

!TiledECEFVariableSampledDEM class methodsFor: 'singleton' stamp: 'jmv 8/10/2018 15:18:03'!
loRes
	"
	TiledECEFVariableSampledDEM loRes
	"
	| answer |
	answer _ self gsd: 90*64 samplesPerTileSide: 512.
	answer name: 'ECEFvar-DEM-LoRes'.
	^ answer datasetPath asDirectoryEntry exists
		ifTrue: [ answer ]! !

!TiledECEFVariableSampledDEM class methodsFor: 'constants' stamp: 'jmv 3/1/2016 12:38'!
outOfFaceValue
	"Value to be used when displaying, for the area outside the 'proper' part of the face.
	Meaning 'this point should be sampled at another face'"

	^0.0! !

!TiledECEFVariableSampledDEM class methodsFor: 'constants' stamp: 'jmv 3/1/2016 10:44'!
tileFilenameExtension
	^ 'int16'! !

!TiledECEFVariableSampledDEM class methodsFor: 'constants' stamp: 'jmv 3/1/2016 10:43'!
tileFilenamePrefix
	^ 'ECEF-DEM-Var-'! !

!TiledECEFVariableSampledNaturalEarth methodsFor: 'building' stamp: 'lpc 3/23/2016 15:34'!
fillTileSamples: samples at: aZeroBasedPoint withDataForECEF: ecefOnTheSphere pointBelongsInFace: pointBelongsInFace pointBelongsInExtendedFace: pointBelongsInExtendedFace

	| llh |
	"This conversion from ecef to llh is rather arbitrary.
	See class comment"
	llh _ GPSPosition fromECEFSpherical: ecefOnTheSphere.
	samples at: aZeroBasedPoint+1 put: (sourceDataset atLatitude: llh phi longitude: llh lambda) luminance! !

!TiledECEFVariableSampledNaturalEarth methodsFor: 'building' stamp: 'jmv 5/31/2016 16:09'!
writeToDisk: samples face: faceNumber tilePosition: positionOfTile
	| w h value filename path |
	path _ self pathForTileAtFace: faceNumber tilePosition: positionOfTile.
	filename _ self filenameForTileAtFace: faceNumber tilePosition: positionOfTile.
	w _ samples width.
	h _ samples height.
	path asDirectoryEntry // filename
		forceWriteStreamDo: [ :stream |
			stream binary.
			1 to: h do: [ :i |
				1 to: w do: [ :j |
					value _ (((samples i: i j: j) min: 1.0 max: 0.0 ) * 255) rounded.
					stream nextPut: value ]]]! !

!TiledECEFVariableSampledNaturalEarth methodsFor: 'testing' stamp: 'jmv 3/2/2016 18:30'!
hasSourceDataset
	^ sourceDataset notNil! !

!TiledECEFVariableSampledNaturalEarth methodsFor: 'private' stamp: 'jmv 5/31/2016 16:09'!
loadFromFile: aFilename in: path
	| pathName fileEntry fileSize wh tile |
	pathName _ path, '/', aFilename.
	fileEntry _ pathName asFileEntry.
	fileEntry exists ifFalse: [ ^ nil "Allow for null tiles" ].
	fileSize _ fileEntry fileSize.
	wh _ fileSize sqrt.
	wh isInteger ifFalse: [
		self error: 'File has invalid size' ].
	tile _ FloatImage extent: wh@wh.
	fileEntry readStreamDo: [ :stream |
		stream binary.
		1 to: wh do: [ :i |
			1 to: wh do: [ :j |
				tile i: i j: j put: stream next / 255.0 ]]].
	^ tile! !

!TiledECEFVariableSampledNaturalEarth methodsFor: 'constants' stamp: 'jmv 3/1/2016 11:10'!
maxSubsamplingRatio
	"rather arbitrary example: don't subsample too much..."

	^ super maxSubsamplingRatio min: 8! !

!TiledECEFVariableSampledNaturalEarth methodsFor: 'constants' stamp: 'jmv 3/15/2016 09:40'!
subsamplingTolerance
	"Maximum allowable error when subsampling tiles"

	^ 0.01 			"This gives a rather posterized result. Just for experimenting."! !

!TiledECEFVariableSampledNaturalEarth methodsFor: 'accessing' stamp: 'jmv 2/23/2016 12:39'!
sourceDataset: aNaturalEarth
	sourceDataset _ aNaturalEarth! !

!TiledECEFVariableSampledNaturalEarth methodsFor: 'initialization' stamp: 'jmv 8/10/2018 15:00:42'!
initialize
	super initialize.
	name _ 'ECEFvar-NaturalEarth'.! !

!TiledECEFVariableSampledNaturalEarth class methodsFor: 'examples' stamp: 'lpc 3/28/2016 14:50'!
example1
	"
	TiledECEFVariableSampledNaturalEarth example1
	"
	| ne sphericCubeNE |
	ne _ NaturalEarth getInstance.
	sphericCubeNE _ TiledECEFVariableSampledNaturalEarth tilesPerFaceSide: 3 samplesPerTileSide: 115.
	sphericCubeNE sourceDataset: ne.
	sphericCubeNE buildAllTiles.
	sphericCubeNE sourceDataset: nil.
	sphericCubeNE displayAll! !

!TiledECEFVariableSampledNaturalEarth class methodsFor: 'examples' stamp: 'jmv 8/9/2018 13:44:31'!
example2

	"
	TiledECEFVariableSampledNaturalEarth example2
	"

	| demo ecefOnTheSphere h lat lat0 lat1 long long0 long1 ne sphericCubeNE |
	ne _ NaturalEarth getInstance.
	sphericCubeNE _ TiledECEFVariableSampledNaturalEarth tilesPerFaceSide: 64 samplesPerTileSide: 512.
	sphericCubeNE sourceDataset: ne.
	sphericCubeNE buildFace: 2 at: 50@56.
	sphericCubeNE buildFace: 2 at: 51@56.
	sphericCubeNE buildFace: 2 at: 50@57.
	sphericCubeNE buildFace: 2 at: 51@57.
	sphericCubeNE sourceDataset: nil.
	sphericCubeNE displayFace: 2 at: 0@0 pixelHeightAndWidth: 400.

	lat0 _ -38. lat1 _ -33.		"South"
	long0 _ -59.5. long1 _ -54.5. "West"
	demo _ Form extent: 800@800 depth: 32.
	demo _ FloatImage extent: 800@800.
	0 to: demo height-1 do: [ :y |
		lat _ y / (demo height-1) * (lat0-lat1) + lat1.
		0 to: demo width-1 do: [ :x |
			long _ x / (demo width-1) * (long1-long0) + long0.
			ecefOnTheSphere _ (GPSPosition latitude: lat longitude: long altitude: 0) 
											asECEF normalized * GPSPosition wgs84SemiMajorAxis.
			h _ sphericCubeNE atECEF: ecefOnTheSphere.
			demo x: x+1 y: y+1 put: h.
			 ]].
		demo displayAutoRangeAt:0@0! !

!TiledECEFVariableSampledNaturalEarth class methodsFor: 'examples' stamp: 'lpc 3/28/2016 14:51'!
example3
	"
	TiledECEFVariableSampledNaturalEarth example3
	"
	| ne sphericCubeNE |
	ne _ NaturalEarth getInstance.
	sphericCubeNE _ TiledECEFVariableSampledNaturalEarth tilesPerFaceSide: 3 samplesPerTileSide: 115.
	sphericCubeNE sourceDataset: ne.
"	sphericCubeNE buildAllTiles.
	sphericCubeNE sourceDataset: nil."
	sphericCubeNE displayAll! !

!TiledECEFVariableSampledNaturalEarth class methodsFor: 'examples' stamp: 'lpc 3/28/2016 14:51'!
example4
	"
	TiledECEFVariableSampledNaturalEarth example4
	"
	| ne sphericCubeNE |
	ne _ NaturalEarth getInstance.
	sphericCubeNE _ TiledECEFVariableSampledNaturalEarth tilesPerFaceSide: 23 samplesPerTileSide: 11.
	sphericCubeNE sourceDataset: ne.
	sphericCubeNE displayAll! !

!TiledECEFVariableSampledNaturalEarth class methodsFor: 'constants' stamp: 'jmv 3/1/2016 12:42'!
outOfFaceValue
	"Value to be used when displaying, for the area outside the 'proper' part of the face.
	Meaning 'this point should be sampled at another face'"

	^0.2! !

!TiledECEFVariableSampledNaturalEarth class methodsFor: 'constants' stamp: 'jmv 3/1/2016 10:44'!
tileFilenameExtension
	^ 'uint8'! !

!TiledECEFVariableSampledNaturalEarth class methodsFor: 'constants' stamp: 'jmv 3/1/2016 10:44'!
tileFilenamePrefix
	^ 'ECEF-NatEarth-Var-'! !

!TiledLatitudeLongitudeSampledWorld methodsFor: 'accessing' stamp: 'jmv 1/13/2016 17:42'!
atGPSPosition: aGPSPosition
	^self atLatitude: aGPSPosition latitude longitude: aGPSPosition longitude! !

!TiledLatitudeLongitudeSampledWorld methodsFor: 'accessing' stamp: 'jmv 1/22/2016 16:16'!
atLatitude: latitude longitude: longitude
	"latitude in -90.0 (south pole) to 90.0 (north pole)
	longitude in -180.0 (west) to 180.0 (east)"

	| globalPosition tile tileExtentSamples |
	"Zero based"
	globalPosition _ self globalPixelPositionForLatitude: latitude longitude: longitude.
	tileExtentSamples _ self class tileWidthInSamples @ self class tileHeightInSamples.
	tile _ self tileAt: globalPosition // tileExtentSamples.
	^ self sampleTile: tile at: globalPosition \\ tileExtentSamples! !

!TiledLatitudeLongitudeSampledWorld methodsFor: 'accessing' stamp: 'jmv 1/13/2016 17:42'!
globalPixelPositionForLatitude: latitude longitude: longitude
	"Answer a zero-based point for requested LL.
	For the latitude and longitude of the topLeft point in the topLeft tile in the dataset, answer 0@0"

	self subclassResponsibility! !

!TiledLatitudeLongitudeSampledWorld methodsFor: 'accessing' stamp: 'jmv 8/14/2018 11:18:49'!
tileAt: zeroBasedTilePosition
	"aPoint is zero-based.
	allTiles coordinates are 1-based, i.e. they go from 1@1 to tileHorizontalCount@tileVerticalCount
	cachedTiles coordinates are 1-based
	"

	| tile oneBasedTilePosition tileCoordinates |
	oneBasedTilePosition _ zeroBasedTilePosition+1.
	tile _ allTiles at: oneBasedTilePosition.
	tile ifNil: [
		cachedTiles size >= self cacheSize ifTrue: [
			"free oldest cache element.
			This is not the LRU element!!
			Freeing the LRU element requires moving the cached element to the first position if found. Worth it?"
			tileCoordinates _ cachedTiles removeLast.
			('Removed from RAM Cache: ', tileCoordinates printString) print.
			allTiles at: tileCoordinates put: nil ].

		tile _ (self loadTileSamples: zeroBasedTilePosition)	"Might answer nil"
			ifNil: [ self newNullTile ]
			ifNotNil: [ :tileData |
				"Null tiles don't take space in the cache."
				cachedTiles addFirst: oneBasedTilePosition.
				self newTileWith: tileData ].

		allTiles at: oneBasedTilePosition put: tile.
		('Cached in RAM: ', oneBasedTilePosition printString) print ].
	^tile! !

!TiledLatitudeLongitudeSampledWorld methodsFor: 'private' stamp: 'jmv 8/14/2018 09:41:53'!
loadTileSamples: aPoint
	"aPoint is zero-based"
	self subclassResponsibility! !

!TiledLatitudeLongitudeSampledWorld methodsFor: 'private' stamp: 'lpc 3/28/2016 11:21'!
newNullTile
	^SampledTile null! !

!TiledLatitudeLongitudeSampledWorld methodsFor: 'private' stamp: 'jmv 3/23/2016 12:37'!
newTileWith: samples
	^SampledTile samples: samples! !

!TiledLatitudeLongitudeSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/8/2018 15:01:17'!
example1
"To run and play with it in a workspace.
Excellent registration between TrueMarble and DEM
TiledLatitudeLongitudeSampledWorld  example1
"

	| b dem demHeight egm96 extent g geoid geoidHeight lat lat0 lat1 long long0 long1 marbl marble marbleColor r srtm |
	egm96 _ EGM96Geoid5 getInstance.
	srtm _ ShuttleRadarTopographyMission3 getInstance.
	marble _ TrueMarble getInstance.

	lat0 _ -34.7. lat1 _ -34.3.		"South"
	long0 _ -58.7. long1 _ -58.3. "West"
	lat0 _ -39. lat1 _ -30.		"South"
	long0 _ -73. long1 _ -55. "West"
	
	lat0 _ -39. lat1 _ -30.		"South"
	long0 _ -73. long1 _ -64. "West"
	
	lat0 _ -20. lat1 _ -17.		"South"
	long0 _ -71. long1 _ -68. "West"
	
	lat0 _ -39. lat1 _ -30.		"South"
	long0 _ -73. long1 _ -64. "West"
	extent _ 800@800.
	
	lat0 _ -31.6. lat1 _ -31.		"South"
	long0 _ -71.7.  long1 _ -71.4. "West"
	extent _ 400@800.

	
" 	((lat1-lat0 * 60.0 / extent y) printString, ' minutes of latitude per pixel') print.
	((long1-long0 *60.0 / extent x) printString, ' minutes of longitude per pixel') print." 
	((lat1-lat0 * 3600.0 / extent y) printString, ' seconds of latitude per pixel') print.
	((long1-long0 *3600.0 / extent x) printString, ' seconds of longitude per pixel') print.

	geoid _ FloatMatrix extent: extent.
	dem _ FloatMatrix extent: extent.
	marbl _ FloatMatrix extent: extent.
	0 to: extent y-1 do: [ :y |
		lat _ y / (extent y-1) * (lat0-lat1) + lat1.
		0 to: extent x-1 do: [ :x |
			long _ x / (extent x-1) * (long1-long0) + long0.
			geoidHeight _ egm96 atLatitude: lat longitude: long.
			demHeight _ srtm atLatitude: lat longitude: long.
			marbleColor _ marble atLatitude: lat longitude: long.
			r _ geoidHeight-10/60 min: 1 max: 0.
			g _ marbleColor red / 2 min: 1 max: 0.
			b _ demHeight + 100 / 5000 min: 1 max: 0.
			
			g _ marbleColor red *2  min: 1 max: 0.
			b _ demHeight / 10 min: 1 max: 0.
			geoid i: y+1 j: x+1 put: r.
			marbl i: y+1 j: x+1 put: g.
			dem i: y+1 j: x+1 put: b.
			]].
	geoid asForm displayAt: extent x@0.
	marbl asForm displayAt: extent x * 2@0.
	dem asForm displayAt: extent x * 3@0.
	(geoid*3 asFormG: marbl b: dem) display.
	(dem asFormG: marbl b: dem) display.! !

!TiledLatitudeLongitudeSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/8/2018 15:03:45'!
example2
"To run and play with it in a workspace.
Good registration of Geoid with TrueMarble and DEM
TiledLatitudeLongitudeSampledWorld  example2
"

	| b dem demHeight egm96 extent g geoid geoidHeight lat lat0 lat1 long long0 long1 marbl marble marbleColor r srtm |
	egm96 _ EGM96Geoid5 getInstance.
	srtm _ ShuttleRadarTopographyMission3 getInstance.
	marble _ TrueMarble getInstance.

	lat0 _ -34.7. lat1 _ -34.3.		"South"
	long0 _ -58.7. long1 _ -58.3. "West"
	lat0 _ -39. lat1 _ -30.		"South"
	long0 _ -73. long1 _ -55. "West"
	
	lat0 _ -39. lat1 _ -30.		"South"
	long0 _ -73. long1 _ -64. "West"
	
	lat0 _ -20. lat1 _ -17.		"South"
	long0 _ -71. long1 _ -68. "West"
	
	lat0 _ -39. lat1 _ -30.		"South"
	long0 _ -73. long1 _ -64. "West"
	extent _ 800@800.

	
" 	((lat1-lat0 * 60.0 / extent y) printString, ' minutes of latitude per pixel') print.
	((long1-long0 *60.0 / extent x) printString, ' minutes of longitude per pixel') print." 
	((lat1-lat0 * 3600.0 / extent y) printString, ' seconds of latitude per pixel') print.
	((long1-long0 *3600.0 / extent x) printString, ' seconds of longitude per pixel') print.

	geoid _ FloatMatrix extent: extent.
	dem _ FloatMatrix extent: extent.
	marbl _ FloatMatrix extent: extent.
	0 to: extent y-1 do: [ :y |
		lat _ y / (extent y-1) * (lat0-lat1) + lat1.
		0 to: extent x-1 do: [ :x |
			long _ x / (extent x-1) * (long1-long0) + long0.
			geoidHeight _ egm96 atLatitude: lat longitude: long.
			demHeight _ srtm atLatitude: lat longitude: long.
			marbleColor _ marble atLatitude: lat longitude: long.
			r _ geoidHeight-10/60 min: 1 max: 0.
			g _ marbleColor red / 2 min: 1 max: 0.
			b _ demHeight + 100 / 5000 min: 1 max: 0.
	
			geoid i: y+1 j: x+1 put: r.
			marbl i: y+1 j: x+1 put: g.
			dem i: y+1 j: x+1 put: b.
			]].
	geoid asForm displayAt: extent x@0.
	marbl asForm displayAt: extent x * 2@0.
	dem asForm displayAt: extent x * 3@0.
	(geoid * 3 asFormG: marbl b: dem) display.! !

!TiledLatitudeLongitudeSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/8/2018 13:57:07'!
example3
"To run and play with it in a workspace.
Excellent registration between TrueMarble and DEM
TiledLatitudeLongitudeSampledWorld example3
"
	| c demH demo lat lat0 lat1 long long0 long1 marble marbleColor srtm |

	marble _ TrueMarble getInstance.
	srtm _ ShuttleRadarTopographyMission3 getInstance.

	lat0 _ -34.7. lat1 _ -34.3.		"South"
	long0 _ -58.7. long1 _ -58.3. "West"
	demo _ Form extent: 800@800 depth: 32.
	0 to: demo height-1 do: [ :y |
		lat _ y / (demo height-1) * (lat0-lat1) + lat1.
		0 to: demo width-1 do: [ :x |
			long _ x / (demo width-1) * (long1-long0) + long0.
			marbleColor _ marble atLatitude: lat longitude: long.
			demH _ ((srtm atLatitude: lat longitude: long) min: 50 max: -10)+10/60.
			c _ Color r: marbleColor red g: marbleColor green b: demH.
			demo colorAt: x@y put: c ]].
	demo display.! !

!TiledLatitudeLongitudeSampledWorld class methodsFor: 'constants' stamp: 'jmv 1/21/2016 16:57'!
tileCount
	"Answer is a Point"
	^ (360@180) / self tileExtentDegrees! !

!EGM96Geoid5 methodsFor: 'constants' stamp: 'jmv 2/19/2016 11:52'!
cacheSize
	^20! !

!EGM96Geoid5 methodsFor: 'accessing' stamp: 'jmv 1/21/2016 17:03'!
globalPixelPositionForLatitude: latitude longitude: longitude
	"Answer a zero-based point for requested LL.
	For the latitude and longitude of the topLeft point in the topLeft tile in the dataset, answer 0@0"

	"Pixel position in seconds. Upper left corner is 90N (90) and 0E (0)
	(center of first pixel is at tile nominal topLeft)"
	"Latitude goes from -90 (south pole) to 90 (north pole)
	Longitude goes from -180 (west of Alaska) to 180 (eastern Siberia)
	
	self new globalPixelPositionForLatitude: 90 longitude: 0
	self new globalPixelPositionForLatitude: -90 longitude: 0
	self new globalPixelPositionForLatitude: 90 longitude: 359.999999
	self new globalPixelPositionForLatitude: -90 longitude: 359.999999
	"

	| x y |
	y _ (90 - latitude) *60*60 / self class sampleHeightSeconds.
	x _ (longitude \\ 360) *60*60 / self class sampleWidthSeconds.
	^x@y! !

!EGM96Geoid5 methodsFor: 'accessing' stamp: 'jmv 8/14/2018 09:41:42'!
loadTileSamples: aPoint
	"aPoint is zero-based.
	All EGM96_5 dataset is contained in one file. Thus, we always load the same file despite the point  recieved"

	| answer |
	('will read: ' , datasetPath) print.
	answer _ self loadFromFile: datasetPath.
	'done' print.
	^ answer! !

!EGM96Geoid5 methodsFor: 'initialization' stamp: 'jmv 8/10/2018 15:15:43'!
initialize
	super initialize.
	name _ 'Geoid'.
	self datasetPath: 'payload/datasets/Geoid/egm96-5.pgm'! !

!EGM96Geoid5 methodsFor: 'private' stamp: 'jmv 5/31/2016 16:09'!
loadFromFile: aFilename

	"height is obtained as follows: height = offset + scale pixel, 
	where pixel is the pixel value read from the 16 bits image, 
	and scale and offset are parameters readed from the file"

 	| m value offset scale height aux aux2 idx scale_str offset_str fileSize file fileHeight fileWidth |  

	file _ aFilename asFileEntry.
	fileWidth _ 4320.
	fileHeight _ 2161.
	fileSize _ file fileSize.
	file readStreamDo: [ :stream |
		
		14 timesRepeat: [
			aux _ stream nextLine.
			(aux beginsWith: '# Offset')
				ifTrue: [
					aux2 _ aux.
					idx _ aux2 findString: '# Offset'. 
					offset_str _ aux2 copyFrom: idx + 8 to: aux2 size.
					offset_str _ offset_str copyReplaceAll:' ' with:''. 
					offset _ offset_str asNumber ].

			(aux beginsWith: '# Scale')
				ifTrue: [
					aux2 _ aux.
					idx _ aux2 findString: '# Scale'. 
					scale_str _ aux2 copyFrom: idx + 7 to: aux2 size.
					scale_str _ scale_str copyReplaceAll:' ' with:''. 
					scale _ scale_str asNumber ].
			].

		self assert: stream nextLine = '4320 2161'.
		self assert: stream nextLine = '65535'.
		self assert: fileSize - stream position = (fileWidth * fileHeight * 2).
		stream binary.
	

		m _ FloatImage extent: fileWidth+1@fileHeight.
		1 to: fileHeight do: [ :i |
			1 to: fileWidth do: [ :j |
				value _ stream nextUnsignedInt16BigEndian: true.
				height _ value * scale + offset.
				m i: i j: j put: height ]]].

	"Add an extra column at the end, repeating the first one,
	because we can't interpolate outside bounds, so pixel values interpolated between last and first (as in wraparound) 
	would not be computed (and answered zero!!)"
	m j: fileWidth+1 put: (m j: 1).
	^m! !

!EGM96Geoid5 methodsFor: 'private' stamp: 'jmv 3/23/2016 12:31'!
sampleTile: tile at: zeroBasedPosition
	"zeroBasedPosition = 0@0 means topLeft.
	Matrices are 1-based, meaning topLeft element is at 1@1

	Aconcagua:
	ShuttleRadarTopographyMission3 getInstance atLatitude: -32.6534 longitude: -70.0111
	EGM96Geoid5 getInstance atLatitude: -32.6534 longitude: -70.0111
	"

	^tile samples bilinearInterpolatedValueAtX: zeroBasedPosition x + 1 y: zeroBasedPosition y + 1! !

!EGM96Geoid5 class methodsFor: 'constants' stamp: 'jmv 1/21/2016 17:01'!
sampleHeightSeconds
	"in seconds of latitude: 5 min"
	^300! !

!EGM96Geoid5 class methodsFor: 'constants' stamp: 'jmv 1/21/2016 17:00'!
sampleWidthSeconds
	"in seconds of latitude: 5 min"
	^300! !

!EGM96Geoid5 class methodsFor: 'constants' stamp: 'jmv 1/21/2016 16:56'!
tileExtentDegrees
	"in degrees of longitude@latitude"
	^360@180! !

!EGM96Geoid5 class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:12'!
tileHeightInSamples
	"Not the count of samples, but the total height.
	If there is superpostion of tiles, do not include it."
	^2161! !

!EGM96Geoid5 class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:12'!
tileWidthInSamples
	"Not the count of samples, but the total width.
	If there is superpostion of tiles, do not include it."
	^4320! !

!NaturalEarth methodsFor: 'constants' stamp: 'jmv 2/19/2016 11:52'!
cacheSize
	^1! !

!NaturalEarth methodsFor: 'accessing' stamp: 'jmv 1/21/2016 17:03'!
globalPixelPositionForLatitude: latitude longitude: longitude
	"Pixel position as if there was only one tile
	(center of first pixel is 0.5@0.5 pixels from tile nominal topLeft)"
	"Latitude goes from -90 (south pole) to 90 (north pole)
	Longitude goes from -180 (west of Alaska) to 180 (eastern Siberia)

	self new globalPixelPositionForLatitude: 90 longitude: -180

	self new globalPixelPositionForLatitude: -90 longitude: -180

	self new globalPixelPositionForLatitude: 90 longitude: 180

	self new globalPixelPositionForLatitude: -90 longitude: 180

	self new globalPixelPositionForLatitude: -90 longitude: -180
	self new globalPixelPositionForLatitude: 90 longitude: 179.999999
	self new globalPixelPositionForLatitude: -90 longitude: 179.999999
	"

	| x y |
	y _ 90.0 - latitude *60*60 / self class sampleHeightSeconds.
	x _ 180.0 + longitude *60*60 / self class sampleWidthSeconds.
	"Special cases."
	latitude = -90 ifTrue: [ y _ y predecessor ].
	longitude = 180 ifTrue: [ x _ x predecessor ].
	^(x @ y) floor! !

!NaturalEarth methodsFor: 'accessing' stamp: 'jmv 8/14/2018 09:41:46'!
loadTileSamples: aPoint
	"aPoint is zero-based."
	"Filename is in datasetPath, as we use a single file
	"
	| samples |
	'...' print.
	samples _ Form fromFileNamed: datasetPath.
	'done' print.
	^ samples! !

!NaturalEarth methodsFor: 'initialization' stamp: 'jmv 8/10/2018 14:57:11'!
initialize
	super initialize.
	name _ 'NaturalEarth'.
	self datasetPath: 'payload/datasets/NaturalEarthPlusBathymetry/NE_Drape-3000x1500.tif'! !

!NaturalEarth methodsFor: 'private' stamp: 'jmv 8/14/2018 10:16:29'!
sampleTile: tile at: zeroBasedPosition
	"zeroBasedPosition = 0@0 means topLeft, width@height means bottomRight
	This datasets considers pixels as areas, not samples.
	Samples is a Form, and therefore zero based.
	This means that:
		- topLeft pixel covers zeroBasedPosition values in 0@0 to 1@1. zeroBasedPosition = 0.5@0.5 hits pixel 0@0 exactly
		- bottomRight pixel covers zeroBasedPosition values in width@height-1 to width@height. zeroBasedPosition = width@height-0.5 hits pixel width@height-1 (bottomRight pixel) exactly
	Additionally, 0@0 is a valid zeroBasedPosition. Make the valid interval for zeroBasedPosition be [0.0 .. width) x [0.0 .. height) and not (0.0 .. width) x (0.0 .. height).
	This is, make the start of the interval closed. Make 0@0 a valid coordinate.
	"
	| x y |
	x _ zeroBasedPosition x.
	x _ x isZero ifTrue: [ 0 ] ifFalse: [ (x - 0.5) rounded ].
	y _ zeroBasedPosition y.
	y _ y isZero ifTrue: [ 0 ] ifFalse: [ (y - 0.5) rounded ].
	^tile samples colorAt: x@y

	"See comment on color interpolation at TrueMarble>>#sampleTile:at:"! !

!NaturalEarth methodsFor: 'examples' stamp: 'jmv 8/9/2018 17:34:26'!
demoResult: extent
	^ Form extent: extent depth: 32! !

!NaturalEarth class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:15'!
sampleHeightSeconds
	"in seconds of latitude"
	^180 * 60 * 60 / self tileHeightInSamples! !

!NaturalEarth class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:15'!
sampleWidthSeconds
	"in seconds of longitude"
	^360 * 60 * 60 / self tileWidthInSamples! !

!NaturalEarth class methodsFor: 'constants' stamp: 'jmv 1/21/2016 16:56'!
tileExtentDegrees
	"in degrees of longitude@latitude"
	^360@180! !

!NaturalEarth class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:13'!
tileHeightInSamples
	"Not the count of samples, but the total height.
	If there is superpostion of tiles, do not include it."

	^1500! !

!NaturalEarth class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:13'!
tileWidthInSamples
	"Not the count of samples, but the total width.
	If there is superpostion of tiles, do not include it."
	^3000! !

!ShuttleRadarTopographyMission methodsFor: 'queries' stamp: 'jmv 3/8/2016 11:44'!
fractionForIntersectionOfRadiusWith: ecef geoid: geid
	"Compute and answer the k that multiplied by ecef argument lies on the surface of terrain.
	This is, the intersection of the rect that passes both through Earth center and ecef, with terrain.
	ecef is usually on the sphere, but that's not really required."

	| fraction count done candidateECEF candidateLL height surfacePointLLH surfacePointECEF msg |
	fraction _ 1.
	count _ 0.
	done _ false.
	[ done ] whileFalse: [
		candidateECEF _ ecef * fraction.
		candidateLL _ GPSPosition fromECEF: candidateECEF.
 		height _  (geid atGPSPosition: candidateLL) + (self atGPSPosition: candidateLL).
	 	surfacePointLLH _ candidateLL withHeight: height.
		surfacePointECEF _ surfacePointLLH asECEF.
		done _ (surfacePointECEF - candidateECEF) length < 1.
		count _ count + 1.
		count > 100 ifTrue: [
			msg _ 'Failure to compute distance to Sphere at ', ecef printString, ' - iterations: ', count printString.
			msg print.
			self halt: msg.
			done _ true ].
		fraction _ surfacePointECEF length / ecef length.
	].
	^fraction! !

!ShuttleRadarTopographyMission methodsFor: 'private' stamp: 'jmv 3/4/2016 10:40'!
globalPixelPositionForLatitude: latitude longitude: longitude
	"Answer a zero-based point for requested LL.
	For the latitude and longitude of the topLeft point in the topLeft tile in the dataset, answer 0@0"

	"Pixel position as if there was only one tile
	(center of first pixel is at tile nominal topLeft)"
	"Latitude goes from -90 (south pole) to 90 (north pole)
	Longitude goes from -180 (west of Alaska) to 180 (eastern Siberia)
	
	ShuttleRadarTopographyMission3 getInstance globalPixelPositionForLatitude: 90 longitude: -180
	ShuttleRadarTopographyMission3 getInstance globalPixelPositionForLatitude: -90 longitude: -180
	ShuttleRadarTopographyMission3 getInstance globalPixelPositionForLatitude: 90 longitude: 179.999999
	ShuttleRadarTopographyMission3 getInstance globalPixelPositionForLatitude: -90 longitude: 179.999999
	ShuttleRadarTopographyMission3 getInstance globalPixelPositionForLatitude: 90 longitude: 180
	ShuttleRadarTopographyMission3 getInstance globalPixelPositionForLatitude: -90 longitude: 180

	ShuttleRadarTopographyMission30 getInstance globalPixelPositionForLatitude: 90 longitude: -180
	ShuttleRadarTopographyMission30 getInstance globalPixelPositionForLatitude: -90 longitude: -180
	ShuttleRadarTopographyMission30 getInstance globalPixelPositionForLatitude: 90 longitude: 179.999999
	ShuttleRadarTopographyMission30 getInstance globalPixelPositionForLatitude: -90 longitude: 179.999999
	ShuttleRadarTopographyMission30 getInstance globalPixelPositionForLatitude: 90 longitude: 180
	ShuttleRadarTopographyMission30 getInstance globalPixelPositionForLatitude: -90 longitude: 180
	"

	| x y |
	"We have some trouble with longitude=180 degrees: we'd jump to an unexisting tile.
	But longitude=180East is the same as 180West..."
	longitude >= 180.0 ifTrue: [
		^ self globalPixelPositionForLatitude: latitude longitude: -180.0 ].
	y _ 90 - latitude *60*60 / self class sampleHeightSeconds.
	x _ 180 + longitude *60*60 / self class sampleWidthSeconds.
	"We can' t currently handle the south pole. Just avoid it.
	(what fails is #filenameForTileAt:, and #testFilenames)"
	latitude <= -90.0 ifTrue: [
		y _ y asFloat predecessor ].
	^x@y! !

!ShuttleRadarTopographyMission methodsFor: 'private' stamp: 'jmv 5/31/2016 16:09'!
loadFromFile: aFilename
 	| m value file tell prevValue |  
	
	file _ aFilename asFileEntry.
	"If file doesn't exist, assume it is all water."
	file exists ifFalse: [ ^ nil "Allow for null tiles" ].

	self class samplesPerTileH * self class samplesPerTileV * 2 = file fileSize ifFalse: [ 
		('Dem file read: ', aFilename, 'Invalid file size. Tile ignored.') print.
		^ nil "Allow for null tiles" ].
	
	m _ FloatImage extent: self class samplesPerTileH @ self class samplesPerTileV.

	tell _ false.

	prevValue _ nil.
	file readStreamDo: [ :stream |
		stream binary.
		1 to: self class samplesPerTileV do: [ :i |
			1 to: self class samplesPerTileH do: [ :j |
				value _ stream nextSignedInt16BigEndian: true.
				"When we find void values inside an SRTM or ViewFinderPanoramas tile,
				it means that this value is unknown. An acceptable solution is to take some neighbour value."
				value = self class voidValue ifTrue: [
					value _ prevValue ifNil: [ 0 ].
					tell _ true ].
				m i: i j: j put: value.
				prevValue _ value.
			]
		].
	].

	tell ifTrue: [
		('Dem file read: ', aFilename, 'There were void values, and they were ignored. Last valid value taken.') print ].

	^m.! !

!ShuttleRadarTopographyMission methodsFor: 'private' stamp: 'jmv 8/14/2018 09:41:50'!
loadTileSamples: aPoint
	"The name is (for example) N40W118.hgt
	Argument is number of tile horizontally @ number of tile vertically, zero based."
	
	| fileName samples |
	fileName _ self filenameForTileAt: aPoint.
"	('will read: ' , fileName) print."

	samples _ self loadFromFile: datasetPath, '/', fileName. 
"	'done' print."
	^ samples! !

!ShuttleRadarTopographyMission3 methodsFor: 'accessing' stamp: 'jmv 4/5/2016 12:45'!
atLatitude: latitude longitude: longitude
	"latitude in -90.0 (south pole) to 90.0 (north pole)
	longitude in -180.0 (west) to 180.0 (east)"

	| globalPosition tile tileExtentSamples tilePos posInTile |
	"Zero based"
	globalPosition _ self globalPixelPositionForLatitude: latitude longitude: longitude.
	tileExtentSamples _ self class tileWidthInSamples @ self class tileHeightInSamples.
	tilePos _ globalPosition // tileExtentSamples.
	posInTile _ globalPosition \\ tileExtentSamples.
	tile _ self tileAt: tilePos.
	tile needsValidation ifTrue: [
		self validate: tile from: tilePos ].
	^ self sampleTile: tile at: posInTile! !

!ShuttleRadarTopographyMission3 methodsFor: 'accessing' stamp: 'jmv 3/11/2016 11:05'!
cacheSize: aNumber
	cacheSize _ aNumber! !

!ShuttleRadarTopographyMission3 methodsFor: 'accessing' stamp: 'jmv 1/22/2016 15:52'!
filenameForTileAt: aPoint
	"Argument is number of tile horizontally @ number of tile vertically, zero based."

	"The names of individual data tiles refer to the longitude and latitude of the lower-left (southwest) corner of the tile.
	For example, the coordinates of the lower-left corner of tile N40W118 are 40 degrees north latitude and 118 degrees west longitude.
	To be more exact, these coordinates refer to the geometric center of the lower left sample.
	
	The name is (for example) N40W118.hgt
	
	Argument is number of tile horizontally @ number of tile vertically
	self new filenameForTileAt: 0@0
	self new filenameForTileAt: 1@1
	
	From: http://www2.jpl.nasa.gov/srtm/faq.html
	The SRTM data files have names like 'N34W119.hgt'. What do the letters and numbers refer to, and what is '.hgt' format?

Each data file covers a one-degree-of-latitude by one-degree-of-longitude block of Earth's surface. The first seven characters indicate the southwest corner of the block, with N, S, E, and W referring to north, south, east, and west. Thus, the 'N34W119.hgt' file covers latitudes 34 to 35 North and longitudes 118-119 West (this file includes downtown Los Angeles, California). The filename extension '.hgt' simply stands for the word 'height', meaning elevation. It is NOT a format type. These files are in 'raw' format (no headers and not compressed), 16-bit signed integers, elevation measured in meters above sea level, in a 'geographic' (latitude and longitude array) projection, with data voids indicated by -32768. International 3-arc-second files have 1201 columns and 1201 rows of data, with a total filesize of 2,884,802 bytes ( = 1201 x 1201 x 2). United States 1-arc-second files have 3601 columns and 3601 rows of data, with a total filesize of 25,934,402 bytes ( = 3601 x 3601 x 2). For more information read the text file 'SRTM_Topo.txt' at http://edcftp.cr.usgs.gov/pub/data/srtm/Readme.html
	"
	| tileBottomLatitude tileLeftLongitude latChar lonChar |

	"See #globalPixelPositionForLatitude:longitude:, conversion from LL to 'global pixel'"
	"y _ 90 - latitude *60*60 / self class sampleHeightSeconds.
	x _ 180 + longitude *60*60 / self class sampleWidthSeconds."

	tileBottomLatitude _ 90 - (aPoint y+1 * self class tileExtentDegrees y).
	latChar _ tileBottomLatitude < 0
		ifTrue: [ 'S' ]
		ifFalse: [ 'N'].	

	tileLeftLongitude _ aPoint x * self class tileExtentDegrees x -180.
	lonChar _ tileLeftLongitude < 0
		ifTrue: [ 'W' ]
		ifFalse: [ 'E'].	
		
	^ latChar, (tileBottomLatitude abs printStringBase: 10 length: 2 padded: true), lonChar, (tileLeftLongitude abs printStringBase: 10 length: 3 padded: true), '.', self class tileExtension.! !

!ShuttleRadarTopographyMission3 methodsFor: 'accessing' stamp: 'jmv 4/5/2016 11:39'!
logFilename: aString
	logFilename _ aString! !

!ShuttleRadarTopographyMission3 methodsFor: 'accessing' stamp: 'jmv 5/31/2016 16:08'!
validate: tile from: aPoint
	"aPoint is zero-based.
	NOP by default
	"
	| tileSamples rightNeighbor downNeighbor downNeighborPos rightNeighborPos |
	tileSamples _ tile samples.
	rightNeighborPos _  aPoint+(1@0) \\ allTiles extent.
	rightNeighbor _  self tileAt: rightNeighborPos.
	rightNeighbor isNull ifTrue: [ | tileColumn |
		tileColumn _ tileSamples j: self class samplesPerTileH.
		tileColumn norm2Squared > 0 ifTrue: [
			(logFilename, 'nznn') asFileEntry appendStreamDo: [ :stream |
				stream
					nextPutAll: 'No right neighbor and nonzero values in ';
					nextPutAll: (self filenameForTileAt: aPoint);
					nextPutAll: '. Values: '.
				tileColumn elements asSet asArray sort printOn: stream.
				stream newLine ].
			tileSamples j: self class samplesPerTileH put: tileColumn * 0 
			]
	] ifFalse: [
		| rightNeighborSamples tileColumn rightTileColumn |
		rightNeighborSamples _ rightNeighbor samples.
		tileColumn _ tileSamples j: self class samplesPerTileH.
		rightTileColumn _ rightNeighborSamples j: 1.
		(tileColumn = rightTileColumn) ifFalse: [
			logFilename asFileEntry appendStreamDo: [ :stream |
				stream
					nextPutAll: 'Inconsistency between ';
					nextPutAll: (self filenameForTileAt: aPoint);
					nextPutAll: ' and ';
					nextPutAll: (self filenameForTileAt: rightNeighborPos);
					nextPutAll: '. Differences: '.
				(tileColumn - rightTileColumn) elements asSet asArray sort printOn: stream.
				stream newLine ].
			tileSamples j: self class samplesPerTileH put: rightTileColumn ]].

	downNeighborPos _  aPoint+(0@1) \\ allTiles extent.
	downNeighbor _  self tileAt: downNeighborPos.
	downNeighbor isNull ifTrue: [ | tileRow |
		tileRow _ tileSamples i: self class samplesPerTileV.
		tileRow norm2Squared > 0 ifTrue: [
			(logFilename, 'nznn') asFileEntry appendStreamDo: [ :stream |
				stream
					nextPutAll: 'No bottom neighbor and nonzero values in ';
					nextPutAll: (self filenameForTileAt: aPoint);
					nextPutAll: '. Values: '.
				tileRow elements asSet asArray sort printOn: stream.
				stream newLine ].
			tileSamples i: self class samplesPerTileV put: tileRow * 0 
			]
	] ifFalse: [
		| downNeighborSamples tileRow downTileRow |
		downNeighborSamples _ downNeighbor samples.
		tileRow _ tileSamples i: self class samplesPerTileV.
		downTileRow _ downNeighborSamples i: 1.
		(tileRow = downTileRow) ifFalse: [
			logFilename asFileEntry appendStreamDo: [ :stream |
				stream
					nextPutAll: 'Inconsistency between ';
					nextPutAll: (self filenameForTileAt: aPoint);
					nextPutAll: ' and ';
					nextPutAll: (self filenameForTileAt: downNeighborPos);
					nextPutAll: '. Differences: '.
				(tileRow - downTileRow) elements asSet asArray sort printOn: stream.
				stream newLine ].
			tileSamples i: self class samplesPerTileV put: downTileRow ]].

	tile validated! !

!ShuttleRadarTopographyMission3 methodsFor: 'constants' stamp: 'jmv 3/11/2016 11:05'!
cacheSize
	^cacheSize! !

!ShuttleRadarTopographyMission3 methodsFor: 'initialization' stamp: 'jmv 8/10/2018 15:02:13'!
initialize
	super initialize.
	name _ 'VFPanoramas3'.
	self datasetPath: 'payload/datasets/DEM-ViewfinderPanoramas'.
	logFilename _ self class name asString, 'validationLog', Date today yyyymmdd asString, '.txt'.
	cacheSize _ 28! !

!ShuttleRadarTopographyMission3 methodsFor: 'private' stamp: 'lpc 3/28/2016 11:21'!
newNullTile
	^ValidableTile null! !

!ShuttleRadarTopographyMission3 methodsFor: 'private' stamp: 'jmv 3/23/2016 12:38'!
newTileWith: samples
	^ValidableTile samples: samples! !

!ShuttleRadarTopographyMission3 methodsFor: 'private' stamp: 'jmv 3/23/2016 12:49'!
sampleTile: tile at: zeroBasedPosition
	"zeroBasedPosition = 0@0 means topLeft.
	Matrices are 1-based, meaning topLeft element is at 1@1

	Aconcagua:
	ShuttleRadarTopographyMission3 getInstance atLatitude: -32.653333 longitude: -70.010833
	"
	^tile samples
		ifNil: [ 0.0 ]	"Missing tiles mean zero meters, i.e. ocean"
		ifNotNil: [ :samples |
			samples bilinearInterpolatedValueAtX: zeroBasedPosition x +1 y: zeroBasedPosition y + 1 ]! !

!ShuttleRadarTopographyMission3 class methodsFor: 'constants' stamp: 'jmv 1/21/2016 17:01'!
sampleHeightSeconds
	"in seconds of latitude"
	^3! !

!ShuttleRadarTopographyMission3 class methodsFor: 'constants' stamp: 'jmv 1/21/2016 17:00'!
sampleWidthSeconds
	"in seconds of longitude"
	^3! !

!ShuttleRadarTopographyMission3 class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:26'!
samplesPerTileH
	"in samples. If there is superposition of tiles, include it."
	^1201! !

!ShuttleRadarTopographyMission3 class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:26'!
samplesPerTileV
	"in samples. If there is superposition of tiles, include it."
	^1201! !

!ShuttleRadarTopographyMission3 class methodsFor: 'constants' stamp: 'lpc 1/13/2016 15:00'!
tileExtension
	^'hgt'! !

!ShuttleRadarTopographyMission3 class methodsFor: 'constants' stamp: 'jmv 1/21/2016 16:56'!
tileExtentDegrees
	"in degrees of longitude@latitude"
	^1@1! !

!ShuttleRadarTopographyMission3 class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:06'!
tileHeightInSamples
	"Not the count of samples, but the total height.
	If there is superpostion of tiles, do not include it."
	^1200! !

!ShuttleRadarTopographyMission3 class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:06'!
tileWidthInSamples
	"Not the count of samples, but the total width.
	If there is superpostion of tiles, do not include it."
	^1200! !

!ShuttleRadarTopographyMission3 class methodsFor: 'constants' stamp: 'jmv 2/19/2016 12:29'!
voidValue
	"When we find void values inside an SRTM or ViewFinderPanoramas tile,
	it means that this value is unknown. An acceptable solution is to take some neighbour value."
	^  -32768! !

!ShuttleRadarTopographyMission30 methodsFor: 'constants' stamp: 'jmv 2/19/2016 11:52'!
cacheSize
	^6! !

!ShuttleRadarTopographyMission30 methodsFor: 'accessing' stamp: 'jmv 1/22/2016 15:49'!
filenameForTileAt: aPoint

	"Tile filenames match the pattern LONXXXLATYYY, where XXX and YYY are the coordinates for the left upper corner of the tile.
	See ShuttleRadarTopographyMission3>>filenameForTileAt: for a more detailed explanation.
	Argument is number of tile horizontally @ number of tile vertically, zero based."
	
	| tileTopLatitude tileLeftLongitude latChar lonChar |
	tileTopLatitude _ 90 - (aPoint y * self class tileExtentDegrees y).
	latChar _ tileTopLatitude < 0
		ifTrue: [ 'S' ]
		ifFalse: [ 'N'].	

	tileLeftLongitude _ aPoint x * self class tileExtentDegrees x - 180.
	lonChar _ tileLeftLongitude < 0
		ifTrue: [ 'W' ]
		ifFalse: [ 'E'].	
		
	^ lonChar, (tileLeftLongitude abs printStringBase: 10 length: 3 padded: true), latChar, (tileTopLatitude abs printStringBase: 10 length: 2 padded: true), '.', self class tileExtension! !

!ShuttleRadarTopographyMission30 methodsFor: 'initialization' stamp: 'jmv 8/10/2018 14:57:27'!
initialize
	super initialize.
	name _ 'SRTM30'.
	self datasetPath: 'payload/datasets/SRTM30'! !

!ShuttleRadarTopographyMission30 methodsFor: 'private' stamp: 'jmv 8/9/2018 16:11:13'!
sampleTile: tile at: zeroBasedPosition
	"zeroBasedPosition = 0@0 means topLeft.
	Matrices are 1-based, meaning topLeft element is at 1@1

	Aconcagua:
	ShuttleRadarTopographyMission30 new atLatitude: -32.6625 longitude: -70.02083333.
	"
	| xx yy |
	^tile samples
		ifNil: [ 0.0 ]	"Missing tiles mean zero meters, i.e. ocean"
		ifNotNil: [
			"We add 0.5 and not 1, because there is an extra -0.5 pixel offset in both directions.
			Differently from ShuttleRadarTopographyMission3 (that has an 1 pixel overlap between tiles),
			ShuttleRadarTopographyMission30 has no overlap. This means that latLong 90,-180 means -0.5@-0.5, i.e.
			the 'topLeft' corner of the topLeft pixel."
			xx _ zeroBasedPosition x + 0.5.
			yy _ zeroBasedPosition y + 0.5.
			tile samples bilinearInterpolatedClampToEdgeValueAtX: xx y: yy ]! !

!ShuttleRadarTopographyMission30 class methodsFor: 'constants' stamp: 'jmv 1/21/2016 17:01'!
sampleHeightSeconds
	"in seconds of latitude"
	^30! !

!ShuttleRadarTopographyMission30 class methodsFor: 'constants' stamp: 'jmv 1/21/2016 17:00'!
sampleWidthSeconds
	"in seconds of longitude"
	^30! !

!ShuttleRadarTopographyMission30 class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:07'!
samplesPerTileH
	"Sample count"
	^4800! !

!ShuttleRadarTopographyMission30 class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:07'!
samplesPerTileV
	"Sample count"
	^6000! !

!ShuttleRadarTopographyMission30 class methodsFor: 'constants' stamp: 'jmv 1/18/2016 15:49'!
tileCount
	"For this dataset, available data doesn't cover the whole world. Only latitudes north to 60 degrees South are covered.
	The super call doesn't answer integers!!
	Act as if there were extra tiles covering from 60 degrees South to the South pole.
	Those tiles don't exist, so they will never be found."
	^ super tileCount ceiling! !

!ShuttleRadarTopographyMission30 class methodsFor: 'constants' stamp: 'lpc 1/13/2016 14:59'!
tileExtension
	^'DEM'! !

!ShuttleRadarTopographyMission30 class methodsFor: 'constants' stamp: 'jmv 1/21/2016 16:57'!
tileExtentDegrees
	"in degrees of longitude@latitude"
	^40@50! !

!ShuttleRadarTopographyMission30 class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:14'!
tileHeightInSamples
	"Not the count of samples, but the total height.
	If there is superpostion of tiles, do not include it."
	^6000! !

!ShuttleRadarTopographyMission30 class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:14'!
tileWidthInSamples
	"Not the count of samples, but the total width.
	If there is superpostion of tiles, do not include it."
	^4800! !

!ShuttleRadarTopographyMission30 class methodsFor: 'constants' stamp: 'jmv 2/19/2016 12:29'!
voidValue
	"When we find void values inside an SRTM or ViewFinderPanoramas tile,
	it means that this value is unknown. An acceptable solution is to take some neighbour value."
	^  -32768! !

!TrueMarble methodsFor: 'accessing' stamp: 'jmv 1/22/2016 16:17'!
atLatitude: latitude longitude: longitude
	"latitude in -90.0 (south pole) to 90.0 (north pole)
	longitude in -180.0 (west) to 180.0 (east)"

	| globalPositionRounded tile tileExtentSamples |
	"Zero based"
	globalPositionRounded _ (self globalPixelPositionForLatitude: latitude longitude: longitude) rounded.
	tileExtentSamples _ self class tileWidthInSamples @ self class tileHeightInSamples.
	tile _ self tileAt: globalPositionRounded // tileExtentSamples.
	^ self sampleTile: tile at: globalPositionRounded \\ tileExtentSamples! !

!TrueMarble methodsFor: 'accessing' stamp: 'jmv 1/21/2016 17:04'!
globalPixelPositionForLatitude: latitude longitude: longitude
	"Pixel position as if there was only one tile
	(center of first pixel is 0.5@0.5 pixels from tile nominal topLeft)"
	"Latitude goes from -90 (south pole) to 90 (north pole)
	Longitude goes from -180 (west of Alaska) to 180 (eastern Siberia)

	self new globalPixelPositionForLatitude: 90 longitude: -180
	(self new globalPixelPositionForLatitude: 90 longitude: -180) rounded
	
	self new globalPixelPositionForLatitude: -90 longitude: -180
	(self new globalPixelPositionForLatitude: -90 longitude: -180) rounded

	self new globalPixelPositionForLatitude: 90 longitude: 180
	(self new globalPixelPositionForLatitude: 90 longitude: 180) rounded
	
	self new globalPixelPositionForLatitude: -90 longitude: 180
	(self new globalPixelPositionForLatitude: -90 longitude: 180) rounded
	
	self new globalPixelPositionForLatitude: 90-(TrueMarble sampleWidthSeconds/3600*0.5) longitude: -180 + (TrueMarble sampleWidthSeconds/3600*0.5)
	self new globalPixelPositionForLatitude: -90 longitude: -180
	self new globalPixelPositionForLatitude: 90 longitude: 179.999999
	self new globalPixelPositionForLatitude: -90 longitude: 179.999999
	
	(self new globalPixelPositionForLatitude: 90-(TrueMarble sampleWidthSeconds/3600*0.5) longitude: -180 + (TrueMarble sampleWidthSeconds/3600*0.5)) rounded
	(self new globalPixelPositionForLatitude: -90 longitude: -180) rounded
	(self new globalPixelPositionForLatitude: 90 longitude: 179.999999) rounded
	(self new globalPixelPositionForLatitude: -90 longitude: 179.999999) rounded
	"

	| x y |
	y _ 90 - latitude *60*60 / self class sampleHeightSeconds - 0.5.
	x _ 180 + longitude *60*60 / self class sampleWidthSeconds - 0.5.
	"Special cases. Essentially due to Float rounding"
	latitude = 90 ifTrue: [ y _ y successor ].
	latitude = -90 ifTrue: [ y _ y predecessor ].
	longitude = -180 ifTrue: [ x _ x successor ].
	longitude = 180 ifTrue: [ x _ x predecessor ].
	^x @ y! !

!TrueMarble methodsFor: 'accessing' stamp: 'jmv 8/14/2018 09:41:56'!
loadTileSamples: aPoint
	"aPoint is zero-based."
	"Filenames are like
	TrueMarble_250m_4320x4320_C2_3_2.png
	"
	| xMajor xMinor yMajor yMinor fileName samples |
	xMajor _ ('ABCDEFGH' at: aPoint x // 5 + 1) asString.
	xMinor _ (aPoint x \\ 5) asString.
	yMajor _ (aPoint y // 5 + 1) asString.
	yMinor _ (aPoint y \\ 5) asString.
	fileName _ 'TrueMarble_250m_4320x4320_', xMajor, yMajor, '_', xMinor, '_', yMinor, '.png'.
	('will read: ' , fileName) print.
	samples _ Form fromFileNamed: datasetPath, '/', fileName.
	'done' print.
	^ samples! !

!TrueMarble methodsFor: 'constants' stamp: 'jmv 8/8/2018 13:46:41'!
cacheSize
	^4*4! !

!TrueMarble methodsFor: 'initialization' stamp: 'jmv 8/10/2018 14:57:47'!
initialize
	super initialize.
	name _ 'TrueMarble'.
	self datasetPath: 'payload/datasets/TrueMarble/TrueMarble-250m'! !

!TrueMarble methodsFor: 'private' stamp: 'jmv 8/14/2018 10:17:55'!
sampleTile: tile at: zeroBasedPosition
	"zeroBasedPosition = 0@0 means topLeft, width@height means bottomRight
	This datasets considers pixels as areas, not samples.
	Samples is a Form, and therefore zero based.
	This means that:
		- topLeft pixel covers zeroBasedPosition values in 0@0 to 1@1. zeroBasedPosition = 0.5@0.5 hits pixel 0@0 exactly
		- bottomRight pixel covers zeroBasedPosition values in width@height-1 to width@height. zeroBasedPosition = width@height-0.5 hits pixel width@height-1 (bottomRight pixel) exactly
	Additionally, 0@0 is a valid zeroBasedPosition. Make the valid interval for zeroBasedPosition be [0.0 .. width) x [0.0 .. height) and not (0.0 .. width) x (0.0 .. height).
	This is, make the start of the interval closed. Make 0@0 a valid coordinate.
	"
	| x y |
	x _ zeroBasedPosition x.
	x _ x isZero ifTrue: [ 0 ] ifFalse: [ (x - 0.5) rounded ].
	y _ zeroBasedPosition y.
	y _ y isZero ifTrue: [ 0 ] ifFalse: [ (y - 0.5) rounded ].
	^tile samples colorAt: x@y

	"About interpolating samples.

	Bilinear interpolation is reasonably ok if the target gsd is not coarser than dataset.
	If target has lower resolution than dataset, the some anti aliasing filtering (such as Area filtering) is in order!!
	
	But if we ever enable interpolation for this dataset, keep in mind
		- Out of range latitudes  (i.e. latitudes really close to the poles) should be rounded. 
			There's no point in iterpolating that, and computing the missing samples is not trivial
		- To be able to interpolate values close to 180W or 180E, an extra first column is needed (duplicating the original last column) 
			and an extra last column is needed (duplicating the original first column).
			Keep in mind that other datasets, like EGM96 and SRTM just need an extra last column that they already include (and not an extra first column),
			because they consider the origin the center of the topLeft pixel, while TrueMarble considers the origin at 
			the topLeft corner of the little square covered by the topLeft pixel

	So, the following doesn't work!!

	^tile colorInterpolatedAt: zeroBasedPosition
	"! !

!TrueMarble methodsFor: 'examples' stamp: 'jmv 8/9/2018 17:34:48'!
demoResult: extent
	^ Form extent: extent depth: 32! !

!TrueMarble class methodsFor: 'constants' stamp: 'jmv 1/21/2016 17:01'!
sampleHeightSeconds
	"in seconds of latitude"
	^7.5! !

!TrueMarble class methodsFor: 'constants' stamp: 'jmv 1/21/2016 17:00'!
sampleWidthSeconds
	"in seconds of longitude"
	^7.5! !

!TrueMarble class methodsFor: 'constants' stamp: 'jmv 1/21/2016 16:57'!
tileExtentDegrees
	"in degrees of longitude@latitude"
	^9@9! !

!TrueMarble class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:06'!
tileHeightInSamples
	"Not the count of samples, but the total height.
	If there is superpostion of tiles, do not include it."
	^4320! !

!TrueMarble class methodsFor: 'constants' stamp: 'jmv 1/22/2016 16:06'!
tileWidthInSamples
	"Not the count of samples, but the total width.
	If there is superpostion of tiles, do not include it."
	^4320! !

!TiledWebmercatorSampledWorld methodsFor: 'initialization' stamp: 'jmv 8/14/2018 12:00:50'!
initializeZoomLevel: anInteger

	projection _ WebmercatorProjection new.
	zoomLevel _ anInteger.
	allTiles _ Dictionary new.
	cachedTiles _ OrderedCollection new! !

!TiledWebmercatorSampledWorld methodsFor: 'accessing' stamp: 'jmv 8/15/2018 10:10:19'!
atGPSPosition: aGPSPosition
	"This method is to be used when source dataset is similar resolution than destination.
	See comment at atGPSPosition:areaFilterMeters:
	"
	^self atLatitude: aGPSPosition latitude longitude: aGPSPosition longitude! !

!TiledWebmercatorSampledWorld methodsFor: 'accessing' stamp: 'jmv 8/17/2018 14:30:26'!
atGPSPosition: aGPSPosition sampleArea: pixelSizeOnGroundMeters
	"Area filtering is useful when source dataset is higher resolution than destination.
	In this cases, source includes high spatial frequency content that destination can't handle. Area filtering is a reasonable way to filter it out."

	^self atLatitude: aGPSPosition latitude longitude: aGPSPosition longitude sampleArea: pixelSizeOnGroundMeters! !

!TiledWebmercatorSampledWorld methodsFor: 'accessing' stamp: 'jmv 9/19/2018 15:58:54'!
atLatitude: lat longitude: lon
	"This method is to be used when source dataset is similar resolution than destination.
	See comment at atLatitude:longitude:areaFilterMeters:
	"
	| positionInMap tileAndPositionInTile tile posInTile |
	positionInMap _ projection mapPositionFromLat: lat long: lon.
	(positionInMap between: 0@0 and: 1@1)
		ifFalse: [ ^ self outOfWorldValue ].	"Invalid coodinates, or outside the area covered by Webmercator"
	tileAndPositionInTile _ positionInMap * (2 raisedToInteger: zoomLevel).
	tile _ self tileAt: tileAndPositionInTile truncated.
	posInTile _ (tileAndPositionInTile - tileAndPositionInTile truncated) * self samplesPerTileSide.
	^ self sampleTile: tile at: posInTile! !

!TiledWebmercatorSampledWorld methodsFor: 'accessing' stamp: 'jmv 8/17/2018 17:23:04'!
atLatitude: lat longitude: lon sampleArea: pixelSizeOnGroundMeters
	"Area filtering is useful when source dataset is higher resolution than destination.
	In this cases, source includes high spatial frequency content that destination can't handle. Area filtering is a reasonable way to filter it out."

	| positionInMap tileAndPositionInTile tile tileGSDmeters |
	positionInMap _ projection mapPositionFromLat: lat long: lon.
	(positionInMap between: 0@0 and: 1@1)
		ifFalse: [ ^ self outOfWorldValue ].	"Invalid coodinates, or outside the area covered by Webmercator"
	tileAndPositionInTile _ positionInMap * (2 raisedToInteger: zoomLevel).
	tile _ self tileAt: tileAndPositionInTile truncated.
	tileGSDmeters _ projection gsdAtLatitude: lat zoomLevel: zoomLevel.
	^ self
		sampleTile: tile
		at: (tileAndPositionInTile - tileAndPositionInTile truncated) * self samplesPerTileSide
		filterHalfExtent: (pixelSizeOnGroundMeters / tileGSDmeters / 2)! !

!TiledWebmercatorSampledWorld methodsFor: 'accessing' stamp: 'jmv 8/17/2018 14:40:01'!
tileAt: zeroBasedTilePosition

	| tileCoordinates tileData |
	^allTiles
		at: zeroBasedTilePosition
		ifAbsent: [
			cachedTiles size >= self cacheSize ifTrue: [
				"free oldest cache element.
				This is not the LRU element!!
				Freeing the LRU element requires moving the cached element to the first position if found. Worth it?"
				tileCoordinates _ cachedTiles removeLast.
				('Removed from RAM Cache: ', tileCoordinates printString) print.
				allTiles removeKey: tileCoordinates ].
			tileData _ self loadTileSamples: zeroBasedTilePosition.	"Might answer nil"
			tileData
				ifNil: [
					tileData _ self fetchTileSamples: zeroBasedTilePosition.
					tileData ifNotNil: [ ('Fetched and cached on Disk: ', zeroBasedTilePosition printString) print ] ].
			allTiles
				at: zeroBasedTilePosition
				put: (tileData
					ifNil: [ SampledTile null ]
					ifNotNil: [
						"Null tiles don't take space in the cache."
						('Cached in RAM: ', name, ' ', zeroBasedTilePosition printString) print.
						cachedTiles addFirst: zeroBasedTilePosition.
						SampledTile samples: tileData ]) ]! !

!TiledWebmercatorSampledWorld methodsFor: 'private' stamp: 'jmv 8/13/2018 16:38:19'!
fileIdFor: aPoint
	"Answer a string with an even number of hex digits"
	| hexDigits |
	hexDigits _ (zoomLevel / 4) ceiling.
	^ (aPoint x printStringBase: 16 length: hexDigits padded: true),
		(aPoint y printStringBase: 16 length: hexDigits padded: true)! !

!TiledWebmercatorSampledWorld methodsFor: 'private' stamp: 'jmv 8/13/2018 16:35:42'!
folderForId: hexFileId createifAbsent: aBoolean

	| folder |
	folder _ datasetPath asDirectoryEntry.
	aBoolean
		ifTrue: [ folder assureExistence ]
		ifFalse: [ folder exists ifFalse: [ ^ nil ]].
	2 to: hexFileId size-2 by: 2 do: [ :i |
		folder _ folder / (hexFileId copyFrom: 1 to: i).
		aBoolean
			ifTrue: [ folder assureExistence ]
			ifFalse: [ folder exists ifFalse: [ ^ nil ]]].
	^ folder! !

!TiledWebmercatorSampledWorld methodsFor: 'constants' stamp: 'jmv 8/17/2018 09:50:00'!
cacheSize

	^3000! !

!TiledWebmercatorSampledWorld methodsFor: 'constants' stamp: 'jmv 8/14/2018 11:30:16'!
outOfWorldValue
	"Value to answer for areas too close to poles, outside Webmercator"
	self subclassResponsibility! !

!TiledWebmercatorSampledWorld methodsFor: 'constants' stamp: 'jmv 8/13/2018 14:34:57'!
samplesPerTileSide
	^ self subclassResponsibility! !

!TiledWebmercatorSampledWorld class methodsFor: 'instance creation' stamp: 'jmv 8/13/2018 19:18:14'!
zoomLevel: anInteger
	instance ifNil: [ instance _ Dictionary new ].
	^ instance at: anInteger ifAbsentPut: [
		self basicNew initializeZoomLevel: anInteger ]! !

!TiledWebmercatorSampledWorld class methodsFor: 'singleton' stamp: 'jmv 8/17/2018 14:37:53'!
getInstance
	^ self error: 'Please call #zoomLevel:'! !

!GoogleSampledWorld methodsFor: 'initialization' stamp: 'jmv 8/13/2018 10:57:44'!
initializeZoomLevel: anInteger
	super initializeZoomLevel: anInteger.
	name _ 'Google', anInteger printString.
	self datasetPath: 'payload/datasets/', name! !

!GoogleSampledWorld methodsFor: 'examples' stamp: 'jmv 8/13/2018 13:31:07'!
demoResult: extent
	^ Form extent: extent depth: 32! !

!GoogleSampledWorld methodsFor: 'private' stamp: 'jmv 9/19/2018 16:25:58'!
sampleTile: tile at: zeroBasedPosition
	"zeroBasedPosition = 0@0 means topLeft, width@height means bottomRight
	This datasets considers pixels as areas, not samples.
	Samples is a Form, and therefore zero based.
	This means that:
		- topLeft pixel covers zeroBasedPosition values in 0@0 to 1@1. zeroBasedPosition = 0.5@0.5 hits pixel 0@0 exactly
		- bottomRight pixel covers zeroBasedPosition values in width@height-1 to width@height. zeroBasedPosition = width@height-0.5 hits pixel width@height-1 (bottomRight pixel) exactly
	"
	^ true
		ifTrue: [ self sampleTileInterpolated: tile at: zeroBasedPosition ]
		ifFalse: [ self sampleTileNearestNeighbor: tile at: zeroBasedPosition ]! !

!GoogleSampledWorld methodsFor: 'private' stamp: 'jmv 8/15/2018 10:07:32'!
sampleTile: tile at: zeroBasedPosition filterHalfExtent: aNumber
	"Call #sampleTile:at: when destination has similar resolution as source dataset.
	Call this method, for higher quality results, with a source dataset with higher resolution than destination."

	"zeroBasedPosition = 0@0 means topLeft, width@height means bottomRight
	This datasets considers pixels as areas, not samples.
	Samples is a Form, and therefore zero based.
	This means that:
		- topLeft pixel covers zeroBasedPosition values in 0@0 to 1@1. zeroBasedPosition = 0.5@0.5 hits pixel 0@0 exactly
		- bottomRight pixel covers zeroBasedPosition values in width@height-1 to width@height. zeroBasedPosition = width@height-0.5 hits pixel width@height-1 (bottomRight pixel) exactly
	Additionally, 0@0 is a valid zeroBasedPosition. Make the valid interval for zeroBasedPosition be [0.0 .. width) x [0.0 .. height) and not (0.0 .. width) x (0.0 .. height).
	This is, make the start of the interval closed. Make 0@0 a valid coordinate.
	"
	| x y |
	x _ zeroBasedPosition x.
	x _ x isZero ifTrue: [ 0 ] ifFalse: [ (x - 0.5) rounded ].
	y _ zeroBasedPosition y.
	y _ y isZero ifTrue: [ 0 ] ifFalse: [ (y - 0.5) rounded ].
	^tile samples areaFilteredColorAt: x@y halfWidth: aNumber halfHeight: aNumber

	"See comment on color interpolation at TrueMarble>>#sampleTile:at:"! !

!GoogleSampledWorld methodsFor: 'private' stamp: 'jmv 9/19/2018 16:25:35'!
sampleTileInterpolated: tile at: zeroBasedPosition
	"zeroBasedPosition = 0@0 means topLeft, width@height means bottomRight
	This datasets considers pixels as areas, not samples.
	Samples is a Form, and therefore zero based.
	This means that:
		- topLeft pixel covers zeroBasedPosition values in 0@0 to 1@1. zeroBasedPosition = 0.5@0.5 hits pixel 0@0 exactly
		- bottomRight pixel covers zeroBasedPosition values in width@height-1 to width@height. zeroBasedPosition = width@height-0.5 hits pixel width@height-1 (bottomRight pixel) exactly
	
	But we can not interpolate before the first sample or after the last sample. Unfortunately this dataset doesn't include extra row and column (copied from next tile) to allow for this, like SRTM and derivatives do.
	So, zeroBasedPositionis in [0.0 .. width) x [0.0 .. height) but interpolation is only correct in [0.5 .. width-0.5] x [0.5 .. height-0.5]
	Slight artifacts will occur at the edges of tiles, when jumping from one tile to another.
	"
	"See comment on color interpolation at TrueMarble>>#sampleTile:at:"
	| px py |
	px _ zeroBasedPosition x - 0.5.
	py _ zeroBasedPosition y - 0.5.
	px _ px min: tile samples width-1 max: 0.
	py _ py min: tile samples height-1 max: 0.
	^ tile samples colorInterpolatedAt: px @ py! !

!GoogleSampledWorld methodsFor: 'private' stamp: 'jmv 9/19/2018 16:02:48'!
sampleTileNearestNeighbor: tile at: zeroBasedPosition
	"zeroBasedPosition = 0@0 means topLeft, width@height means bottomRight
	This datasets considers pixels as areas, not samples.
	Samples is a Form, and therefore zero based.
	This means that:
		- topLeft pixel covers zeroBasedPosition values in 0@0 to 1@1. zeroBasedPosition = 0.5@0.5 hits pixel 0@0 exactly
		- bottomRight pixel covers zeroBasedPosition values in width@height-1 to width@height. zeroBasedPosition = width@height-0.5 hits pixel width@height-1 (bottomRight pixel) exactly
	Additionally, 0@0 is a valid zeroBasedPosition. Make the valid interval for zeroBasedPosition be [0.0 .. width) x [0.0 .. height) and not (0.0 .. width) x (0.0 .. height).
	This is, make the start of the interval closed. Make 0@0 a valid coordinate.
	"
	| x y |
	x _ zeroBasedPosition x.
	x _ x isZero ifTrue: [ 0 ] ifFalse: [ (x - 0.5) rounded ].
	y _ zeroBasedPosition y.
	y _ y isZero ifTrue: [ 0 ] ifFalse: [ (y - 0.5) rounded ].
	^tile samples colorAt: x@y

	"See comment on color interpolation at TrueMarble>>#sampleTile:at:"! !

!GoogleSampledWorld methodsFor: 'constants' stamp: 'jmv 8/14/2018 11:30:24'!
outOfWorldValue
	"Value to answer for areas too close to poles, outside Webmercator"
	^ Color black! !

!GoogleSampledWorld methodsFor: 'constants' stamp: 'jmv 8/13/2018 14:35:07'!
samplesPerTileSide
	^ 256! !

!GoogleSampledWorld methodsFor: 'accessing' stamp: 'jmv 9/21/2018 11:59:17'!
fetchTileSamples: aPoint
	| answer bytes form hexFileId folder |
	answer _ WebClient httpGet: 'http://mt1.google.com/vt/lyrs=s&x=', aPoint x printString, '&y=', aPoint y printString, '&z=', zoomLevel printString.
	bytes _ answer content asByteArray.
	form _ JPEGReadWriter2 new uncompress: bytes into: nil.
	
	"If bad data, retry once"
	form boundingBox area isZero ifTrue: [
		answer _ WebClient httpGet: 'http://mt1.google.com/vt/lyrs=s&x=', aPoint x printString, '&y=', aPoint y printString, '&z=', zoomLevel printString.
		bytes _ answer content asByteArray.
		form _ JPEGReadWriter2 new uncompress: bytes into: nil.
	].
	"Do nothing if bad data. Answer nil, don't save to disk"
	form boundingBox area isZero ifTrue: [ ^ nil ].
	
	hexFileId _ self fileIdFor: aPoint.
	folder _ self folderForId: hexFileId createifAbsent: true.
	folder // (hexFileId, name, '.jpg') forceWriteStreamDo: [ :strm |
		strm binary; nextPutAll: bytes ].
	^ self filter: form! !

!GoogleSampledWorld methodsFor: 'accessing' stamp: 'jmv 9/19/2018 15:42:44'!
filter: form
	| blue green lopass red |
	^loPassSigma
		ifNil: [ form ]
		ifNotNil: [
			lopass _ ConvolutionKernel gaussian: loPassSigma.
			red _ FloatImage from32BitColorForm: form component: #red.
			green _ FloatImage from32BitColorForm: form component: #green.
			blue _ FloatImage from32BitColorForm: form component: #blue.
			red _ red convolutionWith: lopass.
			green _ green convolutionWith: lopass.
			blue _ blue convolutionWith: lopass.
			red asFormG: green b: blue ]! !

!GoogleSampledWorld methodsFor: 'accessing' stamp: 'jmv 9/18/2018 17:37:42'!
loPassSigma: sigma
	loPassSigma _ sigma! !

!GoogleSampledWorld methodsFor: 'accessing' stamp: 'jmv 9/19/2018 15:43:19'!
loadTileSamples: aPoint
	| hexFileId file |
	hexFileId _ self fileIdFor: aPoint.
	(self folderForId: hexFileId createifAbsent: false) ifNotNil: [ :folder |
		file _ folder // (hexFileId, name, '.jpg').
		file exists ifTrue: [ ^ [self filter: file formContents] on: Error do: [nil] ]].
	^ nil! !

!GoogleSampledWorld class methodsFor: 'examples' stamp: 'jmv 8/17/2018 17:21:54'!
example: aString  lat0: lat0 long0: long0 pixelExtent: pixelExtent gsdArcSeconds: gsdArcSeconds
"
	GoogleSampledWorld 
		example: 'VicenteLopez'
		lat0: -34.524
		long0: -58.469
		pixelExtent:  800@800
		gsdArcSeconds: 0.03
"
	| gsdDegrees gsdDegreesAtEquatorScaleZero zoom |
	gsdDegrees _ gsdArcSeconds / 3600.
	gsdDegreesAtEquatorScaleZero _ 360.0 / 256.
	"If we are far enough from equator, we can do with a smaller zoom factor.
	Real applications (this method is just a demo, and we don't care about some overkill here)
	should call #minZoomLevelForGsd:latitude: with the latitud closest to equator they will need."
	zoom _ (gsdDegreesAtEquatorScaleZero / gsdDegrees) log2 ceiling.
	^ (self zoomLevel: zoom)
		example: aString
		lat0: lat0
		long0: long0
		pixelExtent: pixelExtent
		gsdArcSeconds: gsdArcSeconds! !
