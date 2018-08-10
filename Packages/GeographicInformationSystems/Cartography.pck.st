'From Cuis 5.0 of 7 November 2016 [latest update: #3401] on 9 August 2018 at 6:16:32 pm'!
'Description Please enter a description for this package'!
!provides: 'Cartography' 1 12!
!requires: 'Math 3D' 1 4 nil!
!requires: 'Graphics-Files-Additional' 1 4 nil!
SystemOrganization addCategory: #Cartography!


!classDefinition: #GPSPosition category: #Cartography!
Float64Array variableWordSubclass: #GPSPosition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cartography'!
!classDefinition: 'GPSPosition class' category: #Cartography!
GPSPosition class
	instanceVariableNames: ''!

!classDefinition: #CartographicProjection category: #Cartography!
Object subclass: #CartographicProjection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cartography'!
!classDefinition: 'CartographicProjection class' category: #Cartography!
CartographicProjection class
	instanceVariableNames: ''!

!classDefinition: #MercatorProjection category: #Cartography!
CartographicProjection subclass: #MercatorProjection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cartography'!
!classDefinition: 'MercatorProjection class' category: #Cartography!
MercatorProjection class
	instanceVariableNames: ''!

!classDefinition: #PseudoMercatorProjection category: #Cartography!
CartographicProjection subclass: #PseudoMercatorProjection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cartography'!
!classDefinition: 'PseudoMercatorProjection class' category: #Cartography!
PseudoMercatorProjection class
	instanceVariableNames: ''!


!GPSPosition commentStamp: 'jmv 3/2/2015 13:34' prior: 0!
GPS like position: Latitude, Longitude, Altitude (or Height or Elevation). Reference is the WGS 84 ellipsoid.
Latitude is geodetic, in degrees. Positive values are north the equator, negative values are south.
Longitude is in degrees, 0.0 is close to Greenwich. Positive values are east of Greenwich, negative values are west.
Altitude is in meters. 0.0 is the WGS 84 ellipsoid.!

!CartographicProjection commentStamp: '<historical>' prior: 0!
Provides services for converting (x, y) coordinates on a map to and from geographic / geodetic latitude and longitude.!

!MercatorProjection commentStamp: 'jmv 3/17/2015 11:05' prior: 0!
See http://en.wikipedia.org/wiki/Mercator_projection
See "Maps projections, a working manual", p.37

Mercator projection is:
- Cylindrical.
- Conformal.
- Meridians are equally spaced straight lines.
- Parallels are unequally spaced straight lines, closest near the Equator, cutting meridians at right angles.
- Scale is true along the Equator, or along two parallels equidistant from the Equator. (jmv: or along any single parallel)
- Loxodromes (rhumb lines) are straight lines.
- Not perspective.
- Poles are at infinity; great distortion of area in polar regions.
- Used for navigation.
- Presented by Mercator in 1569.
- (jmv: preserves areas, shapes and scales in small areas)

Mercator projection is centered at the Equator and meridian 0 (Greenwich).!

!PseudoMercatorProjection commentStamp: '<historical>' prior: 0!
As in http://en.wikipedia.org/wiki/Web_Mercator
Used by http://www.openstreetmap.org , http://maps.google.com , etc
Uses elliptic (geodetic) latitude and longitude for, but projects them using a spherical model of Earth.
In different words, we take geodetic (elliptical Earth) (latitude, longitude), map them to geographic (latitude, longitude) on a sphere, and do a Mercator projection of the sphere. As there is a slight distorion going from the ellipsoid to the sphere, the PseudoMercatorProjection is not conformal, but almost.

| tm ww hh e pseudoMercator proj ll xInTM yInTM |
tm _ Form fromFileNamed: './Datasets/CleanTOPO2/NaturalEarthPlusBathymetry/NE_Drape-3000x1500.tif'.
ww _ tm width.
hh _ tm height.
e _ 900.
pseudoMercator _ Form extent: e@e depth: 32.
proj _ PseudoMercatorProjection new.
0 to: pseudoMercator height by: 1 do: [ :y | 0 to: pseudoMercator width by: 1 do: [ :x |
		ll _ proj xyToLL: x@y / e.
		xInTM _ ll lambda + 180 / 360 * ww.
		yInTM _ 90 - ll phi / 180 * hh.
		pseudoMercator colorAt: x@y+1 put: (tm colorAt:xInTM@yInTM+1)]].
pseudoMercator display. Display forceToScreen!

!GPSPosition methodsFor: 'accessing' stamp: 'jmv 3/3/2015 11:21'!
altitude
	"Altitude is in meters. 0.0 is the WGS 84 ellipsoid."
	^ self at: 3! !

!GPSPosition methodsFor: 'accessing' stamp: 'jmv 3/2/2015 13:34'!
height
	"Altitude is in meters. 0.0 is the WGS 84 ellipsoid."
	^ self at: 3! !

!GPSPosition methodsFor: 'accessing' stamp: 'jmv 3/2/2015 14:00'!
lambda
	"Longitude is in degrees, 0.0 is close to Greenwich. Positive values are east of Greenwich, negative values are west."
	^ self at: 2! !

!GPSPosition methodsFor: 'accessing' stamp: 'jmv 3/2/2015 13:34'!
latitude
	"Latitude is geodetic, in degrees. Positive values are north the equator, negative values are south."
	^ self at: 1! !

!GPSPosition methodsFor: 'accessing' stamp: 'jmv 3/2/2015 15:19'!
longitude
	"Longitude is in degrees, 0.0 is close to Greenwich. Positive values are east of Greenwich, negative values are west.
	Greenwich observatory is about 100 metres away from 0.0. 
	See http://www.thegreenwichmeridian.org/tgm/articles.php?article=7"
	^ self at: 2! !

!GPSPosition methodsFor: 'accessing' stamp: 'jmv 3/2/2015 14:00'!
phi
	"Latitude is geodetic, in degrees. Positive values are north the equator, negative values are south."
	^ self at: 1! !

!GPSPosition methodsFor: 'converting' stamp: 'jmv 9/18/2015 17:06'!
asECEF
	"Convert from geodetic coordinates to ECEF(also called ECF, called ECR by Landsat docs) Cartesian Coordinates.
	Answer is in meters
	http://en.wikipedia.org/wiki/Geodetic_datum#From_geodetic_to_ECEF
	https://microem.ru/files/2012/08/GPS.G1-X-00006.pdf
	http://earth-info.nga.mil/GandG/publications/tr8350.2/tr8350_2.htmlhttp://www.nalresearch.com/files/Standard%20Modems/A3LA-XG/A3LA-XG%20SW%20Version%201.0.0/GPS%20Technical%20Documents/GPS.G1-X-00006%20(Datum%20Transformations).pdf
	http://www.sage.unsw.edu.au/snap/gps/clynch_pdfs/coordcvt.pdf

	(GPSPosition latitude: -34.6 longitude: -58.4 altitude: 25) asECEF 
	Chequear con: http://www.oc.nps.edu/oc2902w/coord/llhxyz.htm o http://www.sysense.com/products/ecef_lla_converter/index.html
	
	(GPSPosition latitude: 45.19242321643 longitude: 0.0 altitude: 0.0) asECEF
	(GPSPosition latitude: 45.19242321643 longitude: 0.0 altitude: 0.0) asECEFSpherical

	(GPSPosition latitude: 45.0 longitude: 0.0 altitude: 0.0) asECEF
	(GPSPosition latitude: 45.0 longitude: 0.0 altitude: 0.0) asECEFSpherical
	"
	
	| h n x y z lambda phi a b |
	a _ GPSPosition wgs84SemiMajorAxis.
	b _ GPSPosition wgs84SemiMinorAxis.
	n _ GPSPosition normalAtLatitude: self phi.
	phi _ self phi degreesToRadians.
	lambda _ self lambda degreesToRadians.
	h _ self height.
	x _ (n + h) * phi cos * lambda cos.
	y _ (n + h) * phi cos * lambda sin.
	z _ (b squared / a squared * n + h) * phi sin.
	^ Float64Vector3 x: x y: y z: z! !

!GPSPosition methodsFor: 'converting' stamp: 'jmv 9/8/2015 09:35'!
asECEFSpherical
	"como asECEF, pero todo esferico.
	O sea, la latitud es geocentrica, no geodetica.
	En general, NO USAR. Casi nunca tiene sentido!!"
	
	| magnitude latitude longitude parallelCircleRadious x y z |
	magnitude _ GPSPosition wgs84SemiMajorAxis + self height.
	latitude _ self phi degreesToRadians.
	longitude _ self lambda degreesToRadians.
	parallelCircleRadious _ magnitude * latitude cos.
	x _ parallelCircleRadious * longitude cos.
	y _ parallelCircleRadious * longitude sin.
	z _ magnitude * latitude sin.
	^ Float64Vector3 x: x y: y z: z! !

!GPSPosition methodsFor: 'converting' stamp: 'jmv 9/30/2015 12:34'!
withHeight: h
	"
	Answer a new GPSPosition, with same latitude and longitude, and with requested height above the ellipsoid.
	(GPSPosition fromECEF: (GPSPosition latitude: 65 longitude: -151 altitude: 100) asECEF) withHeight: 200
	"
	^ self copy at: 3 put: h; yourself! !

!GPSPosition methodsFor: 'converting' stamp: 'jmv 9/30/2015 12:34'!
withZeroHeight
	"
	Answer a new GPSPosition, with same latitude and longitude, and with requested height above the ellipsoid.
	(GPSPosition fromECEF: (GPSPosition latitude: 65 longitude: -151 altitude: 100) asECEF) withZeroHeight
	"
	^ self withHeight: 0.0! !

!GPSPosition methodsFor: 'printing' stamp: 'jmv 12/2/2016 11:32:28'!
shortPrintString
	^String streamContents: [ :strm |
		strm nextPut: $(.
		self latitude printOn: strm fractionDigits: 5.
		strm nextPutAll: ','.
		self longitude printOn: strm fractionDigits: 5.
		strm nextPut: $) ]! !

!GPSPosition class methodsFor: 'instance creation' stamp: 'jmv 9/30/2015 09:01'!
fromECEF: aPoint3D
	"
	Por qué todo el mundo habla de cálculos iterativos? es que el cálculo directo no es tan bueno por algún motivo???
	
	https://microem.ru/files/2012/08/GPS.G1-X-00006.pdf
	http://www.nalresearch.com/files/Standard%20Modems/A3LA-XG/A3LA-XG%20SW%20Version%201.0.0/GPS%20Technical%20Documents/GPS.G1-X-00006%20(Datum%20Transformations).pdf
	http://www.sage.unsw.edu.au/snap/gps/clynch_pdfs/coordcvt.pdf
	http://www.satsleuth.com/GPS_ECEF_Datum_transformation.htm

	GPSPosition fromECEF: (GPSPosition latitude: -34.6 longitude: -58.4 altitude: 25) asECEF
	Check against: http://www.oc.nps.edu/oc2902w/coord/llhxyz.htm o http://www.sysense.com/products/ecef_lla_converter/index.html
	
	GPSPosition fromECEF: (GPSPosition latitude: 65 longitude: -151 altitude: 0) asECEF.
	GPSPosition fromECEF: (GPSPosition latitude: 65 longitude: 151 altitude: 0) asECEF.
	GPSPosition fromECEF: (GPSPosition latitude: -65 longitude: 151 altitude: 0) asECEF.
	GPSPosition fromECEF: 0 @ 0 @ 6356752.31.
	GPSPosition fromECEF: 1 @ 0 @ 6356752.31.
	GPSPosition fromECEF: 0 @ 0 @ 0.
	GPSPosition fromECEF: 1 @ 0 @ 0.
	"

	| h lambda phi a b distanceToZAxis theta ePrimeSquared eSquared normalDistanceToZ |
	distanceToZAxis _ (aPoint3D x squared + aPoint3D y squared) sqrt.
	b _ self wgs84SemiMinorAxis.
	distanceToZAxis > 0.0
		ifTrue: [		"aPoint3D is not along the Earth rotation axis. Not at the North or South pole."
			lambda _ aPoint3D y arcTan: aPoint3D x.
			a _ self wgs84SemiMajorAxis.
			theta _ (aPoint3D z * a / (distanceToZAxis * b)) arcTan.
			eSquared _ self wgs84FirstEccentricitySquared.
			ePrimeSquared _ self wgs84SecondEccentricitySquared.
			phi _ ((aPoint3D z + (ePrimeSquared * b * theta sin cubed)) / ( distanceToZAxis - (eSquared * a * theta cos cubed))) arcTan.
			normalDistanceToZ _ GPSPosition normalAtLatitude: phi radiansToDegrees.
			h _ distanceToZAxis / phi cos - normalDistanceToZ ]
		ifFalse: [ 	"aPoint3D IS along the Earth rotation axis. We are at North or South pole (at some positive or negative altitude wrt. the WGS84 ellipsoid)."
			lambda _ 0.0.
			phi _ aPoint3D z sign * Float halfPi.
			h _ aPoint3D z abs - b ].
	^GPSPosition latitude: phi radiansToDegrees longitude: lambda radiansToDegrees altitude: h! !

!GPSPosition class methodsFor: 'instance creation' stamp: 'jmv 1/22/2016 11:46'!
fromECEFSpherical: aPoint3D
	"como fromECEF: pero todo esferico.
	O sea, la latitud es geocentrica, no geodetica.
	En general, NO USAR. Casi nunca tiene sentido!!"

	| distanceToZAxis h latitude longitude |
	distanceToZAxis _ (aPoint3D x squared + aPoint3D y squared) sqrt.
	latitude _ aPoint3D z arcTan: distanceToZAxis.
	longitude _ aPoint3D y arcTan: aPoint3D x.
	h _ aPoint3D length - self wgs84SemiMajorAxis.
	^GPSPosition latitude: latitude radiansToDegrees longitude: longitude radiansToDegrees altitude: h! !

!GPSPosition class methodsFor: 'instance creation' stamp: 'jmv 3/3/2015 16:16'!
latitude: phi longitude: lambda altitude: h
	"
	GPSPosition latitude: -34.6 longitude: -58.4 altitude: 25
	" 
	^self new
		at: 1 put: phi;
		at: 2 put: lambda;
		at: 3 put: h;
		yourself! !

!GPSPosition class methodsFor: 'instance creation' stamp: 'jmv 8/9/2018 16:51:22'!
latitudeDegrees: latDeg minutes: latMin seconds: latSec longitudeDegrees: lonDeg minutes: lonMin seconds: lonSec

	^ self
		latitude: latSec/60.0+latMin / 60.0 + latDeg
		longitude: lonSec/60.0+lonMin / 60.0 + lonDeg
		altitude: 0.0! !

!GPSPosition class methodsFor: 'instance creation' stamp: 'jmv 4/7/2015 15:04'!
longitude: lambda latitude: phi altitude: h
	^ self latitude: phi longitude: lambda altitude: h! !

!GPSPosition class methodsFor: 'instance creation' stamp: 'jmv 3/2/2015 13:21'!
numElements
	^3! !

!GPSPosition class methodsFor: 'aux functions' stamp: 'jmv 3/2/2015 13:57'!
normalAtLatitude: phi
	"Distance from the surface to the Z-axis along the ellipsoid normal
	http://en.wikipedia.org/wiki/Geodetic_datum
	GPSPosition earthRadiousAtLatitude: 90 6.39959362575849e6
	GPSPosition earthRadiousAtLatitude: 0 6.378137e6
	" 
	| a eSquared |
	a _ self wgs84SemiMajorAxis.
	eSquared _ self wgs84FirstEccentricitySquared.
	^ a / (1 - (eSquared * phi degreesToRadians sin squared)) sqrt! !

!GPSPosition class methodsFor: 'constants' stamp: 'jmv 3/2/2015 15:58'!
wgs84FirstEccentricitySquared
	"
	GPSPosition wgs84FirstEccentricitySquared
	http://earth-info.nga.mil/GandG/publications/tr8350.2/tr8350_2.htmlhttp://www.nalresearch.com/files/Standard%20Modems/A3LA-XG/A3LA-XG%20SW%20Version%201.0.0/GPS%20Technical%20Documents/GPS.G1-X-00006%20(Datum%20Transformations).pdf
	http://www.satsleuth.com/GPS_ECEF_Datum_transformation.htm
	http://en.wikipedia.org/wiki/Geodetic_datum
	http://earth-info.nga.mil/GandG/publications/tr8350.2/tr8350_2.html
	called e^2
	"
	"^ 6.69437999014e-3"
	| a b |
	a _ self wgs84SemiMajorAxis.
	b _ self wgs84SemiMinorAxis.
	^a squared - b squared / a squared! !

!GPSPosition class methodsFor: 'constants' stamp: 'jmv 3/2/2015 15:57'!
wgs84RecipfocalOfFlattening
	"http://en.wikipedia.org/wiki/Geodetic_datum
	http://earth-info.nga.mil/GandG/publications/tr8350.2/tr8350_2.htmlhttp://www.nalresearch.com/files/Standard%20Modems/A3LA-XG/A3LA-XG%20SW%20Version%201.0.0/GPS%20Technical%20Documents/GPS.G1-X-00006%20(Datum%20Transformations).pdf
	called 1/f
	"
	^ 298.257223563! !

!GPSPosition class methodsFor: 'constants' stamp: 'jmv 3/2/2015 15:57'!
wgs84SecondEccentricitySquared
	"
	GPSPosition wgs84SecondEccentricitySquared
	http://www.satsleuth.com/GPS_ECEF_Datum_transformation.htm
	http://www.nalresearch.com/files/Standard%20Modems/A3LA-XG/A3LA-XG%20SW%20Version%201.0.0/GPS%20Technical%20Documents/GPS.G1-X-00006%20(Datum%20Transformations).pdf
	http://en.wikipedia.org/wiki/Geodetic_datum
	http://earth-info.nga.mil/GandG/publications/tr8350.2/tr8350_2.html
	called e'^2
	"
	"^ 6.73949674228e-3"
	| a b |
	a _ self wgs84SemiMajorAxis.
	b _ self wgs84SemiMinorAxis.
	^a squared - b squared / b squared! !

!GPSPosition class methodsFor: 'constants' stamp: 'jmv 3/2/2015 15:57'!
wgs84SemiMajorAxis
	"
	http://www.satsleuth.com/GPS_ECEF_Datum_transformation.htm
	http://www.nalresearch.com/files/Standard%20Modems/A3LA-XG/A3LA-XG%20SW%20Version%201.0.0/GPS%20Technical%20Documents/GPS.G1-X-00006%20(Datum%20Transformations).pdf
	http://en.wikipedia.org/wiki/Geodetic_datum
	http://earth-info.nga.mil/GandG/publications/tr8350.2/tr8350_2.html
	called a
	In metres
	"
	^ 6378137.0 "m"! !

!GPSPosition class methodsFor: 'constants' stamp: 'jmv 3/2/2015 15:57'!
wgs84SemiMinorAxis
	"
	http://www.satsleuth.com/GPS_ECEF_Datum_transformation.htm
	http://www.nalresearch.com/files/Standard%20Modems/A3LA-XG/A3LA-XG%20SW%20Version%201.0.0/GPS%20Technical%20Documents/GPS.G1-X-00006%20(Datum%20Transformations).pdf
	http://en.wikipedia.org/wiki/Geodetic_datum
	http://earth-info.nga.mil/GandG/publications/tr8350.2/tr8350_2.html
	called b
	In metres
	"
	^ 6356752.3142 "m"! !

!GPSPosition class methodsFor: 'constants' stamp: 'jmv 3/2/2015 15:57'!
wgsFlattening
	"
	GPSPosition wgsFlattening
	http://www.satsleuth.com/GPS_ECEF_Datum_transformation.htm
	http://www.nalresearch.com/files/Standard%20Modems/A3LA-XG/A3LA-XG%20SW%20Version%201.0.0/GPS%20Technical%20Documents/GPS.G1-X-00006%20(Datum%20Transformations).pdf
	"
	| a b |
	a _ self wgs84SemiMajorAxis.
	b _ self wgs84SemiMinorAxis.
	^a-b / a! !

!PseudoMercatorProjection methodsFor: 'conversion' stamp: 'jmv 3/17/2015 14:25'!
llToXY: aGPSPosition
	"
	x and y go from 0.0 to 1.0
	PseudoMercatorProjection new llToXY: (GPSPosition latitude: -34.6 longitude: -58.4 altitude: 25)
	PseudoMercatorProjection new llToXY: (GPSPosition latitude: 0.0 longitude: 0.0 altitude: 0.0)
	PseudoMercatorProjection new llToXY: (GPSPosition latitude: 0.0 longitude: -180.0 altitude: 0.0)
	PseudoMercatorProjection new llToXY: (GPSPosition latitude: 0.0 longitude: 180.0 altitude: 0.0)
	PseudoMercatorProjection new llToXY: (GPSPosition latitude: 85.051129 longitude: 0.0 altitude: 0.0)
	PseudoMercatorProjection new llToXY: (GPSPosition latitude: -85.051129 longitude: 0.0 altitude: 0.0)
	PseudoMercatorProjection new llToXY: (GPSPosition latitude: 66.51326 longitude: 0.0 altitude: 0.0)
	PseudoMercatorProjection new llToXY: (GPSPosition latitude: -66.51326 longitude: 0.0 altitude: 0.0)
	" 
	| x y pi |
	pi _ Float pi.
	x _ 0.5 / pi * (aGPSPosition lambda degreesToRadians + pi).
	y _ 0.5 / pi * ( pi - (pi / 4 + (aGPSPosition phi degreesToRadians/2.0)) tan ln ).
	^ x@y! !

!PseudoMercatorProjection methodsFor: 'conversion' stamp: 'jmv 3/17/2015 14:26'!
xyToLL: aPoint
	"
	x and y go from 0.0 to 1.0
	PseudoMercatorProjection new xyToLL:(PseudoMercatorProjection new llToXY: (GPSPosition latitude: -34.6 longitude: -58.4 altitude: 25))
	PseudoMercatorProjection new xyToLL:(PseudoMercatorProjection new llToXY: (GPSPosition latitude: 0.0 longitude: 0.0 altitude: 0.0))
	PseudoMercatorProjection new xyToLL:(PseudoMercatorProjection new llToXY: (GPSPosition latitude: 0.0 longitude: -180.0 altitude: 0.0))
	PseudoMercatorProjection new xyToLL:(PseudoMercatorProjection new llToXY: (GPSPosition latitude: 0.0 longitude: 180.0 altitude: 0.0))
	PseudoMercatorProjection new xyToLL:(PseudoMercatorProjection new llToXY: (GPSPosition latitude: 85.051129 longitude: 0.0 altitude: 0.0))
	PseudoMercatorProjection new xyToLL:(PseudoMercatorProjection new llToXY: (GPSPosition latitude: -85.051129 longitude: 0.0 altitude: 0.0))
	PseudoMercatorProjection new xyToLL:(PseudoMercatorProjection new llToXY: (GPSPosition latitude: 66.51326 longitude: 0.0 altitude: 0.0))
	PseudoMercatorProjection new xyToLL:(PseudoMercatorProjection new llToXY: (GPSPosition latitude: -66.51326 longitude: 0.0 altitude: 0.0))
	"
	
	| lambda phi |
	lambda _ aPoint x  * 2 * Float pi - Float pi.
	phi _ ((Float e raisedTo: (Float pi - (2 * Float pi * aPoint y))) arcTan - (Float pi / 4)) * 2.
	^ GPSPosition latitude: phi radiansToDegrees longitude: lambda radiansToDegrees altitude: 0.! !
