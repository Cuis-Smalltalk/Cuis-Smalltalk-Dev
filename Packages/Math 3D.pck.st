'From Cuis 4.2 of 25 July 2013 [latest update: #2968] on 4 November 2016 at 10:07:46 am'!
'Description Stuff from Balloon3D-Math by Andreas Raab. Adapted to use 64-bit Float arithmetic (in addition to 32-bit FloatArray).'!
!provides: 'Math 3D' 1 19!
!classDefinition: #Float64Color4 category: #'Math 3D'!
Float64Array variableWordSubclass: #Float64Color4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Math 3D'!
!classDefinition: 'Float64Color4 class' category: #'Math 3D'!
Float64Color4 class
	instanceVariableNames: ''!

!classDefinition: #Float64Matrix3x3 category: #'Math 3D'!
Float64Array variableWordSubclass: #Float64Matrix3x3
	instanceVariableNames: ''
	classVariableNames: 'Float64Matrix3x3Identity Float64Matrix3x3Zero'
	poolDictionaries: ''
	category: 'Math 3D'!
!classDefinition: 'Float64Matrix3x3 class' category: #'Math 3D'!
Float64Matrix3x3 class
	instanceVariableNames: ''!

!classDefinition: #Float64Matrix4x4 category: #'Math 3D'!
Float64Array variableWordSubclass: #Float64Matrix4x4
	instanceVariableNames: ''
	classVariableNames: 'Float64Matrix4x4Identity Float64Matrix4x4Zero'
	poolDictionaries: ''
	category: 'Math 3D'!
!classDefinition: 'Float64Matrix4x4 class' category: #'Math 3D'!
Float64Matrix4x4 class
	instanceVariableNames: ''!

!classDefinition: #Float64Quaternion category: #'Math 3D'!
Float64Array variableWordSubclass: #Float64Quaternion
	instanceVariableNames: ''
	classVariableNames: 'Float64IdentityRotation Identity'
	poolDictionaries: ''
	category: 'Math 3D'!
!classDefinition: 'Float64Quaternion class' category: #'Math 3D'!
Float64Quaternion class
	instanceVariableNames: ''!

!classDefinition: #Float64Vector2 category: #'Math 3D'!
Float64Array variableWordSubclass: #Float64Vector2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Math 3D'!
!classDefinition: 'Float64Vector2 class' category: #'Math 3D'!
Float64Vector2 class
	instanceVariableNames: ''!

!classDefinition: #Float64Vector3 category: #'Math 3D'!
Float64Array variableWordSubclass: #Float64Vector3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Math 3D'!
!classDefinition: 'Float64Vector3 class' category: #'Math 3D'!
Float64Vector3 class
	instanceVariableNames: ''!

!classDefinition: #Float64Vector4 category: #'Math 3D'!
Float64Array variableWordSubclass: #Float64Vector4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Math 3D'!
!classDefinition: 'Float64Vector4 class' category: #'Math 3D'!
Float64Vector4 class
	instanceVariableNames: ''!

!classDefinition: #Float32Matrix3x3 category: #'Math 3D'!
FloatArray variableWordSubclass: #Float32Matrix3x3
	instanceVariableNames: ''
	classVariableNames: 'Float32Matrix3x3Identity Float32Matrix3x3Zero'
	poolDictionaries: ''
	category: 'Math 3D'!
!classDefinition: 'Float32Matrix3x3 class' category: #'Math 3D'!
Float32Matrix3x3 class
	instanceVariableNames: ''!

!classDefinition: #Float32Vector3 category: #'Math 3D'!
FloatArray variableWordSubclass: #Float32Vector3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Math 3D'!
!classDefinition: 'Float32Vector3 class' category: #'Math 3D'!
Float32Vector3 class
	instanceVariableNames: ''!


!Float64Color4 commentStamp: '<historical>' prior: 0!
I represent an RGBA color value in floating point format. I am used during the lighting and shading computations.
A copy of B3DColor4 (from Balloon3D-Math), but with 64 bit Float elements (instead of 32 bit Float).!

!Float64Matrix3x3 commentStamp: '<historical>' prior: 0!
I represent a 3x3 matrix for linear transformations (rotation and scale, maybe also skew) commonly used in 3D computer graphics.
Based on Float64Matrix4x4, but without a displacement.
Currently pretty bare bones. If more stuff from FloatMatrix4x4 is needed, consider making FloatMatrix4x4 a subclass of us, or maybe add a common subclass, to avoid code duplication.!

!Float64Matrix4x4 commentStamp: '<historical>' prior: 0!
I represent a general 4x4 transformation matrix commonly used in computer graphics.
A copy of B3DMatrix4x4 (from Balloon3D-Math), but with 64 bit Float elements (instead of 32 bit Float).!

!Float64Quaternion commentStamp: 'jmv 10/11/2016 10:41:38' prior: 0!
I represent general 3d rotations by using Unit-Quaternions. Unit-Quaternions are one of the best available representation for rotations in computer graphics because they provide an easy way of doing arithmetic with them and also because they allow us to use spherical linear interpolation (so-called "slerps") of rotations.

Indexed Variables:
	a	<Float>	the real part of the quaternion. Scalar component, related to angle of rotation. Also called q0 or q4.
	b	<Float>	the first imaginary part of the quaternion. Related to X coordinate of the rotation axis. Also called q1.
	c	<Float>	the second imaginary part of the quaternion. Related to Y coordinate of the rotation axis. Also called q2.
	d	<Float>	the third imaginary part of the quaternion. Related to Z coordinate of the rotation axis. Also called q3.

Started as a copy of B3DRotation (from Balloon3D-Math), but with 64 bit Float elements (instead of 32 bit Float).

See #a:b:c:d: , #q1:q2:q3:q4: and #x:y:z:a:
See http://bicycle.tudelft.nl/schwab/Publications/quaternion.pdf
See http://tutis.ca/Rotate/7quaternions.htm

Quaternions are used to state the attitude of an aircraft or spaceship with respect to a reference frame.

The reference frame might be ECEF cartesian coordinates (origin at Earth center, +x towards 0° lat, 0° lon, +y towards 0° lat, 90° lon east, +z towards North pole.
The "Body" coordinates have origin at center of mass of aircraft, +x points forward (nose of the plane), +y points to the right, +z points downwards.

The attitude is usually specified as "Inertial to Body" or "ECEF to Body" meaning a quaterion that if applied to the ECEF cartesian axes the result is the Body axes expressed in ECEF coordinates.

If what we want is to convert a direction or point in ECEF coordinates to Body coordinates, then the rotation is the reverse of the required to convert the axes.
See #rotate:, #bodyToInertial: and #inertialToBody:!

!Float64Vector2 commentStamp: '<historical>' prior: 0!
I represent simple (x, y) 2D Cartesian coordinates with 64-bit Float (a.k.a. double) precision.
Based on B3DVector2 (from Balloon3D-Math), but with 64 bit Float elements (instead of 32 bit Float).!

!Float64Vector3 commentStamp: '<historical>' prior: 0!
I represent simple (x, y, z) 3D Cartesian coordinates with 64-bit Float (a.k.a. double) precision.
Based on B3DVector3 (from Balloon3D-Math), but with 64 bit Float elements (instead of 32 bit Float).!

!Float64Vector4 commentStamp: '<historical>' prior: 0!
I represent 3D points in homogenous coordinates.
A copy of B3DVector4 (from Balloon3D-Math), but with 64 bit Float elements (instead of 32 bit Float).!

!Float32Matrix3x3 commentStamp: '<historical>' prior: 0!
I represent a 3x3 matrix for linear transformations (rotation and scale, maybe also skew) commonly used in 3D computer graphics.
Based on Float64Matrix3x3, but 32 bits.!

!Float32Vector3 commentStamp: '<historical>' prior: 0!
I represent simple (x, y, z) 3D Cartesian coordinates with 32-bit Float precision.
Based on Float64Vector3!

!Point methodsFor: '*Math 3D' stamp: 'jmv 2/26/2015 15:05'!
@ aNumber
	^Float64Vector3 x: x y: y z: aNumber! !

!Float64Color4 methodsFor: 'accessing'!
alpha
	^self floatAt: 4! !

!Float64Color4 methodsFor: 'accessing'!
alpha: aNumber
	self floatAt: 4 put: aNumber! !

!Float64Color4 methodsFor: 'converting'!
asColor
	^Color r: self red g: self green b: self blue alpha: self alpha! !

!Float64Color4 methodsFor: 'converting' stamp: 'ar 5/4/2000 17:59'!
asFloat64Color
	^self! !

!Float64Color4 methodsFor: 'accessing'!
blue
	^self floatAt: 3! !

!Float64Color4 methodsFor: 'accessing'!
blue: aNumber
	self floatAt: 3 put: aNumber! !

!Float64Color4 methodsFor: 'accessing'!
green
	^self floatAt: 2! !

!Float64Color4 methodsFor: 'accessing'!
green: aNumber
	self floatAt: 2 put: aNumber! !

!Float64Color4 methodsFor: 'interpolating' stamp: 'jsp 2/8/1999 19:57'!
interpolateTo: end at: amountDone
	"Return the color vector yielded by interpolating from the state of the object to the specified end state at the specified amount done"

	| newColor r g b a |
	r := self red.
	g := self green.
	b := self blue.
	a := self alpha.

	newColor := Float64Color4 new.
	newColor red: r + (((end red) - r) * amountDone).
	newColor green: g + (((end green) - g) * amountDone).
	newColor blue: b + (((end blue) - b) * amountDone).
	newColor alpha: a + (((end alpha) - a) * amountDone).

	^ newColor.
! !

!Float64Color4 methodsFor: 'testing' stamp: 'ar 2/15/1999 22:12'!
isZero
	^self alpha isZero! !

!Float64Color4 methodsFor: 'converting' stamp: 'ar 2/4/1999 20:21'!
pixelValue32
	^self asColor pixelWordForDepth: 32! !

!Float64Color4 methodsFor: 'private' stamp: 'jmv 2/26/2015 11:57'!
privateLoadFrom: aColor

	self red: aColor red.
	self green: aColor green.
	self blue: aColor blue.
	self alpha: aColor alpha.! !

!Float64Color4 methodsFor: 'initialize' stamp: 'ar 2/7/1999 16:21'!
r: rValue g: gValue b: bValue a: aValue
	self red: rValue.
	self green: gValue.
	self blue: bValue.
	self alpha: aValue.! !

!Float64Color4 methodsFor: 'accessing'!
red
	^self floatAt: 1! !

!Float64Color4 methodsFor: 'accessing'!
red: aNumber
	self floatAt: 1 put: aNumber! !

!Float64Color4 class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:22'!
numElements
	^4! !

!Float64Color4 class methodsFor: 'instance creation' stamp: 'ar 2/7/1999 16:21'!
r: rValue g: gValue b: bValue a: aValue
	^self new r: rValue g: gValue b: bValue a: aValue! !

!Float64Color4 class methodsFor: 'instance creation' stamp: 'jsp 2/8/1999 18:46'!
red: r green: g blue: b alpha: a
	"Create an initialize a color vector."

	| newColor |
	newColor := Float64Color4 new.

	newColor red: r.
	newColor green: g.
	newColor blue: b.
	newColor alpha: a.

	^ newColor.
! !

!Float64Matrix3x3 methodsFor: 'arithmetic' stamp: 'jmv 11/24/2015 11:35'!
* aFloat64Vector3
	"Answer the result of multiplying self by an argument vector"
	| x y z rx ry rz |
	"It would be nice to have a plugin for this...
	<primitive: 'Float64MultiplyByVector' module: 'Squeak3D'>"
	self flag: #Float64Primitive.

	x := aFloat64Vector3 x.
	y := aFloat64Vector3 y.
	z := aFloat64Vector3 z.

	rx := (x * self a11) + (y * self a12) + (z * self a13).
	ry := (x * self a21) + (y * self a22) + (z * self a23).
	rz := (x * self a31) + (y * self a32) + (z * self a33).

	^Float64Vector3 x: rx y: ry z: rz! !

!Float64Matrix3x3 methodsFor: 'arithmetic' stamp: 'jmv 11/24/2015 09:54'!
+ aFloat64Matrix
	"Optimized for Matrix/Matrix operations"
	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>"
	self flag: #Float64Primitive.

	^super + aFloat64Matrix! !

!Float64Matrix3x3 methodsFor: 'arithmetic' stamp: 'jmv 11/24/2015 09:54'!
- aFloat64Matrix
	"Optimized for Matrix/Matrix operations"
	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>"
	self flag: #Float64Primitive.

	^super - aFloat64Matrix! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 09:54'!
a11
	"Return the element a11"
	^self at: 1! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 09:54'!
a11: aNumber
	"Store the element a11"
	self at: 1 put: aNumber! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 09:54'!
a12
	"Return the element a12"
	^self at: 2! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 09:54'!
a12: aNumber
	"Store the element a12"
	self at: 2 put: aNumber! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 09:54'!
a13
	"Return the element a13"
	^self at: 3! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 09:54'!
a13: aNumber
	"Store the element a13"
	self at: 3 put: aNumber! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:06'!
a21
	"Return the element a21"
	^self at: 4! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:06'!
a21: aNumber
	"Store the element a21"
	self at: 4 put: aNumber! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:06'!
a22
	"Return the element a22"
	^self at: 5! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:06'!
a22: aNumber
	"Store the element a22"
	self at: 5 put: aNumber! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:06'!
a23
	"Return the element a23"
	^self at: 6! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:06'!
a23: aNumber
	"Store the element a23"
	self at: 6 put: aNumber! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:06'!
a31
	"Return the element a31"
	^self at: 7! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:07'!
a31: aNumber
	"Store the element a31"
	self at: 7 put: aNumber! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:07'!
a32
	"Return the element a32"
	^self at: 8! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:07'!
a32: aNumber
	"Store the element a32"
	self at: 8 put: aNumber! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:07'!
a33
	"Return the element a33"
	^self at: 9! !

!Float64Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 10:07'!
a33: aNumber
	"Store the element a33"
	self at: 9 put: aNumber! !

!Float64Matrix3x3 methodsFor: 'converting' stamp: 'jmv 11/26/2015 15:09'!
asIEEE32BitPrecisionFloat
	| answer |
	answer _ Float32Matrix3x3 new.
	1 to: 9 do: [ :i | answer at: i put: (self at: i) ].
	^answer! !

!Float64Matrix3x3 methodsFor: 'accessing' stamp: 'jmv 11/24/2015 10:09'!
at: i at: j
	^ self at: ((i - 1) * 3 + j).
! !

!Float64Matrix3x3 methodsFor: 'accessing' stamp: 'jmv 11/24/2015 10:09'!
at: i at: j put: aValue
	^ self at: ((i - 1) * 3 + j) put: aValue.
! !

!Float64Matrix3x3 methodsFor: 'testing' stamp: 'jmv 11/24/2015 09:56'!
isIdentity
	^self = Float64Matrix3x3Identity! !

!Float64Matrix3x3 methodsFor: 'testing' stamp: 'jmv 11/24/2015 09:57'!
isZero
	^self = Float64Matrix3x3Zero! !

!Float64Matrix3x3 methodsFor: 'transforming' stamp: 'jmv 11/24/2015 11:35'!
localDirToGlobal: aVector
	"Multiply direction vector with the receiver"

	^self * aVector! !

!Float64Matrix3x3 methodsFor: 'printing' stamp: 'jmv 11/24/2015 09:54'!
print
	self printOn: Transcript.
	Transcript newLine! !

!Float64Matrix3x3 methodsFor: 'printing' stamp: 'jmv 11/24/2015 10:09'!
printOn: aStream
	aStream newLine.
	1 to: 3 do: [ :i |
		aStream nextPutAll: '| '.
		1 to: 3 do: [ :j |
			(self at: i at: j) printOn: aStream integerDigits: 1 fractionDigits: 3.
			aStream space ].
		aStream nextPut: $|; newLine ]! !

!Float64Matrix3x3 methodsFor: 'initialize' stamp: 'jmv 11/24/2015 09:56'!
setIdentity
	"Set the receiver to the identity matrix"
	self loadFrom: Float64Matrix3x3Identity! !

!Float64Matrix3x3 methodsFor: 'initialize' stamp: 'jmv 11/24/2015 09:54'!
setScale: aVector
	self 
		a11: aVector x;
		a22: aVector y;
		a33: aVector z! !

!Float64Matrix3x3 methodsFor: 'initialize' stamp: 'jmv 11/24/2015 09:57'!
setZero
	"Set the receiver to the zero matrix"
	self loadFrom: Float64Matrix3x3Zero! !

!Float64Matrix3x3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 09:54'!
identity
	^self new setIdentity! !

!Float64Matrix3x3 class methodsFor: 'class initialization' stamp: 'jmv 11/24/2015 10:16'!
initialize
	"Float64Matrix3x3 initialize"
	Float64Matrix3x3Zero := self new.
	Float64Matrix3x3Identity := self new.
	Float64Matrix3x3Identity a11: 1.0; a22: 1.0; a33: 1.0! !

!Float64Matrix3x3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 09:58'!
numElements
	^9! !

!Float64Matrix3x3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 10:03'!
withScale: amount
	^self identity setScale: amount! !

!Float64Matrix3x3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 09:54'!
zero
	^self new! !

!Float64Matrix4x4 methodsFor: 'arithmetic' stamp: 'jmv 2/26/2015 13:25'!
+ aFloat64Matrix
	"Optimized for Matrix/Matrix operations"
	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>"
	self flag: #Float64Primitive.

	^super + aFloat64Matrix! !

!Float64Matrix4x4 methodsFor: 'arithmetic' stamp: 'jmv 2/26/2015 13:25'!
- aFloat64Matrix
	"Optimized for Matrix/Matrix operations"
	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>"
	self flag: #Float64Primitive.

	^super - aFloat64Matrix! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a11
	"Return the element a11"
	^self at: 1! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a11: aNumber
	"Store the element a11"
	self at: 1 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a12
	"Return the element a12"
	^self at: 2! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a12: aNumber
	"Store the element a12"
	self at: 2 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a13
	"Return the element a13"
	^self at: 3! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a13: aNumber
	"Store the element a13"
	self at: 3 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a14
	"Return the element a14"
	^self at: 4! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a14: aNumber
	"Store the element a14"
	self at: 4 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a21
	"Return the element a21"
	^self at: 5! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a21: aNumber
	"Store the element a21"
	self at: 5 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a22
	"Return the element a22"
	^self at: 6! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a22: aNumber
	"Store the element a22"
	self at: 6 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a23
	"Return the element a23"
	^self at: 7! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a23: aNumber
	"Store the element a23"
	self at: 7 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a24
	"Return the element a24"
	^self at: 8! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a24: aNumber
	"Store the element a24"
	self at: 8 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a31
	"Return the element a31"
	^self at: 9! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a31: aNumber
	"Store the element a31"
	self at: 9 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a32
	"Return the element a32"
	^self at: 10! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a32: aNumber
	"Store the element a32"
	self at: 10 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a33
	"Return the element a33"
	^self at: 11! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a33: aNumber
	"Store the element a33"
	self at: 11 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a34
	"Return the element a34"
	^self at: 12! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a34: aNumber
	"Store the element a34"
	self at: 12 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a41
	"Return the element a41"
	^self at: 13! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a41: aNumber
	"Store the element a41"
	self at: 13 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a42
	"Return the element a42"
	^self at: 14! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a42: aNumber
	"Store the element a42"
	self at: 14 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a43
	"Return the element a43"
	^self at: 15! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a43: aNumber
	"Store the element a43"
	self at: 15 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!
a44
	"Return the element a44"
	^self at: 16! !

!Float64Matrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!
a44: aNumber
	"Store the element a44"
	self at: 16 put: aNumber! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'jsp 2/25/1999 13:58'!
alternateRotation
	"Return the angular rotation around each axis of the matrix"

	| cp sp cy sy cr sr vAngles |

	vAngles := Float64Vector3 new.

	((self a13) = 0) ifTrue: [ ((self a33) >= 0)  ifTrue: [ vAngles at: 2 put: 0.
													  cr := (self a11).
													  sr := (self a12).
													  cp := (self a33). ]
											 ifFalse: [ vAngles at: 2 put: (Float pi).
														cr := (self a11) negated.
														sr := (self a12) negated.
														cp := (self a33) negated. ]
							]
					ifFalse: [
								vAngles at: 2 put: (((self a13) negated) arcTan: (self a33)).
								cy := (vAngles at: 3) cos.
								sy := (vAngles at: 3) sin.
								cr := (cy * (self a11)) + (sy * (self a31)).
								sr := (cy* (self a12)) + (sy * (self a32)).
								cp := (cy * (self a33)) - (sy * (self a13)).
							].

	sp := (self a23).
 
	vAngles at: 1 put: (sp arcTan: cp).
	vAngles at: 3 put: (sr arcTan: cr).

	vAngles at: 1 put: ((vAngles at: 1) radiansToDegrees).
	vAngles at: 2 put: ((vAngles at: 2) radiansToDegrees).
	vAngles at: 3 put: ((vAngles at: 3) radiansToDegrees).

	^ vAngles.
! !

!Float64Matrix4x4 methodsFor: 'converting'!
asMatrix4x4
	^self! !

!Float64Matrix4x4 methodsFor: 'converting' stamp: 'jmv 10/11/2016 10:14:47'!
asQuaternion
	"Convert the matrix to a quaternion"

	| x y z a a2 x2 y2 a4 |

	a2 := 0.25 * (1.0 + (self a11) + (self a22) + (self a33)).

	(a2 > 0) ifTrue: [
						a := a2 sqrt.
						a4 := 4.0 * a.
						x := ((self a32) - (self a23)) / a4.
						y := ((self a13) - (self a31)) / a4.
						z := ((self a21) - (self a12)) / a4.
					]
			ifFalse: [
						a := 0.
						x2 := -0.5 * ((self a22) + (self a33)).
						(x2 > 0) ifTrue: [
											x := x2 sqrt.
											x2 := 2 * x.
											y := (self a21) / x2.
											z := (self a31) / x2.
										]
								ifFalse: [
											x := 0.
											y2 := 0.5 * (1.0 - (self a33)).
											(y2 > 0) ifTrue: [
																y := y2 sqrt.
																y2 := 2 * y.
																z := (self a32) / y2.
															]
													ifFalse: [
																y := 0.0.
																z := 1.0.
															]
										]
					].

	^ (Float64Quaternion a: a b: x c: y d: z).
! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'jsp 2/11/1999 14:09'!
at: i at: j
	^ self at: ((i - 1) * 4 + j).
! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'jsp 2/11/1999 14:09'!
at: i at: j put: aValue
	^ self at: ((i - 1) * 4 + j) put: aValue.
! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'das 12/11/2002 14:35'!
column1
	"Return column 1"

	^ (Float64Vector3 x: (self a11) y: (self a21) z: (self a31)).
! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'das 12/11/2002 14:37'!
column1: col
	"Set column 1"
	self a11: col x.
	self a21: col y.
	self a31: col z.
! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'das 12/11/2002 14:36'!
column2
	"Return column 2"

	^ (Float64Vector3 x: (self a12) y: (self a22) z: (self a32)).
! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'das 12/11/2002 14:37'!
column2: col
	"Set column 2"

	self a12: col x.
	self a22: col y.
	self a32: col z.
! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'das 12/11/2002 14:37'!
column3
	"Return column 3"

	^ (Float64Vector3 x: (self a13) y: (self a23) z: (self a33)).
! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'das 12/11/2002 14:37'!
column3: col
	"Set column 3"

	self a13: col x.
	self a23: col y.
	self a33: col z.
! !

!Float64Matrix4x4 methodsFor: 'transforming' stamp: 'ar 11/7/2000 14:48'!
composeWith: m2
	"Perform a 4x4 matrix multiplication."
	^self composedWithLocal: m2.! !

!Float64Matrix4x4 methodsFor: 'transforming' stamp: 'DPR 9/3/2002 16:45'!
composeWith: m2 times: nTimes

	"Perform a 4x4 matrix exponentiation and multiplication."

	| result |
	result := self.
	nTimes negative ifTrue: [ self halt ].
	nTimes >= 2 ifTrue: [ 
			result := result composeWith: (m2 composedWithLocal: m2) times: nTimes // 2 ].
	(nTimes \\ 2) = 1 ifTrue: [ result := result composedWithLocal: m2].
	^ result
	
! !

!Float64Matrix4x4 methodsFor: 'transforming' stamp: 'ar 2/15/1999 23:56'!
composedWithGlobal: aFloat64Matrix4x4
	| result |
	result := self class new.
	self privateTransformMatrix: aFloat64Matrix4x4 with: self into: result.
	^result! !

!Float64Matrix4x4 methodsFor: 'transforming' stamp: 'ar 2/15/1999 23:57'!
composedWithLocal: aFloat64Matrix4x4
	| result |
	result := self class new.
	self privateTransformMatrix: self with: aFloat64Matrix4x4 into: result.
	^result! !

!Float64Matrix4x4 methodsFor: 'solving' stamp: 'ar 2/1/1999 21:50'!
inplaceDecomposeLU
	"Decompose the receiver in place by using gaussian elimination w/o pivot search"
	| x |
	1 to: 4 do:[:j|
		"i-th equation (row)"
		j+1 to: 4 do:[:i|
			x := (self at: i at: j) / (self at: j at: j).
			j to: 4 do:[:k|
				self at: i at: k put: (self at: i at: k) - ((self at: j at: k) * x)].
			self at: i at: j put: x]].
! !

!Float64Matrix4x4 methodsFor: 'solving' stamp: 'jmv 2/26/2015 13:25'!
inplaceHouseHolderInvert
	"Solve the linear equation self * aVector = x by using HouseHolder's transformation.
	Note: This scheme is numerically better than using gaussian elimination even though it takes
	somewhat longer"
	| d x sigma beta sum s|
	"It would be nice to have a plugin for this...
	<primitive:'Float64InplaceHouseHolderInvert' module:'Squeak3D'>"
	self flag: #Float64Primitive.

	x := Float64Matrix4x4 identity.
	d := Float64Matrix4x4 new.
	1 to: 4 do:[:j|
		sigma := 0.0.
		j to: 4 do:[:i| sigma := sigma + ((self at: i at: j) squared)].
		sigma isZero ifTrue:[^nil]. "matrix is singular"
		((self at: j at: j) < 0.0) 
			ifTrue:[ s:= sigma sqrt]
			ifFalse:[ s:= sigma sqrt negated].
		1 to: 4 do:[:r| d at: j at: r put: s].
		beta := 1.0 / ( s * (self at: j at: j) - sigma).
		self at: j at: j put: ((self at: j at: j) - s).
		"update remaining columns"
		j+1 to: 4 do:[:k|
			sum := 0.0.
			j to: 4 do:[:i| sum := sum + ((self at: i at: j) * (self at: i at: k))].
			sum := sum * beta.
			j to: 4 do:[:i| 
				self at: i at: k put: ((self at: i at: k) + ((self at: i at: j) * sum))]].
		"update vector"
		1 to: 4 do:[:r|
			sum := nil.
			j to: 4 do:[:i| 
				sum := sum isNil 
					ifTrue:[(x at: i at: r) * (self at: i at: j)] 
					ifFalse:[sum + ((x at: i at: r) * (self at: i at: j))]].
			sum := sum * beta.
			j to: 4 do:[:i| 
				x at: i at: r put:((x at: i at: r) + (sum * (self at: i at: j)))].
		].
	].
	"Now calculate result"
	1 to: 4 do:[:r|
		4 to: 1 by: -1 do:[:i|
			i+1 to: 4 do:[:j|
				x at: i at: r put: ((x at: i at: r) - ((x at: j at: r) * (self at: i at: j))) ].
			x at: i at: r put: ((x at: i at: r) / (d at: i at: r))].
	].
	self loadFrom: x.
	"Return receiver"! !

!Float64Matrix4x4 methodsFor: 'solving'!
inplaceHouseHolderTransform: aVector
	"Solve the linear equation self * aVector = x by using HouseHolder's transformation.
	Note: This scheme is numerically better than using gaussian elimination even though it takes
	somewhat longer"
	| d x sigma beta sum s|
	x := Array with: aVector x with: aVector y with: aVector z with: aVector w.
	d := Array new: 4.
	1 to: 4 do:[:j|
		sigma := 0.0.
		j to: 4 do:[:i| sigma := sigma + ((self at: i at: j) squared)].
		sigma isZero ifTrue:[^nil]. "matrix is singular"
		((self at: j at: j) < 0.0) 
			ifTrue:[ s:= d at: j put: (sigma sqrt)]
			ifFalse:[ s:= d at: j put: (sigma sqrt negated)].
		beta := 1.0 / ( s * (self at: j at: j) - sigma).
		self at: j at: j put: ((self at: j at: j) - s).
		"update remaining columns"
		j+1 to: 4 do:[:k|
			sum := 0.0.
			j to: 4 do:[:i| sum := sum + ((self at: i at: j) * (self at: i at: k))].
			sum := sum * beta.
			j to: 4 do:[:i| 
				self at: i at: k put: ((self at: i at: k) + ((self at: i at: j) * sum))]].
		"update vector"
		sum := nil.
		j to: 4 do:[:i| 
			sum := sum isNil 
				ifTrue:[(x at: i) * (self at: i at: j)] 
				ifFalse:[sum + ((x at: i) * (self at: i at: j))]].
		sum := sum * beta.
		j to: 4 do:[:i| 
			x at: i put:((x at: i) + (sum * (self at: i at: j)))].
	].
	"Now calculate result"
	4 to: 1 by: -1 do:[:i|
		i+1 to: 4 do:[:j|
			x at: i put: ((x at: i) - ((x at: j) * (self at: i at: j))) ].
		x at: i put: ((x at: i) / (d at: i))].
	^Float64Vector4 x: (x at: 1) y: (x at: 2) z: (x at: 3) w: (x at: 4)
! !

!Float64Matrix4x4 methodsFor: 'transforming' stamp: 'jmv 5/14/2015 09:51'!
inverseTransformation
	"Return the inverse matrix of the receiver."

	^ self copy inplaceHouseHolderInvert! !

!Float64Matrix4x4 methodsFor: 'testing' stamp: 'jmv 11/24/2015 09:52'!
isIdentity
	^self = Float64Matrix4x4Identity! !

!Float64Matrix4x4 methodsFor: 'testing' stamp: 'jmv 11/24/2015 09:52'!
isZero
	^self = Float64Matrix4x4Zero! !

!Float64Matrix4x4 methodsFor: 'transforming' stamp: 'jmv 2/26/2015 13:24'!
localDirToGlobal: aVector
	"Multiply direction vector with the receiver"
	| x y z rx ry rz |
	"It would be nice to have a plugin for this...
	<primitive: 'Float64TransformDirection' module: 'Squeak3D'>"
	self flag: #Float64Primitive.

	x := aVector x.
	y := aVector y.
	z := aVector z.

	rx := (x * self a11) + (y * self a12) + (z * self a13).
	ry := (x * self a21) + (y * self a22) + (z * self a23).
	rz := (x * self a31) + (y * self a32) + (z * self a33).

	^Float64Vector3 x: rx y: ry z: rz! !

!Float64Matrix4x4 methodsFor: 'transforming' stamp: 'jmv 2/26/2015 13:24'!
localPointToGlobal: aVector
	"Multiply aVector (temporarily converted to 4D) with the receiver"
	| x y z rx ry rz rw |
	"It would be nice to have a plugin for this...
	<primitive: 'Float64TransformPoint' module: 'Squeak3D'>"
	self flag: #Float64Primitive.

	x := aVector x.
	y := aVector y.
	z := aVector z.

	rx := (x * self a11) + (y * self a12) + (z * self a13) + self a14.
	ry := (x * self a21) + (y * self a22) + (z * self a23) + self a24.
	rz := (x * self a31) + (y * self a32) + (z * self a33) + self a34.
	rw := (x * self a41) + (y * self a42) + (z * self a43) + self a44.

	^Float64Vector3 x:(rx/rw) y: (ry/rw) z: (rz/rw)! !

!Float64Matrix4x4 methodsFor: 'transforming' stamp: 'jmv 5/14/2015 09:51'!
orthoNormInverse
	| m x y z rx ry rz |
	"It would be nice to have a plugin for this...
	<primitive: 'Float64OrthoNormInverseMatrix' module: 'Squeak3D'>"
	self flag: #Float64Primitive.

	m := self copy.
	"transpose upper 3x3 matrix"
	m a11: self a11; a12: self a21; a13: self a31.
	m a21: self a12; a22: self a22; a23: self a32.
	m a31: self a13; a32: self a23; a33: self a33.
	"Compute inverse translation vector"
	x := self a14.
	y := self a24.
	z := self a34.
	rx := (x * m a11) + (y * m a12) + (z * m a13).
	ry := (x * m a21) + (y * m a22) + (z * m a23).
	rz := (x * m a31) + (y * m a32) + (z * m a33).

	m a14: 0.0-rx; a24: 0.0-ry; a34: 0.0-rz.
	^m
" Used to be:
	m := self clone.
	v := m translation.
	m translation: Float64Vector3 zero.
	m := m transposed.
	v := (m localPointToGlobal: v) negated.
	m translation: v.
	^ m.
"! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'jmv 2/26/2015 16:09'!
pitchYawRoll
	"Assume the receiver describes an orthonormal 3x3 matrix, i.e. a pure 3D rotation"
	| pitch yaw roll |
	pitch := self a23 negated arcSin.
	yaw := self a13 arcTan: self a33.
	roll := self a21 arcTan: self a22.
	^pitch radiansToDegrees@yaw radiansToDegrees@roll radiansToDegrees! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'jmv 2/26/2015 16:10'!
pitchYawRoll: ypr
	"Assume the receiver describes an orthonormal 3x3 matrix, i.e. a pure 3D rotation
	plus a translation"
	| offset mx my mz |
	offset := self translation.
	mx := self class identity rotationAroundX: ypr x.
	my := self class identity rotationAroundY: ypr y.
	mz := self class identity rotationAroundZ: ypr z.
	self loadFrom: (mz composeWith: (my composeWith: mx)).
	self translation: offset
! !

!Float64Matrix4x4 methodsFor: 'printing' stamp: 'jmv 7/13/2015 10:10'!
print
	self printOn: Transcript.
	Transcript newLine! !

!Float64Matrix4x4 methodsFor: 'printing' stamp: 'jmv 7/3/2015 12:53'!
printOn: aStream
	aStream newLine.
	1 to: 4 do: [ :i |
		aStream nextPutAll: '| '.
		1 to: 4 do: [ :j |
			(self at: i at: j) printOn: aStream integerDigits: 1 fractionDigits: 3.
			aStream space ].
		aStream nextPut: $|; newLine ]! !

!Float64Matrix4x4 methodsFor: 'private' stamp: 'jmv 2/26/2015 13:29'!
privateTransformMatrix: m1 with: m2 into: m3
	"Perform a 4x4 matrix multiplication
		m2 * m1 = m3
	being equal to first transforming points by m2 and then by m1.
	Note that m1 may be identical to m3.
	NOTE: The primitive implementation does NOT return m3 - and so don't we!!"
	| c1 c2 c3 c4 |
	"It would be nice to have a plugin for this...
	<primitive: 'Float64TransformMatrixWithInto' module:'Squeak3D'>"
	self flag: #Float64Primitive.

	m2 == m3 ifTrue:[^self error:'Argument and result matrix identical'].
	c1 := ((m1 a11 * m2 a11) + (m1 a12 * m2 a21) + 
				(m1 a13 * m2 a31) + (m1 a14 * m2 a41)).
	c2 := ((m1 a11 * m2 a12) + (m1 a12 * m2 a22) + 
				(m1 a13 * m2 a32) + (m1 a14 * m2 a42)).
	c3 := ((m1 a11 * m2 a13) + (m1 a12 * m2 a23) + 
				(m1 a13 * m2 a33) + (m1 a14 * m2 a43)).
	c4 := ((m1 a11 * m2 a14) + (m1 a12 * m2 a24) + 
				(m1 a13 * m2 a34) + (m1 a14 * m2 a44)).

	m3 a11: c1; a12: c2; a13: c3; a14: c4.

	c1 := ((m1 a21 * m2 a11) + (m1 a22 * m2 a21) + 
				(m1 a23 * m2 a31) + (m1 a24 * m2 a41)).
	c2 := ((m1 a21 * m2 a12) + (m1 a22 * m2 a22) + 
				(m1 a23 * m2 a32) + (m1 a24 * m2 a42)).
	c3 := ((m1 a21 * m2 a13) + (m1 a22 * m2 a23) + 
				(m1 a23 * m2 a33) + (m1 a24 * m2 a43)).
	c4 := ((m1 a21 * m2 a14) + (m1 a22 * m2 a24) + 
				(m1 a23 * m2 a34) + (m1 a24 * m2 a44)).

	m3 a21: c1; a22: c2; a23: c3; a24: c4.

	c1 := ((m1 a31 * m2 a11) + (m1 a32 * m2 a21) + 
				(m1 a33 * m2 a31) + (m1 a34 * m2 a41)).
	c2 := ((m1 a31 * m2 a12) + (m1 a32 * m2 a22) + 
				(m1 a33 * m2 a32) + (m1 a34 * m2 a42)).
	c3 := ((m1 a31 * m2 a13) + (m1 a32 * m2 a23) + 
				(m1 a33 * m2 a33) + (m1 a34 * m2 a43)).
	c4 := ((m1 a31 * m2 a14) + (m1 a32 * m2 a24) + 
				(m1 a33 * m2 a34) + (m1 a34 * m2 a44)).

	m3 a31: c1; a32: c2; a33: c3; a34: c4.

	c1 := ((m1 a41 * m2 a11) + (m1 a42 * m2 a21) + 
				(m1 a43 * m2 a31) + (m1 a44 * m2 a41)).
	c2 := ((m1 a41 * m2 a12) + (m1 a42 * m2 a22) + 
				(m1 a43 * m2 a32) + (m1 a44 * m2 a42)).
	c3 := ((m1 a41 * m2 a13) + (m1 a42 * m2 a23) + 
				(m1 a43 * m2 a33) + (m1 a44 * m2 a43)).
	c4 := ((m1 a41 * m2 a14) + (m1 a42 * m2 a24) + 
				(m1 a43 * m2 a34) + (m1 a44 * m2 a44)).

	m3 a41: c1; a42: c2; a43: c3; a44: c4.! !

!Float64Matrix4x4 methodsFor: 'double dispatching' stamp: 'ar 2/8/1999 20:11'!
productFromMatrix4x4: matrix
	"Multiply a 4x4 matrix with the receiver."
	| result |
	result := self class new.
	result a11: ((matrix a11 * self a11) + (matrix a12 * self a21) + 
				(matrix a13 * self a31) + (matrix a14 * self a41)).
	result a12: ((matrix a11 * self a12) + (matrix a12 * self a22) + 
				(matrix a13 * self a32) + (matrix a14 * self a42)).
	result a13: ((matrix a11 * self a13) + (matrix a12 * self a23) + 
				(matrix a13 * self a33) + (matrix a14 * self a43)).
	result a14: ((matrix a11 * self a14) + (matrix a12 * self a24) + 
				(matrix a13 * self a34) + (matrix a14 * self a44)).

	result a21: ((matrix a21 * self a11) + (matrix a22 * self a21) + 
				(matrix a23 * self a31) + (matrix a24 * self a41)).
	result a22: ((matrix a21 * self a12) + (matrix a22 * self a22) + 
				(matrix a23 * self a32) + (matrix a24 * self a42)).
	result a23: ((matrix a21 * self a13) + (matrix a22 * self a23) + 
				(matrix a23 * self a33) + (matrix a24 * self a43)).
	result a24: ((matrix a21 * self a14) + (matrix a22 * self a24) + 
				(matrix a23 * self a34) + (matrix a24 * self a44)).

	result a31: ((matrix a31 * self a11) + (matrix a32 * self a21) + 
				(matrix a33 * self a31) + (matrix a34 * self a41)).
	result a32: ((matrix a31 * self a12) + (matrix a32 * self a22) + 
				(matrix a33 * self a32) + (matrix a34 * self a42)).
	result a33: ((matrix a31 * self a13) + (matrix a32 * self a23) + 
				(matrix a33 * self a33) + (matrix a34 * self a43)).
	result a34: ((matrix a31 * self a14) + (matrix a32 * self a24) + 
				(matrix a33 * self a34) + (matrix a34 * self a44)).

	result a41: ((matrix a41 * self a11) + (matrix a42 * self a21) + 
				(matrix a43 * self a31) + (matrix a44 * self a41)).
	result a42: ((matrix a41 * self a12) + (matrix a42 * self a22) + 
				(matrix a43 * self a32) + (matrix a44 * self a42)).
	result a43: ((matrix a41 * self a13) + (matrix a42 * self a23) + 
				(matrix a43 * self a33) + (matrix a44 * self a43)).
	result a44: ((matrix a41 * self a14) + (matrix a42 * self a24) + 
				(matrix a43 * self a34) + (matrix a44 * self a44)).

	^result! !

!Float64Matrix4x4 methodsFor: 'double dispatching'!
productFromVector3: aVector3
	"Multiply aVector (temporarily converted to 4D) with the receiver"
	| x y z rx ry rz rw |
	x := aVector3 x.
	y := aVector3 y.
	z := aVector3 z.

	rx := (x * self a11) + (y * self a21) + (z * self a31) + self a41.
	ry := (x * self a12) + (y * self a22) + (z * self a32) + self a42.
	rz := (x * self a13) + (y * self a23) + (z * self a33) + self a43.
	rw := (x * self a14) + (y * self a24) + (z * self a34) + self a44.

	^Float64Vector3 x:(rx/rw) y: (ry/rw) z: (rz/rw)! !

!Float64Matrix4x4 methodsFor: 'double dispatching'!
productFromVector4: aVector4
	"Multiply aVector with the receiver"
	| x y z w rx ry rz rw |
	x := aVector4 x.
	y := aVector4 y.
	z := aVector4 z.
	w := aVector4 w.

	rx := (x * self a11) + (y * self a21) + (z * self a31) + (w * self a41).
	ry := (x * self a12) + (y * self a22) + (z * self a32) + (w * self a42).
	rz := (x * self a13) + (y * self a23) + (z * self a33) + (w * self a43).
	rw := (x * self a14) + (y * self a24) + (z * self a34) + (w * self a44).

	^Float64Vector4 x:rx y: ry z: rz w: rw! !

!Float64Matrix4x4 methodsFor: 'transforming' stamp: 'ar 2/7/1999 06:32'!
quickTransformV3ArrayFrom: srcArray to: dstArray
	"Transform the 3 element vertices from srcArray to dstArray.
	ASSUMPTION: a41 = a42 = a43 = 0.0 and a44 = 1.0"
	| a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 x y z index |
	self flag: #Float64Primitive.
	a11 := self a11.	a12 := self a12.	a13 := self a13.	a14 := self a14.
	a21 := self a21.	a22 := self a22.	a23 := self a23.	a24 := self a24.
	a31 := self a31.	a32 := self a32.	a33 := self a33.	a34 := self a34.
	1 to: srcArray size do:[:i|
		index := i-1*3.
		x := srcArray floatAt: index+1.
		y := srcArray floatAt: index+2.
		z := srcArray floatAt: index+3.
		dstArray floatAt: index+1 put: (a11*x) + (a12*y) + (a13*z) + a14.
		dstArray floatAt: index+2 put: (a21*x) + (a22*y) + (a23*z) + a24.
		dstArray floatAt: index+3 put: (a31*x) + (a32*y) + (a33*z) + a34.
	].
	^dstArray! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'jsp 2/25/1999 13:58'!
rotation
	"Return the angular rotation around each axis of the matrix"

	| vRow1 vRow2 vRow3 vScale vShear vAngles vRowCross determinate |

	vRow1 := self row1.
	vRow2 := self row2.
	vRow3 := self row3.

	vScale := Float64Vector3 new.
	vShear := Float64Vector3 new.
	vAngles := Float64Vector3 new.

	vScale at: 1 put: (vRow1 length).
	vRow1 normalize.
	vShear at: 1 put: (vRow1 dot: vRow2).
	vRow2 := vRow2 + (vRow1 * ((vShear at: 1) negated)).

	vScale at: 2 put: (vRow2 length).
	vRow2 normalize.
	vShear at: 1 put: ((vShear at: 1) / (vScale at: 2)).

	vShear at: 2 put: (vRow1 dot: vRow3).
	vRow3 := vRow3 + (vRow1 * ((vShear at: 2) negated)).

	vShear at: 3 put: (vRow2 dot: vRow3).
	vRow3 := vRow3 + (vRow2 * ((vShear at: 3) negated)).

	vScale at: 3 put: (vRow3 length).
	vRow3 normalize.

	vShear at: 2 put: ((vShear at: 2) / (vScale at: 3)).
	vShear at: 3 put: ((vShear at: 3) / (vScale at: 3)).

	vRowCross := vRow2 cross: vRow3.
	determinate := vRow1 dot: vRowCross.

	(determinate < 0.0) ifTrue: [ vRow1 := vRow1 negated.
								vRow2 := vRow2 negated.
								vRow3 := vRow3 negated.
								vScale := vScale negated. ].

	vAngles at: 2 put: ((vRow1 at: 3) negated) arcSin.

	(((vAngles at: 2) cos) ~= 0.0) 
								ifTrue: [ vAngles at: 1 put:
												((vRow2 at: 3) arcTan: (vRow3 at: 3)).
										  vAngles at: 3 put:
												((vRow1 at: 2) arcTan: (vRow1 at: 1)). ]
								ifFalse: [ vAngles at: 1 put:
												((vRow2 at: 1) arcTan: (vRow2 at: 2)).
										  vAngles at: 3 put: 0.0 ].


	vAngles at: 1 put: ((vAngles at: 1) radiansToDegrees).
	vAngles at: 2 put: ((vAngles at: 2) radiansToDegrees).
	vAngles at: 3 put: ((vAngles at: 3) radiansToDegrees).

	^ vAngles.
! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'jmv 2/26/2015 15:28'!
rotation: aVector
	"
	Warning:
	(Float64Matrix4x4 new rotation: 30@0@0) pitchYawRoll x 
		->		-30.0
	!!
	"
	| xRot yRot zRot cosPitch sinPitch cosYaw sinYaw cosRoll sinRoll |

	xRot := (aVector x) degreesToRadians.
	yRot := (aVector y) degreesToRadians.
	zRot := (aVector z) degreesToRadians.

	cosPitch := xRot cos.
	sinPitch := xRot sin.
	cosYaw := yRot cos.
	sinYaw := yRot sin.
	cosRoll := zRot cos.
	sinRoll := zRot sin.

	self a11: (cosRoll*cosYaw).
	self a12: (sinRoll*cosYaw).
	self a13: (sinYaw negated).

	self a21: ((cosRoll*sinYaw*sinPitch) - (sinRoll*cosPitch)).
	self a22: ((cosRoll*cosPitch) + (sinRoll*sinYaw*sinPitch)).
	self a23: (cosYaw*sinPitch).
	self a31: ((cosRoll*sinYaw*cosPitch) + (sinRoll*sinPitch)).
	self a32: ((sinRoll*sinYaw*cosPitch) - (cosRoll*sinPitch)).
	self a33: (cosYaw*cosPitch).

	^ self.
! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'jmv 10/11/2016 10:14:50'!
rotation: anAngle around: aVector3
	"set up a rotation matrix around the direction aVector3"

	self loadFrom: (Float64Quaternion angle: anAngle axis: aVector3) asMatrix4x4! !

!Float64Matrix4x4 methodsFor: 'accessing'!
rotation: anAngle aroundX: xValue y: yValue z: zValue
	"set up a rotation matrix around the direction x/y/z"
	^self rotation: anAngle around:(Float64Vector3 with: xValue with: yValue with: zValue)! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:34'!
rotationAroundX: anAngle
	| rad s c |
	rad := anAngle degreesToRadians.
	s := rad sin.
	c := rad cos.
	self a22: c.
	self a23: s negated.
	self a33: c.
	self a32: s.
	^self! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:34'!
rotationAroundY: anAngle
	| rad s c |
	rad := anAngle degreesToRadians.
	s := rad sin.
	c := rad cos.
	self a11: c.
	self a13: s.
	self a33: c.
	self a31: s negated.
	^self! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:35'!
rotationAroundZ: anAngle
	| rad s c |
	rad := anAngle degreesToRadians.
	s := rad sin.
	c := rad cos.
	self a11: c.
	self a12: s negated.
	self a22: c.
	self a21: s.
	^self! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'jsp 2/24/1999 17:10'!
row1
	"Return row 1"

	^ (Float64Vector3 x: (self a11) y: (self a12) z: (self a13)).
! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'das 9/24/2002 10:27'!
row1: row
	"Set row 1"
	self a11: row x.
	self a12: row y.
	self a13: row z.
! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'jsp 2/24/1999 17:11'!
row2
	"Return row 2"

	^ (Float64Vector3 x: (self a21) y: (self a22) z: (self a23)).
! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'das 9/24/2002 10:28'!
row2: row
	"Set row 2"

	self a21: row x.
	self a22: row y.
	self a23: row z.
! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'jsp 2/24/1999 17:11'!
row3
	"Return row 3"

	^ (Float64Vector3 x: (self a31) y: (self a32) z: (self a33)).
! !

!Float64Matrix4x4 methodsFor: 'row-access' stamp: 'das 9/24/2002 10:29'!
row3: row
	"Set row 3"

	self a31: row x.
	self a32: row y.
	self a33: row z.
! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:35'!
scaling: aVector

	^self scalingX: aVector x y: aVector y z: aVector z! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'das 9/13/2002 13:17'!
scalingX: xValue y: yValue z: zValue

	self a11: self a11 * xValue.
	self a22: self a22 * yValue.
	self a33: self a33 * zValue.
	^self! !

!Float64Matrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:26'!
setBSplineBase
	"Set the receiver to the BSpline base matrix"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -1.0 / 6.0;	a12: 3.0 / 6.0;	a13: -3.0 / 6.0;	a14: 1.0 / 6.0;
		a21: 3.0 / 6.0;	a22: -6.0 / 6.0;	a23: 3.0 / 6.0;	a24: 0.0 / 6.0;
		a31: -3.0 / 6.0;	a32: 0.0 / 6.0;	a33: 3.0 / 6.0;	a34: 0.0 / 6.0;
		a41: 1.0 / 6.0;	a42: 4.0 / 6.0;	a43: 1.0 / 6.0;	a44: 0.0 / 6.0
! !

!Float64Matrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:26'!
setBetaSplineBaseBias: beta1 tension: beta2
	"Set the receiver to the betaSpline base matrix 
	if beta1=1 and beta2=0 then the bSpline base matrix will be returned"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	| b12 b13 delta |
	b12 := beta1 * beta1.
	b13 := beta1 * b12.
	delta := 1.0 / (beta2 + (2.0 * b13) + 4.0 * (b12 + beta1) +2.0).
	
	self
		a11: delta * -2.0 * b13;
		a12: delta * 2.0 * (beta2 + b13 + b12 + beta1);
		a13: delta * -2.0 * (beta2 + b12 + beta1 + 1.0);
		a14: delta * 2.0;
		a21: delta * 6.0 * b13;
		a22: delta * -3.0 * (beta2 + (2.0 * (b13 + b12)));
		a23: delta * 3.0 * (beta2 + (2.0 * b12));
		a24: 0.0;
		a31: delta * -6.0 * b13;
		a32: delta * 6.0 * (b13 - beta1);
		a33: delta * 6.0 * beta1;
		a34: 0.0;
		a41: delta * 2.0 * b13;
		a42: delta * (beta2 + 4.0 * (b12 + beta1));
		a43: delta * 2.0;
		a44: 0.0
! !

!Float64Matrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:27'!
setBezierBase
	"Set the receiver to the bezier base matrix"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -1.0;		a12: 3.0;		a13: -3.0;	a14: 1.0;
		a21: 3.0;		a22: -6.0;	a23: 3.0;	a24: 0.0;
		a31: -3.0;	a32: 3.0;	a33: 0.0;	a34: 0.0;
		a41: 1.0;		a42: 0.0;	a43: 0.0;	a44: 0.0! !

!Float64Matrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:27'!
setCardinalBase
	"Set the receiver to the cardinal spline base matrix - just catmull * 2"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -1.0;		a12: 3.0;		a13: -3.0;	a14: 1.0;
		a21: 2.0;		a22: -5.0;	a23: 4.0;	a24: -1.0;
		a31: -1.0;	a32: 0.0;	a33: 1.0;		a34: 0.0;
		a41: 0.0;		a42: 2.0;	a43: 0.0;	a44: 0.0
! !

!Float64Matrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:27'!
setCatmullBase
	"Set the receiver to the Catmull-Rom base matrix"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -0.5;	a12: 1.5;		a13: -1.5;	a14: 0.5;
		a21: 1.0;		a22: -2.5;	a23: 2.0;	a24: -0.5;
		a31: -0.5;	a32: 0.0;	a33: 0.5;	a34: 0.0;
		a41: 0.0;		a42: 1.0;		a43: 0.0;	a44: 0.0
! !

!Float64Matrix4x4 methodsFor: 'initialize' stamp: 'jmv 11/24/2015 09:52'!
setIdentity
	"Set the receiver to the identity matrix"
	self loadFrom: Float64Matrix4x4Identity! !

!Float64Matrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:27'!
setPolylineBase
	"Set the receiver to the polyline base matrix :)"
	self
		a11: 0.0;		a12: 0.0;		a13: 0.0;		a14: 0.0;
		a21: 0.0;		a22: 0.0;	a23: 0.0;	a24: 0.0;
		a31: 0.0;		a32: -1.0;	a33: 1.0;		a34: 0.0;
		a41: 0.0;		a42: 1.0;		a43: 0.0;	a44: 0.0
! !

!Float64Matrix4x4 methodsFor: 'initialize' stamp: 'ar 2/15/1999 02:55'!
setScale: aVector
	self 
		a11: aVector x;
		a22: aVector y;
		a33: aVector z! !

!Float64Matrix4x4 methodsFor: 'initialize'!
setTranslation: aVector
	self 
		a14: aVector x;
		a24: aVector y;
		a34: aVector z! !

!Float64Matrix4x4 methodsFor: 'initialize' stamp: 'jmv 11/24/2015 09:52'!
setZero
	"Set the receiver to the zero matrix"
	self loadFrom: Float64Matrix4x4Zero! !

!Float64Matrix4x4 methodsFor: 'initialize' stamp: 'das 5/16/2002 11:11'!
skew: vector
	"Set the skew-symetric matrix up"
	self a21: vector z.
	self a12: vector z negated.
	self a31: vector y negated.
	self a13: vector y.
	self a32: vector x.
	self a23: vector x negated.
! !

!Float64Matrix4x4 methodsFor: 'solving' stamp: 'jmv 5/14/2015 09:52'!
solve3x3: aVector
	"Solve a 3x3 system of linear equations. Assume that all the a[4,x] and a[x,4] are zero.
	NOTE: This is a hack, but it's the fastest way for now."
	| m |
	m := self copy.
	m a44: 1. "need this for inversion"
	m := m inplaceHouseHolderInvert.
	m ifNil:[^nil].
	^m localDirToGlobal: aVector.! !

!Float64Matrix4x4 methodsFor: 'solving' stamp: 'jmv 5/14/2015 09:52'!
solve: aVector

	^ self copy inplaceHouseHolderTransform: aVector
	"or:
	^self clone inplaceDecomposeLU solveLU: aVector
	"! !

!Float64Matrix4x4 methodsFor: 'solving'!
solveLU: aVector
	"Given a decomposed matrix using gaussian elimination solve the linear equations."
	| x v |
	v := Array with: aVector x with: aVector y with: aVector z with: aVector w.
	"L first"
	1 to: 4 do:[:i| "Top to bottom"
		x := 0.0.
		1 to: i-1 do:[:j|
			"From left to right w/o diagonal element"
			x := x + ((v at: j) * (self at: i at: j))].
		"No need to divide by the diagonal element - this is always 1.0 in L"
		v at: i put: (v at: i) - x].
	"Now U"
	4 to: 1 by: -1 do:[:i| "Bottom to top"
		x := 0.0.
		4 to: i+1 by: -1 do:[:j|
			"From right to left w/o diagonal element"
			x := x + ((v at: j) * (self at: i at: j))].
		"Divide by diagonal element"
		v at: i put: (v at: i) - x / (self at: i at: i)].
	^Float64Vector4 x: (v at: 1) y: (v at: 2) z: (v at: 3) w: (v at: 4)
! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'ar 4/16/1999 21:51'!
squaredDistanceFrom: aMatrix
	| sum |
	sum := 0.0.
	1 to: 4 do:[:i|
		1 to: 4 do:[:j|
			sum := sum + ((self at: i at: j) - (aMatrix at: i at: j)) squared]].
	^sum! !

!Float64Matrix4x4 methodsFor: 'comparing' stamp: 'ar 2/1/1999 21:53'!
squaredErrorDistanceTo: anotherMatrix
	| result temp |
	result := self - anotherMatrix.
	temp := 0.
	1 to: 4 do: [:i | 1 to: 4 do: [:j| temp := temp + ((result at: i-1*4+j) squared)]].
	^temp sqrt.! !

!Float64Matrix4x4 methodsFor: 'accessing'!
translation

	^(Float64Vector3 x: self a14 y: self a24 z: self a34)! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:36'!
translation: aVector

	^self translationX: aVector x y: aVector y z: aVector z! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:36'!
translationX: xValue y: yValue z: zValue

	self a14: xValue.
	self a24: yValue.
	self a34: zValue.
	^self! !

!Float64Matrix4x4 methodsFor: 'transforming' stamp: 'jmv 2/26/2015 13:25'!
transposed
	"Return a transposed copy of the receiver"
	| matrix |
	"It would be nice to have a plugin for this...
	<primitive: 'Float64TransposeMatrix' module: 'Squeak3D'>"
	self flag: #Float64Primitive.

	matrix := self class new.
	matrix 
		a11: self a11; a12: self a21; a13: self a31; a14: self a41;
		a21: self a12; a22: self a22; a23: self a32; a24: self a42;
		a31: self a13; a32: self a23; a33: self a33; a34: self a43;
		a41: self a14; a42: self a24; a43: self a34; a44: self a44.
	^matrix! !

!Float64Matrix4x4 methodsFor: 'accessing' stamp: 'jsp 2/25/1999 13:58'!
trotation
	"Return the angular rotation around each axis of the matrix"

	| cp sp cy sy cr sr vAngles |

	vAngles := Float64Vector3 new.

	((self a13) = 0) ifTrue: [ ((self a33) >= 0)  ifTrue: [ vAngles at: 2 put: 0.
													  cr := (self a11).
													  sr := (self a12).
													  cp := (self a33). ]
											 ifFalse: [ vAngles at: 2 put: (Float pi).
														cr := (self a11) negated.
														sr := (self a12) negated.
														cp := (self a33) negated. ]
							]
					ifFalse: [
								vAngles at: 2 put: (((self a13) negated) arcTan: (self a33)).
								cy := (vAngles at: 3) cos.
								sy := (vAngles at: 3) sin.
								cr := (cy * (self a11)) + (sy * (self a31)).
								sr := (cy* (self a12)) + (sy * (self a32)).
								cp := (cy * (self a33)) - (sy * (self a13)).
							].

	sp := (self a23).
 
	vAngles at: 1 put: (sp arcTan: cp).
	vAngles at: 3 put: (sr arcTan: cr).

	vAngles at: 1 put: ((vAngles at: 1) radiansToDegrees).
	vAngles at: 2 put: ((vAngles at: 2) radiansToDegrees).
	vAngles at: 3 put: ((vAngles at: 3) radiansToDegrees).

	^ vAngles.
! !

!Float64Matrix4x4 class methodsFor: 'instance creation'!
identity
	^self new setIdentity! !

!Float64Matrix4x4 class methodsFor: 'class initialization' stamp: 'jmv 11/24/2015 09:52'!
initialize
	"Float64Matrix4x4 initialize"
	Float64Matrix4x4Zero := self new.
	Float64Matrix4x4Identity := self new.
	Float64Matrix4x4Identity a11: 1.0; a22: 1.0; a33: 1.0; a44: 1.0.! !

!Float64Matrix4x4 class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:25'!
numElements
	^16! !

!Float64Matrix4x4 class methodsFor: 'instance creation' stamp: 'jmv 10/11/2016 10:14:52'!
rotatedBy: angle around: axis centeredAt: origin
	"Create a matrix rotating points around the given origin using the angle/axis pair"
	| xform |
	xform := self withOffset: origin negated.
	xform := xform composedWithGlobal:(Float64Quaternion angle: angle axis: axis) asMatrix4x4.
	xform := xform composedWithGlobal: (self withOffset: origin).
	^xform! !

!Float64Matrix4x4 class methodsFor: 'instance creation' stamp: 'das 5/16/2002 11:12'!
skew: vector

	^ self new skew: vector.! !

!Float64Matrix4x4 class methodsFor: 'instance creation' stamp: 'ar 2/15/1999 23:48'!
withOffset: amount
	^self identity setTranslation: amount! !

!Float64Matrix4x4 class methodsFor: 'instance creation' stamp: 'ar 5/10/2001 15:27'!
withRotation: angle around: axis
	^self new rotation: angle around: axis! !

!Float64Matrix4x4 class methodsFor: 'instance creation' stamp: 'ar 5/20/2001 00:12'!
withScale: amount
	^self identity setScale: amount! !

!Float64Matrix4x4 class methodsFor: 'instance creation'!
zero
	^self new! !

!Float64Quaternion methodsFor: 'arithmetic' stamp: 'jmv 10/11/2016 10:14:55'!
* aRotation
	"Multiplying two rotations is the same as concatenating the two rotations."
	| v1 v2 v3 vv |
	v1 := self vectorPart * aRotation a.
	v2 := aRotation vectorPart * self a.
	v3 := aRotation vectorPart cross: self vectorPart.
	vv := v1 + v2 + v3.
	^Float64Quaternion
		a: (self a * aRotation a) - (self vectorPart dot: aRotation vectorPart)
		b: vv x
		c: vv y
		d: vv z! !

!Float64Quaternion methodsFor: 'accessing' stamp: 'jmv 10/11/2016 10:14:25'!
a
	^self at: 1! !

!Float64Quaternion methodsFor: 'accessing' stamp: 'jmv 10/11/2016 10:14:25'!
angle
	^(self a arcCos * 2.0 radiansToDegrees)! !

!Float64Quaternion methodsFor: 'converting' stamp: 'jmv 10/11/2016 10:14:26'!
asMatrix3x3
	"Answer a Float64Matrix3x3 that does the same rotation as us"
	| m |
	m _ Float64Matrix3x3 new.
	self asMatrixOn: m.
	^m
! !

!Float64Quaternion methodsFor: 'converting' stamp: 'jmv 10/11/2016 10:14:26'!
asMatrix4x4
	"Answer a Float64Matrix4x4 that does the same rotation as us"
	| m |
	m _ Float64Matrix4x4 identity.
	self asMatrixOn: m.
	^m
! !

!Float64Quaternion methodsFor: 'converting' stamp: 'jmv 10/11/2016 10:14:26'!
asMatrixOn: m
	"Given a quaternion q = (a, [ b , c , d]) the rotation matrix can be calculated as
				|	1 -	2(cc+dd),			2(bc-da),				2(db+ca)	|
		m =	|		2(bc+da),	1 - 	2(bb+dd),			2(cd-ba)	|
				|		2(db-ca),			2(cd+ba),		1 -	2(bb+cc)	|
	"
	| a b c d bb cc dd bc cd db ba ca da |
	a _ self a. b _ self b. c _ self c. d _ self d.
	bb _ (b * b).	cc _ (c * c).	dd _ (d * d).
	bc _ (b * c).	cd _ (c * d).	db _ (d * b).
	ba _ (b * a).	ca _ (c * a).	da _ (d * a).
	m 
		a11: 1.0 - (cc + dd * 2.0);	a12: (bc - da * 2.0); 			a13: (db + ca * 2.0);
		a21: (bc + da * 2.0);		a22: 1.0 - (bb + dd * 2.0);	a23: (cd - ba * 2.0);
		a31: (db - ca * 2.0);			a32: (cd + ba * 2.0);		a33: 1.0 - (bb + cc * 2.0)! !

!Float64Quaternion methodsFor: 'accessing' stamp: 'jmv 10/11/2016 10:14:26'!
axis
	| sinAngle |
	sinAngle := self a arcCos sin.
	sinAngle isZero ifTrue:[^Float64Vector3 zero].
	^Float64Vector3 
		x: (self b / sinAngle)
		y: (self c / sinAngle)
		z: (self d / sinAngle)! !

!Float64Quaternion methodsFor: 'accessing' stamp: 'jmv 10/11/2016 10:14:26'!
b
	^self at: 2! !

!Float64Quaternion methodsFor: 'rotating' stamp: 'jmv 10/11/2016 10:40:10'!
bodyToInertial: vectorInSpacecraftFrame
	"See class comment

	http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/geometric/quaternionTraining001.pdf

	Vector Rotation:
		We know a Vector in the Spacecraft Frame,
		Where is it pointing in Inertial Frame?
		(eg: Given Spacecraft Attitude and Spacecraft Frame Star Tracker Vector,
		Where is the Star Tracker Pointed in Inertial Frame?)

		Vs: Vector in Spacecraft Frame (argument)
		Vi: Vector in Inertial Frame (answer)
		Q: Spacecraft Attitude Quaternion (receiver)
		Q*: Quaternion conjugate (receiver conjugate)

	Vi = Q* . Vs .  Q
	"
	"If we were able to represent any quaternion, and not just unit quaternions, this would be correct:
	^(self conjugate * vectorInSpacecraftFrame asPureQuaternion * self) vectorPart
	"

	"This should be inlined!!!!!!!!!!!!"
	^(self conjugate * vectorInSpacecraftFrame asPureQuaternion * self) vectorPart * vectorInSpacecraftFrame length! !

!Float64Quaternion methodsFor: 'accessing' stamp: 'jmv 10/11/2016 10:14:26'!
c
	^self at: 3! !

!Float64Quaternion methodsFor: 'arithmetic' stamp: 'jmv 10/11/2016 10:14:56'!
conjugate
	^Float64Quaternion
		a: self a
		b: self b negated
		c: self c negated
		d: self d negated! !

!Float64Quaternion methodsFor: 'accessing' stamp: 'jmv 10/11/2016 10:14:26'!
d
	^self at: 4! !

!Float64Quaternion methodsFor: 'rotating' stamp: 'jmv 10/11/2016 10:40:01'!
inertialToBody: vectorInInertialFrame
	"See class comment

	http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/geometric/quaternionTraining001.pdf

	Coordinate Transformation:
		We know a Vector in the Inertial Frame,
		What is it in the Spacecraft Frame?
		(eg: Given Spacecraft Attitude, Earth and Spacecraft Ephemeris,
		Where do we point de Payload relative to the Spacecraft?)

		Vs: Vector in Spacecraft Frame (answer)
		Vi: Vector in Inertial Frame (argument)
		Q: Spacecraft Attitude Quaternion (receiver)
		Q*: Quaternion conjugate (receiver conjugate)

	Vs = Q . Vi . Q*
	"
	"If we were able to represent any quaternion, and not just unit quaternions, this would be correct:
	^(self * vectorInInertialFrame asPureQuaternion * self conjugate) vectorPart
	"

	"This should be inlined!!!!!!!!!!!!"
	^(self * vectorInInertialFrame asPureQuaternion * self conjugate) vectorPart * vectorInInertialFrame length! !

!Float64Quaternion methodsFor: 'interpolating' stamp: 'jmv 10/11/2016 10:14:26'!
interpolateTo: aRotation at: t
	"Spherical linear interpolation (slerp) from the receiver to aQuaternion"
	^self slerpTo: aRotation at: t extraSpins: 0! !

!Float64Quaternion methodsFor: 'arithmetic' stamp: 'jmv 10/11/2016 10:14:58'!
negated
	"Negating a quaternion is the same as reversing the angle of rotation"
	^Float64Quaternion
		a: self a negated
		b: self b
		c: self c
		d: self d! !

!Float64Quaternion methodsFor: 'arithmetic' stamp: 'jmv 10/11/2016 10:14:25'!
normalize
	"Normalize the receiver. Note that the actual angle (a) determining the amount of 
	rotation is fixed, since we do not want to modify angles. This leads to:
		a^2 + b^2 + c^2 + d^2 = 1.
		b^2 + c^2 + d^2 = 1 - a^2.
	Note also that the angle (a) can not exceed 1.0 (due its creation by cosine) and
	if it is 1.0 we have exactly the unit quaternion ( 1, [ 0, 0, 0]).
	"
	| oneMinusASquared length |
	"If already normalized, don't bother"
	self squaredLength = 1.0 ifTrue: [
		^self ].
	
	oneMinusASquared := 1.0 - (self a squared).
	oneMinusASquared < 1.0e-10 ifTrue: [
		" ^self setIdentity."
		"Almost identity. Can not do angle preserving normalization with good accuracy.
		Just normalize, perhaps very slightly modifying angle. Sort of preserve axis."
		^ super normalize ].

	length := ((self b squared + self c squared + self d squared) / oneMinusASquared) sqrt.
	length = 0.0 ifTrue: [ ^self setIdentity ].
	self at: 2 put: self b / length.
	self at: 3 put: self c / length.
	self at: 4 put: self d / length! !

!Float64Quaternion methodsFor: 'converting' stamp: 'jmv 10/11/2016 10:14:26'!
normalized
	"Our instances are always normalized... This is not really needed...
	(well, except someone calls: #a:, #b:, #c:, #d: or #loadFrom:)"
	^self copy normalize! !

!Float64Quaternion methodsFor: 'printing' stamp: 'jmv 10/11/2016 10:16:06'!
printAbcd
	"Alternative way of printing a quaternion
		(Float64Quaternion q1: -0.120340 q2: 0.399205 q3: 0.906501 q4: -0.066405) printAbcd 
			'(-0.066405, -0.120340, 0.399205, 0.906501)' 
	"
	^String streamContents: [ :strm |
		strm nextPut: $(.
		self a printOn: strm fractionDigits: 6.
		strm nextPutAll: ', '.
		self b printOn: strm fractionDigits: 6.
		strm nextPutAll: ', '.
		self c printOn: strm fractionDigits: 6.
		strm nextPutAll: ', '.
		self d printOn: strm fractionDigits: 6.
		strm nextPut: $) ]! !

!Float64Quaternion methodsFor: 'printing' stamp: 'jmv 10/11/2016 10:14:26'!
printOn: aStream

	aStream 
		nextPutAll: self class name;
		nextPut:$(;
		print: self angle;
		nextPut: Character space;
		print: self axis;
		nextPut:$).! !

!Float64Quaternion methodsFor: 'printing' stamp: 'jmv 10/11/2016 10:14:27'!
printq1q2q3q4
	^String streamContents: [ :strm |
		strm nextPut: $(.
		self b printOn: strm fractionDigits: 6.
		strm nextPutAll: ', '.
		self c printOn: strm fractionDigits: 6.
		strm nextPutAll: ', '.
		self d printOn: strm fractionDigits: 6.
		strm nextPutAll: ', '.
		self a printOn: strm fractionDigits: 6.
		strm nextPut: $) ]! !

!Float64Quaternion methodsFor: 'rotating' stamp: 'jmv 10/11/2016 10:43:28'!
rotate: aFloat64Vector3
	"
	Apply a rotation to some vector. For instance rotating around the z axis is
	a standard (counterclockwise) rotation on the XY plane
		(Float64Quaternion axis: 0@0@1 angle: 30) rotate:1@0@0
			-> a Float64Vector3(0.866 0.5 0.0)

	"
	
	"Two equivalent (?) definitions"
	"
	^self asMatrix4x4 localDirToGlobal: aFloat64Vector3
	"

	"If we were able to represent any quaternion, and not just unit quaternions, this would be correct:
	^(self conjugate * aFloat64Vector3 asPureQuaternion * self) vectorPart
	"

	^(self conjugate * aFloat64Vector3 asPureQuaternion * self) vectorPart * aFloat64Vector3 length
	"See class comment"! !

!Float64Quaternion methodsFor: 'private' stamp: 'jmv 10/11/2016 10:16:20'!
setA: aValue b: bValue c: cValue d: dValue

	self at: 1 put: aValue.
	self at: 2 put: bValue.
	self at: 3 put: cValue.
	self at: 4 put: dValue.
	"
	(Float64Quaternion axis: 0@0@1 angle: 0.01) rotate: 0@0@ -1.
	should answer 0@0@-1,
	but was answering 0@0@1
	because of this.
	It is subtle, but a conjugate must be a conjugate, etc. In this particular case, it was the product #*
	(jmv)
	"
	"(aValue < 0.0) ifTrue:[self *= -1.0]."
	
	self normalize! !

!Float64Quaternion methodsFor: 'initialize' stamp: 'jmv 10/11/2016 10:18:03'!
setIdentity
	^self loadFrom: Identity! !

!Float64Quaternion methodsFor: 'interpolating' stamp: 'jmv 10/11/2016 10:14:26'!
slerpTo: aRotation at: t
	"Spherical linear interpolation (slerp) from the receiver to aQuaternion"
	^self slerpTo: aRotation at: t extraSpins: 0! !

!Float64Quaternion methodsFor: 'interpolating' stamp: 'jmv 10/11/2016 10:15:01'!
slerpTo: aRotation at: t extraSpins: spin
	"Sperical Linear Interpolation (slerp).
	Calculate the new quaternion when applying slerp from the receiver (t = 0.0)
	to aRotation (t = 1.0). spin indicates the number of extra rotations to be added.
	The code shown below is from Graphics Gems III"
	| cosT alpha beta flip theta phi sinT |
	alpha := t.
	flip := false.
	"calculate the cosine of the two quaternions on the 4d sphere"
	cosT := self dot: aRotation.
	"if aQuaternion is on the opposite hemisphere reverse the direction
	(note that in quaternion space two points describe the same rotation)"
	cosT < 0.0 ifTrue:[
		flip := true.
		cosT := cosT negated].
	"If the aQuaternion is nearly the same as I am use linear interpolation"
	cosT > 0.99999 ifTrue:[
		"Linear Interpolation"
		beta := 1.0 - alpha
	] ifFalse:[
		"Spherical Interpolation"
		theta := cosT arcCos.
		phi := (spin * Float pi) + theta.
		sinT := theta sin.
		beta := (theta - (alpha * phi)) sin / sinT.
		alpha := (alpha * phi) sin / sinT].

	flip ifTrue:[alpha := alpha negated].
	^Float64Quaternion 
		a: (alpha * aRotation a) + (beta * self a)
		b: (alpha * aRotation b) + (beta * self b)
		c: (alpha * aRotation c) + (beta * self c)
		d: (alpha * aRotation d) + (beta * self d)! !

!Float64Quaternion methodsFor: 'accessing' stamp: 'jmv 10/11/2016 10:14:26'!
turns
	"Answer the number of turns"
	^self angle / 360.0! !

!Float64Quaternion methodsFor: 'private' stamp: 'jmv 10/11/2016 10:14:27'!
vectorPart
	^Float64Vector3 x: self b y: self c z: self d! !

!Float64Quaternion class methodsFor: 'instance creation' stamp: 'jmv 10/11/2016 10:14:27'!
a: aValue b: bValue c: cValue d: dValue
	^self new setA: aValue b: bValue c: cValue d: dValue! !

!Float64Quaternion class methodsFor: 'instance creation' stamp: 'jmv 10/11/2016 10:14:27'!
angle: anAngle axis: aVector3

	^self radiansAngle: anAngle degreesToRadians axis: aVector3! !

!Float64Quaternion class methodsFor: 'instance creation' stamp: 'jmv 10/11/2016 10:14:27'!
axis: aVector3 angle: anAngle
	^self angle: anAngle axis: aVector3! !

!Float64Quaternion class methodsFor: 'instance creation' stamp: 'jmv 10/11/2016 10:14:27'!
from: startVector to: endVector
	"Create a rotation from startVector to endVector. 
	Vectors should be normalized first."
	| axis cos sin someNonColinearVector somePerpendicularVector |
	startVector = endVector ifTrue: [^ self identity].

	axis := startVector cross: endVector.
	cos := ((1 + (startVector dot: endVector)) / 2) sqrt.	"half-angle relation"
	sin := cos isZero 
				ifTrue: [
					"180 degree rotation"
					someNonColinearVector _ 1@0@0.
					somePerpendicularVector _ someNonColinearVector cross: startVector normalized.
					somePerpendicularVector length < 0.2 ifTrue: [
						someNonColinearVector _ 0@1@0.
						somePerpendicularVector _ someNonColinearVector cross: startVector ].
					^ self angle: 180 axis: somePerpendicularVector]
				ifFalse: [axis length / 2 / cos].			"double angle relation"
	axis safelyNormalize.
	^self a: cos b: axis x * sin c: axis y * sin d: axis z * sin. ! !

!Float64Quaternion class methodsFor: 'instance creation' stamp: 'jmv 10/11/2016 10:14:27'!
identity
	^self new setIdentity! !

!Float64Quaternion class methodsFor: 'class initialization' stamp: 'jmv 10/11/2016 10:18:12'!
initialize
	"
	Float64Quaternion initialize
	"
	Identity := self new.
	Identity floatAt: 1 put: 1.0.! !

!Float64Quaternion class methodsFor: 'instance creation' stamp: 'jmv 10/11/2016 10:14:27'!
numElements
	^4! !

!Float64Quaternion class methodsFor: 'instance creation' stamp: 'jmv 10/11/2016 10:16:47'!
q1: q1 q2: q2 q3: q3 q4: q4
	"As in STK and some tutorials
		http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/geometric/quaternionTraining001.pdf
	
	The quaternions represent the rotation from the reference coordinate frame to the vehicle's body coordinate frame.
	http://www.agi.com/resources/help/online/stk/10.1/index.html?page=source%2Fstk%2Fimportfiles-01.htm
	Float64Quaternion q1: -0.120340 q2: 0.399205 q3: 0.906501 q4: -0.066405
	"
	^self a: q4 b: q1 c: q2 d: q3! !

!Float64Quaternion class methodsFor: 'instance creation' stamp: 'jmv 10/11/2016 10:14:27'!
radiansAngle: anAngle axis: aVector3
	"http://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation
	The rotation is clockwise if our line of sight points in the same direction as aVector3."

	| angle sin cos |
	angle := anAngle / 2.0.
	cos := angle cos.
	sin := angle sin.
	^self a: cos b: aVector3 x * sin c: aVector3 y * sin d: aVector3 z * sin! !

!Float64Quaternion class methodsFor: 'instance creation' stamp: 'jmv 10/11/2016 10:14:27'!
x: xValue y: yValue z: zValue a: anAngle

	| angle sin cos |
	angle := (anAngle degreesToRadians) / 2.0.
	cos := angle cos.
	sin := angle sin.
	^self a: cos b: xValue * sin c: yValue * sin d: zValue * sin! !

!Float64Vector2 methodsFor: 'converting' stamp: 'ar 2/13/1999 20:03'!
asPoint
	^self x @ self y! !

!Float64Vector2 methodsFor: 'accessing' stamp: 'ar 2/6/1999 23:26'!
u
	^self floatAt: 1! !

!Float64Vector2 methodsFor: 'accessing' stamp: 'ar 2/6/1999 23:27'!
u: aFloat
	self floatAt: 1 put: aFloat! !

!Float64Vector2 methodsFor: 'initialize' stamp: 'ar 2/6/1999 23:30'!
u: uValue v: vValue
	self u: uValue.
	self v: vValue.! !

!Float64Vector2 methodsFor: 'accessing' stamp: 'ar 2/6/1999 23:27'!
v
	^self floatAt: 2! !

!Float64Vector2 methodsFor: 'accessing' stamp: 'ar 2/6/1999 23:27'!
v: aFloat
	self floatAt: 2 put: aFloat! !

!Float64Vector2 methodsFor: 'accessing' stamp: 'ar 2/7/1999 02:58'!
x
	^self at: 1! !

!Float64Vector2 methodsFor: 'accessing' stamp: 'ar 5/4/2000 16:00'!
x: aFloat
	self floatAt: 1 put: aFloat! !

!Float64Vector2 methodsFor: 'initialize' stamp: 'ar 5/4/2000 15:50'!
x: uValue y: vValue
	self x: uValue.
	self y: vValue.! !

!Float64Vector2 methodsFor: 'accessing' stamp: 'ar 2/7/1999 02:58'!
y
	^self at: 2! !

!Float64Vector2 methodsFor: 'accessing' stamp: 'ar 5/4/2000 16:00'!
y: aFloat
	self floatAt: 2 put: aFloat! !

!Float64Vector2 class methodsFor: 'instance creation' stamp: 'ar 2/6/1999 23:31'!
numElements
	^2! !

!Float64Vector2 class methodsFor: 'instance creation' stamp: 'ar 2/6/1999 23:31'!
u: uValue v: vValue
	^self new u: uValue v: vValue! !

!Float64Vector2 class methodsFor: 'instance creation' stamp: 'ar 5/4/2000 15:49'!
x: uValue y: vValue
	^self new x: uValue y: vValue! !

!Float64Vector3 methodsFor: 'converting' stamp: 'jmv 11/3/2016 16:01:12'!
@ aNumber
	^Float64Vector4 x: self x y: self y z: self z w: aNumber! !

!Float64Vector3 methodsFor: 'vector functions' stamp: 'jmv 10/26/2016 18:04:01'!
angleTo: aVector 
	"calculate the rotation angle that rotates this vector into aVector. I return the value in terms of radians."
	| cosine |

	"In many cases doing v dot: v with v normalized gives an answer sligtly above 1.0, because of rounding.
	Actually asking for cosine > 1.0 is enough."
	cosine _ (self dot: aVector) / self length / aVector length.
	cosine > 1.0 ifTrue: [
		^ 0.0 ].

	^ cosine arcCos! !

!Float64Vector3 methodsFor: 'converting' stamp: 'ar 2/6/1999 00:06'!
asFloat64Vector3
	^self! !

!Float64Vector3 methodsFor: 'converting' stamp: 'ar 2/6/1999 00:07'!
asFloat64Vector4
	^Float64Vector4 x: self x y: self y z: self z w: 1.0! !

!Float64Vector3 methodsFor: 'converting' stamp: 'jmv 11/26/2015 15:40'!
asIEEE32BitPrecisionFloat
	| answer |
	answer _ Float32Vector3 new.
	1 to: 3 do: [ :i | answer at: i put: (self at: i) ].
	^answer! !

!Float64Vector3 methodsFor: 'converting' stamp: 'ar 10/18/2002 16:30'!
asMoveDirection
	^self! !

!Float64Vector3 methodsFor: 'converting' stamp: 'ar 5/23/2001 00:24'!
asPoint
	^self x @ self y! !

!Float64Vector3 methodsFor: 'converting' stamp: 'jmv 10/11/2016 10:15:16'!
asPureQuaternion
	"Answer a Quaternion (an instance of Float64Quaternion) with real part zero."

	^Float64Quaternion q1: self x q2: self y q3: self z q4: 0.0! !

!Float64Vector3 methodsFor: 'converting' stamp: 'ar 10/18/2002 16:30'!
asTurnDirection
	^self! !

!Float64Vector3 methodsFor: 'vector functions'!
cross: aVector 
	"calculate the cross product from the receiver with aVector"
	^self species
		x: self y * aVector z - (aVector y * self z)
		y: self z * aVector x - (aVector z * self x)
		z: self x * aVector y - (aVector x * self y)! !

!Float64Vector3 methodsFor: 'vector functions' stamp: 'ar 2/11/2003 18:41'!
distanceTo: aVector
	^(self - aVector) length! !

!Float64Vector3 methodsFor: 'interpolating' stamp: 'jsp 2/9/1999 11:17'!
interpolateTo: end at: amountDone
	"Interpolates a new vector based on the instance vector, the end state vector, and the amount already done (between 0 and 1)."

	| tX tY tZ |
	tX := self x.
	tY := self y.
	tZ := self z.

	^ (Float64Vector3 x: (tX + (((end x) - tX) * amountDone))
				y: (tY + (((end y) - tY) * amountDone))
				z: (tZ + (((end z) - tZ) * amountDone))).
! !

!Float64Vector3 methodsFor: 'testing' stamp: 'laza 3/16/2000 16:30'!
isZero
	^self = Float64Vector3 zero! !

!Float64Vector3 methodsFor: 'vector functions'!
length: newLength
	self safelyNormalize *= newLength! !

!Float64Vector3 methodsFor: 'vector functions' stamp: 'ar 2/6/1999 00:32'!
max: aVector
	^Float64Vector3 
		x: (self x max: aVector x)
		y: (self y max: aVector y)
		z: (self z max: aVector z)! !

!Float64Vector3 methodsFor: 'vector functions' stamp: 'ar 2/6/1999 00:31'!
min: aVector
	^Float64Vector3 
		x: (self x min: aVector x)
		y: (self y min: aVector y)
		z: (self z min: aVector z)! !

!Float64Vector3 methodsFor: 'vector functions'!
normalize
	self /= self length! !

!Float64Vector3 methodsFor: 'vector functions'!
normalized
	^self / self length! !

!Float64Vector3 methodsFor: 'private' stamp: 'jmv 3/16/2016 14:18'!
printOn: aStream 
	"The receiver prints on aStream in terms of infix notation."

	self x printOn: aStream.
	aStream nextPut: $@.
	self y printOn: aStream.
	aStream nextPut: $@.
	self z printOn: aStream! !

!Float64Vector3 methodsFor: 'private'!
privateLoadFrom: srcObject
	self x: srcObject x y: srcObject y z: srcObject z.! !

!Float64Vector3 methodsFor: 'vector functions' stamp: 'jmv 11/24/2015 10:16'!
rotationTo: aVector 
	"calculate the rotation matrix that rotates this vector into aVector. From 'Real-Time Rendering' by Moller and Haines, pgs. 50-52.
	We could also answer a Float64Matrix3x3..."
	| m v e h s t |
	s := self normalized.
	t := aVector normalized.
	v := s cross: t.
	e := s dot: t.
	h := (1-e)/(v dot: v).
	m := Float64Matrix4x4 new.
	m a11: e + (h * v x * v x).
	m a12: (h * v x * v y) - v z.
	m a13: (h * v x * v z) + v y.
	m a21: (h * v x * v y) + v z.
	m a22: e + (h * v y * v y).
	m a23: (h * v y * v z) - v x.
	m a31: (h * v x * v z) - v y.
	m a32: (h * v y * v z) + v x.
	m a33: e + (h * v z * v z).
	m a44: 1.0.
	^ m
! !

!Float64Vector3 methodsFor: 'vector functions' stamp: 'ar 2/7/1999 00:43'!
safelyNormalize
	"Safely normalize the receiver, e.g. check if the length is non-zero"
	| length |
	length := self length.
	length = 1.0 ifTrue:[^self].
	length = 0.0 ifFalse:[self /= length].! !

!Float64Vector3 methodsFor: 'vector functions'!
safelyNormalized
	"Safely normalize the receiver, e.g. check if the length is non-zero"
	^self copy safelyNormalize! !

!Float64Vector3 methodsFor: 'vector functions' stamp: 'jmv 10/26/2016 18:19:06'!
scalarProjectionOnto: aVector 
	"https://en.wikipedia.org/wiki/Vector_projection
	1@2@3 scalarProjectionOnto: 10@0@0. 
		1.0
	1@2@3 scalarProjectionOnto: 0@1.23@0.
		2.0 
	1@2@3 scalarProjectionOnto: 0@0@Float pi.
		3.0.
	"

	^(self dot: aVector) / aVector length! !

!Float64Vector3 methodsFor: 'vector functions'!
squaredLength: newLength
	self length: newLength sqrt! !

!Float64Vector3 methodsFor: 'vector functions' stamp: 'jmv 10/26/2016 18:19:53'!
vectorProjectionOnto: aVector 
	"https://en.wikipedia.org/wiki/Vector_projection
	1@2@3 vectorProjectionOnto: 10@0@0.
		1.0@0.0@0.0 
	1@2@3 vectorProjectionOnto: 0@1.23@0.
		0.0@2.0@0.0 
	1@2@3 vectorProjectionOnto: 0@0@Float pi.
		0.0@0.0@3.0 
	"

	^(self dot: aVector) / aVector length squared * aVector! !

!Float64Vector3 methodsFor: 'vector functions' stamp: 'jmv 10/26/2016 18:20:37'!
vectorRejectionOnto: aVector
	"https://en.wikipedia.org/wiki/Vector_projection
	1@2@3 vectorRejectionOnto: 10@0@0.
		0.0@2.0@3.0 
	1@2@3 vectorRejectionOnto: 0@1.23@0.
		1.0@0.0@3.0 
	1@2@3 vectorRejectionOnto: 0@0@Float pi.
		1.0@2.0@0.0 
	"

	^ self - (self vectorProjectionOnto: aVector )! !

!Float64Vector3 methodsFor: 'enumerating' stamp: 'ar 5/20/2001 00:05'!
vertexPositionsDo: aBlock
	aBlock value: self! !

!Float64Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
x
	^self at: 1! !

!Float64Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
x: aFloat
	self at: 1 put: aFloat! !

!Float64Vector3 methodsFor: 'initialize'!
x: x y: y z: z
	self x: x.
	self y: y.
	self z: z.! !

!Float64Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
y
	^self at: 2! !

!Float64Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
y: aFloat
	self at: 2 put: aFloat! !

!Float64Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
z
	^self at: 3! !

!Float64Vector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!
z: aFloat
	self at: 3 put: aFloat! !

!Float64Vector3 class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:23'!
numElements
	^3! !

!Float64Vector3 class methodsFor: 'instance creation' stamp: 'ar 2/15/1999 02:56'!
value: aFloat
	^self x: aFloat y: aFloat z: aFloat! !

!Float64Vector3 class methodsFor: 'instance creation'!
x: x y: y z: z
	^self new x: x y: y z: z! !

!Float64Vector3 class methodsFor: 'instance creation'!
zero
	^self new! !

!Float64Vector4 methodsFor: 'converting' stamp: 'ar 2/6/1999 00:08'!
asFloat64Vector3
	| wValue |
	wValue := self w.
	wValue = 0.0 ifTrue:[^Float64Vector3 zero].
	^Float64Vector3 x: self x / wValue y: self y / wValue z: self z / wValue! !

!Float64Vector4 methodsFor: 'converting' stamp: 'ar 2/6/1999 00:07'!
asFloat64Vector4
	^self! !

!Float64Vector4 methodsFor: 'private'!
privateLoadFrom: srcObject
	self x: srcObject x y: srcObject y z: srcObject z w: srcObject w.! !

!Float64Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!
w
	^self at: 4! !

!Float64Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!
w: aFloat
	self at: 4 put: aFloat! !

!Float64Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!
x
	^self at: 1! !

!Float64Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!
x: aFloat
	self at: 1 put: aFloat! !

!Float64Vector4 methodsFor: 'initialize'!
x: x y: y z: z w: w
	self x: x.
	self y: y.
	self z: z.
	self w: w.! !

!Float64Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!
y
	^self at: 2! !

!Float64Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!
y: aFloat
	self at: 2 put: aFloat! !

!Float64Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!
z
	^self at: 3! !

!Float64Vector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!
z: aFloat
	self at: 3 put: aFloat! !

!Float64Vector4 class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:21'!
numElements
	^4! !

!Float64Vector4 class methodsFor: 'instance creation'!
x: x y: y z: z
	^self x: x y: y z: z w: 1.0! !

!Float64Vector4 class methodsFor: 'instance creation'!
x: x y: y z: z w: w
	^self new x: x y: y z: z w: w! !

!Float64Vector4 class methodsFor: 'instance creation'!
zero
	^self new! !

!Float32Matrix3x3 methodsFor: 'arithmetic' stamp: 'jmv 11/24/2015 16:33'!
* aFloatVector3
	"Answer the result of multiplying self by an argument vector"
	| x y z rx ry rz |
	"It would be nice to have a plugin for this...
	<primitive: 'Float32MultiplyByVector' module: 'Squeak3D'>"
	self flag: #Float32Primitive.

	x := aFloatVector3 x.
	y := aFloatVector3 y.
	z := aFloatVector3 z.

	rx := (x * self a11) + (y * self a12) + (z * self a13).
	ry := (x * self a21) + (y * self a22) + (z * self a23).
	rz := (x * self a31) + (y * self a32) + (z * self a33).

	^Float32Vector3 x: rx y: ry z: rz! !

!Float32Matrix3x3 methodsFor: 'arithmetic' stamp: 'jmv 11/24/2015 16:33'!
+ aFloat32Matrix
	"Optimized for Matrix/Matrix operations"
	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>"
	self flag: #Float32Primitive.

	^super + aFloat32Matrix! !

!Float32Matrix3x3 methodsFor: 'arithmetic' stamp: 'jmv 11/24/2015 16:34'!
- aFloat32Matrix
	"Optimized for Matrix/Matrix operations"
	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>"
	self flag: #Float32Primitive.

	^super - aFloat32Matrix! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a11
	"Return the element a11"
	^self at: 1! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a11: aNumber
	"Store the element a11"
	self at: 1 put: aNumber! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a12
	"Return the element a12"
	^self at: 2! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a12: aNumber
	"Store the element a12"
	self at: 2 put: aNumber! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a13
	"Return the element a13"
	^self at: 3! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a13: aNumber
	"Store the element a13"
	self at: 3 put: aNumber! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a21
	"Return the element a21"
	^self at: 4! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a21: aNumber
	"Store the element a21"
	self at: 4 put: aNumber! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a22
	"Return the element a22"
	^self at: 5! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a22: aNumber
	"Store the element a22"
	self at: 5 put: aNumber! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a23
	"Return the element a23"
	^self at: 6! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a23: aNumber
	"Store the element a23"
	self at: 6 put: aNumber! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a31
	"Return the element a31"
	^self at: 7! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a31: aNumber
	"Store the element a31"
	self at: 7 put: aNumber! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a32
	"Return the element a32"
	^self at: 8! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a32: aNumber
	"Store the element a32"
	self at: 8 put: aNumber! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a33
	"Return the element a33"
	^self at: 9! !

!Float32Matrix3x3 methodsFor: 'element-access' stamp: 'jmv 11/24/2015 16:30'!
a33: aNumber
	"Store the element a33"
	self at: 9 put: aNumber! !

!Float32Matrix3x3 methodsFor: 'accessing' stamp: 'jmv 11/24/2015 16:30'!
at: i at: j
	^ self at: ((i - 1) * 3 + j).
! !

!Float32Matrix3x3 methodsFor: 'accessing' stamp: 'jmv 11/24/2015 16:30'!
at: i at: j put: aValue
	^ self at: ((i - 1) * 3 + j) put: aValue.
! !

!Float32Matrix3x3 methodsFor: 'testing' stamp: 'jmv 11/24/2015 16:31'!
isIdentity
	^self = Float32Matrix3x3Identity! !

!Float32Matrix3x3 methodsFor: 'testing' stamp: 'jmv 11/24/2015 16:31'!
isZero
	^self = Float32Matrix3x3Zero! !

!Float32Matrix3x3 methodsFor: 'transforming' stamp: 'jmv 11/24/2015 16:30'!
localDirToGlobal: aVector
	"Multiply direction vector with the receiver"

	^self * aVector! !

!Float32Matrix3x3 methodsFor: 'printing' stamp: 'jmv 11/24/2015 16:30'!
print
	self printOn: Transcript.
	Transcript newLine! !

!Float32Matrix3x3 methodsFor: 'printing' stamp: 'jmv 11/24/2015 16:30'!
printOn: aStream
	aStream newLine.
	1 to: 3 do: [ :i |
		aStream nextPutAll: '| '.
		1 to: 3 do: [ :j |
			(self at: i at: j) printOn: aStream integerDigits: 1 fractionDigits: 3.
			aStream space ].
		aStream nextPut: $|; newLine ]! !

!Float32Matrix3x3 methodsFor: 'initialize' stamp: 'jmv 11/24/2015 16:31'!
setIdentity
	"Set the receiver to the identity matrix"
	self loadFrom: Float32Matrix3x3Identity! !

!Float32Matrix3x3 methodsFor: 'initialize' stamp: 'jmv 11/24/2015 16:30'!
setScale: aVector
	self 
		a11: aVector x;
		a22: aVector y;
		a33: aVector z! !

!Float32Matrix3x3 methodsFor: 'initialize' stamp: 'jmv 11/24/2015 16:31'!
setZero
	"Set the receiver to the zero matrix"
	self loadFrom: Float32Matrix3x3Zero! !

!Float32Matrix3x3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 16:30'!
identity
	^self new setIdentity! !

!Float32Matrix3x3 class methodsFor: 'class initialization' stamp: 'jmv 11/24/2015 16:31'!
initialize
	"Float32Matrix3x3 initialize"
	Float32Matrix3x3Zero := self new.
	Float32Matrix3x3Identity := self new.
	Float32Matrix3x3Identity a11: 1.0; a22: 1.0; a33: 1.0! !

!Float32Matrix3x3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 16:42'!
new
	^self new: self numElements! !

!Float32Matrix3x3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 16:30'!
numElements
	^9! !

!Float32Matrix3x3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 16:30'!
withScale: amount
	^self identity setScale: amount! !

!Float32Matrix3x3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 16:30'!
zero
	^self new! !

!Float32Vector3 methodsFor: 'vector functions' stamp: 'jmv 11/24/2015 16:32'!
normalize
	self /= self length! !

!Float32Vector3 methodsFor: 'vector functions' stamp: 'jmv 11/24/2015 16:32'!
normalized
	^self / self length! !

!Float32Vector3 methodsFor: 'printing' stamp: 'jmv 3/16/2016 14:18'!
printOn: aStream 
	"The receiver prints on aStream in terms of infix notation."

	self x printOn: aStream.
	aStream nextPut: $@.
	self y printOn: aStream.
	aStream nextPut: $@.
	self z printOn: aStream! !

!Float32Vector3 methodsFor: 'private' stamp: 'jmv 11/24/2015 16:32'!
privateLoadFrom: srcObject
	self x: srcObject x y: srcObject y z: srcObject z.! !

!Float32Vector3 methodsFor: 'vector functions' stamp: 'jmv 11/24/2015 16:32'!
safelyNormalize
	"Safely normalize the receiver, e.g. check if the length is non-zero"
	| length |
	length := self length.
	length = 1.0 ifTrue:[^self].
	length = 0.0 ifFalse:[self /= length].! !

!Float32Vector3 methodsFor: 'vector functions' stamp: 'jmv 11/24/2015 16:32'!
safelyNormalized
	"Safely normalize the receiver, e.g. check if the length is non-zero"
	^self copy safelyNormalize! !

!Float32Vector3 methodsFor: 'accessing' stamp: 'jmv 11/24/2015 16:32'!
x
	^self at: 1! !

!Float32Vector3 methodsFor: 'accessing' stamp: 'jmv 11/24/2015 16:32'!
x: aFloat
	self at: 1 put: aFloat! !

!Float32Vector3 methodsFor: 'initialize' stamp: 'jmv 11/24/2015 16:33'!
x: x y: y z: z
	self x: x.
	self y: y.
	self z: z.! !

!Float32Vector3 methodsFor: 'accessing' stamp: 'jmv 11/24/2015 16:32'!
y
	^self at: 2! !

!Float32Vector3 methodsFor: 'accessing' stamp: 'jmv 11/24/2015 16:32'!
y: aFloat
	self at: 2 put: aFloat! !

!Float32Vector3 methodsFor: 'accessing' stamp: 'jmv 11/24/2015 16:32'!
z
	^self at: 3! !

!Float32Vector3 methodsFor: 'accessing' stamp: 'jmv 11/24/2015 16:33'!
z: aFloat
	self at: 3 put: aFloat! !

!Float32Vector3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 16:41'!
new
	^self new: self numElements! !

!Float32Vector3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 16:33'!
numElements
	^3! !

!Float32Vector3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 16:33'!
value: aFloat
	^self x: aFloat y: aFloat z: aFloat! !

!Float32Vector3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 16:33'!
x: x y: y z: z
	^self new x: x y: y z: z! !

!Float32Vector3 class methodsFor: 'instance creation' stamp: 'jmv 11/24/2015 16:33'!
zero
	^self new! !
Float64Matrix3x3 initialize!
Float64Matrix4x4 initialize!
Float64Quaternion initialize!
Float32Matrix3x3 initialize!
