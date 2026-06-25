'From Cuis7.9 [latest update: #8024] on 25 June 2026 at 11:56:27 am'!

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 25/Jun/2026, 2:30:52 pm (UTC)'!
normalCursorBig
	"Answer the default top left pointing arrow cursor.
	This is an anti aliased big version drawn with Vector Graphics"

	| form cursor |
	form:= Form extent: 32@32 depth: 32.
	(VectorEngineWholePixel onForm: form subPixelAntiAliasing: false)
		enableTargetTranslucency;
		geometryTransformation: (AffineTransformation withScale: 1.8 position: 1@1);
		strokeWidth: 1.0 color: Color white fillColor: Color black do: [ :engine |
			engine
				pathSequence: #[
					0.0 0.0 0.0
					1.0 10.0 10.0
					1.0 5.56 10.0
					1.0 7.67 15.0
					1.0 6.67 16.0
					1.0 5.27 16.0
					1.0 3.16 11.0
					1.0 0.0 14.0
					1.0 0.0 0.0
					]
				].
	cursor := CursorWithAlpha extent: form extent depth: 32.
	form displayOn: cursor.
	cursor offset: -2@ -1.
	cursor offset: -3@ -3.
	cursor preMultiplyAlpha.
	^cursor! !


!Collection methodsFor: 'converting' stamp: 'jmv 25/Jun/2026, 2:39:41 pm (UTC)'!
asFloat32Array
	"Answer a Float32Array whose elements are the elements of the receiver"

	^self as: Float32Array! !


!FloatArray methodsFor: 'interpolating' stamp: 'jmv 25/Jun/2026, 2:36:27 pm (UTC)'!
interpolatedValueAt: floatIndex
	"Do a linear interpolation.
	Gives usual error if argument outside bounds:
	#[ 4.0 5.0 ] interpolatedValueAt: 0.999
	#[ 4.0 5.0 ] interpolatedValueAt: 1.0
	#[ 4.0 5.0 ] interpolatedValueAt: 1.0001
	#[ 4.0 5.0 ] interpolatedValueAt: 1.5
	#[ 4.0 5.0 ] interpolatedValueAt: 1.999
	#[ 4.0 5.0 ] interpolatedValueAt: 2.0
	#[ 4.0 5.0 ] interpolatedValueAt: 2.001
	#[ 4.0 5.0 ] interpolatedValueAt: 3
	"
	| size index0 index1 weight0 weight1 |

	size := self size.
	index0 := floatIndex truncated. 		"Could be #floor. But as we only care for values >=1, it is the same. But faster."

	weight1 := floatIndex - index0.
	weight0 := 1.0 - weight1.

	index1 := (index0 = size and: [ weight1 = 0.0 ]) 			"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ index0 + 1 ]
		ifTrue: [ index0 ].

	"/* perform interpolation */"
	^ (weight0 * (self at: index0)) + (weight1 * (self at: index1))! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 25/Jun/2026, 2:37:02 pm (UTC)'!
primDivScalar: scalarValue
	"This primitive doesn't fail if argument is zero. It fills result with infinity or nan.
	For consistency with division with arrays, and general practice, an exception block or value might be used in public protocol. If needed, call directly this method instead.

	#[1.0 2.0 3.141592 0.0] primDivScalar: 0.0.
	#[1.0 2.0 3.141592 0.0] / 0.0.
	#[1.0 2.0 3.141592 0.0] divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200
	"
	1 to: self size do: [ :i | self at: i put: (self at: i) / scalarValue].! !


!Float32Array methodsFor: 'primitives-plugin' stamp: 'jmv 25/Jun/2026, 2:36:46 pm (UTC)'!
primDivScalar: scalarValue
	"This primitive doesn't fail if argument is zero. It fills result with infinity or nan.
	For consistency with division with arrays, and general practice, an exception block or value might be used in public protocol. If needed, call directly this method instead.

	#[1.0 2.0 3.141592 0.0] primDivScalar: 0.0.
	#[1.0 2.0 3.141592 0.0] / 0.0.
	#[1.0 2.0 3.141592 0.0] divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200
	"
	<primitive: 'primitiveDivScalar' module: 'FloatArrayPlugin'>
	^super primDivScalar: scalarValue! !


!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 25/Jun/2026, 2:40:05 pm (UTC)'!
primFromFloat32Array: aFloat32Array
	"
	(Float64Array new: 3) primFromFloat32Array: #[1.0 2.0 3.0]
	"
	<primitive: 'primitiveFromFloatArray' module: 'Float64ArrayPlugin'>
	^self fillWith: aFloat32Array! !


!UnicodeString methodsFor: 'initialization' stamp: 'jmv 25/Jun/2026, 2:33:36 pm (UTC)'!
initialize
	"Just an empty string."

	self privateBytes: #[] codePointCount: 0.! !


!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 25/Jun/2026, 2:33:05 pm (UTC)'!
empty
	"Return the empty UnicodeString instance.
	Since my instances are readonly, this could be cached."

	^ self fromUtf8Bytes: #[] codePointCount: 0.! !


!Sample03Smiley methodsFor: 'drawing' stamp: 'jmv 25/Jun/2026, 2:29:38 pm (UTC)'!
drawOn: aCanvas
	"Example of the #pathSequence drawing operation, that can represent many drawing operations
			in one compact literal, and draw them at once.
	0.0		x	y									Move pen to position x@y
	1.0		x	y									Draw a line to position x@y
	2.0		toX	toY	controlX		controlY						Draw a Quadratic Bezier Curve
	3.0		toX	toY	control1X	control1Y			control2X	control2Y			Draw a Cubic Bezier Curve
	See #circleApproximationExamples
	Sample03Smiley new openInHand
	"

	aCanvas strokeWidth: 30 color: `Color gray` fillColor: `Color lightGreen duller` do: [
		aCanvas
				pathSequence: #[
					0.0  150.0  150.0
					3.0  300.0  300.0  150.0  233.0  217.0  300.0
					3.0  450.0  150.0  380.3  300.0  450.0  233.0
					3.0  300.0  0.0      450.0  67.0    383.0  0.0
					3.0  150.0  150.0  210.7  0.0      150.0  67.0
					]
		].

	aCanvas strokeWidth: 30 color: `Color gray` do: [
		aCanvas
				pathSequence: #[
					0.0 220.0 190.0
					2.0 300.0 240.0 245.0 240.0
					2.0 380.0 190.0 355.0 240.0
					0.0 250.0 100.0
					1.0 250.0 120.0
					0.0 350.0 100.0
					1.0 350.0 120.0
					]
		].! !


!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 25/Jun/2026, 2:30:07 pm (UTC)'!
pathSequence: aFloat32Array
	"This method allows high performance drawing of arbitrary shapes. It uses little memory and is very fast.
	It is recommended to use it with literal Float32Arrays as argument.
	See comment at VectorEngine>>#pathSequence:
	See, for example, #drawHandLarge

	'Circle approximation by bezier curves
	https://spencermortensen.com/articles/bezier-circle/'.
	(VectorCanvas onForm: Display)
		geometryTransformation: (AffineTransformation withScale: 14 position: 100@100);
		strokeWidth: 3 color: Color random do: [ :engine |
			engine
				pathSequence: #[
					0.0 15.0 15.0
					3.0 30.0 30.0 15.0 23.3 21.7 30.0
					3.0 45.0 15.0 38.3 30.0 45.0 23.3
					3.0 30.0 0.0 45.0 6.7 38.3 0.0
					3.0 15.0 15.0 21.7 0.0 15.0 6.7
					0.0 22.0 19.0
					2.0 30.0 24.0 24.5 24.0
					2.0 38.0 19.0 35.5 24.0
					0.0 25.0 10.0
					1.0 25.0 12.0
					0.0 35.0 10.0
					1.0 35.0 12.0
					]
				].
	Display forceToScreen
	"
	engine pathSequence: aFloat32Array! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 25/Jun/2026, 2:29:50 pm (UTC)'!
drawHandLarge

	self strokeWidth: 2.2 color: `Color black` fillColor: `Color white` do: [ :canvas |
		canvas pathSequence:
			#[
				0.0    10.97    35.70
				1.0    -3.17    26.78
				2.0    -7.18    20.17    -8.01    23.69
				2.0    -3.16    18.50    -6.55    18.00
				1.0    4.84    20.92
				1.0    5.97    23.63
				1.0    -2.81    2.44
				2.0    -1.35    -3.26    -4.61    -1.91
				2.0    3.71    -0.26    1.91    -4.61
				1.0    9.56    13.87
				1.0    6.41    6.26
				1.0    9.67    4.91
				2.0    13.20    7.27    11.85    4.01
				1.0    15.45    12.70
				1.0    12.75    6.18
				1.0    16.01    4.83
				2.0    19.53    7.19    18.18    3.93
				1.0    21.33    11.54
				1.0    19.08    6.10
				1.0    22.34    4.75
				2.0    25.87    7.11    24.52    3.85
				1.0    27.67    11.46
				1.0    30.82    19.07
				2.0    30.79    34.36    33.52    25.59
				1.0    17.74    39.77
				2.0    11.42    36.78    14.48    41.12
				1.0    10.97    35.70
			] ].
! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 25/Jun/2026, 2:29:58 pm (UTC)'!
drawHandSmall

	self strokeWidth: 1.1 color: `Color black` fillColor: `Color white` do: [ :canvas |
		canvas pathSequence:
			#[
				0.0    5.33    17.34
				1.0    -1.54    13.01
				2.0    -3.49    9.79    -3.89    11.51
				2.0    -1.54    8.99    -3.18    8.74
				1.0    2.35    10.16
				1.0    2.90    11.48
				1.0    -1.37    1.18
				2.0    -0.66    -1.58    -2.24    -0.93
				2.0    1.80    -0.13    0.93    -2.24
				1.0    4.65    6.74
				1.0    3.11    3.04
				1.0    4.70    2.38
				2.0    6.41    3.53    5.75    1.95
				1.0    7.50    6.17
				1.0    6.19    3.00
				1.0    7.78    2.35
				2.0    9.49    3.49    8.83    1.91
				1.0    10.36    5.60
				1.0    9.27    2.96
				1.0    10.85    2.31
				2.0    12.56    3.45    11.91    1.87
				1.0    13.44    5.57
				1.0    14.97    9.26
				2.0    14.95    16.69    16.28    12.43
				1.0    8.62    19.31
				2.0    5.55    17.87    7.04    19.97
				1.0    5.33    17.34
			] ].! !


!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 25/Jun/2026, 2:31:02 pm (UTC)'!
pushPinIconData: aSize
	| scale |
	(IconPathData at: #pushPinIconSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #pushPinIcon ifPresent: [ :found | ^found ]].
	IconPathData at: #pushPinIconSize put: aSize.
	scale := aSize / 19.0.
	^IconPathData at: #pushPinIcon put: (
		self scalePathData: #[
			0 0 21
			1 10 13
			1 15 18
			1 14 13
			1 21 6
			1 15 0
			1 8 7
			1 3 6
			1 8 11
			1 0 21
		] scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 25/Jun/2026, 2:31:13 pm (UTC)'!
scrollDownPathData: aSize
	| scale |
	(IconPathData at: #scrollDownSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollDown ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollDownSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollDown put: (
		self scalePathData: #[
			0 4 7
			1 9 13
			1 14 7
		] scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 25/Jun/2026, 2:31:19 pm (UTC)'!
scrollLeftPathData: aSize
	| scale |
	(IconPathData at: #scrollLeftSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollLeft ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollLeftSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollLeft put: (
		self scalePathData: #[
			0 12 4
			1 6 9
			1 12 14
		] scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 25/Jun/2026, 2:31:25 pm (UTC)'!
scrollRightPathData: aSize
	| scale |
	(IconPathData at: #scrollRightSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollRight ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollRightSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollRight put: (
		self scalePathData: #[
			0 7 4
			1 13 9
			1 7 14
			] scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 25/Jun/2026, 2:31:32 pm (UTC)'!
scrollUpPathData: aSize
	| scale |
	(IconPathData at: #scrollUpSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollUp ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollUpSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollUp put: (
		self scalePathData: #[
			0 4 12
			1 9 6
			1 14 12
			] scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 25/Jun/2026, 2:31:39 pm (UTC)'!
windowButtonClosePathData: aSize
	| scale |
	(IconPathData at: #windowButtonCloseSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonClose ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonCloseSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonClose put: (
		self scalePathData: #[
			0.0   6.5 14.5
			1.0 14.5   6.5
			0.0   6.5   6.5
			1.0 14.5 14.5
		] scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 25/Jun/2026, 2:31:45 pm (UTC)'!
windowButtonCollapsePathData: aSize
	| scale |
	(IconPathData at: #windowButtonCollapseSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonCollapse ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonCollapseSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonCollapse put: (
		self scalePathData: #[
			0.0   5.5  10.5
			1.0 15.5  10.5
		] scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 25/Jun/2026, 2:31:51 pm (UTC)'!
windowButtonExpandPathData: aSize
	| scale |
	(IconPathData at: #windowButtonExpandSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonExpand ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonExpandSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonExpand put: (
		self scalePathData: #[
			0.0    5.5  10.5
			1.0  15.5  10.5
			0.0  10.5    5.5
			1.0  10.5  15.5
		] scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 25/Jun/2026, 2:31:58 pm (UTC)'!
windowButtonMenuPathData: aSize
	| scale |
	(IconPathData at: #windowButtonMenuSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonMenu ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonMenuSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonMenu put: (
		self scalePathData: #[
			0.0    4.5    7.5
			1.0  16.5    7.5
			1.0  10.5  15.5
			1.0    4.5    7.5
		] scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 25/Jun/2026, 2:32:04 pm (UTC)'!
windowButtonPathData: aSize
	| scale |
	(IconPathData at: #windowButtonSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButton ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButton put: (
		self scalePathData: #[
			0.0   0.5  10.5
			2    10.5  20.5    1.0  20.0
			2    20.5  10.5  20.0  20.0
			2    10.5    0.5  20.0    1.0
			2      0.5  10.5    1.0    1.0
		] scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 25/Jun/2026, 2:32:11 pm (UTC)'!
windowSquareButtonPathData: aSize
	| scale |
	(IconPathData at: #windowSquareButtonSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowSquareButton ifPresent: [ :found | ^found ]].
	IconPathData at: #windowSquareButtonSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowSquareButton put: (
		self scalePathData: #[
			0.0    0.5    0.5
			1.0    0.5  20.5
			1.0  20.5  20.5
			1.0  20.5    0.5
			1.0    0.5    0.5
		] scale: scale)! !


!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 25/Jun/2026, 2:30:39 pm (UTC)'!
pathSequence: aFloat32Array
	"This method allows high performance drawing of arbitrary shapes. It uses little memory and is very fast.
	It is recommended to use it with literal Float32Arrays as argument.

	Commands are:
	0.0: #moveTo: aPoint
	1.0: #lineTo: aPoint
	2.0: #quadraticBezierTo: toPoint control: controlPoint
	3.0: #cubicBezierTo: toPoint control1: controlPoint1 control2: controlPoint2

	Circle, Ellipse and Arc primitives are currently not supported.

	(VectorEngine onForm: Display subPixelAntiAliasing: true)
		geometryTransformation: (AffineTransformation withScale: 14 position: 100@100);
		strokeWidth: 3 color: Color black do: [ :engine |
			engine
				pathSequence: #[
					0.0 15.0 15.0
					3.0 30.0 30.0 15.0 23.3 21.7 30.0
					3.0 45.0 15.0 38.3 30.0 45.0 23.3
					3.0 30.0 0.0 45.0 6.7 38.3 0.0
					3.0 15.0 15.0 21.7 0.0 15.0 6.7
					0.0 22.0 19.0
					2.0 30.0 24.0 24.5 24.0
					2.0 38.0 19.0 35.5 24.0
					0.0 25.0 10.0
					1.0 25.0 12.0
					0.0 35.0 10.0
					1.0 35.0 12.0
					]
				].
	Display forceToScreen
	"
	| size i |
	size := aFloat32Array size.
	i := 1.
	[ i < size ] whileTrue: [
		| commandType p1x p1y p2x p2y p3x p3y |
		commandType := aFloat32Array at: i. i := i + 1.
		commandType caseOf: {
			[0.0] -> [
				i < size ifFalse: [ ^self "failure" ].
				p1x := aFloat32Array at: i. i := i + 1.
				p1y := aFloat32Array at: i. i := i + 1.
				self moveTo: p1x@p1y].
			[1.0] -> [
				i < size ifFalse: [ ^self "failure" ].
				p1x := aFloat32Array at: i. i := i + 1.
				p1y := aFloat32Array at: i. i := i + 1.
				self lineTo: p1x@p1y].
			[2.0] -> [
				i+2 < size ifFalse: [ ^self "failure" ].
				p1x := aFloat32Array at: i. i := i + 1.
				p1y := aFloat32Array at: i. i := i + 1.
				p2x := aFloat32Array at: i. i := i + 1.
				p2y := aFloat32Array at: i. i := i + 1.
				self quadraticBezierTo: p1x@p1y control: p2x@p2y ].
			[3.0] -> [
				i+4 < size ifFalse: [ ^self "failure" ].
				p1x := aFloat32Array at: i. i := i + 1.
				p1y := aFloat32Array at: i. i := i + 1.
				p2x := aFloat32Array at: i. i := i + 1.
				p2y := aFloat32Array at: i. i := i + 1.
				p3x := aFloat32Array at: i. i := i + 1.
				p3y := aFloat32Array at: i. i := i + 1.
				self cubicBezierTo: p1x@p1y control1: p2x@p2y control2: p3x@p3y ]}
		otherwise: [ ^self "failure"]].! !


!TextEditor methodsFor: 'attributes' stamp: 'jmv 25/Jun/2026, 2:45:05 pm (UTC)'!
offerFontStyleMenu
	"This is a user command, and generates undo"

	| emphases menuStrings entries reply code startIndex attribute |
	startIndex := self startIndex.
	code := model actualContents emphasisAt: startIndex.
	emphases := #(bold italic underlined struckThrough superscript subscript).
	menuStrings := Array streamContents: [ :strm |
		strm nextPut:(code isZero ifTrue: ['<on>'] ifFalse: ['<off>']), 'normal'.
		emphases do: [ :emph |
			strm nextPut:
				((code anyMask: (TextEmphasis perform: emph) emphasisCode)
					ifTrue: ['<on>'] ifFalse: ['<off>']),
				emph asPlainString ]].
	entries := #(normal), emphases.
	reply := (SelectionMenu labelList: menuStrings lines: #(1) selections: entries) startUpMenu.
	reply ifNotNil: [
		attribute := TextEmphasis perform: reply.
		((menuStrings at: (entries indexOf: reply)) beginsWith: '<on>')
			ifTrue: [ self unapplyAttributes: {attribute} ]
			ifFalse: [ self applyAttribute:  attribute ]].
	morph updateFromTextComposition.! !

