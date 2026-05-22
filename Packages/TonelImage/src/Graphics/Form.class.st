"
A rectangular array of pixels, used for holding images.  All pictures, including character images are Forms.  The depth of a Form is how many bits are used to specify the color at each pixel.  The actual bits are held in a Bitmap, whose internal structure is different at each depth.  Class Color allows you to deal with colors without knowing how they are actually encoded inside a Bitmap.
	  The supported depths (in bits) are 1, 2, 4, 8, 16, and 32.  The number of actual colors at these depths are: 2, 4, 16, 256, 32768, and 16 million.
	Forms are indexed starting at 0 instead of 1; thus, the top-left pixel of a Form has coordinates 0@0.
	Forms are combined using BitBlt.  See the comment in class BitBlt.

	colorAt: x@y						Returns the abstract Color at this location
	displayAt: x@y					shows this form on the screen
	displayOn: aMedium at: x@y	shows this form in a Window, a Form, or other DisplayMedium
	fillColor: aColor					Set all the pixels to the color.
	edit								launch an editor to change the bits of this form.
	pixelValueAt: x@y				The encoded color.  The encoding depends on the depth.

"
Class {
	#name : 'Form',
	#superclass : 'Object',
	#instVars : [
		'bits',
		'width',
		'height',
		'depth',
		'offset'
	],
	#category : 'Graphics-Display Objects',
	#package : 'Graphics',
	#tag : 'Display Objects'
}

{ #category : 'mode constants' }
Form class >> and [
	"Answer the integer denoting the logical 'and' combination rule."
	^1
]

{ #category : 'mode constants' }
Form class >> blend [
	"Answer the integer denoting BitBlt's alpha blend combination rule."
	^24
]

{ #category : 'mode constants' }
Form class >> blendAlpha [
	"Answer the integer denoting BitBlt's blend-with-constant-alpha rule."

	^ 30
]

{ #category : 'creation - anti aliased' }
Form class >> bottomLeftCorner: r height: height [
	"Create and answer a Form for the bottomLeft corner of a rounded rectangle"
	| f aaw l d width dy |
	width := r.
	aaw := 1.3.
	f := Form
		extent: width @ height
		depth: 32.
height > 1 ifTrue: [
	0
		to: height - 1
		do: [ :y |
			0
				to: width - 1
				do: [ :x |
					dy := height - r - y.
					dy > 0
						ifTrue: [ l := 1.0 ]
						ifFalse: [
							d := (width - 1 - x @ dy) r.
							l := ((r - d max: 0.0) min: aaw) / aaw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
].
	^ f
]

{ #category : 'creation - anti aliased' }
Form class >> bottomRightCorner: r height: height [
	"Create and answer a Form for the bottomRight corner of a rounded rectangle"
	| f aaw l d width dy |
	width := r.
	aaw := 1.3.
	f := Form
		extent: width @ height
		depth: 32.
height > 1 ifTrue: [
	0
		to: height - 1
		do: [ :y |
			0
				to: width - 1
				do: [ :x |
					dy := height - r - y.
					dy > 0
						ifTrue: [ l := 1.0 ]
						ifFalse: [
							d := (x @ dy) r.
							l := ((r - d max: 0.0) min: aaw) / aaw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
].
	^ f
]

{ #category : 'instance creation' }
Form class >> dotOfSize: diameter [
	"Create a form which contains a round black dot."
	| radius form bb rect centerX centerY centerYBias centerXBias radiusSquared xOverY maxy dx |
	radius := diameter//2.
	form := self extent: diameter@diameter offset: `0@0` - radius.
	bb := (BitBlt toForm: form)
		sourceX: 0; sourceY: 0;
		combinationRule: Form over;
		fillColor: `Color black`.
	rect := form boundingBox.
	centerX := rect center x.
	centerY := rect center y.
	centerYBias := rect height odd ifTrue: [0] ifFalse: [1].
	centerXBias := rect width odd ifTrue: [0] ifFalse: [1].
	radiusSquared := (rect height asFloat / 2.0) squared - 0.01.
	xOverY := rect width asFloat / rect height asFloat.
	maxy := rect height - 1 // 2.

	"First do the inner fill, and collect x values"
	0 to: maxy do: [ :dy |
		dx := ((radiusSquared - (dy * dy) asFloat) sqrt * xOverY) truncated.
		bb	destX: centerX - centerXBias - dx
			destY: centerY - centerYBias - dy
			width: dx + dx + centerXBias + 1
			height: 1;
			copyBits.
		bb	destY: centerY + dy;
			copyBits].
	^ form
"
Time millisecondsToRun:
	[1 to: 20 do: [:i | (Form dotOfSize: i) displayAt: (i*20)@(i*20)]]
"
]

{ #category : 'mode constants' }
Form class >> erase [
	"Answer the integer denoting mode erase."
	^4
]

{ #category : 'mode constants' }
Form class >> erase1bitShape [
	"Answer the integer denoting mode erase."

	^ 26
]

{ #category : 'instance creation' }
Form class >> extent: extentPoint [
	"Answer an instance of me with a blank bitmap of depth 1."

	^ self extent: extentPoint depth: 1

]

{ #category : 'instance creation' }
Form class >> extent: extentPoint depth: bitsPerPixel [
	"Answer an instance of me with blank bitmap of the given dimensions and depth."

	^ self basicNew setExtent: extentPoint depth: bitsPerPixel

]

{ #category : 'instance creation' }
Form class >> extent: extentPoint depth: bitsPerPixel bits: aBitmap [
	"Answer an instance of me with blank bitmap of the given dimensions and depth."

	^ self basicNew setExtent: extentPoint depth: bitsPerPixel bits: aBitmap
]

{ #category : 'instance creation' }
Form class >> extent: extentPoint depth: bitsPerPixel fromArray: anArray offset: offsetPoint  [
	"Answer an instance of me with a pixmap of the given depth initialized from anArray."
	"See Form >> storeOn:base:"

	^ (self extent: extentPoint depth: bitsPerPixel)
		offset: offsetPoint;
		initFromArray: anArray

]

{ #category : 'instance creation' }
Form class >> extent: extentPoint nativeDepthFrom: bitsPerPixel [
	"Answer an instance of me with blank bitmap of the given dimensions and depth.
	Assume bitsPerPixel is positive. Use native endianness."

	^ self
		extent: extentPoint
		depth: (Smalltalk isLittleEndian ifTrue: [ bitsPerPixel negated ] ifFalse: [ bitsPerPixel ])
]

{ #category : 'instance creation' }
Form class >> extent: extentPoint nativeDepthFrom: bitsPerPixel bits: aBitmap [
	"Answer an instance of me with blank bitmap of the given dimensions and depth.
	Assume bitsPerPixel is positive. Use native endianness."

	^ self
		extent: extentPoint
		depth: (Smalltalk isLittleEndian ifTrue: [ bitsPerPixel negated ] ifFalse: [ bitsPerPixel ])
		bits: aBitmap
]

{ #category : 'instance creation' }
Form class >> extent: extentPoint offset: offsetPoint  [
	"Answer an instance of me with a blank bitmap of depth 1."

	^ (self extent: extentPoint depth: 1) offset: offsetPoint

]

{ #category : 'instance creation' }
Form class >> fakeSphereAt: aPoint diameter: diameter color: aColor [
	"Create a form which contains a round black dot."
	| bb rect d p |
	bb := BitBltCanvasEngine toForm: Display.
	bb fillColor: aColor.
	bb combinationRule: Form blend.
	rect := aPoint extent: diameter.
	bb fillOval: rect.
	p := (diameter * 2 // 5) asPoint + aPoint.
	#(2 3 4 5 6 ) do: [ :i |
		"simulate facade by circles of gray"
		bb fillColor: `Color white alpha: 0.3`.
		d := i * diameter // 10.
		rect := Rectangle
			center: p
			extent: d roundedToEven.
		bb fillOval: rect ].
	Display forceToScreen.
"
	Form fakeSphereAt: 70@70 diameter: 40 color:  (Color red alpha: 0.5)
"
]

{ #category : 'instance creation' }
Form class >> fromBinaryStream: aBinaryStream [
	"Read a Form or ColorForm from given file, using the first byte of the file to guess its format. Currently handles: GIF, uncompressed BMP, and both old and new DisplayObject writeOn: formats, JPEG, and PCX. Return nil if the file could not be read or was of an unrecognized format."

	| firstByte |
	firstByte := aBinaryStream next.
	firstByte = 2 ifTrue: [
		"Squeak form format"
		^ self new readFrom: aBinaryStream].

	"Try for JPG, GIF, or PCX..."
	"Note: The following call closes the stream."
	^ ImageReadWriter formFromStream: aBinaryStream

]

{ #category : 'instance creation' }
Form class >> fromDisplay: aRectangle  [
	"Answer an instance of me with bitmap initialized from the area of the 
	display screen defined by aRectangle."

	^ (self extent: aRectangle extent depth: Display depth)
		fromDisplay: aRectangle
]

{ #category : 'instance creation' }
Form class >> fromFileEntry: aFileEntry [
	"Read a Form or ColorForm from the given file."

	^aFileEntry readStreamDo: [ :stream |
		self fromBinaryStream: stream binary ]
]

{ #category : 'instance creation' }
Form class >> fromFileNamed: filename [
	"Read a Form or ColorForm from the given file."

	^ self fromFileEntry: filename asFileEntry
]

{ #category : 'instance creation' }
Form class >> fromFileNamedOrNil: fileName [
	"Read a Form or ColorForm from the given file.
	This method shows the usual way to do this."

	^ [ self fromFileNamed: fileName ]
		on: FileDoesNotExistException
		do: nil
]

{ #category : 'instance creation' }
Form class >> fromUser [
	"Answer an instance of me with bitmap initialized from the area of the 
	display screen designated by the user."

	^ self fromDisplay: Rectangle fromUser
]

{ #category : 'instance creation' }
Form class >> fromUserWithExtent: anExtent [
	"Answer an instance of me with bitmap initialized from the area of the 
	display screen whose origin is designated by the user and whose size is anExtent"

	^ self fromDisplay: (Rectangle originFromUser: anExtent)

"(Form fromUserWithExtent: 50@50) displayAt: 10@10"
]

{ #category : 'mode constants' }
Form class >> oldErase1bitShape [
	"Answer the integer denoting mode erase."

	^ 17
]

{ #category : 'mode constants' }
Form class >> oldPaint [
	"Answer the integer denoting the 'paint' combination rule."

	^16
]

{ #category : 'mode constants' }
Form class >> over [
	"Answer the integer denoting mode over."
	^3
]

{ #category : 'mode constants' }
Form class >> paint [
	"Answer the integer denoting the 'paint' combination rule."

	^25
]

{ #category : 'mode constants' }
Form class >> paintAlpha [
	"Answer the integer denoting BitBlt's paint-with-constant-alpha rule."

	^ 31
]

{ #category : 'mode constants' }
Form class >> reverse [
	"Answer the integer denoting mode reverse."
	^6
]

{ #category : 'mode constants' }
Form class >> rgbMul [
	"Answer the integer denoting 'Multiply each color component, their values regarded as fractions of 1' rule."

	^ 37
]

{ #category : 'instance creation' }
Form class >> small1BitExtent: extentPoint pixelBits: pixelBits [
	"Answer an instance of me of depth 1 with bitmap initialized from pixelBits.
	Requested width can be no more than 32. Result has one 32 bit word per line."

	^ (self extent: extentPoint depth: 1)
		fillSmall1BitWith: pixelBits
]

{ #category : 'instance creation' }
Form class >> small1BitExtent: extentPoint pixelBits: pixelBits offset: offsetPoint [
	"Answer an instance of me of depth 1 with bitmap initialized from pixelBits.
	Requested width can be no more than 32. Result has one 32 bit word per line."

	^ (self extent: extentPoint depth: 1)
		offset: offsetPoint;
		fillSmall1BitWith: pixelBits
]

{ #category : 'examples' }
Form class >> toothpaste: diam	 [
	"
	Display restoreAfter: [Form toothpaste: 30]
	"
	"Not completely unlike the ST-80 implementation :)
	Original comment:
	Draws wormlike lines by laying down images of spheres.
	See Ken Knowlton, Computer Graphics, vol. 15 no. 4 p352.
	Draw with mouse button down; terminate by option-click."
	| point queue color q colors colr colr2 |
	colors := `Color red wheel: 12`.
	color := 8.
	[ true ] whileTrue: [
		queue := OrderedCollection new.
		16 timesRepeat: [queue addLast: `-40@ -40`].
		Sensor waitButton.
		Sensor isMouseButton2Pressed ifTrue: [^ self].
		point := Sensor mousePoint.
		colr := colors atWrap: (color := color + 5).  "choose increment relatively prime to colors size"
		colr2 := colr alpha: 0.3.
		[Sensor isMouseButton1Pressed or: [queue size > 0]] whileTrue: [
			point := point * 4 + Sensor mousePoint // 5.
			point := point.
			Form fakeSphereAt: point diameter: diam color: colr.
			(q := queue removeFirst) ifNil: [^ self].	"exit"
			Form fakeSphereAt: q diameter: diam color: colr2.
			Sensor isMouseButton1Pressed ifTrue: [queue addLast: point]]]
]

{ #category : 'creation - anti aliased' }
Form class >> topLeftCorner: r height: height [
	"Create and answer a Form for the topLeft corner of a rounded rectangle"
	| f bw l d w |
	w := r.
	bw := 1.3.
	f := Form
		extent: w @ height
		depth: 32.
height > 1 ifTrue: [
	0
		to: height - 1
		do: [ :y |
			0
				to: w - 1
				do: [ :x |
					l := 1.0.
					y < r ifTrue: [
						d := (w - 1 - x @ (w - 1 - y)) r.
						l := ((r - d max: 0.0) min: bw) / bw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
].
	^ f
]

{ #category : 'creation - anti aliased' }
Form class >> topRightCorner: r height: height [
	"Create and answer a Form for the topRight corner of a rounded rectangle"
	| f bw l d w |
	w := r.
	bw := 1.3.
	f := Form
		extent: w @ height
		depth: 32.
height > 1 ifTrue: [
	0
		to: height - 1
		do: [ :y |
			0
				to: w - 1
				do: [ :x |
					l := 1.0.
					y < r ifTrue: [
						d := (x @ (w - y - 1)) r.
						l := ((r - d max: 0.0) min: bw) / bw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
].
	^ f
]

{ #category : 'mode constants' }
Form class >> under [
	"Answer the integer denoting mode under."
	^7
]

{ #category : 'examples' }
Form class >> xorHack: size  "Display restoreAfter: [Form xorHack: 256]" [
	"Draw a smiley face or stick figure, and end with option-click.
	Thereafter image gets 'processed' as long as you have button down.
	If you stop at just the right time, you'll see you figure upside down,
	and at the end of a full cycle, you'll see it perfectly restored.
	Dude -- this works in color too!"
	| rect form i bb |
	rect := 5@5 extent: size@size.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	Display border: (rect topRight - (0@2) extent: rect extent*2 + 4) width: 2.
	Form exampleSketch.
	form := Form fromDisplay: rect.
	bb := form boundingBox.
	i := 0.
	[Sensor isMouseButton2Pressed] whileFalse:
		[[Sensor isMouseButton1Pressed] whileTrue:
			[i := i + 1.
			(Array with: 0@1 with: 0@ -1 with: 1@0 with: -1@0) do:
				[:d | form copyBits: bb from: form at: d
					clippingBox: bb rule: Form reverse ].
			form displayAt: rect topLeft.
			i+2\\size < 4 ifTrue: [(Delay forMilliseconds: 300) wait]].
		(form magnify: form boundingBox by: 2@2) displayAt: rect topRight + (2@0).
		Sensor waitButton].
]

{ #category : 'comparing' }
Form >> = other  [
	"Compare for equal contents. Expensive!
	See comment at #hash"

	self == other ifTrue: [ ^ true ].

	^ (other class == self class) 
		and: [ other width = width
			and: [ other height = height
				and: [ other depth = self depth
					and: [ other offset = self offset
						and: [ other bits = bits ]]]]]
]

{ #category : 'encoding' }
Form >> addDeltasFrom: previousForm [

	(BitBlt 
		destForm: self 
		sourceForm: previousForm 
		fillColor: nil 
		combinationRule: Form reverse
		destOrigin: `0@0`
		sourceOrigin: `0@0`
		extent: self extent 
		clipRect: self boundingBox) copyBits.
	^self
]

{ #category : 'converting' }
Form >> as8BitColorForm [
	"Answer an 8 bit ColorForm with an optimized palette with up to 256 entries."

	^self asColorFormOfDepth: 8
]

{ #category : 'converting' }
Form >> as8BitStandardPaletteColorForm [
	"Simple conversion of zero pixels to transparent. Force it to 8 bits."

	| f |
	f := ColorForm extent: self extent depth: 8.
	f colors: Color indexedColors copy.
	self displayOn: f at: self offset negated.
	f offset: self offset.
	^ f
]

{ #category : 'converting' }
Form >> asColorFormOfDepth: destDepth [
	"
	Answer a ColorForm with a custom optimized palette of up to 256, 16 or 4 entries.
	self runningWorld backgroundImage asColorFormOfDepth: 8 :: display
	self runningWorld backgroundImage orderedDither32To16 asColorFormOfDepth: 8 :: display
	self runningWorld backgroundImage orderedDither32To16 asColorFormOfDepth: 4 :: display
	"
	| answer bitsPerColor clusterIndexToSplit clusterToSplit clusters colors desiredNumberOfClusters errors firstCluster map metricToSplitOn tally valueToSplitOn box hueWeightFactor brightnessWeightFactor saturationWeightFactor |
	self depth > 8 ifFalse: [
		^self error: 'Only for 16 bit and 32 bit Forms' ].
	desiredNumberOfClusters := 1 bitShift: destDepth.

	"Wheights to balance error on each color metric"
	hueWeightFactor := 8.0 / 360.0.
	saturationWeightFactor := 1.0.
	brightnessWeightFactor := 3.0.
	destDepth < 8 ifTrue: [
		brightnessWeightFactor := 13.0.
		destDepth < 4 ifTrue: [
			hueWeightFactor := 0.0.
			saturationWeightFactor := 0.0. ]].
	"Assign all pixels to a single cluster"
	tally := self tallyPixelValues.
	firstCluster := OrderedCollection new.
	tally withIndexDo: [ :pixelCount :pixelValuePlusOne |
		pixelCount = 0 ifFalse: [ |c|
			c := Color colorFromPixelValue: pixelValuePlusOne -1 depth: 16.
			firstCluster add: {c hue. c saturation. c brightness. pixelCount. pixelValuePlusOne } ]].
	clusters := OrderedCollection with: firstCluster.

	"Pick the existing cluster with the largest error, and split it."
	[clusters size < desiredNumberOfClusters and: [ clusters anySatisfy: [:eachCluster | eachCluster size > 1]]] whileTrue: [ | cluster1 cluster2 |
		"Detect cluster with largest error, and split it"
		errors := clusters collect: [ :eachCluster | | sum average error |
			sum := (eachCluster sum: [ :e | {e first * e fourth. e second * e fourth. e third * e fourth. e fourth }]).
			average := {sum first. sum second. sum third} / sum fourth.
			error := eachCluster sum: [ :e | | hueError |
				hueError := (e first - average first) abs. hueError > (360/2) ifTrue: [hueError := 360 - hueError].
				{hueError * hueWeightFactor. (e second-average second) abs * saturationWeightFactor. (e third-average third) abs * brightnessWeightFactor} * e fourth ].
			{error max. error indexOfMax. average at: error indexOfMax} ].
		clusterIndexToSplit := (errors collect: [ :e | e first]) indexOfMax.
		metricToSplitOn := (errors at: clusterIndexToSplit) second.
		valueToSplitOn := (errors at: clusterIndexToSplit) third.
		clusterToSplit := clusters at: clusterIndexToSplit.
		cluster1 := OrderedCollection new.
		cluster2 := OrderedCollection new.
		clusterToSplit do: [ :pixelMetricsAndCount |
			(pixelMetricsAndCount at: metricToSplitOn) < valueToSplitOn
				ifTrue: [cluster1 add: pixelMetricsAndCount]
				ifFalse: [cluster2 add: pixelMetricsAndCount]].
		clusters at: clusterIndexToSplit put: cluster1.
		clusters add: cluster2.
	].

	colors := clusters collect: [ :eachCluster | | sum average |
		average := eachCluster average.
			sum := (eachCluster sum: [ :a | {a first * a fourth. a second * a fourth. a third * a fourth. a fourth }]).
			average := {sum first. sum second. sum third} / sum fourth.
		Color hue: average first saturation: average second brightness: average third ].
	answer := ColorForm extent: self extent depth: destDepth.
	answer colors: colors.

	"Build colormap for displaying self on answer"
	bitsPerColor := 5.				"To read 16bpp source"
	map := Bitmap new: (1 bitShift: 3*bitsPerColor).
	clusters withIndexDo: [ :eachCluster :clusterIndex |
		eachCluster do: [ :pixMetrixsAndCount |
			map at: pixMetrixsAndCount fifth put: clusterIndex-1 ]].

	box := self boundingBox.
	answer copyBits: box from: self at: 0@0 clippingBox: box rule: Form over map: map.
	^answer

]

{ #category : 'converting' }
Form >> asCursorForm [

	^ self
]

{ #category : 'converting' }
Form >> asForm [
	"In optional packages (LinearAlgebra, ImageProcessing) we might have #asForm
	 conversion methods for other kinds of objects."
	^self
]

{ #category : 'converting' }
Form >> asFormAutoRange [
	"In optional packages (LinearAlgebra, ImageProcessing) we might have #asFormAutoRange
	 conversion methods for other kinds of objects."
	^self
]

{ #category : 'converting' }
Form >> asFormOfDepth: d [
	| newForm source |
	d = depth ifTrue: [ ^self ].
	source := (self depth = 32 and: [ d abs < 32 ])
		ifTrue: [ self copy convertAlphaToZeroValueTransparency ]
		ifFalse: [ self ].
	newForm := Form extent: source extent depth: d.
	(BitBlt toForm: newForm)
		colorMap: (source colormapIfNeededFor: newForm);
		copy: source boundingBox
		from: `0@0` in: source
		fillColor: nil rule: Form over.
	"If we build a 32bpp from one of smaller depth,
	it will have zero in the alpha channel (until BitBlt is fixed!)"
	(newForm depth = 32 and: [self depth < 32]) ifTrue: [
		newForm fixAlpha ].
	^ newForm
]

{ #category : 'converting' }
Form >> asGrayForm [
	| answer map |

	"For lower bit depths, the 15 bit ColorMap loses no information,
	and answers the real #luminance of each pixel."
	self depth < 32 ifTrue: [
		answer := GrayForm extent: width@height.
		map := self colormapIfNeededForGray8bpp.
		(BitBlt toForm: answer)
			colorMap: map;
			copy: self boundingBox
			from: `0@0` in: self
			fillColor: nil rule: Form over.
		answer offset: self offset.
		^ answer ].

	"For 32bpp, the approach above would use just 5bits per component.
	Generally it is best to extract a component in full 8bpp and keep full dynamic range.
	Green usually is a good choice."
	^ self asGrayForm: 3
]

{ #category : 'converting' }
Form >> asGrayForm: componentIndex [
"Native depth"
	"Assume the receiver is a grayscale image. Return a GrayForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit GrayForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)

	If component = 1, take the alpha component
	If component = 2, take the red component
	If component = 3, take the green component
	If component = 4, take the blue component
	"
	| f32 result map mask shift |
	self depth = 32 ifFalse: [
		f32 := Form extent: self extent depth: 32.
		self displayOn: f32.
		^ f32 asGrayForm: componentIndex ].
	
	result := GrayForm extent: self extent.
	shift := #(-24 -16 -8 0) at: componentIndex.
	mask := #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
	map := ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 }.
	(BitBlt toForm: result)
		sourceForm: self;
		combinationRule: Form over;
		colorMap: map;
		sourceRect: self boundingBox ;
		destOrigin: `0@0`;
		copyBits.
	^ result
]

{ #category : 'converting' }
Form >> asGrayScale [
	"Redefined in ColorForm as an optimization."

	^ self asGrayForm
]

{ #category : 'converting' }
Form >> asGrayScaleAndTransparent [
"Native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)"

	"By default, take the green component"
	^self asGrayScaleAndTransparent: 3
]

{ #category : 'converting' }
Form >> asGrayScaleAndTransparent: componentIndex [
"Native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)

	If component = 1, take the alpha component
	If component = 2, take the red component
	If component = 3, take the green component
	If component = 4, take the blue component
	"
	| f32 result map mask shift |
	self depth = 32 ifFalse: [
		f32 := Form extent: self extent depth: 32.
		self displayOn: f32.
		^ f32 asGrayScaleAndTransparent: componentIndex ].

	result := ColorForm grayScaleAndTransparentExtent: self extent.
	shift := #(-24 -16 -8 0) at: componentIndex.
	mask := #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
	map := ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 }.
	(BitBlt toForm: result)
		sourceForm: self;
		combinationRule: Form over;
		colorMap: map;
		sourceRect: self boundingBox;
		destOrigin: `0@0`;
		copyBits.

	"final BitBlt to zero-out pixels that were truely transparent in the original"
	map := Bitmap new: 512.
	map at: 1 put: 16rFF.
	(BitBlt toForm: result)
		sourceForm: self;
		sourceRect: self boundingBox;
		destOrigin: `0@0`;
		combinationRule: Form erase;
		colorMap: map;
		copyBits.
	^ result
]

{ #category : 'color mapping' }
Form >> bitPatternFor: aColor [
	"Return the pixel word for representing the given color on the receiver"
	^aColor bitPatternForDepth: self depth
]

{ #category : 'accessing' }
Form >> bits [
	"Answer the receiver's Bitmap containing its bits."
	^bits
]

{ #category : 'accessing' }
Form >> bits: aBitmap  [
	"Reset the Bitmap containing the receiver's bits."

	bits := aBitmap
]

{ #category : 'accessing' }
Form >> bitsSize [

	^self bitsSizeForWidth: width height: height depth: self depth
]

{ #category : 'accessing' }
Form >> bitsSizeForWidth: aWidth height: aHeight depth: aDepth [

	^ (self wordsPerLine: aWidth depth: aDepth) * aHeight
]

{ #category : 'copying' }
Form >> blankCopyOf: aRectangle scaledBy: scale [

        ^ self class extent: (aRectangle extent * scale) truncated depth: depth
]

{ #category : 'bordering' }
Form >> border: aRectangle width: borderWidth  [
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses black for 
	drawing the border."

	self border: aRectangle width: borderWidth fillColor: `Color black`
]

{ #category : 'bordering' }
Form >> border: aRectangle width: borderWidth borderHeight: borderHeight fillColor: aColor [
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth@borderHeight. Uses aHalfTone for 
	drawing the border."

	self border: aRectangle
		widthRectangle: 
			(Rectangle
				left: borderWidth
				right: borderWidth
				top: borderHeight
				bottom: borderHeight)
		rule: Form over
		fillColor: aColor
]

{ #category : 'bordering' }
Form >> border: aRectangle width: borderWidth fillColor: aHalfTone  [
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses aHalfTone for 
	drawing the border."

	self border: aRectangle
		widthRectangle: 
			(Rectangle
				left: borderWidth
				right: borderWidth
				top: borderWidth
				bottom: borderWidth)
		rule: Form over
		fillColor: aHalfTone
]

{ #category : 'bordering' }
Form >> border: aRectangle width: borderWidth rule: combinationRule fillColor: aHalfTone  [
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses aHalfTone for 
	drawing the border."

	self border: aRectangle
		widthRectangle: 
			(Rectangle
				left: borderWidth
				right: borderWidth
				top: borderWidth
				bottom: borderWidth)
		rule: combinationRule
		fillColor: aHalfTone
]

{ #category : 'bordering' }
Form >> border: aRectangle widthRectangle: insets rule: combinationRule fillColor: aHalfTone [
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of each edge of the border is determined by the four coordinates 
	of insets. Uses aHalfTone and combinationRule for drawing the border."

	aRectangle
		areasOutside: (aRectangle insetBy: insets)
		do: [ :edgeStrip |
			self fill: edgeStrip rule: combinationRule fillColor: aHalfTone ]
]

{ #category : 'bordering' }
Form >> borderWidth: borderWidth borderHeight: borderHeight fillColor: aColor [
	self border: self boundingBox width: borderWidth borderHeight: borderHeight fillColor: aColor
]

{ #category : 'display box access' }
Form >> boundingBox [
	^ Rectangle
		origin: `0 @ 0`
		corner: width @ height
]

{ #category : 'accessing' }
Form >> center [
	"Note that offset is ignored here.  Are we really going to embrace offset?  "
	^ (width @ height) // 2
]

{ #category : 'analyzing' }
Form >> cgForPixelValue: pv orNot: not [
	"Return the center of gravity for all pixels of value pv.
	Note:  If orNot is true, then produce the center of gravity for all pixels
	that are DIFFERENT from the supplied (background) value"
	| pixCount weighted xAndY |
	xAndY := (Array with: (self xTallyPixelValue: pv orNot: not)
					with: (self yTallyPixelValue: pv orNot: not)) collect: [ :profile |	"For both x and y profiles..."
		pixCount := 0.  weighted := 0.
		profile withIndexDo: [ :t :i |
			pixCount := pixCount + t.
			weighted := weighted + (t*i)].
		pixCount = 0  "Produce average of nPixels weighted by coordinate"
			ifTrue: [0.0]
			ifFalse: [weighted asFloat / pixCount asFloat - 1.0]].

	^ xAndY first @ xAndY last
"
| r f cg |
[Sensor isAnyButtonPressed] whileFalse: [
	r := Sensor mousePoint extent: 50@50.
	f := Form extent: r extent depth: 16.
	f fromDisplay: r.
	cg := f cgForPixelValue: (Color black pixelValueForDepth: f depth) orNot: false.
	f displayAt: 0@0.
	Display fill: (cg extent: 4) fillColor: Color red ]
"
]

{ #category : 'pixel access' }
Form >> colorAt: aPoint [
	"Return the color in the pixel at the given point.  "

	^ Color 
		colorFromPixelValue: (self pixelValueAt: aPoint)
		depth: self depth

]

{ #category : 'pixel access' }
Form >> colorAt: aPoint put: aColor [
	"Store a Color into the pixel at coordinate aPoint.  "

	self pixelValueAt: aPoint put: (self pixelValueFor: aColor).

"
[Sensor isAnyButtonPressed] whileFalse:
	[Display colorAt: Sensor mousePoint put: Color red]
"
]

{ #category : 'pixel access' }
Form >> colorInterpolatedAt: aPoint [
	"Evaluate a bilinear (i.e. cheap) interpolation
	Like OpenCV's CV_INTER_LINEAR
	Answer Color transparent if outside image bounds.
	Copied almost verbatim from FloatImage.
	Answer Colors with float components. Will be rounded to be stored in, for example, a 32-bit Form."

	| x y w interpolated xWeight1 xWeight0 yWeight1 yWeight0 xIndex0 xIndex1 yIndex0 yIndex1 |
	x := aPoint x.
	y := aPoint y.
	x < 0.0 ifTrue: [ ^ `Color transparent` ].
	xIndex0 := x truncated. 		"Could be #floor. But as we only care for values >=0, it is the same. But faster."
	xIndex0 > (width-1) ifTrue: [ ^ `Color transparent` ].
	(xIndex0 = (width-1) and: [ x > (width-1) ]) ifTrue: [ ^ `Color transparent` ].
	xIndex1 := xIndex0 = (width-1) 	"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ xIndex0 + 1 ]
		ifTrue: [ xIndex0 ].

	y < 0.0 ifTrue: [ ^ `Color transparent` ].
	yIndex0 := y truncated.			"Could be #floor. But as we only care for values >=0, it is the same. But faster."
	yIndex0 > (height-1) ifTrue: [ ^ `Color transparent` ].
	(yIndex0 = (height-1) and: [ y > (height-1) ]) ifTrue: [ ^ `Color transparent` ].
	yIndex1 := yIndex0 = (height-1) 	"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ yIndex0 + 1 ]
		ifTrue: [ yIndex0 ].

	xWeight1 := x - xIndex0.
	xWeight0 := 1.0 - xWeight1.

	yWeight1 := y - yIndex0.
	yWeight0 := 1.0 - yWeight1.

	"/* perform interpolation */"
	w := ((self colorAt: xIndex0 @ yIndex0) * xWeight0) +
		((self colorAt: xIndex1 @ yIndex0) * xWeight1).
	interpolated := w * yWeight0.

	w := ((self colorAt: xIndex0 @ yIndex1) * xWeight0) +
		((self colorAt: xIndex1 @ yIndex1) * xWeight1).
	interpolated := w * yWeight1 + interpolated.

	^interpolated
]

{ #category : 'color mapping' }
Form >> colormapIfNeededFor: destForm [
	"Return a ColorMap mapping from the receiver to destForm.
	double dispatch"

	^destForm colormapIfNeededFrom: self
]

{ #category : 'color mapping' }
Form >> colormapIfNeededForDepth: destDepth [
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	self depth = destDepth ifTrue: [^ nil].  "not needed if depths are the same"
	^ Color colorMapIfNeededFrom: self depth to: destDepth

]

{ #category : 'color mapping' }
Form >> colormapIfNeededForGray8bpp [
	"Return a colormap for displaying the receiver at the given depth.
	Note: Uses 5 bits per color component. 32bit Forms will lose information!"

	^ Color cachedColormapForGrayFrom: self depth
]

{ #category : 'color mapping' }
Form >> colormapIfNeededFrom: sourceForm [
	"Return a ColorMap mapping from sourceForm to the receiver."
	^sourceForm colormapIfNeededForDepth: self depth
]

{ #category : 'converting' }
Form >> convertAlphaToZeroValueTransparency [
	"For a 32bpp Form, for each pixel, take the alpha value, and if less than 128
	make the pixel value zero (i.e. transparent for 2, 4, 8 and 16 bpp)
	Side effect: make value of alpha = 0 or 128 for all pixels. Not a problem if we're converting to lower bpp anyway...
	This method also loses the lsb of the color components. Again, not a problem for going to lower bpp.
	"
	"
	| bananas1 |
	bananas1 := Form fromFileNamed: 'bananas1.png'.
	bananas1 convertAlphaToZeroValueTransparency..
	bananas1 convertAlphaToZeroValueTransparency.
	(bananas1 asFormOfDepth: 16) display
	"
	| bitBlt map multiplier |
	self depth = 32 ifFalse: [
		^self ].
	
	"Prepare multiplier to multiply each pixel by 0.5 or 0 (according to alpha)"
	multiplier := Form extent: self extent depth: 32.
	map := ColorMap
		masks: { 16r80000000. 16r80000000. 16r80000000. 16r80000000}
		shifts:  { 0.  -8. -16. -24 }.
	bitBlt := BitBlt new.
	bitBlt
		setDestForm: multiplier;
		sourceForm: self;
		colorMap: map;
		combinationRule: Form over;
		copyBits.
	"Now fix for the fact that using a ColorMap turns pixelValues 0's into 1's"
	bitBlt
		sourceForm: nil;
		colorMap: nil;
		fillBitmap: (Bitmap with: 16rFFFFFFFE);
		combinationRule: Form and;
		copyBits.
	"Apply multiplier to ourselves"
	bitBlt
		fillBitmap: nil;
		setDestForm: self;
		sourceForm: multiplier;
		combinationRule: Form rgbMul;
		copyBits.
	"Now correct for the fact that the multiplier had 0.5's and 0's, not 1's and 0's"
	bitBlt
		sourceForm: self;
		combinationRule: 20; "Form rgbAdd"
		copyBits
]

{ #category : 'copying' }
Form >> copy: aRect [
 	"Return a new form which derives from the portion of the original form delineated by aRect."
	| newForm |
	newForm := self class extent: aRect extent depth: depth.
	^ newForm copyBits: aRect from: self at: `0@0`
		clippingBox: newForm boundingBox rule: Form over
]

{ #category : 'copying' }
Form >> copy: destRectangle from: sourcePt in: sourceForm rule: rule  [
	"Make up a BitBlt table and copy the bits."
	(BitBlt toForm: self)
		copy: destRectangle
		from: sourcePt in: sourceForm
		fillColor: nil rule: rule
]

{ #category : 'copying' }
Form >> copy: sourceRectangle from: sourceForm to: destPt rule: rule [
	^ self copy: (destPt extent: sourceRectangle extent)
		from: sourceRectangle topLeft in: sourceForm rule: rule
]

{ #category : 'copying' }
Form >> copyBits: sourceForm at: destOrigin translucent: factor [
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: 30
		destOrigin: destOrigin
		sourceOrigin: `0@0`
		extent: sourceForm extent
		clipRect: self boundingBox)
			copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f := Form fromUser. f2 := Form fromDisplay: (0@0 extent: f extent). f3 := f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 := f2 copy. f3 copyBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"
]

{ #category : 'copying' }
Form >> copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule [
	"Make up a BitBlt table and copy the bits."

	(BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: clipRect) copyBits
]

{ #category : 'copying' }
Form >> copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule map: map [
	"Make up a BitBlt table and copy the bits.  Use a colorMap."

	((BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: clipRect) colorMap: map) copyBits
]

{ #category : 'private' }
Form >> copyFromByteArray: bigEndianByteArray [
	"This method should work with either byte orderings.
	See comment at Bitmap>>#asByteArray
	Also see #copyFromByteArray2:to:"

	| myHack byteHack |
	myHack := Form new hackBits: bits.
	byteHack := Form new hackBits: bigEndianByteArray.
	"We are passing a ByteArray instead of a Words object. Will be accessed according to native endianness."
	Smalltalk isLittleEndian = self isLittleEndian ifFalse: [byteHack swapEndianness].
	byteHack displayOn: myHack at: `0 @ 0` rule: Form over
]

{ #category : 'private' }
Form >> copyFromByteArray2: bigEndianByteArray to: i [
	"This method should work with either byte orderings.
	See comment at Bitmap>>#asByteArray
	Also see #copyFromByteArray:"

	| myHack byteHack |
	myHack := Form new hackBits: bits.
	byteHack := Form new hackBits: bigEndianByteArray.
	"We are passing a ByteArray instead of a Words object. Will be accessed according to native endianness."
	Smalltalk isLittleEndian = self isLittleEndian ifFalse: [byteHack swapEndianness].
	byteHack displayOn: myHack at: 0@i rule: Form over
]

{ #category : 'testing' }
Form >> couldBeTranslucent [
	"Answer whether this form may be translucent"
	^self depth = 32
]

{ #category : 'accessing' }
Form >> depth [
	^ depth < 0 ifTrue:[0-depth] ifFalse:[depth]
]

{ #category : 'displaying-generic' }
Form >> display  [
	"Display the receiver on the Display at location 0,0."

	self displayOn: Display
]

{ #category : 'displaying-generic' }
Form >> displayAt: aDisplayPoint  [
	"Display the receiver located at aDisplayPoint with default settings for 
	the displayMedium, rule and halftone."

	self displayOn: Display at: aDisplayPoint.
]

{ #category : 'displaying' }
Form >> displayAutoRangeAt: aPoint [
	"Display receiver, mapping used range to available gray levels"

	^ self displayAutoRangeAt: aPoint zoom: 1
]

{ #category : 'displaying' }
Form >> displayAutoRangeAt: aPoint zoom: scale [
	"Display receiver, compatibility with Matrix and subclasses such as FloatImage"

	| form |
	form := self.
	scale = 1 ifFalse: [
		form := form magnifyBy: scale ].
	form displayAt: aPoint.
	^ form
]

{ #category : 'displaying-generic' }
Form >> displayOn: aDisplayMedium [
	"Simple default display in order to see the receiver in the upper left 
	corner of screen."

	self displayOn: aDisplayMedium at: `0 @ 0`
]

{ #category : 'displaying-generic' }
Form >> displayOn: aForm at: aDisplayPoint [
	"Display the receiver located at aDisplayPoint with default settings for 
	rule and halftone."

	| toBeDrawn rule |
	"Rule Form paint treats pixels with a value of zero as transparent"
	toBeDrawn := self.
	(aForm depth = 32 and: [ self depth = 32 ]) 
		ifTrue: [ rule := Form blend ] 	"Handle translucent pixels correctly. Requires both source and dest of 32bpp"
		ifFalse: [
			"Warning. Using 'Form paint' with a 32bpp source that includes
			traslucent or transparent alphas will give incorrect results (alpha values will be ignored).
			Doing what follows might be terribly slow. It is best to convert to lower depth on image load."
			"self depth = 32 ifTrue: [
				toBeDrawn := self asFormOfDepth: aForm depth ]."
			rule := Form paint ].
	toBeDrawn displayOn: aForm
		at: aDisplayPoint
		clippingBox: aForm boundingBox
		rule: rule
]

{ #category : 'displaying' }
Form >> displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: rule [

	aDisplayMedium copyBits: self boundingBox
		from: self
		at: aDisplayPoint + self offset
		clippingBox: clipRectangle
		rule: rule
		map: (self colormapIfNeededFor: aDisplayMedium).

]

{ #category : 'displaying-generic' }
Form >> displayOn: aDisplayMedium at: aDisplayPoint rule: ruleInteger [
	"Display the receiver located at aPoint with default setting for the 
	halftone and clippingBox."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: aDisplayMedium boundingBox
		rule: ruleInteger
]

{ #category : 'other' }
Form >> divideByAlpha [
	"Divide each pixel by its alpha. Needed after using rule 24 (alphaBlend) on translucent forms."

	| v a r g b |
	depth = 32 ifFalse: [^self].
	1 to: bits size do: [ :i |
		v := bits at: i.
		a := v bitShift: -24.
		a = 0 ifFalse: [
			r := ((v bitShift: -16) bitAnd: 255) * 255 // a.
			g := ((v bitShift: -8) bitAnd: 255) * 255 // a.
			b := (v bitAnd: 255) * 255 // a.
			bits at: i put: (a bitShift: 24) + (r bitShift: 16) + (g bitShift: 8) + b]].
]

{ #category : 'analyzing' }
Form >> dominantColor [
	| tally max maxi |
	self depth > 16 ifTrue:
		[^(self asFormOfDepth: 16) dominantColor].
	tally := self tallyPixelValues.
	max := maxi := 0.
	tally withIndexDo: [:n :i | n > max ifTrue: [max := n. maxi := i]].
	^ Color colorFromPixelValue: maxi - 1 depth: self depth
]

{ #category : 'filling' }
Form >> eraseShape: bwForm [
	"use bwForm as a mask to clear all pixels where bwForm has 1's"
	((BitBlt destForm: self sourceForm: bwForm
		combinationRule: Form erase1bitShape	"Cut a hole in the picture with my mask"
		destOrigin: bwForm offset 
		sourceOrigin: `0@0`
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits
]

{ #category : 'accessing' }
Form >> extent [
	^ width @ height
]

{ #category : 'coloring' }
Form >> fill: aRectangle fillColor: aForm  [
	"Replace a rectangular area of the receiver with the pattern described by 
	aForm according to the rule over."

	self fill: aRectangle rule: Form over fillColor: aForm
]

{ #category : 'filling' }
Form >> fill: aRectangle rule: anInteger fillColor: aForm  [
	"Replace a rectangular area of the receiver with the pattern described by aForm 
	according to the rule anInteger."
	(BitBlt toForm: self)
		copy: aRectangle
		from: `0@0` in: nil
		fillColor: aForm rule: anInteger
]

{ #category : 'coloring' }
Form >> fillBlack [
	"Set all bits in the receiver to black (ones)."

	self fill: self boundingBox fillColor: `Color black`
]

{ #category : 'coloring' }
Form >> fillBlack: aRectangle  [
	"Set all bits in the receiver's area defined by aRectangle to black (ones)."

	self fill: aRectangle rule: Form over fillColor: `Color black`
]

{ #category : 'coloring' }
Form >> fillColor: aColor [
	"Set all pixels in the receiver to the color.  Must be a correct color for this depth of medium.  TK 1 Jun 96"

	self fill: self boundingBox fillColor: aColor
]

{ #category : 'filling' }
Form >> fillFromXColorBlock: colorBlock [
	"Horizontal Gradient Fill.
	Supply relative x in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| xRel |
	0 to: width-1 do:
		[:x |  xRel := x asFloat / (width-1) asFloat.
		self fill: (x@0 extent: 1@height) 
			fillColor: (colorBlock value: xRel)]
"
((Form extent: 100@100 depth: Display depth)
	fillFromXColorBlock: [:x | Color r: x g: 0.0 b: 0.5]) display
"
]

{ #category : 'filling' }
Form >> fillFromXYColorBlock: colorBlock [
	"General Gradient Fill.
	Supply relative x and y in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| poker yRel xRel |
	poker := BitBlt bitPokerToForm: self.
	0 to: height-1 do:
		[:y | yRel := y asFloat / (height-1) asFloat.
		0 to: width-1 do:
			[:x |  xRel := x asFloat / (width-1) asFloat.
			poker pixelAt: x@y
				put: ((colorBlock value: xRel value: yRel) pixelWordForDepth: self depth)]]
"
 | d |
((Form extent: 100@20 depth: Display depth)
	fillFromXYColorBlock:
	[:x :y | d := 1.0 - (x - 0.5) abs - (y - 0.5) abs.
	Color r: d g: 0 b: 1.0-d]) display
"
]

{ #category : 'filling' }
Form >> fillFromYColorBlock: colorBlock [
	"Vertical Gradient Fill.
	Supply relative y in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| yRel |
	0 to: height-1 do:
		[:y |  yRel := y asFloat / (height-1) asFloat.
		self fill: (0@y extent: width@1) 
			fillColor: (colorBlock value: yRel)]
"
((Form extent: 100@100 depth: Display depth)
	fillFromYColorBlock: [:y | Color r: y g: 0.0 b: 0.5]) display
"
]

{ #category : 'coloring' }
Form >> fillGray [
	"Set all bits in the receiver to gray."

	self fill: self boundingBox fillColor: `Color gray`
]

{ #category : 'coloring' }
Form >> fillGray: aRectangle [
	"Set all bits in the receiver's area defined by aRectangle to the gray mask."

	self fill: aRectangle rule: Form over fillColor: `Color gray`
]

{ #category : 'coloring' }
Form >> fillShape: aShapeForm fillColor: aColor [
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	^ self fillShape: aShapeForm fillColor: aColor at: `0@0`
]

{ #category : 'coloring' }
Form >> fillShape: aShapeForm fillColor: aColor at: location [
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	((BitBlt destForm: self sourceForm: aShapeForm fillColor: aColor
		combinationRule: Form paint
		destOrigin: location + aShapeForm offset sourceOrigin: `0@0`
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits
]

{ #category : 'private' }
Form >> fillSmall1BitWith: pixelBits [
	"Fill a narrow 1 bit Form.
	Each value in argument holds pixels (i.e. bits) for one line.
	Useful for Cursors and small icons."

	| shift |
	self assert: depth = 1.
	self assert: width <= 32.						"meaning self wordsPerLine = 1."
	shift := 32 - width.
	1 to: (height min: pixelBits size) do: [ :i |
		bits at: i put: (pixelBits at: i) << shift ].
]

{ #category : 'coloring' }
Form >> fillWhite [
	"Set all bits in the form to white."

	self fill: self boundingBox fillColor: `Color white`
]

{ #category : 'coloring' }
Form >> fillWhite: aRectangle [
	"Set all bits in the receiver's area defined by aRectangle to white."

	self fill: aRectangle rule: Form over fillColor: `Color white`
]

{ #category : 'coloring' }
Form >> fillWithColor: aColor [
	"Fill the receiver's bounding box with the given color."

	self fill: self boundingBox fillColor: aColor
]

{ #category : 'other' }
Form >> fixAlpha [
	"Fix the alpha channel if the receiver is 32bit
	For any pixel with zero alpha value, and not zero rgb, set alpha to 255.
	Note: assumes that a pixel with 0 is meant to be transparent, and won't make it opaque black.
	See #makeAllPixelsOpaque"
	| bb |
	self depth = 32 ifFalse: [ ^self ].
	bb := BitBlt toForm: self.
	bb combinationRule: 40. "fixAlpha:with:"
	bb copyBits
]

{ #category : 'scaling, rotation' }
Form >> flippedBy: direction [
	"Return a copy of the receiver flipped either #vertical, #horizontal or #both. (#both is a 180 degrees rotation)
	ImageMorph defaultForm display.
	(ImageMorph defaultForm flippedBy: #vertical) display.
	(ImageMorph defaultForm flippedBy: #horizontal) display.
	(ImageMorph defaultForm flippedBy: #both) display.
	"
	| newForm quad |
	newForm := self class extent: self extent depth: depth.
	quad := self boundingBox innerCorners.
	quad := (
		direction caseOf: {
			[ #vertical ] 		-> [#(2 1 4 3)].
			[ #horizontal ] 		-> [#(4 3 2 1)].
			[ #both ] 			-> [#(3 4 1 2)]})
		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
"	newForm offset: (self offset flippedBy: direction centerAt: aPoint)."
	^ newForm
"
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
			flippedBy: #vertical centerAt: 0@0) display]
"
"Consistency test...
 | f f2 p |
[ Sensor isAnyButtonPressed ] whileFalse: [
	f := Form fromDisplay: ((p := Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 := f flippedBy: #vertical centerAt: 0@0.
	(f2 flippedBy: #vertical centerAt: 0@0) displayAt: p ]
"
]

{ #category : 'initialization' }
Form >> fromDisplay: aRectangle  [
	"Create a virtual bit map from a user specified rectangular area on the 
	display screen. Reallocates bitmap only if aRectangle ~= the receiver's 
	extent."

	(width = aRectangle width and: [height = aRectangle height])
		ifFalse: [self setExtent: aRectangle extent depth: depth].
	self
		copyBits: (aRectangle origin extent: self extent)
		from: Display
		at: `0 @ 0`
		clippingBox: self boundingBox
		rule: Form over
]

{ #category : 'accessing' }
Form >> getCanvas [
	"Return a Canvas that can be used to draw onto the receiver"
	^MorphicCanvas onForm: self
]

{ #category : 'private' }
Form >> hackBits: bitThing [
	"This method provides an initialization so that BitBlt may be used, eg, to 
	copy ByteArrays and other non-pointer objects efficiently.
	The resulting form looks 4 wide, 8 deep, and bitThing-size-in-words high."
	bitThing class isBits ifFalse: [self error: 'bitThing must be a non-pointer object'].
	depth := 8.
	width := 4.
	height := bitThing byteSize // width.
	bits := bitThing
]

{ #category : 'private' }
Form >> hackBits64: bitThing [
	"This method provides an initialization so that BitBlt may be used, eg, to 
	copy ByteArrays and other non-pointer objects efficiently.
	The resulting form looks 8 wide, 8 deep, and bitThing-size-in-words high."
	bitThing class isBits ifFalse: [self error: 'bitThing must be a non-pointer object'].
	depth := 8.
	width := 8.
	height := bitThing byteSize // width.
	bits := bitThing
]

{ #category : 'comparing' }
Form >> hash [
	"Hash is re-implemented because #= is re-implemented.
	But it is expensive.

	Storing (sub)instances of Form in Sets, or using them as keys in Dictionaries is 
	usually not a good idea, because #= and #hash are expensive, and because Forms
	are usually mutable.
	Maybe use an IdentitySet or IdentityDictionary instead?"

	^bits hash
]

{ #category : 'accessing' }
Form >> height [
	^ height
]

{ #category : 'pixel access' }
Form >> i: i j: j [
	"Compatibility with Matrices"
	^ self colorAt: j@i -1
]

{ #category : 'pixel access' }
Form >> i: i j: j put: aColor [
	"Compatibility with Matrices"
	^ self colorAt: j@i -1 put: aColor
]

{ #category : 'converting' }
Form >> icon [
	"Answer a 16 x 16 icon of myself"
	
	^self magnifyTo: `16 @ 16`
]

{ #category : 'private' }
Form >> initFromArray: array [
	"Fill the bitmap from array.  If the array is shorter,
	then cycle around in its contents until the bitmap is filled."
	| ax aSize |
	ax := 0.
	aSize := array size.
	1 to: bits size do: [ :index |
		(ax := ax + 1) > aSize ifTrue: [ax := 1].
		bits at: index put: (array at: ax)]
]

{ #category : 'analyzing' }
Form >> innerPixelRectFor: pv orNot: not [
	"Return a rectangle describing the smallest part of me that includes 
	all pixels of value pv.
	Note:  If orNot is true, then produce a copy that includes all pixels
	that are DIFFERENT from the supplied (background) value"

	| xTally yTally |
	xTally := self xTallyPixelValue: pv orNot: not.
	yTally := self yTallyPixelValue: pv orNot: not.
	^ ((xTally findFirst: [:t | t>0]) - 1) @ ((yTally findFirst: [:t | t>0]) - 1)
		corner:
			(xTally findLast: [:t | t>0])@(yTally findLast: [:t | t>0])
]

{ #category : 'testing' }
Form >> is: aSymbol [
	^ aSymbol == #Form or: [ super is: aSymbol ]
]

{ #category : 'testing' }
Form >> isAllWhite [
	"Answer whether all bits in the receiver are white (=0)."

	1 to: bits size do: [:i | (bits at: i) = 0 ifFalse: [^ false]].
	^ true
]

{ #category : 'analyzing' }
Form >> isAnyPixel: pv [
	"Return an array of the number of pixels with value pv by y-value.
	Note that if not is true, then this will tally those different from pv.
	Based on #yTallyPixelValue:orNot:
	Form lena isAnyPixel: 0
	Form lena isAnyPixel: 100
	"
	| cm slice copyBlt countBlt |
	cm := self newColorMap.		"Map all colors but pv to zero"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice := Form extent: width@1.
	copyBlt := (BitBlt destForm: slice sourceForm: self
				combinationRule: Form over
				destOrigin: `0@0` sourceOrigin: `0@0` extent: slice width @ 1
				clipRect: slice boundingBox)
					colorMap: cm.
	countBlt := (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (`0@0` extent: slice extent);
				combinationRule: 32.
	(0 to: height-1) do: [ :y |
		copyBlt sourceOrigin: 0@y; copyBits.
		countBlt copyBits > 0 ifTrue: [ ^ true ]].
	^ false
]

{ #category : 'testing' }
Form >> isBigEndian [
	"Return true if the receiver contains big endian pixels, meaning the left-most pixel is stored in the most significant bits of a word."
	^depth > 0
]

{ #category : 'testing' }
Form >> isLittleEndian [
	"Return true if the receiver contains little endian pixels, meaning the left-most pixel is stored in the least significant bits of a word."
	^depth < 0
]

{ #category : 'pixel access' }
Form >> isTransparentAt: aPoint  [
	"Return true if the receiver is transparent at the given point."

	| p d |
	d := self depth.

	"no transparency at depth 1"
	d = 1 ifTrue: [^ false].
	
	"Check inside the palette"
	d <= 8 ifTrue: [ 
		^(self colorAt: aPoint) isTransparent ].

	p := self pixelValueAt: aPoint.

	"32bpp, rgba. Pixels with alpha = 0 are transparent"
	d = 32 ifTrue: [
		^(p anyMask: 16rFF000000) not ].

	"pixel pattern 0 is transparent for RGB"
	^p = 0
]

{ #category : 'scaling, rotation' }
Form >> magnify: aRectangle by: scale  [
	"Answer a Form created as a scaling of the receiver.
	Scale may be a Float, and may be greater or less than 1.0."
	^ self magnify: aRectangle by: scale smoothing: 1

"Dynamic test...
[Sensor isAnyButtonPressed] whileFalse:
	[(Display magnify: (Sensor mousePoint extent: 31@41) by: 5@3) display]
"
"Scaling test...
| f cp | f := Form fromDisplay: (Rectangle originFromUser: 100@100).
Display restoreAfter: [Sensor waitNoButton.
[Sensor isAnyButtonPressed] whileFalse:
	[cp := Sensor mousePoint.
	(f magnify: f boundingBox by: (cp x asFloat@cp y asFloat)/f extent) display]]
"
"Consistency test...
 | f f2 p | [Sensor isAnyButtonPressed] whileFalse:
	[f := Form fromDisplay: ((p := Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 := f magnify: f boundingBox by: 5@3.
	(f2 shrink: f2 boundingBox by: 5@3) displayAt: p]
"

]

{ #category : 'scaling, rotation' }
Form >> magnify: aRectangle by: scale smoothing: cellSize [
        "Answer a Form created as a scaling of the receiver.
        Scale may be a Float, and may be greater or less than 1.0."
        | newForm |
        newForm := self blankCopyOf: aRectangle scaledBy: scale.
        (WarpBlt toForm: newForm)
                sourceForm: self;
                colorMap: (self colormapIfNeededFor: newForm);
                cellSize: cellSize;  "installs a new colormap if cellSize > 1"
                combinationRule: 3;
                copyQuad: aRectangle innerCorners toRect: newForm boundingBox.
        ^ newForm

"Dynamic test...
[Sensor isAnyButtonPressed] whileFalse:
        [(Display magnify: (Sensor mousePoint extent: 131@81) by: 0.5 smoothing: 2) display]
"
"Scaling test...
| f cp | f := Form fromDisplay: (Rectangle originFromUser: 100@100).
Display restoreAfter: [Sensor waitNoButton.
[Sensor isAnyButtonPressed] whileFalse:
        [cp := Sensor mousePoint.
        (f magnify: f boundingBox by: (cp x asFloat@cp y asFloat)/f extent smoothing: 2) display]]
"
]

{ #category : 'scaling, rotation' }
Form >> magnify: aRectangle to: extent smoothing: cellSize [
        "Answer a Form created as a scaling of the receiver.
        Scale may be a Float, and may be greater or less than 1.0."
        | newForm |
        newForm := Form extent: extent depth: depth.
        (WarpBlt toForm: newForm)
                sourceForm: self;
                colorMap: (self colormapIfNeededFor: newForm);
                cellSize: cellSize;  "installs a new colormap if cellSize > 1"
                combinationRule: 3;
                copyQuad: aRectangle innerCorners toRect: newForm boundingBox.
        ^ newForm

"Dynamic test...
[Sensor isAnyButtonPressed] whileFalse:
        [(Display magnify: (Sensor mousePoint extent: 131@81) to: 300@200 smoothing: 2) display]
"
]

{ #category : 'scaling, rotation' }
Form >> magnifyBy: scale  [
	"Answer a Form created as a scaling of the receiver.
	Scale may be a Float, and may be greater or less than 1.0."

	^ self magnify: self boundingBox by: scale
			smoothing: (scale magnitude < 1 ifTrue: [2] ifFalse: [1])
]

{ #category : 'scaling, rotation' }
Form >> magnifyTo: extent [
	"Answer a Form created as a scaling of the receiver."

	^ self
		magnify: self boundingBox
		to: extent
		smoothing: 3
]

{ #category : 'other' }
Form >> makeAllPixelsAlpha: a [
	"Fix the alpha channel if the receiver is 32bit
	Set alpha values to 255 for all pixels.
	Note: assumes that a pixel with 0 is meant to be black, not transparent (As Squeak / Cuis usually handles rgb=0 for bpp<=16).
	See #fixAlpha"
	| bitBlt |
	self depth = 32 ifFalse: [ ^self ].
	bitBlt := BitBlt toForm: self.
	bitBlt combinationRule: 1.
	bitBlt fillBitmap: (Bitmap with: (16r00FFFFFF)).
	bitBlt copyBits.
	bitBlt combinationRule: 7.
	bitBlt fillBitmap: (Bitmap with: (a bitShift: 24)).
	bitBlt copyBits
]

{ #category : 'other' }
Form >> makeAllPixelsOpaque [
	"Fix the alpha channel if the receiver is 32bit
	Set alpha values to 255 for all pixels.
	Note: assumes that a pixel with 0 is meant to be black, not transparent (As Squeak / Cuis usually handles rgb=0 for bpp<=16).
	See #fixAlpha"
	| bitBlt |
	self depth = 32 ifFalse: [ ^self ].
	bitBlt := BitBlt toForm: self.
	bitBlt combinationRule: 7. "bitOr:with:"
	bitBlt fillBitmap: (Bitmap with: 16rFF000000).
	bitBlt copyBits
]

{ #category : 'other' }
Form >> makeAllPixelsOpaqueBlack [
	"Make all pixels opaque black.
	Useful for preparing 32bpp forms for later stuff.
	Set alpha values to 255 for all pixels.
	Set r, g, b values to zero for all pixels."
	| bitBlt |
	bitBlt := BitBlt toForm: self.
	bitBlt combinationRule: 3.
	bitBlt fillBitmap: (Bitmap with: 16rFF000000).
	bitBlt copyBits
]

{ #category : 'color mapping' }
Form >> mapColor: oldColor to: newColor [
	"Make all pixels of the given color in this Form to the given new color."
	"Warnings: This method modifies the receiver. It may lose some color accuracy on 32-bit Forms, since the transformation uses a color map with only 15-bit resolution."

	| map |
	map := (Color cachedColormapFrom: self depth to: self depth) copy.
	map at: (oldColor indexInMap: map) put: (newColor pixelWordForDepth: self depth).
	(BitBlt toForm: self)
		sourceForm: self;
		sourceOrigin: `0@0`;
		combinationRule: Form over;
		destX: 0 destY: 0 width: width height: height;
		colorMap: map;
		copyBits
]

{ #category : 'color mapping' }
Form >> maskingMap [
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero."
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."
	^Color maskingMap: self depth
]

{ #category : 'testing' }
Form >> mightBeTranslucent [
	"Answer whether this form may be translucent"
	^self depth = 32
]

{ #category : 'accessing' }
Form >> nativeDepth [
	"Return the 'native' depth of the receiver, e.g., including the endianess"
	^depth
]

{ #category : 'color mapping' }
Form >> newColorMap  [
	"Return an uninitialized color map array appropriate to this Form's depth."

	^ Bitmap new: (1 bitShift: (self depth min: 15))

]

{ #category : 'accessing' }
Form >> offset [
	^offset ifNil: [`0@0`]
]

{ #category : 'accessing' }
Form >> offset: aPoint  [
	offset := aPoint
]

{ #category : 'converting' }
Form >> orderedDither32To16 [
	"Do an ordered dithering for converting from 32 to 16 bit depth."
	| ditherMatrix ii out inBits outBits index pv dmv r di dmi dmo g b pvOut outIndex |
	self depth = 32 ifFalse:[^self error:'Must be 32bit for this'].
	ditherMatrix := #(	
						0	8	2	10
						12	4	14	6
						3	11	1	9
						15	7	13	5).
	ii := (0 to: 31) collect:[:i| i].
	out := Form extent: self extent depth: 16.
	inBits := self bits.
	outBits := out bits.
	index := outIndex := 0.
	pvOut := 0.
	0 to: self height-1 do:[:y|
		0 to: self width-1 do:[:x|
			pv := inBits at: (index := index + 1).
			dmv := ditherMatrix at: (y bitAnd: 3) * 4 + (x bitAnd: 3) + 1.
			r := pv bitAnd: 255.	di := r * 496 bitShift: -8.
			dmi := di bitAnd: 15.	dmo := di bitShift: -4.
			r := dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			g := (pv bitShift: -8) bitAnd: 255.	di := g * 496 bitShift: -8.
			dmi := di bitAnd: 15.	dmo := di bitShift: -4.
			g := dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			b := (pv bitShift: -16) bitAnd: 255.	di := b * 496 bitShift: -8.
			dmi := di bitAnd: 15.	dmo := di bitShift: -4.
			b := dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			pvOut := (pvOut bitShift: 16) + 
						(b bitShift: 10) + (g bitShift: 5) + r.
			(x bitAnd: 1) = 1 ifTrue:[
				outBits at: (outIndex := outIndex+1) put: pvOut.
				pvOut := 0].
		].
		(self width bitAnd: 1) = 1 ifTrue:[
			outBits at: (outIndex := outIndex+1) put: (pvOut bitShift: -16).
			pvOut := 0].
	].
	^out
]

{ #category : 'transitions' }
Form >> pageWarp: otherImage at: topLeft forward: forward [
	"Produce a page-turning illusion that gradually reveals otherImage
	located at topLeft in this form.
	forward == true means turn pages toward you, else away. [ignored for now]"
	| pageRect oldPage nSteps buffer p leafRect sourceQuad warp oldBottom d delay |
	delay := Delay forSeconds: 0.1.
	pageRect := otherImage boundingBox.
	oldPage := self copy: (pageRect translatedBy: topLeft).
	(forward ifTrue: [oldPage] ifFalse: [otherImage])
		border: pageRect
		widthRectangle: (Rectangle
				left: 0
				right: 2
				top: 1
				bottom: 1)
		rule: Form over
		fillColor: `Color black`.
	oldBottom := self copy: ((pageRect bottomLeft + topLeft) extent: (pageRect width@(pageRect height//4))).
	nSteps := 8.
	buffer := Form extent: otherImage extent + (0@(pageRect height//4)) depth: self depth.
	d := pageRect topLeft + (0@(pageRect height//4)) - pageRect topRight.
	1 to: nSteps-1 do:
		[:i | forward
			ifTrue: [buffer copy: pageRect from: otherImage to: `0@0` rule: Form over.
					p := pageRect topRight + (d * i // nSteps)]
			ifFalse: [buffer copy: pageRect from: oldPage to: `0@0` rule: Form over.
					p := pageRect topRight + (d * (nSteps-i) // nSteps)].
		buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
		leafRect := pageRect topLeft corner: p x @ (pageRect bottom + p y).
		sourceQuad := Array with: pageRect topLeft
			with: pageRect bottomLeft + (0@p y)
			with: pageRect bottomRight
			with: pageRect topRight - (0@p y).
		warp := (WarpBlt toForm: buffer)
				clipRect: leafRect;
				sourceForm: (forward ifTrue: [oldPage] ifFalse: [otherImage]);
				combinationRule: Form paint.
		warp copyQuad: sourceQuad toRect: leafRect.
		self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
		Display forceToScreen: leafRect.
		delay wait.
		].

	buffer copy: pageRect from: otherImage to: `0@0` rule: Form over.
	buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
	self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
	Display forceToScreen.
	delay wait.
"
1 to: 4 do: [:corner | Display pageWarp:
				(Form fromDisplay: (10@10 extent: 200@300)) reverse
			at: 10@10 forward: false]
"
]

{ #category : 'displaying' }
Form >> paintBits: sourceForm at: destOrigin translucent: factor [
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt destForm: self
		sourceForm: sourceForm
		combinationRule: 31
		destOrigin: destOrigin
		sourceOrigin: `0@0`
		extent: sourceForm extent
		clipRect: self boundingBox)
			copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f := Form fromUser. f replaceColor: f dominantColor withColor: Color transparent.
f2 := Form fromDisplay: (0@0 extent: f extent). f3 := f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 := f2 copy. f3 paintBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"
]

{ #category : 'analyzing' }
Form >> pixelCompare: aRect with: otherForm at: otherLoc [
	"Compare the selected bits of this form (those within aRect) against
	those in a similar rectangle of otherFrom.  Return the sum of the
	absolute value of the differences of the color values of every pixel.
	Obviously, this is most useful for rgb (16- or 32-bit) pixels but,
	in the case of 8-bits or less, this will return the sum of the differing
	bits of the corresponding pixel values (somewhat less useful)"
	"Just use 32 bits..."
	| pixPerWord temp |
	otherForm nativeDepth = 32 ifFalse: [
		^ self pixelCompare: aRect with: (otherForm asFormOfDepth: 32) at: otherLoc ].
	self nativeDepth = 32 ifFalse: [
		^ (self asFormOfDepth: 32) pixelCompare: aRect with: otherForm at: otherLoc ].
	
	pixPerWord := 32//self depth.
	(aRect left\\pixPerWord = 0 and: [aRect right\\pixPerWord = 0]) ifTrue: [
		"If word-aligned, use on-the-fly difference"
		^ (BitBlt toForm: self) copy: aRect from: otherLoc in: otherForm
				fillColor: nil rule: 32].
	"Otherwise, combine in a word-sized form and then compute difference"
	temp := self copy: aRect.
	temp copy: aRect from: otherLoc in: otherForm rule: 21.
	^ (BitBlt toForm: temp) copy: aRect from: otherLoc in: nil
				fillColor: (Bitmap with: 0) rule: 32
"  Dumb example prints zero only when you move over the original rectangle...
 | f diff |
f := Form fromUser.
[ Sensor isAnyButtonPressed ] whileFalse: [
	diff := f
		pixelCompare: f boundingBox
		with: Display
		at: Sensor mousePoint.
	Display fill: (0@0 extent: 100@20) fillColor: Color white.
	diff printString , '        ' displayAt: 0@0 ]
"
]

{ #category : 'pixel access' }
Form >> pixelValueAt: aPoint [
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth. Typical clients use colorAt: to get a Color.  "

	depth = 32 ifTrue: [^ bits basicAt: aPoint y * width + aPoint x + 1].
	^ self primPixelValueAtX: aPoint x y: aPoint y
]

{ #category : 'pixel access' }
Form >> pixelValueAt: aPoint put: pixelValue [
	"Store the given raw pixel value at the given point. Typical clients use colorAt:put: to store a color. "

	aPoint isIntegerPoint ifFalse: [
		^self pixelValueAt: aPoint asIntegerPoint put: pixelValue ].
	depth = 32 ifTrue: [^ bits basicAt: aPoint y * width + aPoint x + 1 put: pixelValue].
	(BitBlt bitPokerToForm: self) pixelAt: aPoint put: pixelValue
]

{ #category : 'color mapping' }
Form >> pixelValueFor: aColor [
	"Return the pixel word for representing the given color on the receiver"
	^aColor pixelValueForDepth: self depth
]

{ #category : 'accessing' }
Form >> pixelsPerWord [
	^self pixelsPerWord: self depth
]

{ #category : 'accessing' }
Form >> pixelsPerWord: aDepth [
	^32 // aDepth
]

{ #category : 'copying' }
Form >> postCopy [
	self
		 bits: bits copy;
		 offset: offset copy
]

{ #category : 'other' }
Form >> preMultiplyAlpha [
	"Pre-multiply each pixel by its alpha, for proper alpha compositing (BitBlt rule 34).
	E.g., half-transparent green 16r7F00FF00 becomes 16r7F007F00"

	| v a r g b |
	depth = 32 ifFalse: [^self].
	1 to: bits size do: [ :i |
		v := bits at: i.
		a := v bitShift: -24.
		r := ((v bitShift: -16) bitAnd: 255) * a // 255.
		g := ((v bitShift: -8) bitAnd: 255) * a // 255.
		b := (v bitAnd: 255) * a // 255.
		bits at: i put: (a bitShift: 24) + (r bitShift: 16) + (g bitShift: 8) + b].
]

{ #category : 'analyzing' }
Form >> primCountBits [
	"Count the non-zero pixels of this form."
	self depth > 8 ifTrue: [
		^(self asFormOfDepth: 8) primCountBits].
	^ (BitBlt toForm: self)
		fillColor: (Bitmap with: 0);
		destRect: self boundingBox;
		combinationRule: 32;
		copyBits
]

{ #category : 'pixel access' }
Form >> primPixelValueAtX: x y: y [
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth.
	Typical clients use colorAt: to get a Color"

	<primitive: 'primitivePixelValueAt' module:'BitBltPlugin'>
	^(BitBlt bitPeekerFromForm: self) pixelAt: x@y
]

{ #category : 'other' }
Form >> primPrintHScale: hScale vScale: vScale landscape: aBoolean [
	"On platforms that support it, this primitive prints the receiver, assumed to be a Form, to the default printer."
	"(Form extent: 10@10) primPrintHScale: 1.0 vScale: 1.0 landscape: true"

	<primitive: 232>
	self primitiveFailed

]

{ #category : 'fileIn/Out' }
Form >> printOn: aStream [
	aStream isText
		ifTrue:
			[aStream withAttribute: (TextAnchor new anchoredFormOrMorph: self) do: [aStream nextPut: $*].
			^ self].
	aStream
		nextPutAll: self class name;
		nextPut: $(; print: width;
		nextPut: $x; print: height;
		nextPut: $x; print: depth;
		nextPut: $)
]

{ #category : 'fileIn/Out' }
Form >> readAttributesFrom: aBinaryStream [
	| offsetX offsetY |
	depth := aBinaryStream next.
	(self depth isPowerOfTwo and: [self depth between: 1 and: 32])
		ifFalse: [self error: 'invalid depth; bad Form file?'].
	width := aBinaryStream nextUint16BigEndian: true.
	height := aBinaryStream nextUint16BigEndian: true.
	offsetX  := aBinaryStream nextInt16BigEndian: true.
	offsetY := aBinaryStream nextInt16BigEndian: true.
	offset := Point x: offsetX y: offsetY
]

{ #category : 'fileIn/Out' }
Form >> readBitsFrom: aBinaryStream [
	
	bits := Bitmap newFromStream: aBinaryStream.
	bits size = self bitsSize ifFalse: [self error: 'wrong bitmap size; bad Form file?'].
	^ self

]

{ #category : 'fileIn/Out' }
Form >> readFrom: aBinaryStream [
	"Reads the receiver from the given binary stream with the format:
		depth, extent, offset, bits."
	self readAttributesFrom: aBinaryStream.
	self readBitsFrom: aBinaryStream
]

{ #category : 'coloring' }
Form >> reverse [
	"Change all the bits in the receiver that are white to black, and the ones 
	that are black to white.
	Display reverse. Display forceToScreen.
	"

	(BitBlt toForm: self)
		combinationRule: `Form reverse`;
		copyBits.
]

{ #category : 'coloring' }
Form >> reverse: aRectangle [
	"Change all the bits in the receiver's area that intersects with aRectangle 
	that are white to black, and the ones that are black to white."

	self fill: aRectangle rule: Form reverse fillColor: `Color white`
]

{ #category : 'coloring' }
Form >> reverse: aRectangle fillColor: aMask	 [
	"Change all the bits in the receiver's area that intersects with aRectangle 
	according to the mask. Black does not necessarily turn to white, rather it 
	changes with respect to the rule and the bit in a corresponding mask 
	location. Bound to give a surprise."

	self fill: aRectangle rule: Form reverse fillColor: aMask
]

{ #category : 'scaling, rotation' }
Form >> rotated180Degrees [
	"Return a copy of the receiver rotated by 180 degrees.
	ImageMorph defaultForm display.
	ImageMorph defaultForm rotated90Degrees display.
	ImageMorph defaultForm rotated180Degrees display.
	ImageMorph defaultForm rotated270Degrees display.
	"
	| newForm quad |
	newForm := self class extent: self extent depth: depth.
	quad := self boundingBox innerCorners.
	quad := #(3 4 1 2)		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
	^ newForm
]

{ #category : 'scaling, rotation' }
Form >> rotated270Degrees [
	"Return a copy of the receiver rotated by 270 degrees.
	ImageMorph defaultForm display.
	ImageMorph defaultForm rotated90Degrees display.
	ImageMorph defaultForm rotated180Degrees display.
	ImageMorph defaultForm rotated270Degrees display.
	"
	| newForm quad |
	newForm := self class extent: self extent transposed depth: depth.
	quad := self boundingBox innerCorners.
	quad := #(4 1 2 3)		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
	^ newForm
]

{ #category : 'scaling, rotation' }
Form >> rotated90Degrees [
	"Return a copy of the receiver rotated by 90 degrees.
	ImageMorph defaultForm display.
	ImageMorph defaultForm rotated90Degrees display.
	ImageMorph defaultForm rotated180Degrees display.
	ImageMorph defaultForm rotated270Degrees display.
	"
	| newForm quad |
	newForm := self class extent: self extent transposed depth: depth.
	quad := self boundingBox innerCorners.
	quad := #(2 3 4 1)		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
	^ newForm
]

{ #category : 'scaling, rotation' }
Form >> rotatedBy: deg smoothing: cellSize [
	"Rotate the receiver by the indicated number of degrees."
	"rot is the destination form, bit enough for any angle."
	| side rot warp r1 pts p center |
	side := 1 + ((width*width) + (height*height)) asFloat sqrt asInteger.
	rot := Form extent: side@side depth: self depth.
	center := rot extent // 2.

	"Now compute the sin and cos constants for the rotation angle." 
	warp := (WarpBlt toForm: rot)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: rot);
		cellSize: cellSize;  "installs a new colormap if cellSize > 1"
		combinationRule: Form over.
	r1 := rot boundingBox aligned: center with: self boundingBox center.

	pts := r1 innerCorners collect: [ :pt |
		p := pt - r1 center.
		(r1 center x asFloat + (p x asFloat*deg degreeCos) + (p y asFloat*deg degreeSin)) @
		(r1 center y asFloat - (p x asFloat*deg degreeSin) + (p y asFloat*deg degreeCos))].
	warp copyQuad: pts toRect: rot boundingBox.
	^ rot
"
 | a f |  f := Form fromDisplay: (0@0 extent: 200@200).  a := 0.
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
		rotatedBy: (a := a+0.1) smoothing: 2) display].
f display
"
]

{ #category : 'scaling, rotation' }
Form >> rotatedByDegrees: deg [
	"Rotate the receiver by the indicated number of degrees."
	"rot is the destination form, bit enough for any angle."

	^ self rotatedBy: deg smoothing: 1
"
 | a f |  f := Form fromDisplay: (0@0 extent: 200@200).  a := 0.
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
		rotatedByDegrees: (a := a+0.1)) display].
f display
"
]

{ #category : 'private' }
Form >> setExtent: extent depth: bitsPerPixel [
	"Create a virtual bit map with the given extent and bitsPerPixel."

	| newBits newDepth newHeight newWidth |

	"Find parameters and allocate new bits before modifying this instance.
	If, for whatever reason, allocation fails, an exception will be raised
	and this instance (if already initialized) will still be in a consistent state."
	newWidth := extent x asInteger max: 0.
	newHeight := extent y asInteger max: 0.
	newDepth := bitsPerPixel.
	newBits := Bitmap new: (self bitsSizeForWidth: newWidth height: newHeight depth: newDepth abs).

	"If we get to this point, allocation succeeded. If we are already initialized (bits notNil), then set
	width, height and depth before setting bits, only if bits will later shrink. This is to avoid an unlikely
	invalid access in some other process if this instance is used elsewhere. This could be a serious problem
	for instance for Display."
	(bits notNil and: [ 		newBits size < bits size ]) ifTrue: [
		width := newWidth.
		height := newHeight.
		depth := newDepth ].

	"All good. Assign the new state."
	bits := newBits.
	width := newWidth.
	height := newHeight.
	depth := newDepth.
]

{ #category : 'private' }
Form >> setExtent: extent depth: bitsPerPixel bits: bitmap [
	"Create a virtual bit map with the given extent and bitsPerPixel."
	width := extent x asInteger.
	width < 0 ifTrue: [ width := 0 ].
	height := extent y asInteger.
	height < 0 ifTrue: [ height := 0 ].
	depth := bitsPerPixel.
	bits := bitmap
]

{ #category : 'scaling, rotation' }
Form >> shrink: aRectangle by: scale  [
	| scalePt |
	scalePt := scale asPoint.
	^ self magnify: aRectangle by: (1.0 / scalePt x asFloat) @ (1.0 / scalePt y asFloat)
]

{ #category : 'accessing' }
Form >> size [
	"Should no longer be used -- use bitsSize instead.  length of variable part of instance."
	^ super size
]

{ #category : 'transitions' }
Form >> slideImage: otherImage at: topLeft delta: delta [
	"
	Display slideImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse
		at: 40@40 delta: 3@ -4
	"
	| bb nSteps clipRect |
	bb := otherImage boundingBox.
	clipRect := topLeft extent: otherImage extent.
	nSteps := 1.
	delta x = 0 ifFalse: [nSteps := nSteps max: (bb width//delta x abs) + 1].
	delta y = 0 ifFalse: [nSteps := nSteps max: (bb height//delta y abs) + 1].
	1 to: nSteps do: [ :i | 
			self 
				copyBits: bb from: otherImage
				at: delta*(i-nSteps) + topLeft
				clippingBox: clipRect rule: Form paint.
			Display forceToScreen: clipRect.
		]
]

{ #category : 'image manipulation' }
Form >> smear: dir distance: dist [
	"Smear any black pixels in this form in the direction dir in Log N steps"
	| skew bb |
	bb := BitBlt destForm: self sourceForm: self
		combinationRule: Form under destOrigin: `0@0` sourceOrigin: `0@0`
		extent: self extent clipRect: self boundingBox.
	skew := 1.
	[skew < dist] whileTrue: [
		bb destOrigin: dir*skew; copyBits.
		skew := skew+skew]
]

{ #category : 'fileIn/Out' }
Form >> storeBitsOn: aStream base: anInteger [
	bits do: [ :word | 
		anInteger = 10
			ifTrue: [aStream space]
			ifFalse: [aStream newLineTab: 2].
		anInteger = 2
			ifTrue: [
				"Print binary with radix, but padded, so the bit pattern is easy to see."
				aStream nextPut: $2. 
				aStream nextPut: $r. 
				word printOn: aStream base: 2 length: 32 padded: true ]
			ifFalse: [
				word storeOn: aStream base: anInteger ]]
]

{ #category : 'fileIn/Out' }
Form >> storeOn: aStream [

	self storeOn: aStream base: 10
]

{ #category : 'fileIn/Out' }
Form >> storeOn: aStream base: anInteger  [
	"Store the receiver out as an expression that can be evaluated to recreate a Form with the same contents as the original."

	(depth = 1 and: [ width <= 32 ]) ifTrue: [
		^self storeSmall1BitOn: aStream ].

	aStream nextPut: $(.
	aStream nextPutAll: self species name.
	aStream newLineTab: 1.
	aStream nextPutAll: 'extent: '.
	self extent printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'depth: '.
	self nativeDepth printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'fromArray: #('.
	self storeBitsOn: aStream base: anInteger.
	aStream nextPut: $).
	aStream newLineTab: 1.
	aStream nextPutAll: 'offset: '.
	self offset printOn: aStream.
	aStream nextPut: $).

]

{ #category : 'fileIn/Out' }
Form >> storeSmall1BitBitsOn: aStream [
	"Only valid for 1 bit narrow Forms."

	| shift |
	shift := 32 - width.
	bits do: [ :word | 
		"Print binary with radix, but padded, so the bit pattern is easy to see."
		aStream newLineTab: 2.
		aStream nextPut: $2. 
		aStream nextPut: $r. 
		word >> shift printOn: aStream base: 2 length: width padded: true ]
]

{ #category : 'fileIn/Out' }
Form >> storeSmall1BitOn: aStream [
	"Store the receiver out as an expression that can be evaluated to recreate a Form with the same contents as the original."
	aStream nextPut: $(.
	aStream nextPutAll: self species name.
	aStream newLineTab: 1.
	aStream nextPutAll: 'small1BitExtent: '.
	self extent printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'pixelBits: #('.
	self storeSmall1BitBitsOn: aStream.
	aStream nextPut: $).
	aStream newLineTab: 1.
	aStream nextPutAll: 'offset: '.
	self offset printOn: aStream.
	aStream nextPut: $).

]

{ #category : 'initialization' }
Form >> swapEndianness [
	"Swap from big to little endian pixels and vice versa"
	depth := 0 - depth.
]

{ #category : 'image manipulation' }
Form >> tallyPixelValues [
	"Return a Bitmap with tallies in it of the number of pixels in this Form that have each pixel value. Note that several Forms may be tallied into the same table by tallyPixelValuesPrimitive:into: with the same table. Also Forms of depth 16 or 32 can be tallied into a tables of size 512, 4096, or 32768 entries by making a direct call with a Bitmap of the given size."

	^ self tallyPixelValuesInRect: self boundingBox
		into: (Bitmap new: (1 bitShift: (self depth min: 15)))
"
Move a little rectangle around the screen and print its tallies...
 | r tallies nonZero |
Cursor blank showWhile: [
[Sensor isAnyButtonPressed] whileFalse:
	[r := Sensor mousePoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies := (Display copy: r) tallyPixelValues.
	nonZero := (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	Display fill: (0@0 extent: Display width@20) fillColor: Color white.
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"
]

{ #category : 'analyzing' }
Form >> tallyPixelValuesInRect: destRect into: valueTable [
	"Tally the selected pixels of this Form into valueTable, a Bitmap of depth 2^depth similar to a color map. Answer valueTable."

	(BitBlt toForm: self)
		sourceForm: self;  "src must be given for color map ops"
		sourceOrigin: `0@0`;
		colorMap: valueTable;
		combinationRule: 33;
		destRect: destRect;
		copyBits.
	^ valueTable

"
Move a little rectangle around the screen and print its tallies...
 | r tallies nonZero |
Cursor blank showWhile: [
[Sensor isAnyButtonPressed] whileFalse:
	[r := Sensor mousePoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies := (Display copy: r) tallyPixelValues.
	nonZero := (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	Display fill: (0@0 extent: Display width@20) fillColor: Color white.
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"
]

{ #category : 'accessing' }
Form >> width [
	^ width
]

{ #category : 'transitions' }
Form >> wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: rectForIndexBlock [

	| i clipRect t rectsOrNil waitTime |
	i := 0.
	clipRect := topLeft extent: otherImage extent.
	clipBox ifNotNil: [ clipRect := clipRect intersect: clipBox ].
	[ rectsOrNil := rectForIndexBlock value: (i := i + 1) ]
		whileNotNil: [
			t := Time localMillisecondClock.
			rectsOrNil do: [ :r |
				self copyBits: r from: otherImage at: topLeft + r topLeft
					clippingBox: clipRect rule: Form over ].
			Display forceToScreen: clipRect.
			waitTime := 3 - (Time localMillisecondClock - t).
			waitTime > 0 ifTrue:
				["(Delay forMilliseconds: waitTime) wait"]]
]

{ #category : 'transitions' }
Form >> wipeImage: otherImage at: topLeft delta: delta [
	"Display wipeImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse
		at: 40@40 delta: 0@ -2"

	self wipeImage: otherImage at: topLeft delta: delta clippingBox: nil.

]

{ #category : 'transitions' }
Form >> wipeImage: otherImage at: topLeft delta: delta clippingBox: clipBox [

	| wipeRect bb nSteps |
	bb := otherImage boundingBox.
	wipeRect := delta x = 0
		ifTrue:
		[delta y = 0 ifTrue: [nSteps := 1. bb "allow 0@0"] ifFalse: [
		nSteps := bb height//delta y abs + 1.  "Vertical movement"
		delta y > 0
			ifTrue: [bb topLeft extent: bb width@delta y]
			ifFalse: [bb bottomLeft+delta extent: bb width@delta y negated]]]
		ifFalse:
		[nSteps := bb width//delta x abs + 1.  "Horizontal movement"
		delta x > 0
			ifTrue: [bb topLeft extent: delta x@bb height]
			ifFalse: [bb topRight+delta extent: delta x negated@bb height]].
	^ self wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: [ :i |
		i <= nSteps
			ifTrue: [{wipeRect translatedBy: (delta* (i-1))}]
			ifFalse: [nil]]
]

{ #category : 'accessing' }
Form >> wordsPerLine [

	^self wordsPerLine: width depth: self depth.

]

{ #category : 'accessing' }
Form >> wordsPerLine: aWidth depth: aDepth [
	| pixPerWord |
	pixPerWord := self pixelsPerWord: aDepth.
	^ aWidth + pixPerWord - 1 // pixPerWord
]

{ #category : 'fileIn/Out' }
Form >> writeAttributesOn: file [

	file nextPut: depth.
	file nextUint16Put: width bigEndian: true.
	file nextUint16Put: height bigEndian: true.
	file nextInt16Put: self offset x bigEndian: true.
	file nextInt16Put: self offset y bigEndian: true
]

{ #category : 'fileIn/Out' }
Form >> writeBMPfileNamed: fName  "Display writeBMPfileNamed: 'display.bmp'" [
	BMPReadWriter putForm: self onFileNamed: fName
]

{ #category : 'fileIn/Out' }
Form >> writeFileNamed: filename  [
	" 
	Display writeFileNamed: 'pp.bmp'
	Display writeFileNamed: 'pp.jpg'
	Display writeFileNamed: 'pp.png'
	Display writeFileNamed: 'ppp'
	"
	ImageReadWriter write: self onFileNamed: filename
]

{ #category : 'fileIn/Out' }
Form >> writeJPEGfileNamed: fileName  [
	"Write a JPEG file to the given filename using default settings"

	self writeJPEGfileNamed: fileName progressive: false

"
Display writeJPEGfileNamed: 'display.jpeg'
Form fromUser writeJPEGfileNamed: 'yourPatch.jpeg'
"
]

{ #category : 'fileIn/Out' }
Form >> writeJPEGfileNamed: fileName  progressive: aBoolean [
	"Write a JPEG file to the given filename using default settings.  Make it progressive or not, depending on the boolean argument"

	JPEGReadWriter2 putForm: self quality: -1 "default" progressiveJPEG: aBoolean onFileNamed: fileName

"
Display writeJPEGfileNamed: 'display.jpeg' progressive: false.
Form fromUser writeJPEGfileNamed: 'yourPatch.jpeg' progressive: true
"
]

{ #category : 'fileIn/Out' }
Form >> writeOn: file [
	"Write the receiver on the file in the format
		depth, extent, offset, bits."
	self writeAttributesOn: file.
	bits writeOn: file
]

{ #category : 'fileIn/Out' }
Form >> writeOnFileNamed: fileName  [
	"Saves the receiver on the file fileName in the format:
		fileCode, depth, extent, offset, bits."

	fileName asFileEntry forceWriteStreamDo: [ :file |
		file binary.
		file nextPut: 2.  "file code = 2"
		self writeOn: file ].

"
 | f |
[(f := Form fromUser) boundingBox area>25] whileTrue:
	[f writeOnFileNamed: 'test.form'.
	(Form fromFileNamed: 'test.form') display].
"
]

{ #category : 'analyzing' }
Form >> xTallyPixelValue: pv orNot: not [
	"Return an array of the number of pixels with value pv by x-value.
	Note that if not is true, then this will tally those different from pv."
	| cm slice countBlt copyBlt |
	cm := self newColorMap.		"Map all colors but pv to zero"
	not ifTrue: [cm atAllPut: 1].		"... or all but pv to one"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice := Form extent: 1@height.
	copyBlt := (BitBlt destForm: slice sourceForm: self
				combinationRule: Form over
				destOrigin: `0@0` sourceOrigin: `0@0` extent: 1 @ slice height
				clipRect: slice boundingBox)
					colorMap: cm.
	countBlt := (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (`0@0` extent: slice extent);
				combinationRule: 32.
	^ (0 to: width-1) collect: [ :x |
		copyBlt sourceOrigin: x@0; copyBits.
		countBlt copyBits]
]

{ #category : 'analyzing' }
Form >> yTallyPixelValue: pv orNot: not [
	"Return an array of the number of pixels with value pv by y-value.
	Note that if not is true, then this will tally those different from pv."
	| cm slice copyBlt countBlt |
	cm := self newColorMap.		"Map all colors but pv to zero"
	not ifTrue: [cm atAllPut: 1].		"... or all but pv to one"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice := Form extent: width@1.
	copyBlt := (BitBlt destForm: slice sourceForm: self
				combinationRule: Form over
				destOrigin: `0@0` sourceOrigin: `0@0` extent: slice width @ 1
				clipRect: slice boundingBox)
					colorMap: cm.
	countBlt := (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (`0@0` extent: slice extent);
				combinationRule: 32.
	^ (0 to: height-1) collect: [ :y |
		copyBlt sourceOrigin: 0@y; copyBits.
		countBlt copyBits]
]

{ #category : 'transitions' }
Form >> zoomIn: goingIn orOutTo: otherImage at: topLeft vanishingPoint: vp  [
	"Display zoomInTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40.
	Display zoomOutTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40."
	| nSteps j bigR lilR minTime startTime lead |
	nSteps := 16.
	minTime := 500.  "milliseconds"
	startTime := Time localMillisecondClock.
	^ self wipeImage: otherImage at: topLeft clippingBox: nil rectForIndex: [ :i | "i runs from 1 to nsteps"
		i > nSteps
			ifTrue: [nil "indicates all done"]
			ifFalse: [
				"If we are going too fast, delay for a bit"
				lead := startTime + (i-1*minTime//nSteps) - Time localMillisecondClock.
				lead > 10 ifTrue: [(Delay forMilliseconds: lead) wait].

				"Return an array with the difference rectangles for this step."
				j := goingIn ifTrue: [i] ifFalse: [nSteps+1-i].
				bigR := vp - (vp*(j)//nSteps) corner:
					vp + (otherImage extent-vp*(j)//nSteps).
				lilR := vp - (vp*(j-1)//nSteps) corner:
					vp + (otherImage extent-vp*(j-1)//nSteps).
				bigR areasOutside: lilR ]]
]

{ #category : 'transitions' }
Form >> zoomInTo: otherImage at: topLeft [
	"Display zoomInTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40"
	^ self zoomIn: true orOutTo: otherImage at: topLeft
		vanishingPoint: otherImage extent//2+topLeft
]

{ #category : 'transitions' }
Form >> zoomOutTo: otherImage at: topLeft [
	"Display zoomOutTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40"
	^ self zoomIn: false orOutTo: otherImage at: topLeft
		vanishingPoint: otherImage extent//2+topLeft
]
