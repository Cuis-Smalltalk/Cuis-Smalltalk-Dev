'From Cuis 4.2 of 25 July 2013 [latest update: #2023] on 30 June 2014 at 3:35:53.340116 pm'!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace '
	classVariableNames: 'CachedColormaps ColorNamesDict GrayToIndexMap IndexedColors MaskingMap PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow RandomStream '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color class methodsFor: 'class initialization' stamp: 'jmv 6/30/2014 15:31'!
initialize
	"
	Color initialize
	"

	"Details: Externally, the red, green, and blue components of color
	are floats in the range [0.0..1.0]. Internally, they are represented
	as integers in the range [0..ComponentMask] packing into a
	small integer to save space and to allow fast hashing and
	equality testing.

	For a general description of color representations for computer
	graphics, including the relationship between the RGB and HSV
	color models used here, see Chapter 17 of Foley and van Dam,
	Fundamentals of Interactive Computer Graphics, Addison-Wesley,
	1982."

	RandomStream _ Random new.

	self initializeIndexedColors.
	self initializeGrayToIndexMap.
	self initializeNames! !

Color initialize!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace'
	classVariableNames: 'CachedColormaps ColorNamesDict GrayToIndexMap IndexedColors MaskingMap RandomStream'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
