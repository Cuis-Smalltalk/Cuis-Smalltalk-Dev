'From Cuis 4.2 of 25 July 2013 [latest update: #2224] on 5 April 2015 at 10:21:29.020134 pm'!
!classDefinition: #Collection category: #'Collections-Abstract'!
Object subclass: #Collection
	instanceVariableNames: ''
	classVariableNames: 'MutexForPicking RandomForPicking '
	poolDictionaries: ''
	category: 'Collections-Abstract'!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace '
	classVariableNames: 'CachedColormaps ColorNamesDict GrayToIndexMap IndexedColors MaskingMap RandomStream '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color class methodsFor: 'instance creation' stamp: 'jmv 4/4/2015 20:28'!
random2: aRandom
	"Return a random color with a distribution that spans over all possible colors."

	^ self basicNew
		setRed: aRandom next
		green: aRandom next
		blue: aRandom next
		colorSpace: #sRGB! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 4/4/2015 20:28'!
random: aRandom
	"Return a random color that isn't too dark or under-saturated."

	^ self basicNew
		setHue: (360.0 * aRandom next)
		saturation: (0.3 + (aRandom next * 0.7))
		brightness: (0.4 + (aRandom next * 0.6))! !


!Collection methodsFor: 'accessing' stamp: 'jmv 4/4/2015 20:24'!
atRandom
	"Answer a random element of the receiver.  Uses a shared random 
	number generator owned by class Collection.  If you use this a lot, 
	define your own instance of Random and use #atRandom:.  Causes 
	an error if self has no elements."

	^ Random withDefaultDo: [ :random |
		self atRandom: random ]

"Examples:
	#('one' 'or' 'the' 'other') atRandom
	(1 to: 10) atRandom
	'Just pick one of these letters at random' atRandom
	#(3 7 4 9 21) asSet atRandom		(just to show it also works for Sets)
"! !


!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 4/4/2015 20:26'!
shuffled
	^ Random withDefaultDo: [ :random |
		self shuffledBy: random ]

"Examples:
	($A to: $Z) shuffled
"! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 4/4/2015 20:30'!
random
	"Return a random color that isn't too dark or under-saturated.
		Display fill: (10@10 extent: 200@200) fillColor: Color random
	"

	^ Random withDefaultDo: [ :random |
		self random: random ]! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 4/4/2015 20:30'!
random2
	"Return a random color with a distribution that spans over all possible colors.
		Display fill: (10@10 extent: 200@200) fillColor: Color random2
	"

	^ Random withDefaultDo: [ :random |
		self random2: random ]! !

!Color class methodsFor: 'class initialization' stamp: 'jmv 4/4/2015 20:30'!
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

	self initializeIndexedColors.
	self initializeGrayToIndexMap.
	self initializeNames! !


!Integer methodsFor: 'truncation and round off' stamp: 'jmv 4/4/2015 20:25'!
atRandom
	"Answer a random integer from 1 to self.  This implementation uses a
	shared generator. Heavy users should their own implementation or use
	Interval>atRandom: directly."
	"
	7 atRandom
	"

	self = 0 ifTrue: [ ^0 ].
	self < 0 ifTrue: [ ^self negated atRandom negated ].
	^ Random withDefaultDo: [ :random |
		self atRandom: random ]! !

Color initialize!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace'
	classVariableNames: 'CachedColormaps ColorNamesDict GrayToIndexMap IndexedColors MaskingMap'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
!methodRemoval: Collection class #initialize!
Collection class removeSelector: #initialize!
!methodRemoval: Collection class #mutexForPicking!
Collection class removeSelector: #mutexForPicking!
!methodRemoval: Collection class #randomForPicking!
Collection class removeSelector: #randomForPicking!
Collection initialize!

!Collection class reorganize!
('instance creation' newFrom: with: with:with: with:with:with: with:with:with:with: with:with:with:with:with: with:with:with:with:with:with: withAll:)
!

!classDefinition: #Collection category: #'Collections-Abstract'!
Object subclass: #Collection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Abstract'!
