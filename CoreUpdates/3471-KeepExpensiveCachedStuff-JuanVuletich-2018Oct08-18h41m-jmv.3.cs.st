'From Cuis 5.0 of 7 November 2016 [latest update: #3470] on 8 October 2018 at 6:54:40 pm'!

!Character class methodsFor: 'class initialization' stamp: 'jmv 10/8/2018 18:36:22'!
releaseClassState
	UnaccentedTable _ nil.
	UnicodeCodePoints _ nil! !

!Character class methodsFor: 'constants' stamp: 'jmv 10/8/2018 18:39:44'!
unaccentedTable
	UnaccentedTable ifNil: [ self initializeLookupTables ].
	^UnaccentedTable! !

!Character class methodsFor: 'constants' stamp: 'jmv 10/8/2018 18:37:36'!
unicodeCodePoints
	UnicodeCodePoints ifNil: [ self initializeUnicodeCodePoints ].
	^UnicodeCodePoints! !


!Color class methodsFor: 'cached state access' stamp: 'jmv 10/8/2018 18:29:30'!
releaseClassState
	"Do not remove them in every image save (#releaseClassCachedState) to save startup time."
	"
	Color releaseClassState
	"
	CachedColormaps _ nil. 			"Maps to translate between color depths"
	ToGrayColormaps _ nil.			"Maps colors to 8bpp grays for various color depths"
	FromGrayColormaps _ nil.		"Maps from 8bpp grays to colors of various depths"
	MaskingMap _ nil. 				"Maps all colors except transparent to black for creating a mask"
	IndexedColors _ nil.
	GrayToIndexMap _ nil! !


!BitBlt class methodsFor: 'cached state access' stamp: 'jmv 10/8/2018 18:31:40'!
releaseClassState
	"Do not remove them in every image save (#releaseClassCachedState) to save startup time."

	CachedFontColorMaps _ ColorConvertingMaps _ nil! !


!StrikeFont class methodsFor: 'class cached access' stamp: 'jmv 10/8/2018 18:53:44'!
releaseClassState
	"Do not remove them in every image save (#releaseClassCachedState) to save startup time."
	"
	StrikeFont releaseClassState
	"
	"Deallocate synthetically derived copies of base fonts to save space"
	self allSubInstancesDo: [ :sf | sf reset ]! !


!BitBltCanvas class methodsFor: 'cached state access' stamp: 'jmv 10/8/2018 18:33:14'!
releaseClassState
	"Do not remove them in every image save (#releaseClassCachedState) to save startup time."
	"
	BitBltCanvas releaseClassState
	"
	CachedForms _ nil.! !


!Behavior methodsFor: 'system startup' stamp: 'jmv 10/8/2018 18:31:00'!
releaseClassCachedState
	"Will be called for each class on shutdown or snapshot.
	All class vars or class instVar vars that can be cheaply recreated lazily on demand, should be nilled.
	For more expensive stuff to recreate, consider #releaseClassState that is not called on every image save.
	See implementors for examples"! !


!Character methodsFor: 'accessing' stamp: 'jmv 10/8/2018 18:39:19'!
codePoint
	"
	self assert: $A codePoint hex = '16r41'.
	self assert: $¤ codePoint hex = '16r20AC'.
	"
	^self class unicodeCodePoints at: self numericValue + 1! !

!Character methodsFor: 'converting' stamp: 'jmv 10/8/2018 18:44:52'!
asUnaccented
	"
	$A asUnaccented
	$Á asUnaccented
	(0 to: 255) collect: [ :i | (Character numericValue: i) asUnaccented ]
	(0 to: 255) collect: [ :i | (Character numericValue: i) asUnaccented  asLowercase]
	"
	^ Character
		numericValue: (self class unaccentedTable at: self numericValue + 1)! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 10/8/2018 18:46:39'!
initialize
	"Create the table of unique Characters.
	Character initialize
	"
	self initializeLookupTables .
	self initializeUnicodeCodePoints.! !

!Character class methodsFor: 'converting' stamp: 'jmv 10/8/2018 18:37:44'!
iso8859s15CodeForUnicodeCodePoint: codePoint
	"
	Answer nil if the Unicode codePoint is not a valid ISO 8859-15 character
	
	self assert: (Character iso8859s15CodeForUnicodeCodePoint: 16r41) = $A iso8859s15Code.
	self assert: (Character iso8859s15CodeForUnicodeCodePoint: 16r20AC) = $¤ iso8859s15Code.
	"
	| code |
	code _ (self unicodeCodePoints indexOf: codePoint) -1.
	code = -1 ifTrue: [ ^nil ].
	^code! !


!BitBltCanvas class methodsFor: 'cached state access' stamp: 'jmv 10/8/2018 18:33:24'!
releaseClassCachedState
	"
	BitBltCanvas releaseClassCachedState
	"
	AccessProtect _ nil.
	AuxForm _ nil.
	AuxBlitter _ nil! !

!methodRemoval: StrikeFont class #releaseClassCachedState!
StrikeFont class removeSelector: #releaseClassCachedState!
!methodRemoval: BitBlt class #releaseClassCachedState!
BitBlt class removeSelector: #releaseClassCachedState!
!methodRemoval: Color class #releaseClassCachedState!
Color class removeSelector: #releaseClassCachedState!
!methodRemoval: Character class #initClassCachedState!
Character class removeSelector: #initClassCachedState!
!methodRemoval: Character class #releaseClassCachedState!
Character class removeSelector: #releaseClassCachedState!
Character initialize!
