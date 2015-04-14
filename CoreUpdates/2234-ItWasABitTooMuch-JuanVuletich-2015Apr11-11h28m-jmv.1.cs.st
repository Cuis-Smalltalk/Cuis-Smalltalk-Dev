'From Cuis 4.2 of 25 July 2013 [latest update: #2233] on 11 April 2015 at 11:31:47.517986 am'!

!String methodsFor: 'converting' stamp: 'jmv 12/6/2011 10:34'!
prefixAndSuffix: aCharacter
	"Answer an array with the prefix up to the last occurrence of aCharacter, and the suffix after it.
	Answer nil if aCharacter is not in self
	'1164-ReferenceStreamFix-jmv.1.cs' prefixAndSuffix: $-
	"
	| i |
	i _ self findLast: [ :c | c = aCharacter ].
	i = 0 ifTrue: [ ^ nil ].
	^ { self copyFrom: 1 to: i-1 . self copyFrom: i+1 to: self size }! !


!Form methodsFor: 'converting' stamp: 'jm 11/12/97 19:28'!
as8BitColorForm
	"Simple conversion of zero pixels to transparent.  Force it to 8 bits."

	| f map |
	f _ ColorForm extent: self extent depth: 8.
	self displayOn: f at: self offset negated.
	map _ Color indexedColors copy.
	map at: 1 put: Color transparent.
	f colors: map.
	f offset: self offset.
	^ f
! !

!Form methodsFor: 'displaying-generic' stamp: 'jmv 1/22/2015 10:19'!
displayOn: aDisplayMedium at: aDisplayPoint rule: ruleInteger
	"Display the receiver located at aPoint with default setting for the 
	halftone and clippingBox."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: aDisplayMedium boundingBox
		rule: ruleInteger
		fillColor: nil! !

!Form methodsFor: 'encoding' stamp: 'RAA 7/29/2000 09:01'!
addDeltasFrom: previousForm

	(BitBlt 
		destForm: self 
		sourceForm: previousForm 
		fillColor: nil 
		combinationRule: Form reverse
		destOrigin: 0@0
		sourceOrigin: 0@0
		extent: self extent 
		clipRect: self boundingBox) copyBits.
	^self! !

