'From Cuis7.5 [latest update: #7358] on 15 July 2025 at 2:23:13 pm'!

!BoxMorph methodsFor: 'layout' stamp: 'jmv 7/15/2025 13:23:51'!
minimumLayoutExtent: cachedValues
	"I combine information from a Morph and its optional LayoutSpec.
	Answer is expressed in owner's coordinates."
	| minExtent |

	^cachedValues at: self ifAbsentPut: [
		minExtent :=  self externalizeDistance: self minimumExtent.
		layoutSpec
			ifNil: [ minExtent ] 
			ifNotNil: [ :ls | minExtent max: ls minimumSpecExtent ]]! !


!PlacedMorph methodsFor: 'layout' stamp: 'jmv 7/15/2025 13:25:05'!
minimumLayoutExtent: cachedValues
	"I combine information from a Morph and its optional LayoutSpec.
	Answer is expressed in owner's coordinates."
	| minExtent |

	^cachedValues at: self ifAbsentPut: [
		minExtent :=  self minimumExtent.
		layoutSpec
			ifNil: [ minExtent ] 
			ifNotNil: [ :ls | minExtent max: ls minimumSpecExtent ]]! !

!methodRemoval: Morph #minimumLayoutExtent: stamp: 'jmv 7/15/2025 14:22:07'!
Morph removeSelector: #minimumLayoutExtent:!
