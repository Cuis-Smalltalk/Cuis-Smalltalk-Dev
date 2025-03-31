'From Cuis7.3 [latest update: #7093] on 31 March 2025 at 3:40:40 pm'!

!Morph methodsFor: 'layout' stamp: 'jmv 3/31/2025 15:00:56'!
minimumLayoutExtent: cachedValues
	"I combine information from a Morph and its optional LayoutSpec.
	Answer is expressed in owner's coordinates."

	^cachedValues at: self ifAbsentPut: [
		self externalizeDistance: self minimumExtent ].! !


!PlacedMorph methodsFor: 'layout' stamp: 'jmv 3/31/2025 15:01:28'!
minimumLayoutExtent: cachedValues
	"I combine information from a Morph and its optional LayoutSpec.
	Answer is expressed in owner's coordinates."
	| minExtent |

	^cachedValues at: self ifAbsentPut: [
		minExtent :=  self externalizeDistance: self minimumExtent.
		layoutSpec
			ifNil: [ minExtent ] 
			ifNotNil: [ :ls | minExtent max: ls minimumSpecExtent ]]! !

