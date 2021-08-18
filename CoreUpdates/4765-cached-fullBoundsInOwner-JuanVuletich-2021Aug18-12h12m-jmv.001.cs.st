'From Cuis 5.0 [latest update: #4764] on 18 August 2021 at 12:12:53 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/18/2021 12:12:18'!
fullBoundsInOwner
	"Answer fullBoundsInOwner. Cache the found bounds."

	(self valueOfProperty: #fullBoundsInOwner) ifNotNil: [ :fullBoundsInOwner |
		^fullBoundsInOwner ].

	^self setProperty: #fullBoundsInOwner toValue: self findFullBoundsInOwner.! !

