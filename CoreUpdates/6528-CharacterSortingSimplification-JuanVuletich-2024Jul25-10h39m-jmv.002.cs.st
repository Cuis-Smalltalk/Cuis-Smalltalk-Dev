'From Cuis7.1 [latest update: #6526] on 25 July 2024 at 10:49:49 am'!

!Character methodsFor: 'comparing' stamp: 'jmv 7/25/2024 10:49:21'!
<= aCharacter
	"Answer whether the receiver is less than or equal to the argument."
	"Reduce all cases to #< and #= to simplify code."

	self == aCharacter ifTrue: [ ^true ].
	^self < aCharacter.! !

!Character methodsFor: 'comparing' stamp: 'jmv 7/25/2024 10:49:27'!
> aCharacter
	"Answer whether the receiver is greater than the argument."
	"Reduce all cases to #< and #= to simplify code."

	self == aCharacter ifTrue: [ ^false ].
	^(self < aCharacter) not.! !

!Character methodsFor: 'comparing' stamp: 'jmv 7/25/2024 10:49:32'!
>= aCharacter
	"Answer whether the receiver is greater than or equal to the argument."
	"Reduce all cases to #< and #= to simplify code."

	^(self < aCharacter) not.! !

