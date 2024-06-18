'From Cuis7.1 [latest update: #6477] on 18 June 2024 at 2:10:44 pm'!

!Browser methodsFor: 'class list' stamp: 'jmv 6/18/2024 12:48:37'!
classListIndexOf: classNameToFind

	"Answer the index of the aClassName selection."

	classNameToFind ifNil: [ ^0 ].
	^self classList findFirst: [ :displayed | | start |
		"Works regardless of currently showing hierarchically or alphabetically."
		start := displayed firstNonSeparator.
		start-1 + classNameToFind size		 = 		displayed size and: [
			displayed is: classNameToFind substringAt: start ]]! !

!methodRemoval: CharacterSequence #afterBlanksEndsWith: stamp: 'jmv 6/18/2024 14:10:21'!
CharacterSequence removeSelector: #afterBlanksEndsWith:!
