'From Cuis7.9 [latest update: #8225] on 24 September 2026 at 2:35:54 pm'!

!Behavior methodsFor: 'method dictionary' stamp: 'FJG 27/Oct/2025, 3:00:18 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel05ClassAndMethodCreation}!
lookupSelector: selector ifAbsent: alternativeBlock
	"Look up the given selector in my methodDictionary.
	Return the corresponding method if found.
	Otherwise chase the superclass chain and try again.
	Answer the value of alternativeBlock if no method is found."
	
	^ self methodDict at: selector ifAbsent: [
		superclass
			ifNil: [ alternativeBlock value ]
			ifNotNil: [ superclass lookupSelector: selector ifAbsent: alternativeBlock ]
	]! !

