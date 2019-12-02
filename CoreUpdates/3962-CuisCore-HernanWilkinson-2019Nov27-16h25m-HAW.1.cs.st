'From Cuis 5.0 [latest update: #3958] on 27 November 2019 at 4:38:55 pm'!

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 8/18/2018 16:30:01'!
assertIsValidKeywordForNewParameter: aNewKeyword

	(aNewKeyword isKeyword and: [aNewKeyword numArgs = 1 ]) ifFalse: [ self signalNotValidKeywordForNewParameter]! !

!AddParameter class methodsFor: 'exceptions' stamp: 'HAW 11/27/2019 16:26:50'!
notValidKeywordForNewParameterErrorMessage
	
	^'New keyword can not be unary or binary. It has to be a keyword with one parameter'! !


!AddParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 11/27/2019 16:34:16'!
askNewKeyword

 	| enteredString |
	
	enteredString := (self request: 'Enter keyword for new parameter') withBlanksTrimmed.
	(enteredString endsWith: ':') ifFalse: [ enteredString := enteredString, ':' ].
	newKeyword := enteredString asSymbol.
	self refactoringClass assertIsValidKeywordForNewParameter: newKeyword! !

