'From Cuis6.3 [latest update: #6284] on 20 March 2024 at 4:04:01 pm'!

!TestCase methodsFor: 'assertions' stamp: 'jmv 3/20/2024 16:00:59'!
should: aBlockToFail raise: anExceptionHandlingCondition withMessageText: expectedErrorMessageOrBlock

	self 
		should: aBlockToFail 
		raise: anExceptionHandlingCondition 
		withExceptionDo: [ :anException |
			self assert: (anException messageText endsWith: expectedErrorMessageOrBlock value) ]! !


!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'jmv 3/20/2024 16:01:35'!
assertAndCreateNamed: aNewParameter 
	extractedFromAll: allIntervals
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection
	creatingAddParameterWith: addParameterBlock

	| addParameter sourceCodeToExtract trimmedIntervalToReplace intervals |
	
	self assert: aSourceMethod isInImplementors: implementorsCollection.
	intervals := self assertCanExtractedFromAll: allIntervals for: aSourceMethod.
	
	trimmedIntervalToReplace := intervals first second.
	sourceCodeToExtract := aSourceMethod sourceCode copyFrom: trimmedIntervalToReplace first to: trimmedIntervalToReplace last.
	
	addParameter := addParameterBlock value: sourceCodeToExtract.

	^self new 
		initializeReplacingAll: (intervals collect: [ :both | both first ])
		at: aSourceMethod 
		addingParameterWith: addParameter! !

