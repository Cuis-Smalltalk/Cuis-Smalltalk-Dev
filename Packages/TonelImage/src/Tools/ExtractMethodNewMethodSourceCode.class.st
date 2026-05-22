"
I responsible of creating the new source code for the extracted method
"
Class {
	#name : 'ExtractMethodNewMethodSourceCode',
	#superclass : 'Object',
	#instVars : [
		'intervalToExtract',
		'newMessage',
		'newSourceCode',
		'existingMethod',
		'newArgumentNames',
		'sourceCodeToExtract'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ExtractMethodNewMethodSourceCode class >> fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage withArgumentNames: renamedArguments  [

	^ self new
		initializeFrom: anIntervalToExtract 
		of: aMethodToExtractCodeFrom
		to: newMessage
		withArgumentNames: renamedArguments
]

{ #category : 'initialization' }
ExtractMethodNewMethodSourceCode >> initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage withArgumentNames: renamedArguments   [

	intervalToExtract := anIntervalToExtract.
	existingMethod := aMethodToExtractCodeFrom.
	newMessage := aNewMessage.
	newArgumentNames := renamedArguments .
	
]

{ #category : 'private - source code' }
ExtractMethodNewMethodSourceCode >> initializeSourceCodeToExtract [

	sourceCodeToExtract := existingMethod sourceCode
		copyFrom: intervalToExtract first
		to: intervalToExtract last
]

{ #category : 'private - source code' }
ExtractMethodNewMethodSourceCode >> initiliazeNewSourceCode [

	newSourceCode := String streamContents: [ :stream |
		stream
			nextPutAll: self newMessageString;
			nextPutAll: self startingMethodIdentation;
			nextPutAll: self returnCharacterIfNeeded;
			nextPutAll: sourceCodeToExtract ]
]

{ #category : 'private - source code' }
ExtractMethodNewMethodSourceCode >> needsExplicitReturn: extractedMethodNode [

	^ extractedMethodNode numberOfStatements > 1 or: [ extractedMethodNode hasTemporaryVariables ]
]

{ #category : 'private - source code' }
ExtractMethodNewMethodSourceCode >> newMessageString [

	^ newMessage fullName
]

{ #category : 'private - source code' }
ExtractMethodNewMethodSourceCode >> renameArgumentFrom: currentArgumentName to: newArgumentName [

	| methodNode | 
	
	methodNode := self sourceClass methodNodeFor: newSourceCode. 
	newSourceCode := (RenameTemporary from: currentArgumentName to: newArgumentName in: methodNode) apply
]

{ #category : 'private - source code' }
ExtractMethodNewMethodSourceCode >> renameArguments [

	newArgumentNames keysAndValuesDo: [ :currentArgumentName :newArgumentName | 
		self renameArgumentFrom: currentArgumentName to: newArgumentName ].

]

{ #category : 'private - source code' }
ExtractMethodNewMethodSourceCode >> returnCharacterIfNeeded [

	| extractedMethodNode |
	
	extractedMethodNode := Parser parse: sourceCodeToExtract class: self sourceClass noPattern: true.
	
	^ (self needsExplicitReturn: extractedMethodNode) ifTrue: [ '' ] ifFalse: [ '^ ' ]
]

{ #category : 'private - source code' }
ExtractMethodNewMethodSourceCode >> sourceClass [

	^ existingMethod methodClass
]

{ #category : 'private - source code' }
ExtractMethodNewMethodSourceCode >> startingMethodIdentation [

	^ `String lfString , String lfString , String tab`
]

{ #category : 'value' }
ExtractMethodNewMethodSourceCode >> value [

	self 
		initializeSourceCodeToExtract;
		initiliazeNewSourceCode;
		renameArguments. 
	
	^newSourceCode 
]
