Class {
	#name : 'RemoveParameterTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
RemoveParameterTest >> classToRefactorName [
	
	^#ClassToRemoveParameter
]

{ #category : 'tests' }
RemoveParameterTest >> test01CanNotRemoveParameterFromUnaryMessages [

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 1
			named: 'aParameter' 
			from: #m1
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter canNotRemoveParameterFromUnaryMessagesErrorMessage ]

]

{ #category : 'tests' }
RemoveParameterTest >> test02CanNotRemoveParameterFromBinaryMessages [

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 1
			named: '+' asSymbol 
			from: #+ 
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter canNotRemoveParameterFromBinaryMessagesErrorMessage ]

]

{ #category : 'tests' }
RemoveParameterTest >> test03CanNotRemoveParameterNotInMessage [

	| classToRefactor selectorToRemoveParameterFrom parameterNotInMessage |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter'.
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			named: parameterNotInMessage 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: {classToRefactor >> selectorToRemoveParameterFrom } 
			senders: {} ]
		failsWith: [ RemoveParameter errorMessageForParameterNotInMessage: parameterNotInMessage ]

]

{ #category : 'tests' }
RemoveParameterTest >> test04AllImplementorsMustImplementSelector [

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove implementors |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	parameterToRemove := 'parameter'.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter'.
	implementors := {thisContext method}.
	
	self 
		assertCreation: [ RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: implementors 
			senders: {} ]
		failsWith: [ RemoveParameter errorMessageForInvalidImplementors: implementors ]

]

{ #category : 'tests' }
RemoveParameterTest >> test05AllSendersShouldSendSelector [

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove invalidSenders |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	parameterToRemove := 'parameter'.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter'.
	invalidSenders := {thisContext method}.
	
	self 
		assertCreation: [ RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: {} 
			senders: invalidSenders ]
		failsWith: [ RemoveParameter errorMessageForInvalidSenders: invalidSenders of: selectorToRemoveParameterFrom ]
	

]

{ #category : 'tests' }
RemoveParameterTest >> test06NoImplementorReferencesParameterToRemove [

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove implementorsReferencingParameter |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	parameterToRemove := 'parameter'.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter ^parameter'.
	implementorsReferencingParameter := {classToRefactor>>selectorToRemoveParameterFrom }.
	
	self 
		assertCreation: [ RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: implementorsReferencingParameter 
			senders: {} ]
		failsWith: [ RemoveParameter errorMessageForParameterToRemoveIsReferenced: parameterToRemove ]
]

{ #category : 'tests' }
RemoveParameterTest >> test07FirstParameterIsRemovedCorrectly [

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToRemove, keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	classToRefactor compile: keywordToRemove,parameterToRemove, ' 
	', keywordToKeep, parameterToKeep.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: keywordToKeep asSymbol).
	self assert: (classToRefactor sourceCodeAt: keywordToKeep asSymbol) equals: (keywordToKeep, parameterToKeep) 
]

{ #category : 'tests' }
RemoveParameterTest >> test08LastParameterIsRemovedCorrectly [

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToKeep := 'm1:'.
	keywordToRemove := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToKeep, keywordToRemove) asSymbol. 
	parameterToKeep := 'parameter1'.
	parameterToRemove := 'parameter2'.
	classToRefactor compile: keywordToKeep,parameterToKeep, ' ', keywordToRemove, parameterToRemove,'
	| t1 |'.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: keywordToKeep asSymbol).
	self assert: (classToRefactor sourceCodeAt: keywordToKeep asSymbol) equals: (keywordToKeep, parameterToKeep,' 
	| t1 |') 
]

{ #category : 'tests' }
RemoveParameterTest >> test09MiddleParameterIsRemovedCorrectly [

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring newSelector |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := ('m0:',keywordToRemove, keywordToKeep) asSymbol. 
	newSelector := ('m0:', keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	classToRefactor compile: 'm0: parameter0 
	',keywordToRemove,parameterToRemove, ' 
	', keywordToKeep, parameterToKeep.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: newSelector).
	self assert: (classToRefactor sourceCodeAt: newSelector) equals: ('m0: parameter0 
	',keywordToKeep, parameterToKeep) 
]

{ #category : 'tests' }
RemoveParameterTest >> test10ParameterFromOneKeywordSelectorIsRemovedCorrectly [

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove refactoring |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol. 
	parameterToRemove := 'parameter1'.
	classToRefactor compile: selectorToRemoveParameterFrom asString,parameterToRemove.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: 'm1' asSymbol).
]

{ #category : 'tests' }
RemoveParameterTest >> test11ParameterOfSendersOfMoreThanOneKeywordSelectorsIsRemovedCorrectly [

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring senderSelector senderMethod |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToRemove, keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	senderSelector := 'sender' asSymbol.
	classToRefactor compile: keywordToRemove,parameterToRemove, ' ', keywordToKeep, parameterToKeep.
	classToRefactor compile: senderSelector asString,'
		self 
			', keywordToRemove, ' 1 
			', keywordToKeep, ' 2 '.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: keywordToKeep asSymbol).
	self assert: senderMethod sourceCode equals: (senderSelector asString,'
		self 
			', keywordToKeep, ' 2 ')
	
	
]

{ #category : 'tests' }
RemoveParameterTest >> test12ParameterOfSendersOfOneKeywordSelectorsIsRemovedCorrectly [

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove refactoring senderSelector senderMethod |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol. 
	parameterToRemove := 'parameter1'.
	senderSelector := 'sender' asSymbol.
	classToRefactor compile: selectorToRemoveParameterFrom,parameterToRemove.
	classToRefactor compile: senderSelector asString,'
		self 
			', selectorToRemoveParameterFrom, '
			 (1+2).'.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: 'm1' asSymbol).
	self assert: senderMethod sourceCode equals: (senderSelector asString,'
		self 
			m1.')
	
	
]

{ #category : 'tests' }
RemoveParameterTest >> test13ParameterIsRemovedCorrecltyFromSendersWithMoreThanOneSend [

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring senderSelector senderMethod |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToRemove, keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	senderSelector := 'sender' asSymbol.
	classToRefactor compile: keywordToRemove,parameterToRemove, ' ', keywordToKeep, parameterToKeep.
	classToRefactor compile: senderSelector asString,'
		self ', keywordToRemove, ' (1+1) ', keywordToKeep, ' 2.
		self ', keywordToRemove, ' 3 ', keywordToKeep, ' 4.'.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: keywordToKeep asSymbol).
	self assert: senderMethod sourceCode equals: (senderSelector asString,'
		self ', keywordToKeep, ' 2.
		self ', keywordToKeep, ' 4.')
	
	
]

{ #category : 'tests' }
RemoveParameterTest >> test14CanNotRemoveParameterIndexLessThanOne [

	| parameterNotInMessage |
	
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 0
			named: parameterNotInMessage 
			from: #m1:  
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter invalidParameterIndexErrorMessage ]

]

{ #category : 'tests' }
RemoveParameterTest >> test15CanNotRemoveParameterIndexBiggerThanOldSelectorNumberOfKeywords [

	| parameterNotInMessage |
	
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 2
			named: parameterNotInMessage 
			from: #m1:  
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter invalidParameterIndexErrorMessage ]

]

{ #category : 'tests' }
RemoveParameterTest >> test16ParameterIndexMustBeInteger [

	| parameterNotInMessage |
	
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 1.5
			named: parameterNotInMessage 
			from: #m1:m2: 
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter invalidParameterIndexErrorMessage ]

]
