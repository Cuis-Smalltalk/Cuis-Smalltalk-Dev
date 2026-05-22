Class {
	#name : 'AddParameterTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
AddParameterTest >> classToRefactorName [
	
	^#ClassToAddParameter
]

{ #category : 'tests' }
AddParameterTest >> test01AddingParameterToUnaryMessageAddsColonAndParameter [

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue equals: (senderMethod methodNode block statements first arguments first literalValue) printString



]

{ #category : 'tests' }
AddParameterTest >> test02AddingParameterToKeywordMessageAddsNewKeywordAndParameter [

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, ' 1'.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)



]

{ #category : 'tests' }
AddParameterTest >> test03AddingParameterToKeywordMessageMultipleSendsInSameMethodSameLiteral [

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' 1.'.
	classToRefactor compile: senderSelector asString, collaboration, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)



]

{ #category : 'tests' }
AddParameterTest >> test04AddingParameterToKeywordMessageWithMessageSendAsLastFormalParameter [

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' self size.'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)



]

{ #category : 'tests' }
AddParameterTest >> test05AddingParameterToKeywordMessageWithBinaryMessageSendAsLastFormalParameter [

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' self size + 4.'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)



]

{ #category : 'tests' }
AddParameterTest >> test06AddingParameterToKeywordMessageWithKeywordMessageSendAsLastFormalParameter [

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' (self at: 1 put: 2).'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)



]

{ #category : 'tests' }
AddParameterTest >> test07AddingParameterToUnaryMessageChangesReferencesToSelector [

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' #', oldSelector asString, ' size'.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: 0 equals: (senderMethod indexOfLiteral: oldSelector).
	self assert: 1 equals: (senderMethod indexOfLiteral: newSelector).
	
	

]

{ #category : 'tests' }
AddParameterTest >> test08AddingParameterToKeywordMessageChangesReferencesToSelector [

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' #', oldSelector asString, ' size.'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: 0 equals: (senderMethod indexOfLiteral: oldSelector).
	self assert: 1 equals: (senderMethod indexOfLiteral: newSelector).
	


]

{ #category : 'tests' }
AddParameterTest >> test09FailsIfNewParameterValueCanNotBeCompiled [

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '+' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter newParameterValueDoesNotCompileErrorMessage ]
	

]

{ #category : 'tests' }
AddParameterTest >> test10FailsIfNewParameterValueIsEmpty [

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: ' ' toUnarySelector: thisContext selector
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter newParameterValueCanNotBeEmptyErrorMessage ]

]

{ #category : 'tests' }
AddParameterTest >> test11NewParameterNameCanNotBeEmpty [

	| newParameter |
	
	newParameter := ' '.
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidParameterName: newParameter withBlanksTrimmed ]
	

]

{ #category : 'tests' }
AddParameterTest >> test12NewParameterNameCanNotStartWithNumber [

	| newParameter |
	
	newParameter := '1x'.
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidParameterName: newParameter  ]
	

]

{ #category : 'tests' }
AddParameterTest >> test13NewParameterNameCanNotContainSpaces [

	| newParameter |
	
	newParameter := 'an Integer'.
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidParameterName: newParameter  ]
	

]

{ #category : 'tests' }
AddParameterTest >> test14NewParameterCanNotBeDefinedAsInstanceVariableInAnyImplementor [

	| classToRefactor oldSelector newParameter  |
	
	oldSelector := 'm1' asSymbol.
	newParameter := 'newParam'.
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newParameter.
	classToRefactor compile: oldSelector asString.
	
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: oldSelector 
			implementors: {classToRefactor>>oldSelector} senders: {} ]
		 failsWith: [ AddParameter errorMessageForNewParameter: newParameter definedAsInstanceVariableIn: {classToRefactor} ]
	

]

{ #category : 'tests' }
AddParameterTest >> test15NewParameterCanNotBeDefinedAsParameterInAnyImplementor [

	| classToRefactor oldSelector newParameter implementors |
	
	oldSelector := 'm1:' asSymbol.
	newParameter := 'newParam'.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, newParameter.
	implementors := {classToRefactor>>oldSelector}.
	
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' using: 'm2:' asSymbol toKeywordSelector: oldSelector 
			implementors: implementors senders: {} ]
		 failsWith: [ AddParameter errorMessageForNewParameterDefinedAsLocal: newParameter ]
	

]

{ #category : 'tests' }
AddParameterTest >> test16WhenCreatedForUnarySelectorMustBeUnary [

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' toUnarySelector: 'm1:' asSymbol 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter selectorMustBeUnaryErrorMessage ]
	

]

{ #category : 'tests' }
AddParameterTest >> test17WhenCreatedForBinarySelectorMustBeBinary [

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: 'm1:' asSymbol toKeywordSelector: thisContext selector
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter selectorMustBeKeywordErrorMessage ]
	

]

{ #category : 'tests' }
AddParameterTest >> test18CanNotAddParameterToBinaryKeyword [

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' at: 1 addingLast: true initializedWith: '1' to: '+' asSymbol implementing: 'm1:' asSymbol
			addingToImplementors: '' addingToSenders: ''
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter selectorCanNotBeBinaryErrorMessage ]
	

]

{ #category : 'tests' }
AddParameterTest >> test19AllImplementorsMustImplementOldSelector [

	| implementors |
	
	implementors := {thisContext method}.
	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' toUnarySelector: (thisContext selector, 'x') asSymbol
			implementors: implementors senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidImplementors: implementors ]
	

]

{ #category : 'tests' }
AddParameterTest >> test20AllSendersShouldSendOldSelector [

	| oldSelector invalidSenders |
	
	oldSelector := thisContext selector.
	invalidSenders := {thisContext method}.
	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' toUnarySelector: oldSelector
			implementors: {thisContext method} senders: invalidSenders ]
		 failsWith: [ AddParameter errorMessageForInvalidSenders: invalidSenders of: oldSelector ]
	

]

{ #category : 'tests' }
AddParameterTest >> test21NewKeywordToAddToOldSelectorCanNotBeUnary [

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: 'm1' asSymbol toKeywordSelector: 'm1:' asSymbol
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter notValidKeywordForNewParameterErrorMessage ]
	

]

{ #category : 'tests' }
AddParameterTest >> test22NewKeywordToAddToOldSelectorCanNotBeBinary [

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: '+' asSymbol toKeywordSelector: 'm1:' asSymbol
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter notValidKeywordForNewParameterErrorMessage ]
	

]

{ #category : 'tests' }
AddParameterTest >> test23NewKeywordToAddToOldSelectorCanNotBeKeywordWithMoreThanOneParameter [

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: 'm1:m2:' asSymbol toKeywordSelector: 'm1:' asSymbol
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter notValidKeywordForNewParameterErrorMessage ]
	

]

{ #category : 'tests' }
AddParameterTest >> test24AddingParameterToSendersTakeCaresOfSeparators [

	| refactoring classToRefactor oldSelector newSelector newParameter newParameterValue senderSelector |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, Character newLineCharacter asString,'	'.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	self shouldnt: [ refactoring apply ] raise: Error.
	
	
]

{ #category : 'tests' }
AddParameterTest >> test25IfNewParameterValueIsKeywordMessageSendAddParenthesisToItForUnaryMessages [

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector newSelector senderMethod senderMethodNode senderSourceCode senderSourceRange |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := 'self at: 1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: newSelector).
	
	senderMethodNode := senderMethod methodNode.
	senderSourceRange := senderMethodNode encoder rangeForNode: senderMethodNode block statements first ifAbsent: [ self fail ].
	senderSourceCode := senderMethodNode sourceText copyFrom: senderSourceRange first to: senderSourceRange last.
	self assert: (senderSourceCode endsWith: '(',newParameterValue,')')
	
	
]

{ #category : 'tests' }
AddParameterTest >> test26IfNewParameterValueIsKeywordMessageSendAddParenthesisToItForKeywordMessages [

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector newSelector senderMethod newSelectorAddedKeyword senderMethodNode senderSourceCode senderSourceRange |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := 'self at: 1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' oldParam'.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, ' 1'.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: newSelector).
	
	senderMethodNode := senderMethod methodNode.
	senderSourceRange := senderMethodNode encoder rangeForNode: senderMethodNode block statements first ifAbsent: [ self fail ].
	senderSourceCode := senderMethodNode sourceText copyFrom: senderSourceRange first to: senderSourceRange last.
	self assert: (senderSourceCode endsWith: '(',newParameterValue,')')
	
	
]

{ #category : 'tests' }
AddParameterTest >> test27NewParameterValueCanNotHaveMoreThanOneStatement [

	self 
		assertCreation: [ AddParameter 
			named: 'newParameter' initializedWith: 'self m1. self m2.' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		failsWith: [ AddParameter newParameterValueCanNotHaveMoreThanOneStatementErrorMessage ].
	
	
]

{ #category : 'tests' }
AddParameterTest >> test28WhenSenderLastParameterIsABlockAddedParameterIsNotAddedInsideTheBlock [

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector newSelector senderMethod newSelectorAddedKeyword senderMethodNode senderSourceCode senderSourceRange senderFirstParameterValue |

	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	senderFirstParameterValue := '[ 1 factorial ]'.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' oldParam'.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, senderFirstParameterValue.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: newSelector).
	
	senderMethodNode := senderMethod methodNode.
	senderSourceRange := senderMethodNode encoder rangeForNode: senderMethodNode block statements first arguments first closureCreationNode ifAbsent: [ self fail ].
	senderSourceCode := senderMethodNode sourceText copyFrom: senderSourceRange first to: senderSourceRange last.
	self assert: senderFirstParameterValue equals: senderSourceCode
	
]

{ #category : 'tests' }
AddParameterTest >> test29AddingParameterAsFirstOneWorksAsExpected [

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := 'm2:m1:' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'm1: p1'.
	classToRefactor compile: senderSelector asString, ' self m1: 1'.
	
	refactoring := AddParameter 
		named: newParameter at: 1 initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	self assert: (newImplementorMethodNode arguments at: 1) name equals: newParameter.
	
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments first literalValue).
	self assert: senderMethod sourceCode equals: senderSelector asString, ' self m2: 2 m1: 1'.

]

{ #category : 'tests' }
AddParameterTest >> test30AddingParameterInTheMiddleWorksAsExpected [

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:m3:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := 'm1:m2:m3:' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'm1: p1 m3: p3'.
	classToRefactor compile: senderSelector asString, ' self m1: 1 m3: 3'.
	
	refactoring := AddParameter 
		named: newParameter at: 2 initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode arguments at: 2) name equals: newParameter.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue).
	self assert: senderMethod sourceCode equals: senderSelector asString, ' self m1: 1 m2: 2 m3: 3'.

]

{ #category : 'tests' }
AddParameterTest >> test31AddingParameterInTheMiddleWithManySendersWorksAsExpected [

	| refactoring classToRefactor oldSelector newSelector newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:m3:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := 'm1:m2:m3:' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'm1: p1 m3: p3'.
	classToRefactor compile: senderSelector asString, '
	self m1: 1 m3: 3.
	self
		m1: 4
		m3: 6'.
	
	refactoring := AddParameter 
		named: newParameter at: 2 initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: senderMethod sourceCode equals: senderSelector asString, '
	self m1: 1 m2: 2 m3: 3.
	self
		m1: 4
		m2: 2 m3: 6'.

]

{ #category : 'tests' }
AddParameterTest >> test32IndexCanNotBeLessThanOne [
	
	self 
		assertCreation: [ 
			AddParameter 
				named: 'newParam' 
				at: 0
				initializedWith: '1' 
				using: 'm2:' asSymbol 
				toKeywordSelector: 'm1:' asSymbol  
				implementors: {} 
				senders: {} ]
		failsWith: [ AddParameter errorMessageForInvalidParameterIndex: 0 for: 1 ]
]

{ #category : 'tests' }
AddParameterTest >> test33IndexCanNotBeBiggerThanNumberOfParametersPlusOne [
	
	self 
		assertCreation: [ 
			AddParameter 
				named: 'newParam' 
				at: 3
				initializedWith: '1' 
				using: 'm2:' asSymbol 
				toKeywordSelector: 'm1:' asSymbol  
				implementors: {} 
				senders: {} ]
		failsWith: [ AddParameter errorMessageForInvalidParameterIndex: 3 for: 1 ]
]

{ #category : 'tests' }
AddParameterTest >> test34IndexMustBeInteger [
	
	self 
		assertCreation: [ 
			AddParameter 
				named: 'newParam' 
				at: 1.5
				initializedWith: '1' 
				using: 'm2:' asSymbol 
				toKeywordSelector: 'm1:' asSymbol  
				implementors: {} 
				senders: {} ]
		failsWith: [ AddParameter errorMessageForInvalidParameterIndex: 1.5 for: 1 ]
]

{ #category : 'tests' }
AddParameterTest >> test35AddingParameterRetractsInsertionPointWhenAtInsertionPointIsANewLine [

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector |
	
	oldSelector := 'm1' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, Character newLineCharacter asString, Character newLineCharacter asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	self shouldnt: [ refactoring apply ] raise: Error.
	
	
]

{ #category : 'tests' }
AddParameterTest >> test36ParentesisAreAddedToSendersOfUnaryMessage [

	self shouldFail: [ | refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector |
	
	oldSelector := 'm1' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, ', self size'.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self assert: senderSelector asString, ' (self ', oldSelector asString, '), self size' equals: (classToRefactor>>senderSelector) sourceCode 
	]
]
