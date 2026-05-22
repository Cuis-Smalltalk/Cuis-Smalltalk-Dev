Class {
	#name : 'TemporaryToInstanceVariableTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'assertions' }
TemporaryToInstanceVariableTest >> assertMethodNamed: aMethodName in: aClass hasSourceEqualTo: aSourceCode [

	| changedMethodNode |

	changedMethodNode _ self methodNodeIn: aClass named: aMethodName.
	self assert: aSourceCode equals: changedMethodNode sourceText.
			
]

{ #category : 'class factory' }
TemporaryToInstanceVariableTest >> classToRefactorName [

	^ #ClassToChangeVariable.
]

{ #category : 'class factory' }
TemporaryToInstanceVariableTest >> compileMethodNodeIn: aClass named: aSelector sourceCode: sourceCode [

	aClass compile: sourceCode.

	^self methodNodeIn: aClass named: aSelector.
]

{ #category : 'class factory' }
TemporaryToInstanceVariableTest >> methodNodeIn: aClass named: aSelector [

	^ aClass compiledMethodAt: aSelector :: methodNode.
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> methodSourceCodeNamed: aSelector withTemporaryVariableDeclaration: aTemporaryVariableName [

	^aSelector, '| ', aTemporaryVariableName, ' |'.
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> methodSourceCodeNamed: aSelector withTemporaryVariableDeclarations: temporaryVariableNames [
	^String streamContents: [ :stream |
		stream nextPutAll: aSelector, '| '.
		temporaryVariableNames do: [ :temporaryVariableName |
			stream nextPutAll: temporaryVariableName, ' ' ].	
		stream nextPut: $|. ].
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test01ApplyChangesTemporaryVariableToInstanceVariable [

	| variableName classToRefactor messageName refactoring sourceText methodNode changedMethodNode |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	sourceText _ self methodSourceCodeNamed: messageName withTemporaryVariableDeclaration: variableName.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	self assert: (classToRefactor definesInstanceVariableNamed: variableName).
	self assert: (classToRefactor canUnderstand: messageName).
	
	changedMethodNode _ self methodNodeIn: classToRefactor named: messageName.
	self assert: 0 equals: changedMethodNode temporaries size.
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test02ApplyDoesntChangeRestOfSourceCode [

	| variableName classToRefactor messageName refactoring sourceText methodNode changedMethodNode restOfSourceCode |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	sourceText _ self methodSourceCodeNamed: messageName withTemporaryVariableDeclaration: variableName. 
	restOfSourceCode _ variableName, ' := 2.
	^', variableName.
	sourceText _ sourceText, restOfSourceCode.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	changedMethodNode _ self methodNodeIn: classToRefactor named: messageName.
	self assert: (changedMethodNode sourceText findString: restOfSourceCode :: > 0).
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test03TemporaryToChangeShouldExist [

	|  classToRefactor messageName methodNode sourceText |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	sourceText _ messageName.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	self 
		should: [TemporaryToInstanceVariable named: 'a' fromMethod: methodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable inexistentTemporaryErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: sourceText ].
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test04TemporaryShouldNotExistInOtherClassMethod [

	|  classToRefactor firstMessageName firstMethodNode firstSourceText variableName secondMessageName secondSourceText |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	variableName _ 'a'.
	firstMessageName _ #m1.
	firstSourceText _ self 
		methodSourceCodeNamed: firstMessageName 
		withTemporaryVariableDeclaration: variableName.
	firstMethodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: firstMessageName 
		sourceCode: firstSourceText.
	
	secondMessageName _ #m2.
	secondSourceText _ self 
		methodSourceCodeNamed: secondMessageName 
		withTemporaryVariableDeclaration: variableName.
	classToRefactor compile: secondSourceText.
	
	self 
		should: [TemporaryToInstanceVariable named: variableName fromMethod: firstMethodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsInOtherMethodsErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: firstMessageName in: classToRefactor hasSourceEqualTo: firstSourceText.
			self assertMethodNamed: secondMessageName in: classToRefactor hasSourceEqualTo: secondSourceText ].
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test05InstanceVariableShouldNotExistInSubclass [

	| classToRefactor messageName methodNode sourceText variableName subclassToRefactor |

	variableName _ 'a'.
	
	classToRefactor _ self createClassNamed: self classToRefactorName.
	subclassToRefactor _ self 
		createClassNamed: #SubclassToRefactor
		subclassOf: classToRefactor
		instanceVariableNames: variableName
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
	
	messageName _ #m1.
	sourceText _ self 
		methodSourceCodeNamed: messageName 
		withTemporaryVariableDeclaration: variableName.
	methodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.

	self 
		should: [TemporaryToInstanceVariable named: variableName fromMethod: methodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsAsInstVarInSubclassesErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: sourceText.
			self assert: (subclassToRefactor instVarNames includes: variableName) ].
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test06TemporaryShouldNotExistInSubclassMethod [

	| classToRefactor firstMessageName firstMethodNode firstSourceText variableName secondMessageName secondSourceText subclassToRefactor |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	subclassToRefactor _ self 
		createClassNamed: #SubclassToRefactor
		subclassOf: classToRefactor
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
	
	variableName _ 'a'.
	firstMessageName _ #m1.
	firstSourceText _ self 
		methodSourceCodeNamed: firstMessageName 
		withTemporaryVariableDeclaration: variableName.
	firstMethodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: firstMessageName 
		sourceCode: firstSourceText.
	
	secondMessageName _ #m2.
	secondSourceText _ self 
		methodSourceCodeNamed: secondMessageName 
		withTemporaryVariableDeclaration: variableName.
	subclassToRefactor compile: secondSourceText.
	
	self 
		should: [TemporaryToInstanceVariable named: variableName fromMethod: firstMethodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsInOtherMethodsErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: firstMessageName in: classToRefactor hasSourceEqualTo: firstSourceText.
			self assertMethodNamed: secondMessageName in: subclassToRefactor hasSourceEqualTo: secondSourceText ].
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test07RemovesPipesWhenRemovingLastTemporaryInMethod [

	| variableName classToRefactor messageName refactoring sourceText methodNode |
	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	sourceText _ self methodSourceCodeNamed: messageName withTemporaryVariableDeclaration: variableName.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: 'm1 '.
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test08DoesntRemoveOtherVariables [

	| variableName classToRefactor messageName refactoring sourceText methodNode otherVariableName |
	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	otherVariableName _ 'b'.
	sourceText _ self 
		methodSourceCodeNamed: messageName 
		withTemporaryVariableDeclarations: {variableName. otherVariableName}.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: 'm1|  b |'.
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test09TemporaryShouldNotExistInOtherBlockInSameMethod [

	| classToRefactor messageName methodNode sourceText variableName blockWithVariableString |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	variableName _ 'a'.
	messageName _ #m1.
	blockWithVariableString _ '[ | ', variableName, ' | ].'.
	sourceText _ messageName, '
	', blockWithVariableString, '
	', blockWithVariableString.
	methodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.
	
	self 
		should: [ TemporaryToInstanceVariable named: variableName fromMethod: methodNode ]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsInOtherBlockErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: sourceText. ].
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test10RemovesPipesWhenRemovingLastTemporaryInBlock [

	| classToRefactor messageName methodNode sourceText variableName blockWithVariableString newSourceCode refactoring |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	variableName _ 'a'.
	messageName _ #m1.
	blockWithVariableString _ '[ | ', variableName, ' | ].'.
	sourceText _ messageName, '
	', blockWithVariableString.
	methodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	newSourceCode _ messageName, '
	[   ].'.
	
	self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: newSourceCode.
]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test11TemporaryShouldNotExistAsArgumentInOtherBlocksInSameMethod [

	| classToRefactor messageName methodNode sourceText variableName blockWithTempString blockWithArgumentString |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	variableName _ 'a'.
	messageName _ #m1.
	blockWithTempString _ '[ | ', variableName, ' | ].'.
	blockWithArgumentString _ '[ :', variableName, ' | ].'.
	sourceText _ messageName, ' ', blockWithTempString, ' ', blockWithArgumentString.
	methodNode _ self
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.
		
	self assertCreation: [ TemporaryToInstanceVariable named: variableName fromMethod: methodNode ]
		failsWith: [ TemporaryToInstanceVariable temporaryExistsInOtherBlockErrorDescription ].

]

{ #category : 'testing' }
TemporaryToInstanceVariableTest >> test12CantExtractArgument [

	| classToRefactor methodNode sourceText |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	sourceText := 'm1: a'.
	methodNode _ self
		compileMethodNodeIn: classToRefactor 
		named: #m1:
		sourceCode: sourceText.
		
	self assertCreation: [ TemporaryToInstanceVariable named: 'a' fromMethod: methodNode ]
		failsWith: [ TemporaryToInstanceVariable inexistentTemporaryErrorDescription ].

]
