Class {
	#name : 'ChangeKeywordsSelectorOrderTest',
	#superclass : 'RefactoringTest',
	#instVars : [
		'classToRefactor'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
ChangeKeywordsSelectorOrderTest >> classToRefactorName [

	^ #ClassToChangeSelectorOrder
]

{ #category : 'class factory' }
ChangeKeywordsSelectorOrderTest >> compile: aSourceCode [

	self createClassToRefactorIfNecessary.
	classToRefactor compile: aSourceCode
]

{ #category : 'class factory' }
ChangeKeywordsSelectorOrderTest >> createClassToRefactorIfNecessary [

	classToRefactor ifNil: [ classToRefactor := self createClassNamed: self classToRefactorName ]
]

{ #category : 'source code creation' }
ChangeKeywordsSelectorOrderTest >> selectorAndParametersFor: aKeywordSelector [

	^String streamContents: [ :stream |
		aKeywordSelector keywords
			withIndexDo: [ :aKeyword :index |
				stream
					nextPutAll: aKeyword;
					space;
					nextPut: $p;
					print: index ]
			separatedBy: [  stream space ]]
				
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test01CannotChangeSelectorOrderInUnaryMessages [

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #m1 to: #m1 implementors: #() senders: #() ]
		failsWith: [ ChangeKeywordsSelectorOrder selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage ]
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test02CannotChangeSelectorOrderInBinaryMessages [

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #+ to: #+ implementors: #() senders: #() ]
		failsWith: [ ChangeKeywordsSelectorOrder selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage ]
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test03CannotChangeSelectorOrderInKeywordMessagesWithOneParameterOnly [

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #m1: to: #m1: implementors: #() senders: #() ]
		 failsWith: [ ChangeKeywordsSelectorOrder selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage ]
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test04ChangesKeywordsOrder [

	|  refactoring oldSelector newSelector |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	self compile: (self selectorAndParametersFor: oldSelector).

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: #().
	refactoring apply.

	self assert: (classToRefactor includesSelector: newSelector).
	self deny: (classToRefactor includesSelector: oldSelector)
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test05ChangesParametersOrder [

	| refactoring oldSelector newSelector renamedMethod parameters selectorAndParameters |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: #().
	refactoring apply.

	renamedMethod := classToRefactor >> newSelector.
	parameters := selectorAndParameters substrings reject: [ :aKeywordOrParameter | oldSelector keywords includes: aKeywordOrParameter  ].
	self
		assert: (OrderedCollection with: parameters second with: parameters first)
		equals: renamedMethod methodNode argumentNames 
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test06NewSelectorMustIncludeOldSelectorKeywords [

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #m1:m2: to: #m3:m1: implementors: #() senders: #() ]
		failsWith: [ ChangeKeywordsSelectorOrder newSelectorDoesNotIncludeOldSelectorKeywordsErrorMessage ]
		
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test07ChangesKeywordsAndParametersOrderForMoreThanTwoKeywords [

	| refactoring renamedMethod oldSelector newSelector selectorAndParameters parameters |

	oldSelector := #m1:m2:m3:.
	newSelector := #m2:m3:m1:.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: #().
	refactoring apply.

	renamedMethod := classToRefactor >> newSelector.
	parameters := selectorAndParameters substrings reject: [ :aKeywordOrParameter | oldSelector keywords includes: aKeywordOrParameter  ].
	self
		assert: (OrderedCollection with: parameters second with: parameters third with: parameters first)
		equals: renamedMethod methodNode argumentNames 
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test08_ChangesArguments_WhenTheyAreLiterals [

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: 1 m2: 2.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, ' self m2: 2 m1: 1.'
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test09_ChangesArguments_WhenTheyAreCollaborations [

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: self value m2: self hash.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, ' self m2: self hash m1: self value.'
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test10_ChangesArguments_WhenTheyAreBlocks [

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: [ ] m2: [].'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, ' self m2: [] m1: [ ].'
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test11_ChangesArguments_WhenTheyAreCollaborationsAndBlocks [

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: [ |a| a:= 10. a ] m2: self value.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, ' self m2: self value m1: [ |a| a:= 10. a ].'
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test12_ChangesArguments_WhenTheKeywordsAreNotTheSameSize [

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #a:bb:ccc:.
	newSelector := #ccc:a:bb:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self a: 1 bb: 2 ccc: 3.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderSelector, ' self ccc: 3 a: 1 bb: 2.'
		equals: senderMethod sourceCode
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test13_ChangesArguments_WhenArgumentsAndKeywordsAreNotTheSameSize [

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #a:bb:ccc:.
	newSelector := #ccc:a:bb:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self a: 111 bb: 22 ccc: 3.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderSelector, ' self ccc: 3 a: 111 bb: 22.'
		equals: senderMethod sourceCode
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test14_ChangesArguments_WhenTheKeywordsAreNotTheSameSize [

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #a:bb:ccc:.
	newSelector := #ccc:a:bb:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self a: self value bb: 123412341234 ccc: 3.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderSelector, ' self ccc: 3 a: self value bb: 123412341234.'
		equals: senderMethod sourceCode
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test15_ChangingArguments_KeepsTheBlanksAtTheArgumentPosition [

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #a:bb:ccc:.
	newSelector := #ccc:a:bb:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self 
		a: self value 
		bb: 123412341234 
		ccc: 3.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderSelector, ' self 
		ccc: 3 
		a: self value 
		bb: 123412341234.'
		equals: senderMethod sourceCode
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test16_CanChangeSelectorOrder_WhenHavingMultipleCollaborationsInSameMessage [

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, '
	self m1: 1 m2: 2.
	self m1: 1 m2: 2.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, '
	self m2: 2 m1: 1.
	self m2: 2 m1: 1.'
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test17_CanChangeSelectorOrder_WhenHavingMultipleMessagesWithTheCollaboration [

	| refactoring oldSelector newSelector anotherSenderSelector selectorAndParameters senderSelector senderMethod anotherSenderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	anotherSenderSelector := #m4.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: 1 m2: 2.'.
	self compile: anotherSenderSelector, ' self m1: 111 m2: 22.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector . classToRefactor >> anotherSenderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	anotherSenderMethod := classToRefactor  >> anotherSenderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, ' self m2: 2 m1: 1.'.
	
	self 
		assert: anotherSenderMethod sourceCode
		equals: anotherSenderSelector, ' self m2: 22 m1: 111.'
]

{ #category : 'tests' }
ChangeKeywordsSelectorOrderTest >> test18_CanChangeSelectorOrder_WhenHavingTheChangingCollaborationRecursively [

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: (self m1: 1 m2: 2) m2: (self m1: 11 m2: 22).'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderSelector, ' self m2: (self m2: 22 m1: 11) m1: (self m2: 2 m1: 1).'
		equals: senderMethod sourceCode.
]
