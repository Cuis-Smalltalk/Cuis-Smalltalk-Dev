Class {
	#name : 'RenameSelectorTest',
	#superclass : 'RefactoringTest',
	#instVars : [
		'oldSelector',
		'newSelector',
		'senderOfOldSelector',
		'classToRefactor'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
RenameSelectorTest >> allClassCategoriesOfTestData [

	^Array 
		with: self classCategoryOfTestData 
		with: self anotherClassCategoryOfTestData 
		with: self siblingClassCategory 
]

{ #category : 'class factory' }
RenameSelectorTest >> anotherClassCategoryOfTestData [

	^self classCategoryOfTestData,'-Another'
]

{ #category : 'assertions' }
RenameSelectorTest >> assertWasNotRenamedInClass: aClass  [

	| senderMethod |
	
	senderMethod := aClass compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsSelector: newSelector).
	self assert: (senderMethod sendsSelector: oldSelector)
]

{ #category : 'assertions' }
RenameSelectorTest >> assertWasRenamedInClass: aClass [
	
	| senderMethod |
	
	self assert: (aClass includesSelector: newSelector).
	self deny: (aClass includesSelector: oldSelector).
	senderMethod := aClass compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).

]

{ #category : 'assertions' }
RenameSelectorTest >> assertWasRenamedInClassAndMeta: aClass [
	
	self
		assertWasRenamedInClass: aClass;
		assertWasRenamedInClass: aClass class
]

{ #category : 'class factory' }
RenameSelectorTest >> classToRefactorName [

	^#ClassToRenameSelector
]

{ #category : 'class factory' }
RenameSelectorTest >> createClassToRefactor [

	classToRefactor := self 
		createClassWithImplementorAndSenderNamed: self classToRefactorName 
		subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData

]

{ #category : 'class factory' }
RenameSelectorTest >> createClassWithImplementorAndSenderInMetaTooNamed: aName subclassOf: aSuperclass categorizedAd: aCategory [

	| newClass |
	
	newClass := self createClassWithImplementorAndSenderNamed: aName subclassOf: aSuperclass categorizedAd: aCategory.
	newClass class compile: oldSelector asString.
	newClass class compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	^newClass

]

{ #category : 'class factory' }
RenameSelectorTest >> createClassWithImplementorAndSenderNamed: aName subclassOf: aSuperclass categorizedAd: aCategory [

	| newClass |
	
	newClass := self createClassNamed: aName subclassOf: aSuperclass category: aCategory.
	newClass compile: oldSelector asString.
	newClass compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	^newClass

]

{ #category : 'setup' }
RenameSelectorTest >> setUp [

	super setUp.
	
	oldSelector := 'oldXyz__' asSymbol.
	newSelector := 'newXyz__' asSymbol.
	senderOfOldSelector := 'm1__' asSymbol.
]

{ #category : 'class factory' }
RenameSelectorTest >> siblingClassCategory [

	^self classCategoryOfTestData,'-sibling'
]

{ #category : 'tests' }
RenameSelectorTest >> test01MessageWithNoSendersCreatesNewMethodAndRemovesOldOne [
	
	| rename |
	
	classToRefactor := self createClassNamed: self classToRefactorName..
	classToRefactor compile: oldSelector asString.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #().
	rename apply.
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector) 

]

{ #category : 'tests' }
RenameSelectorTest >> test02SendersOfMessageAreRenamed [
	
	| rename senderMethod |
	
	self createClassToRefactor.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>senderOfOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector)
]

{ #category : 'tests' }
RenameSelectorTest >> test03OtherMessagesSendsAreNotRenamed [

	|  rename senderMethod selectorToKeep |

	selectorToKeep := #toKeep.
	self createClassToRefactor.
	classToRefactor compile: senderOfOldSelector asString , ' self ' , oldSelector asString , '. self ' , selectorToKeep asString.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>senderOfOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	self assert: (senderMethod sendsSelector: selectorToKeep) 

]

{ #category : 'tests' }
RenameSelectorTest >> test04OldSelectorCanNotBeEmpty [

	self 
		assertCreation: [ RenameSelector from: '' asSymbol to: newSelector implementors: #() senders: #() ]
		failsWith: [ RenameSelector oldSelectorCanNotBeEmptyErrorMessage ]

]

{ #category : 'tests' }
RenameSelectorTest >> test05NewSelectorCanNotBeEmpty [

	self 
		assertCreation: [ RenameSelector from: oldSelector to: '' asSymbol implementors: #() senders: #() ]
		failsWith: [ RenameSelector newSelectorCanNotBeEmptyErrorMessage ]

]

{ #category : 'tests' }
RenameSelectorTest >> test06CanRenameWithoutImplementors [

	self shouldntFail: [ RenameSelector from: oldSelector to: newSelector implementors: #() senders: #() ]
		
]

{ #category : 'tests' }
RenameSelectorTest >> test07AllImplementorsMustImplementOldSelector [

	| invalidImplementors |
	
	invalidImplementors := Array with: Object>>#printString.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: invalidImplementors senders: #() ]
		failsWith: [ RenameSelector errorMessageForInvalidImplementors: invalidImplementors ]

]

{ #category : 'tests' }
RenameSelectorTest >> test08AllSendersShouldSendOldSelector [

	| implementors invalidSenders |
	
	oldSelector := #printString.
	implementors := Array with: Object>>oldSelector.
	invalidSenders := Array with: Object>>#size.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: implementors senders: invalidSenders ]
		failsWith: [ RenameSelector errorMessageForInvalidSenders: invalidSenders of: oldSelector ]

]

{ #category : 'tests' }
RenameSelectorTest >> test09NewSelectorMustBeOfSameTypeAsOldSelector [

	oldSelector := #size.
	newSelector := #+.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: #() senders: #() ]
		failsWith: [ RenameSelector errorMessageForNewSelector: newSelector isNotOfSameTypeAs: oldSelector ]

]

{ #category : 'tests' }
RenameSelectorTest >> test10NewSelectorMustHaveSameNumberOfArgumentsAsOldSelector [

	oldSelector := #printOn:.
	newSelector := #do:separatedBy:.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: #() senders: #() ]
		failsWith: [ RenameSelector errorMessageForNewSelector: newSelector doesNotHaveSameNumberOfArgumentsAs: oldSelector ]

]

{ #category : 'tests' }
RenameSelectorTest >> test11NewImplementorsKeepMethodCategory [
	
	| rename oldSelectorCategory |
	
	oldSelectorCategory := 'oldSelectorCategory'.
	self createClassToRefactor.
	classToRefactor organization classify: oldSelector under: oldSelectorCategory.
		
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>senderOfOldSelector).
	rename apply.
	
	self assert: oldSelectorCategory equals: (classToRefactor organization categoryOfElement: newSelector)

]

{ #category : 'tests' }
RenameSelectorTest >> test12RenamesRecursiveMethodsWhenNotInSenders [

	| rename senderMethod |
	
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString , ' self ' , oldSelector asString.
		
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #().
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: newSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector)

]

{ #category : 'tests' }
RenameSelectorTest >> test13RenamesRecursiveMethodsWhenInSenders [

	| rename senderMethod implementorsAndSenders |
	
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString , ' self ' , oldSelector asString.
	implementorsAndSenders := Array with: classToRefactor>>oldSelector.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: implementorsAndSenders senders: implementorsAndSenders.
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: newSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector)

]

{ #category : 'tests' }
RenameSelectorTest >> test14RenamesSymbolsEqualToOldSelector [

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' #' , oldSelector asString, ' size'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector) 

]

{ #category : 'tests' }
RenameSelectorTest >> test15ScopeToRenameCanBeClassOnly [
	
	| rename anotherClassSendingMessage |
	
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: self defaultSuperclass categorizedAd: self classCategoryOfTestData.
	
	anotherClassSendingMessage := self createClassNamed: #ClassToAvoidRenameSelector.
	anotherClassSendingMessage compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	rename := RenameSelector from: oldSelector to: newSelector in: classToRefactor.
	rename apply.
	
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasNotRenamedInClass: anotherClassSendingMessage 
]

{ #category : 'tests' }
RenameSelectorTest >> test16ScopeToRenameCanBeHierarchyOnly [
	
	| rename anotherClassSendingMessage superclassToRefactor subclassToRefactor |
	
	superclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SuperclassToRenameSelector subclassOf: self defaultSuperclass categorizedAd: self classCategoryOfTestData.
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: superclassToRefactor categorizedAd: self classCategoryOfTestData.
	subclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SubclassToRenameSelector subclassOf: classToRefactor categorizedAd: self classCategoryOfTestData.

	anotherClassSendingMessage := self createClassNamed: #ClassToAvoidRenameSelector.
	anotherClassSendingMessage compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	rename := RenameSelector from: oldSelector to: newSelector inHierarchyOf: classToRefactor.
	rename apply.
	
	self assertWasRenamedInClassAndMeta: superclassToRefactor.
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: subclassToRefactor.
	self assertWasNotRenamedInClass: anotherClassSendingMessage 
]

{ #category : 'tests' }
RenameSelectorTest >> test17CategoryScopeRenamesInAllCategoryTree [
	
	| rename classToRefactorInSiblingCategory  anotherClassToRefactor |
	
	classToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: self 
		classToRefactorName subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData.
	anotherClassToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #AnotherclassToRenameSelector 
		subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData.
	classToRefactorInSiblingCategory := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #AnotherclassToRenameSelectorInSiblingCategory 
		subclassOf: self defaultSuperclass 
		categorizedAd: self siblingClassCategory.
	
	rename := RenameSelector from: oldSelector to: newSelector inCategoryOf: classToRefactor organizedBy: SystemOrganization.
	rename apply.

	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: anotherClassToRefactor.
	self assertWasRenamedInClassAndMeta: classToRefactorInSiblingCategory
		
]

{ #category : 'tests' }
RenameSelectorTest >> test18HierarchyAndCategoryRenamesInHierarchyAndCategoryTree [
	
	| rename superclassToRefactor subclassToRefactor anotherClassToRefactor classInOtherCategoryToRefactor |
	
	superclassToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #SuperclassToRenameSelector 
		subclassOf: self defaultSuperclass 
		categorizedAd: self anotherClassCategoryOfTestData.
	classToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName 
		subclassOf: superclassToRefactor 
		categorizedAd: self classCategoryOfTestData.
	subclassToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #SubclassToRenameSelector 
		subclassOf: classToRefactor 
		categorizedAd: self classCategoryOfTestData.
	anotherClassToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #AnotheclassToRenameSelector 
		subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData.
	classInOtherCategoryToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #OtherCatClassToRenameSelector 
		subclassOf: self defaultSuperclass 
		categorizedAd: self anotherClassCategoryOfTestData.
	
	rename := RenameSelector 
		from: oldSelector 
		to: newSelector 
		inCategoriesAndHierarchyOf: classToRefactor 
		organizedBy: SystemOrganization.
	rename apply.

	self assertWasRenamedInClassAndMeta: superclassToRefactor.
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: subclassToRefactor.
	self assertWasRenamedInClassAndMeta: anotherClassToRefactor.
	self assertWasRenamedInClassAndMeta: classInOtherCategoryToRefactor.

]

{ #category : 'tests' }
RenameSelectorTest >> test19ScopeToRenameCanBeTheCompleteSystem [
	
	| rename anotherClassToRefactor |
	
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: self defaultSuperclass categorizedAd: self classCategoryOfTestData.
	anotherClassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #AnotherclassToRenameSelector subclassOf: self defaultSuperclass categorizedAd: self classCategoryOfTestData.

	rename := RenameSelector from: oldSelector to: newSelector inSystem: Smalltalk.
	"This is just to be sure that no more than the expected methods will be renamed because I'm using Smalltalk as system.
	I could mock Smalltalk but I want to have a real test using Smalltalk, not a mock, that it is why I have these assertions here - Hernan"
	self assert: 4 equals: rename implementorsSize.
	self assert: 4 equals: rename sendersSize.
	
	rename apply.

	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: anotherClassToRefactor  

]

{ #category : 'tests' }
RenameSelectorTest >> test20CanNotRenameToItSelf [

	self 
		assertCreation: [ RenameSelector from: oldSelector to: oldSelector asSymbol implementors: #() senders: #() ]
		failsWith: [ RenameSelector newSelectorEqualToOldSelectorErrorMessage ]

]

{ #category : 'tests' }
RenameSelectorTest >> test21NewSelectorCanNotBeImplementedOnAnyClassImplementingOldSelector [

	| implementors |
	
	oldSelector := #printString.
	newSelector := #size.
	implementors := Array with: Object>>oldSelector.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: implementors senders: #() ]
		failsWith: [ RenameSelector errorMessageForNewSelector: newSelector implementedInAll: (Array with: Object) ]

]

{ #category : 'tests' }
RenameSelectorTest >> test22WarnsWhenOverridesSuperclassImplementation [

	| implementors |
	
	oldSelector := #negated.
	newSelector := #size.
	implementors := Array with: Number>>oldSelector.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: implementors senders: #() ]
		warnsWith: [ RenameSelector warningMessageForImplementationOf: newSelector in: Number willOverrideImplementationIn: Object ]

]

{ #category : 'tests' }
RenameSelectorTest >> test23OldSelectorIsRenamedWhenDefinedInLiteralArray [

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ')'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector) 

]

{ #category : 'tests' }
RenameSelectorTest >> test24OldSelectorIsRenamedWhenDefinedInLiteralArrayMoreThanOnce [

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,')'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector) 

]

{ #category : 'tests' }
RenameSelectorTest >> test25OldSelectorIsRenamedWhenDefinedInMoreThanOneLiteralArrayMoreThanOnce [

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,'), #(#' , oldSelector asString, ' #' , oldSelector asString,')'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector) 

]

{ #category : 'tests' }
RenameSelectorTest >> test26DoesNotRenameLiteralsThatBeginsWithOldSelectorInsideLiteralArray [

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,'1)'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector). 
	self assert: (senderMethod hasLiteral: (oldSelector,'1') asSymbol) 

]

{ #category : 'tests' }
RenameSelectorTest >> test27DoesNotRenameLiteralsThatBeginsWithOldSelectorAndEndWithColonInsideLiteralArray [

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,':)'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector). 
	self assert: (senderMethod hasLiteral: (oldSelector,':') asSymbol) 

]

{ #category : 'tests' }
RenameSelectorTest >> test28CanRenameFromBinaryToKeywordOfOneParameter [

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	oldSelector := '&&' asSymbol.
	newSelector := 'abc:' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' aParameter'.
	classToRefactor compile: referencerToOldSelector asString , ' ^self ' , oldSelector asString,' 1'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	self assert: (classToRefactor includesSelector: newSelector).
	self deny: (classToRefactor includesSelector: oldSelector).
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector). 

]

{ #category : 'tests' }
RenameSelectorTest >> test29CanRenameFromKeywordOfOneParameterToBinary [

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	oldSelector := 'abc:' asSymbol.
	newSelector := '&&' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' aParameter'.
	classToRefactor compile: referencerToOldSelector asString , ' ^self ' , oldSelector asString,' 1'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	self assert: (classToRefactor includesSelector: newSelector).
	self deny: (classToRefactor includesSelector: oldSelector).
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector). 

]

{ #category : 'tests' }
RenameSelectorTest >> test30CanNotRenameFromBinaryToKeywordOfMoreThanOneParameter [

	oldSelector := '&&' asSymbol.
	newSelector := 'abc:def:' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' aParameter'.

	self 
		should: [ RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #() ]
		raise: Error
		withMessageText: (RenameSelector errorMessageForNewSelector: newSelector isNotOfSameTypeAs: oldSelector)
]

{ #category : 'tests' }
RenameSelectorTest >> test31CanNotRenameFromKeywordOfOneParameterToBinary [

	oldSelector := 'abc:def:' asSymbol.
	newSelector := '&&' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'abc: p1 def: p2'.

	self 
		should: [ RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #() ]
		raise: Error
		withMessageText: (RenameSelector errorMessageForNewSelector: newSelector isNotOfSameTypeAs: oldSelector)
]

{ #category : 'tests' }
RenameSelectorTest >> test32HierarchyScopeRenamesSelectorsFromSuperclassDefiningSelector [
	
	| rename superclassToRefactor subclassToRefactor |
	
	superclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SuperclassToRenameSelector subclassOf: self defaultSuperclass categorizedAd: self classCategoryOfTestData.
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: superclassToRefactor categorizedAd: self classCategoryOfTestData.
	subclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SubclassToRenameSelector subclassOf: superclassToRefactor categorizedAd: self classCategoryOfTestData.

	rename := RenameSelector from: oldSelector to: newSelector inHierarchyOf: classToRefactor.
	rename apply.
	
	self assertWasRenamedInClassAndMeta: superclassToRefactor.
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: subclassToRefactor.
	
]

{ #category : 'tests' }
RenameSelectorTest >> test33DoNotRenameSendersAndImplementorsWhoseCompilerClassIsDifferentToCompiler [
	
	"Requested by Phil B. to avoid refactoring OMeta2 classes - Hernan"
	
	| rename anotherClassSendingMessage  |
	
	classToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName 
		subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData.
	
	anotherClassSendingMessage := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #ClassToAvoidRenameSelector 
		subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData.
	
	anotherClassSendingMessage class compile: 'compilerClass ^nil'.

	rename := RenameSelector 
		from: oldSelector 
		to: newSelector 
		inCategoryOf: classToRefactor 
		organizedBy: SystemOrganization.
	rename apply.

	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasNotRenamedInClass: anotherClassSendingMessage.
	self assertWasRenamedInClass: anotherClassSendingMessage class.
	
]
