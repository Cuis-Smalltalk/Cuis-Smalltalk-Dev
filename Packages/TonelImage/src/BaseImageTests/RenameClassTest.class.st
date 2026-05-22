Class {
	#name : 'RenameClassTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'tests' }
RenameClassTest >> test01NewClassNameHasToBeDifferentToOldOne [

	self 
		assertCreation: [ RenameClass from: self class to: self class name ]
		failsWith: [ RenameClass newNameEqualsOldNameErrorMessage]
]

{ #category : 'tests' }
RenameClassTest >> test02NewClassNameHasToBeASymbol [

	self 
		assertCreation: [ RenameClass from: self class to: 'aString' ]
		failsWith: [ NewClassPrecondition newNameMustBeSymbolErrorMessage]
]

{ #category : 'tests' }
RenameClassTest >> test03NewClassNameHasToStartWithUppercaseLetter [

	self 
		assertCreation: [ RenameClass from: self class to: #_A ]
		failsWith: [ NewClassPrecondition newNameMustStartWithRightLetterErrorMessage]
]

{ #category : 'tests' }
RenameClassTest >> test04_01NewClassNameShouldNotBeAGlobalVariable [

	| newClassName |
	
	newClassName := #Smalltalk.
	
	self 
		assertCreation: [ RenameClass from: self class to: newClassName in: Smalltalk ]
		failsWith: [ NewClassPrecondition errorMessageForAlreadyExistGlobalNamed: newClassName ]
]

{ #category : 'tests' }
RenameClassTest >> test04NewClassShouldNotExist [

	| newClassName |
	
	newClassName := #Object.
	
	self 
		assertCreation: [ RenameClass from: self class to: newClassName in: Smalltalk ]
		failsWith: [ NewClassPrecondition errorMessageForAlreadyExistClassNamed: newClassName ]
]

{ #category : 'tests' }
RenameClassTest >> test05NewClassShouldNotBeUndeclare [

	| newClassName undeclared |
	
	newClassName := #UndeclareClass.
	undeclared := Dictionary new.
	undeclared at: newClassName put: nil.
	
	self 
		assertCreation: [ RenameClass from: self class to: newClassName in: Smalltalk undeclared: undeclared ]
		failsWith: [ NewClassPrecondition errorMessageForNewClassIsUndeclared: newClassName ]
]

{ #category : 'tests' }
RenameClassTest >> test06OldClassIsRenamedToNewClass [

	| oldClassName newClassName classToRefactor rename |

	oldClassName :=  'OldClassTest07' asSymbol.
	newClassName := 'NewClassTest07' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	self assert: (Smalltalk classNamed: oldClassName) isNil.
	self deny: (Smalltalk classNamed: newClassName) isNil.
]

{ #category : 'tests' }
RenameClassTest >> test07OldClassDirectReferencesAreRenamed [

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest07' asSymbol.
	newClassName := 'NewClassTest07' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ', oldClassName asString, ' new'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteral: newClassName).
	self deny: (referencingMethod hasLiteral: oldClassName)
]

{ #category : 'tests' }
RenameClassTest >> test08OldClassLiteralReferencesAreRenamed [

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest08' asSymbol.
	newClassName := 'NewClassTest8' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' #', oldClassName asString, ' size'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteral: newClassName).
	self deny: (referencingMethod hasLiteral: oldClassName)
]

{ #category : 'tests' }
RenameClassTest >> test09OtherClassDirectReferencesAreNotRenamed [

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest09' asSymbol.
	newClassName := 'NewClassTest09' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ', oldClassName asString, ' new. Object new'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteral: newClassName).
	self deny: (referencingMethod hasLiteral: oldClassName).
	self assert: (referencingMethod hasLiteral: #Object).

]

{ #category : 'tests' }
RenameClassTest >> test10OtherLiteralReferencesAreNotRenamed [

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest10' asSymbol.
	newClassName := 'NewClassTest10' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' #', oldClassName asString, ' size. #Object size'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteral: newClassName).
	self deny: (referencingMethod hasLiteral: oldClassName).
	self assert: (referencingMethod hasLiteral: #Object).

]

{ #category : 'tests' }
RenameClassTest >> test11OldClassDirectAndLiteralReferencesAreRenameAtOnce [

	| oldClassName newClassName classToRefactor selector rename renamedReferences |
	
	oldClassName := 'OldClassTest11' asSymbol.
	newClassName := 'NewClassTest11' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ', oldClassName asString, ' new. #', oldClassName asString, ' size'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	renamedReferences := rename apply.
	
	self assert: 1 equals: renamedReferences size.
	self assert: selector equals: renamedReferences anyOne selector.
	self assert: newClassName equals: renamedReferences anyOne classSymbol.
	self assert: ((classToRefactor sourceCodeAt: selector) includesSubString: '#', newClassName)
]

{ #category : 'tests' }
RenameClassTest >> test12CanNotRenameAMetaclass [

	self 
		assertCreation: [ RenameClass from: self class class to: #Object ]
		failsWith: [ RenameClass classToRenameCanNotBeMetaclassErrorMessage]
]

{ #category : 'tests' }
RenameClassTest >> test13NewClassNameHasToBeASymbol [

	self 
		assertCreation: [ RenameClass from: self class to: '' asSymbol ]
		failsWith: [ NewClassPrecondition newClassNameCanNotBeEmptyErrorMessage]
]

{ #category : 'tests' }
RenameClassTest >> test14NewClassNameCanNotHaveSpaces [

	| oldClassName classToRefactor |

	oldClassName :=  'OldClassTest14' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.

	self 
		assertCreation: [ RenameClass from: classToRefactor to: 'With spaces' asSymbol ]
		failsWith: [ NewClassPrecondition newClassNameCanNotHaveSeparatorsErrorMessage]
]

{ #category : 'tests' }
RenameClassTest >> test15ItDoesNotRenameReferencesToClassVariableWithSameName [

	| classWithClassVariable oldClassName classToRefactor newClassName rename renamedReferences |

	oldClassName :=  'OldClassTest15' asSymbol.
	newClassName := 'NewClassTest15' asSymbol.
	"First I create a class var with the same name as the class to rename and a reference to it - Hernan"
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol.
	classWithClassVariable addClassVarName: oldClassName.
	classWithClassVariable compile: 'm1 ^', oldClassName.

	classToRefactor := self createClassNamed: oldClassName.

	rename := RenameClass from: classToRefactor to: newClassName.
	renamedReferences := rename apply.
	
	self assert: renamedReferences isEmpty.
	
	
	
]

{ #category : 'tests' }
RenameClassTest >> test16ItDoesNotRenameReferencesToClassVariableWithSameNameDefinedInAnySuperclass [

	| classWithClassVariable oldClassName classToRefactor newClassName rename renamedReferences superClassOfClassWithClassVarReference |

	oldClassName :=  'OldClassTest16' asSymbol.
	newClassName := 'NewClassTest16' asSymbol.

	superClassOfClassWithClassVarReference := self createClassNamed: 'ClassReferencingClassVarSuperclass' asSymbol.
	superClassOfClassWithClassVarReference addClassVarName: oldClassName.
	
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol subclassOf: superClassOfClassWithClassVarReference.
	classWithClassVariable compile: 'm1 ^', oldClassName.

	classToRefactor := self createClassNamed: oldClassName.

	rename := RenameClass from: classToRefactor to: newClassName.
	renamedReferences := rename apply.
	
	self assert: renamedReferences isEmpty.
	
	
	
]

{ #category : 'tests' }
RenameClassTest >> test17ItDoesNotRenameReferencesToClassVariableWithSameNameAsNewClassDefinedInAnySuperclass [

	| classWithClassVariable oldClassName classToRefactor newClassName rename renamedReferences superClassOfClassWithClassVarReference |

	oldClassName :=  'OldClassTest17' asSymbol.
	newClassName := 'NewClassTest17' asSymbol.

	superClassOfClassWithClassVarReference := self createClassNamed: 'ClassReferencingClassVarSuperclass' asSymbol.
	superClassOfClassWithClassVarReference addClassVarName: newClassName.
	
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol subclassOf: superClassOfClassWithClassVarReference.
	classWithClassVariable compile: 'm1 ^', newClassName.

	classToRefactor := self createClassNamed: oldClassName.

	rename := RenameClass from: classToRefactor to: newClassName.
	renamedReferences := rename apply.
	
	self assert: renamedReferences isEmpty.
	
	
	
]

{ #category : 'tests' }
RenameClassTest >> test18WorksCorrectlyWhenSourceCodeEndsWithClassToRename [

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest18' asSymbol.
	newClassName := 'NewClassTest18' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ^', oldClassName asString.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteral: newClassName).
	self deny: (referencingMethod hasLiteral: oldClassName)
]

{ #category : 'tests' }
RenameClassTest >> test19RenamesClassWhenMethodReferencesClassAndDoesNothing [

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	"Currently the rename class is not handling this case because message #allCallsOn: does not return a method
	that references a class but does nothing because it is optimize to return self... it is a really wierd case so I decided
	no to solve it by now - Hernan"
	self shouldFail: [
		oldClassName := 'OldClassTest18' asSymbol.
		newClassName := 'NewClassTest18' asSymbol.
		classToRefactor := self createClassNamed: oldClassName.
		selector := #m1.
		classToRefactor compile: selector, ' ', oldClassName asString.
	
		rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
		rename apply.
	
		referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
		self assert: (referencingMethod hasLiteral: newClassName).
		self deny: (referencingMethod hasLiteral: oldClassName)] 
]

{ #category : 'tests' }
RenameClassTest >> test20ItDoesNotRenameReferencesToClassVariableWithSameNameAndReferencedInClassMethod [

	| classWithClassVariable oldClassName classToRefactor newClassName rename renamedReferences |

	oldClassName :=  'OldClassTest15' asSymbol.
	newClassName := 'NewClassTest15' asSymbol.
	"First I create a class var with the same name as the class to rename and a reference to it - Hernan"
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol.
	classWithClassVariable addClassVarName: oldClassName.
	classWithClassVariable class compile: 'm1 ^', oldClassName.

	classToRefactor := self createClassNamed: oldClassName.

	rename := RenameClass from: classToRefactor to: newClassName.
	renamedReferences := rename apply.
	
	self assert: renamedReferences isEmpty.
	self assert: (classWithClassVariable definesClassVariableNamedInHierarchy: oldClassName)
	
	
	
]
