Class {
	#name : 'RenameGlobalTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'tests' }
RenameGlobalTest >> define: anOldName with: aGlobalValue toBeRenamedAs: aNewName while: aBlock [

	| oldNameAsSymbol newNameAsSymbol |
	
	oldNameAsSymbol := anOldName asSymbol.
	newNameAsSymbol := aNewName asSymbol.
	
	self assert: (Smalltalk at: oldNameAsSymbol ifAbsent: [ nil ]) isNil.
	self assert: (Smalltalk at: newNameAsSymbol ifAbsent: [ nil ]) isNil.
	
	[ 
		Smalltalk at: oldNameAsSymbol put: aGlobalValue.
		aBlock value: oldNameAsSymbol value: newNameAsSymbol value: aGlobalValue ] ensure: [
			Smalltalk removeKey: oldNameAsSymbol ifAbsent: [].
			Smalltalk removeKey: newNameAsSymbol ifAbsent: []].
		
]

{ #category : 'tests' }
RenameGlobalTest >> test00GlobalCanNotBeClass [

	self 
		assertCreation: [ RenameGlobal from: #Object to: 'NewObject__' asSymbol ]
		failsWith: [ RenameGlobal globalCanNotBeClassErrorMessage]
]

{ #category : 'tests' }
RenameGlobalTest >> test01NewNameHasToBeDifferentToOldOne [

	self 
		define: 'OldNameToRename__' 
		with: Object new 
		toBeRenamedAs: 'OldNameToRename__' 
		while: [ :oldName :newName :globalValue |
			self 
				assertCreation: [ RenameGlobal from: oldName to: newName ]
				failsWith: [ RenameGlobal newNameEqualsOldNameErrorMessage]]
]

{ #category : 'tests' }
RenameGlobalTest >> test02NewNameHasToBeASymbol [

	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: newName asString ]
			failsWith: [ NewGlobalPrecondition newNameMustBeSymbolErrorMessage]]
]

{ #category : 'tests' }
RenameGlobalTest >> test03NewNameShouldNotBeAClass [

	| className |
	
	className := #Object.
	
	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: className in: Smalltalk ]
			failsWith: [ NewGlobalPrecondition errorMessageForAlreadyExistClassNamed: className ]]
]

{ #category : 'tests' }
RenameGlobalTest >> test04NewNameShouldNotBeAGlobalVariable [

	| globalName |
	
	globalName := #Smalltalk.
	
	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: globalName in: Smalltalk ]
			failsWith: [ NewGlobalPrecondition errorMessageForAlreadyExistGlobalNamed: globalName ]]
]

{ #category : 'tests' }
RenameGlobalTest >> test06OldNameIsRenamedToNewName [

	| rename |

	self withDefaultsDo: [ :oldName :newName :globalValue |
		rename := RenameGlobal from: oldName to: newName in: Smalltalk.
		rename apply.

		self assert: (Smalltalk at: oldName ifAbsent: [nil]) isNil.
		self assert: (Smalltalk at: newName) equals: globalValue ].
]

{ #category : 'tests' }
RenameGlobalTest >> test07OldDirectReferencesAreRenamed [

	| rename classToRefactor referencingMethod selector |

	self withDefaultsDo: [ :oldName :newName :globalValue |
		classToRefactor := self createClassNamed: #AClassToRefactor.
		selector := 'm1' asSymbol.
		classToRefactor compile: selector, ' ^', oldName asString.

		rename := RenameGlobal from: oldName to: newName in: Smalltalk.
		rename apply.

		referencingMethod := classToRefactor compiledMethodAt: selector.
		self assert: (referencingMethod hasLiteral: newName).
		self deny: (referencingMethod hasLiteral: oldName).
		self assert: (classToRefactor new perform: selector) equals: globalValue ].
		
			
	

]

{ #category : 'tests' }
RenameGlobalTest >> test08OldNameLiteralReferencesAreRenamed [

	| rename classToRefactor referencingMethod selector |

	self withDefaultsDo: [ :oldName :newName :globalValue |
		classToRefactor := self createClassNamed: #AClassToRefactor.
		selector := 'm1' asSymbol.
		classToRefactor compile: selector, ' ^#', oldName asString.

		rename := RenameGlobal from: oldName to: newName in: Smalltalk.
		rename apply.

		referencingMethod := classToRefactor compiledMethodAt: selector.
		self assert: (referencingMethod hasLiteral: newName).
		self deny: (referencingMethod hasLiteral: oldName).
		self assert: (classToRefactor new perform: selector) equals: newName ].
		
			
]

{ #category : 'tests' }
RenameGlobalTest >> test09NewNameHasToBeASymbol [

	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: '' asSymbol ]
			failsWith: [ NewGlobalPrecondition newNameCanNotBeEmptyErrorMessage]]
]

{ #category : 'tests' }
RenameGlobalTest >> test10NewNameCanNotHaveSpaces [

	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: 'With spaces' asSymbol ]
			failsWith: [ NewGlobalPrecondition newNameCanNotHaveSeparatorsErrorMessage]]
]

{ #category : 'tests' }
RenameGlobalTest >> test11ItDoesNotRenameReferencesToClassVariableWithSameName [

	| rename classWithClassVariable renamedReferences oldNameAsString |

	oldNameAsString := 'OldNameToRename__'.
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol.
	classWithClassVariable addClassVarName: oldNameAsString asSymbol.
	classWithClassVariable compile: 'm1 ^', oldNameAsString.

	self 
		define: oldNameAsString 
		with: Object new 
		toBeRenamedAs: 'NewNameRenamed__' 
		while: [ :oldName :newName :globalValue |

			rename := RenameGlobal from: oldName to: newName.
			renamedReferences := rename apply.
	
			self assert: renamedReferences isEmpty ]
	
	
]

{ #category : 'tests' }
RenameGlobalTest >> test12ItDoesNotRenameReferencesToClassVariableWithSameNameDefinedInAnySuperclass [

	| classWithClassVariable rename renamedReferences superClassOfClassWithClassVarReference oldNameAsString |

	oldNameAsString := 'OldNameRenamed__'.

	superClassOfClassWithClassVarReference := self createClassNamed: 'ClassReferencingClassVarSuperclass' asSymbol.
	superClassOfClassWithClassVarReference addClassVarName: oldNameAsString asSymbol.
	
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol subclassOf: superClassOfClassWithClassVarReference.
	classWithClassVariable compile: 'm1 ^', oldNameAsString.

	self 
		define: oldNameAsString 
		with: Object new 
		toBeRenamedAs: 'NewNameRenamed__' 
		while: [ :oldName :newName :globalValue |

			rename := RenameGlobal from: oldName to: newName.
			renamedReferences := rename apply.
	
			self assert: renamedReferences isEmpty ]
	
	
	
]

{ #category : 'tests' }
RenameGlobalTest >> test13OldNameMustBeDefined [
	
	self 
		assertCreation: [ RenameGlobal from: 'AGlobal__' asSymbol to: 'AGlobal' asSymbol in: Smalltalk ]
		failsWith: [ RenameGlobal errorMessageForGlobalNotDefined: 'AGlobal__' asSymbol ]
]

{ #category : 'tests' }
RenameGlobalTest >> test14ItDoesNotRenameReferencesToClassVariableFromClassSideWithSameName [

	| rename classWithClassVariable renamedReferences oldNameAsString |

	oldNameAsString := 'OldNameToRename__'.
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol.
	classWithClassVariable addClassVarName: oldNameAsString asSymbol.
	classWithClassVariable class compile: 'm1 ^', oldNameAsString.

	self 
		define: oldNameAsString 
		with: Object new 
		toBeRenamedAs: 'NewNameRenamed__' 
		while: [ :oldName :newName :globalValue |

			rename := RenameGlobal from: oldName to: newName.
			renamedReferences := rename apply.
	
			self assert: renamedReferences isEmpty ]
	
	
]

{ #category : 'tests' }
RenameGlobalTest >> test15OldNameCanNotBeEmpty [
	
	self 
		assertCreation: [ RenameGlobal from: '' asSymbol to: 'AGlobal' asSymbol in: Smalltalk ]
		failsWith: [ RenameGlobal oldNameCanNotBeEmptyErrorMessage ]
]

{ #category : 'tests' }
RenameGlobalTest >> withDefaultsDo: aBlock [

	self 
		define: 'OldNameToRename__' 
		with: Object new 
		toBeRenamedAs: 'NewNameToRename__' 
		while: aBlock
]
