Class {
	#name : 'PushUpInstanceVariableTest',
	#superclass : 'RefactoringTest',
	#instVars : [
		'classToRefactor',
		'instanceVariableToPushUp',
		'classToRefactorSuperClass'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'assertions' }
PushUpInstanceVariableTest >> assertPushUpCreationFailsWith: aStringOrBlock [

	self
		assertCreation: [ PushUpInstanceVariable named: instanceVariableToPushUp from: classToRefactor. ]
		failsWith: aStringOrBlock.
]

{ #category : 'class factory' }
PushUpInstanceVariableTest >> classToRefactorName [
	^#ClassToPushInstanceVariableUp.
]

{ #category : 'setup' }
PushUpInstanceVariableTest >> setUp [

	super setUp.
	
	instanceVariableToPushUp := 'a'.
]

{ #category : 'tests' }
PushUpInstanceVariableTest >> test01AnInstanceVariableThatIsNotUsedOnClassToRefactorShouldBePushedUpToSuperclass [

	|  pushUp |
	
	classToRefactorSuperClass := self createClassNamed: #SuperclassWithoutInstVar.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperClass instanceVariableNames: instanceVariableToPushUp.

	pushUp := PushUpInstanceVariable named: instanceVariableToPushUp from: classToRefactor.
	pushUp apply.
	
	self assert: (classToRefactorSuperClass definesInstanceVariableNamed: instanceVariableToPushUp).
	classToRefactorSuperClass subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushUp)
	].
]

{ #category : 'tests' }
PushUpInstanceVariableTest >> test02AnInstanceVariableOnMultipleSubclassesShouldBeRemovedFromAllSubclassesAndPushedUpToSuperClass [

	|  pushUp  |
	
	classToRefactorSuperClass := self createClassNamed: #SuperclassWithoutInstVar.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperClass  instanceVariableNames: instanceVariableToPushUp.
	self createClassNamed: #AnoterSubClassWithInstVar subclassOf: classToRefactorSuperClass instanceVariableNames: instanceVariableToPushUp.
	
	pushUp := PushUpInstanceVariable named: instanceVariableToPushUp from: classToRefactor.
	pushUp apply.
	
	self assert: (classToRefactorSuperClass definesInstanceVariableNamed: instanceVariableToPushUp).
	classToRefactorSuperClass subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushUp)
	].
]

{ #category : 'tests' }
PushUpInstanceVariableTest >> test03AnInstanceVariableDoesNotExistOnClassToRefactorCannotBePushedUp [
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	
	self assertPushUpCreationFailsWith: PushUpInstanceVariable instanceVariableDoesNotExistOnClassToRefactor.
	
	self deny: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushUp).
]

{ #category : 'tests' }
PushUpInstanceVariableTest >> test04AnInstanceVariableThatIsUsedOnSuperClassAsTemporaryVariableCannotBePushedUp [

	| selector |
	
	selector := #newMethod.
	
	classToRefactorSuperClass := self createClassNamed: #SuperclassWithVariableAsTemporaryVariable.
	classToRefactorSuperClass compile: selector, '| ' , instanceVariableToPushUp , ' |'.
	
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperClass  instanceVariableNames: instanceVariableToPushUp.
	
	self assertPushUpCreationFailsWith: (PushUpInstanceVariable 
		errorMessageForInstanceVariable: instanceVariableToPushUp 
		isDefinedInMethodsOf: classToRefactorSuperClass).
	
	self deny: (classToRefactorSuperClass definesInstanceVariableNamed: instanceVariableToPushUp).
	self assert: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushUp).
]

{ #category : 'tests' }
PushUpInstanceVariableTest >> test05AnInstanceVariableOnMultipleSubclassesInDifferentLevelsShouldBeRemovedFromAllSubclassesAndPushedUpToSuperClass  [

	|  pushUp subclassWithoutInstVar  |

	classToRefactorSuperClass := self createClassNamed: #SuperclassWithoutInstVar.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperClass  instanceVariableNames: instanceVariableToPushUp.
	subclassWithoutInstVar := self createClassNamed: #SubClassWithoutInstVar subclassOf: classToRefactorSuperClass.
	self createClassNamed: #SubSubClassWithInstVar subclassOf: subclassWithoutInstVar instanceVariableNames: instanceVariableToPushUp.

	pushUp := PushUpInstanceVariable named: instanceVariableToPushUp from: classToRefactor.
	pushUp apply.

	self assert: (classToRefactorSuperClass definesInstanceVariableNamed: instanceVariableToPushUp).
	classToRefactorSuperClass allSubclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushUp)
	].
]
