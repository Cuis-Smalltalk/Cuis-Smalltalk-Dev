Class {
	#name : 'PushDownInstanceVariableTest',
	#superclass : 'RefactoringTest',
	#instVars : [
		'classToRefactor',
		'instanceVariableToPushDown'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'assertions' }
PushDownInstanceVariableTest >> assertPushDownCreationFailsWith: aStringOrBlock [

	self
		assertCreation: [ PushDownInstanceVariable named: instanceVariableToPushDown from: classToRefactor. ]
		failsWith: aStringOrBlock 
]

{ #category : 'class factory' }
PushDownInstanceVariableTest >> classToRefactorName [
	^#ClassToPushInstanceVariableDown.
]

{ #category : 'setup' }
PushDownInstanceVariableTest >> setUp [

	super setUp.
	
	instanceVariableToPushDown := 'a'.
]

{ #category : 'tests' }
PushDownInstanceVariableTest >> test01AnInstanceVariableDoesNotExistOnClassToFactorCannotBePushedDownToSubclasses [
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	
	self assertPushDownCreationFailsWith: PushDownInstanceVariable instanceVariableDoesNotExistOnClassToRefactor .
	
	self deny: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushDown).
	classToRefactor subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushDown).
	].
]

{ #category : 'tests' }
PushDownInstanceVariableTest >> test02AnInstanceVariableThatIsAccessedOnClassToRefactorCannotBePushedDownToAllSubclasses [

	|  selector |
	
	selector := #newMethod.
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: instanceVariableToPushDown.
	classToRefactor compile: selector, '^ ', instanceVariableToPushDown.
	
	self createClassNamed: #SubclassWithoutInstVarOne subclassOf: classToRefactor.
	self createClassNamed: #SubclassWithoutInstVarTwo subclassOf: classToRefactor.
	
	self assertPushDownCreationFailsWith: (PushDownInstanceVariable 
		errorMessageForInstanceVariable: instanceVariableToPushDown 
		isAccessedInMethodsOf: classToRefactor).
	
	self assert: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushDown).
	classToRefactor subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushDown)].
]

{ #category : 'tests' }
PushDownInstanceVariableTest >> test03AnInstanceVariableThatIsNotAccessedOnClassToRefactorShouldBePushedDownToAllSubclasses [

	|  pushDown |
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: instanceVariableToPushDown.
	self createClassNamed: #SubclassWithoutInstVarOne subclassOf: classToRefactor.
	self createClassNamed: #SubclassWithoutInstVarTwo subclassOf: classToRefactor.
	
	pushDown := PushDownInstanceVariable named: instanceVariableToPushDown from: classToRefactor.
	pushDown apply.
	
	self deny: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushDown).
	classToRefactor subclassesDo: [ :subClass |
		self assert: (subClass definesInstanceVariableNamed: instanceVariableToPushDown).
	].
]
