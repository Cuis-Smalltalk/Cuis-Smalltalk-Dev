Class {
	#name : 'RemoveInstanceVariable',
	#superclass : 'Refactoring',
	#instVars : [
		'variableToRemove',
		'classToRefactor'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
RemoveInstanceVariable class >> assert: aClass defines: anInstanceVariable [

	(aClass definesInstanceVariableNamed: anInstanceVariable) ifFalse: [ self signalInstanceVariable: anInstanceVariable notDefinedIn: aClass ].
]

{ #category : 'pre-conditions' }
RemoveInstanceVariable class >> assert: aVaraible isNotReferencedInHierarchyOf: aClassToRefactor [

	| references |
	
	references := OrderedCollection new.
	aClassToRefactor withAllSubclassesDo: [ :aClass | 
		(aClass whichSelectorsAccess: aVaraible) do: [ :aSelector | references add: (MethodReference class: aClass selector: aSelector) ]].
	
	references notEmpty ifTrue: [ self signalInstanceVariable: aVaraible isReferencedInAll: references ]
]

{ #category : 'exceptions' }
RemoveInstanceVariable class >> errorMessageForInstanceVariable: aVariable isReferencedInAll: methods [

	^aVariable, ' can not be removed because it has references'.
		
]

{ #category : 'exceptions' }
RemoveInstanceVariable class >> errorMessageForInstanceVariable: aName notDefinedIn: aClass [

	^ 'Instance variable ''' , aName , ''' is not defined in ' , aClass name.
]

{ #category : 'instance creation' }
RemoveInstanceVariable class >> named: aVariable from: aClassToRefactor  [
	
	self assert: aClassToRefactor defines: aVariable.
	self assert: aVariable isNotReferencedInHierarchyOf: aClassToRefactor.
	
	^self new initializeNamed: aVariable from: aClassToRefactor 

]

{ #category : 'exceptions' }
RemoveInstanceVariable class >> signalInstanceVariable: aVariable isReferencedInAll: methods [

	self 
		canNotRefactorDueToReferencesError: (self errorMessageForInstanceVariable: aVariable isReferencedInAll: methods)
		 references: methods
		 to: aVariable
]

{ #category : 'exceptions' }
RemoveInstanceVariable class >> signalInstanceVariable: aName notDefinedIn: aClass [

	^ self refactoringError: (self errorMessageForInstanceVariable: aName notDefinedIn: aClass).
]

{ #category : 'applying' }
RemoveInstanceVariable >> apply [
	
	classToRefactor removeInstVarName: variableToRemove 
]

{ #category : 'initialization' }
RemoveInstanceVariable >> initializeNamed: aVariable from: aClassToRefactor  [

	variableToRemove := aVariable.
	classToRefactor := aClassToRefactor 
]
