Class {
	#name : 'RenameInstanceVariable',
	#superclass : 'Refactoring',
	#instVars : [
		'oldVariable',
		'renamedReferences',
		'methodsAndRangesToChange',
		'newVariable',
		'classToRefactor',
		'originalClassToRefactor',
		'originalClassDefinition'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
RenameInstanceVariable class >> assert: aClass defines: anInstanceVariable [

	(aClass definesInstanceVariableNamed: anInstanceVariable) ifFalse: [ self signalInstanceVariable: anInstanceVariable notDefinedIn: aClass ].
]

{ #category : 'exceptions' }
RenameInstanceVariable class >> errorMessageForInstanceVariable: aName notDefinedIn: aClass [

	^ 'Instance variable ''' , aName , ''' is not defined in ' , aClass name.
]

{ #category : 'instance creation' }
RenameInstanceVariable class >> from: anOldvariable to: aNewVariable in: aClassToRefactor  [
	
	| trimmedNewVariable |
	
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	self assert: aClassToRefactor defines: anOldvariable.	
	NewInstanceVariablePrecondition valueOf: trimmedNewVariable for: aClassToRefactor.	
	
	^self new initializeFrom: anOldvariable to: trimmedNewVariable in: aClassToRefactor 
]

{ #category : 'exceptions' }
RenameInstanceVariable class >> signalInstanceVariable: aName notDefinedIn: aClass [

	^ self refactoringError: (self errorMessageForInstanceVariable: aName notDefinedIn: aClass).
]

{ #category : 'applying' }
RenameInstanceVariable >> apply [

	originalClassDefinition := classToRefactor definition.
	originalClassToRefactor := classToRefactor copy.
	
	self 
		lookForMethodsReferencingOldVariable;
		changeInstanceVariableName;
		logChange;
		renameReferencesToOldVariable.
		
	^renamedReferences 
		
]

{ #category : 'applying - private' }
RenameInstanceVariable >> changeInstanceVariableName [

	| instanceVariableNames oldVariableIndex |
	
	instanceVariableNames := classToRefactor instVarNames.
	oldVariableIndex := instanceVariableNames indexOf: oldVariable.
	instanceVariableNames at: oldVariableIndex put: newVariable.
]

{ #category : 'initialization' }
RenameInstanceVariable >> initializeFrom: anOldvariable to: aNewVariable in: aClassToRefactor  [

	oldVariable := anOldvariable.
	newVariable := aNewVariable.
	classToRefactor := aClassToRefactor 
]

{ #category : 'applying - private' }
RenameInstanceVariable >> keepMethodToChangeNamed: aSelector in: aClass [

	| methodToChange rangesToChange |

	methodToChange := aClass >> aSelector.
	rangesToChange :=  methodToChange notOptimizedMethodNode positionsForInstanceVariable: oldVariable ifAbsent: [ #() ].

	methodsAndRangesToChange add: methodToChange -> rangesToChange 
]

{ #category : 'applying - private' }
RenameInstanceVariable >> logChange [
		
	Smalltalk
		logChange: classToRefactor definition 
		preamble: classToRefactor definitionPreamble.
		
	ChangeSet
		classDefinitionChangedFrom: originalClassToRefactor
		to: classToRefactor
		oldClassDefinition: originalClassDefinition.
]

{ #category : 'applying - private' }
RenameInstanceVariable >> lookForMethodsReferencingOldVariable [
	
	methodsAndRangesToChange := OrderedCollection new.
	classToRefactor withAllSubclassesDo: [ :aClass |  self lookForMethodsReferencingOldVariableIn: aClass ].

]

{ #category : 'applying - private' }
RenameInstanceVariable >> lookForMethodsReferencingOldVariableIn: aClass [

	(aClass whichSelectorsAccess: oldVariable) do: [ :aSelector | self keepMethodToChangeNamed: aSelector in: aClass ].
	
]

{ #category : 'applying - private' }
RenameInstanceVariable >> newSourceOf: aMethodAndRangesToChange  [
	
	| newSource ranges methodToChange |
	
	methodToChange := aMethodAndRangesToChange key.
	ranges := aMethodAndRangesToChange value.
	newSource := methodToChange sourceCode copyReplacing: ranges with: newVariable.
	
	^newSource
	
]

{ #category : 'applying - private' }
RenameInstanceVariable >> renameReferencesToOldVariable [
	
	renamedReferences := OrderedCollection new.
	methodsAndRangesToChange do: [ :aMethodAndRangesToChange | self renameReferencesToOldVariableInMethod: aMethodAndRangesToChange ].
	
]

{ #category : 'applying - private' }
RenameInstanceVariable >> renameReferencesToOldVariableInMethod: aMethodAndRangesToChange  [
	
	| methodToChange |
	
	methodToChange := aMethodAndRangesToChange key.
	methodToChange methodClass compile: (self newSourceOf: aMethodAndRangesToChange).
	renamedReferences add: methodToChange methodReference 
]
