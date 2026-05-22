"
I can add a new instance variable to a class. Input parameters are:

* name of the new variable
* class to add that variable
"
Class {
	#name : 'AddInstanceVariable',
	#superclass : 'Refactoring',
	#instVars : [
		'newVariable',
		'classToRefactor'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
AddInstanceVariable class >> named: aVariable to: aClassToRefactor [

	| trimmedVariable |
	
	trimmedVariable := aVariable withBlanksTrimmed.
	NewInstanceVariablePrecondition valueOf: trimmedVariable for: aClassToRefactor.	
	^self new initializeNamed: trimmedVariable to: aClassToRefactor 
	
]

{ #category : 'applying' }
AddInstanceVariable >> apply [
	
	classToRefactor addInstVarName: newVariable.
	
]

{ #category : 'accessing' }
AddInstanceVariable >> classToRefactor [

	^classToRefactor 
]

{ #category : 'initialization' }
AddInstanceVariable >> initializeNamed: aNewVariable to: aClassToRefactor  [
	
	newVariable := aNewVariable.
	classToRefactor := aClassToRefactor 
]

{ #category : 'accessing' }
AddInstanceVariable >> newVariable [
	
	^newVariable 
]
