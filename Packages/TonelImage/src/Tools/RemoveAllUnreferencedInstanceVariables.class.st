Class {
	#name : 'RemoveAllUnreferencedInstanceVariables',
	#superclass : 'Refactoring',
	#instVars : [
		'classToRefactor'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
RemoveAllUnreferencedInstanceVariables class >> from: aClassToRefactor [

	^self new initializeFrom: aClassToRefactor 
]

{ #category : 'applying' }
RemoveAllUnreferencedInstanceVariables >> apply [
	
	| variableNamesToRemove |
	
	variableNamesToRemove := classToRefactor unreferencedInstanceVariables.
	variableNamesToRemove do: [ :aVariableName | classToRefactor removeInstVarName: aVariableName ].
	
	^variableNamesToRemove
]

{ #category : 'initialization' }
RemoveAllUnreferencedInstanceVariables >> initializeFrom: aClassToRefactor [

	classToRefactor := aClassToRefactor 
]
