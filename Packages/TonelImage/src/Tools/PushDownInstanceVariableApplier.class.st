Class {
	#name : 'PushDownInstanceVariableApplier',
	#superclass : 'TransferInstanceVariableApplier',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'refactoring - creation' }
PushDownInstanceVariableApplier >> createRefactoring [
		
	^PushDownInstanceVariable named: instanceVariableName from: classToRefactor.
	
]

{ #category : 'refactoring - parameters request' }
PushDownInstanceVariableApplier >> selectVariableLabel [
	
	^'Select instance variable to push down'
]
