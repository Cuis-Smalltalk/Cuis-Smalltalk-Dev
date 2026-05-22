Class {
	#name : 'TemporaryToInstanceVariableApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'smalltalkEditor',
		'classToRefactor',
		'methodNode',
		'variableName'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
TemporaryToInstanceVariableApplier class >> on: aSmalltalkEditor for: aTemporaryVariableName [
	
	^self new initializeOn: aSmalltalkEditor for: aTemporaryVariableName
]

{ #category : 'refactoring - creation' }
TemporaryToInstanceVariableApplier >> createRefactoring [

	^TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
]

{ #category : 'initialization' }
TemporaryToInstanceVariableApplier >> initializeOn: aSmalltalkEditor for: aTemporaryVariableName [

	smalltalkEditor := aSmalltalkEditor.
	classToRefactor := smalltalkEditor codeProvider selectedClassOrMetaClass.
	methodNode := classToRefactor methodNodeFor: smalltalkEditor actualContents string.
	variableName := aTemporaryVariableName 
	
]

{ #category : 'refactoring - parameters request' }
TemporaryToInstanceVariableApplier >> requestRefactoringParameters [
	
]

{ #category : 'refactoring - changes' }
TemporaryToInstanceVariableApplier >> showChanges [

	smalltalkEditor actualContents: changes.
	
]
