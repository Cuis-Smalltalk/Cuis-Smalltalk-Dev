Class {
	#name : 'RenameTemporaryApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'classToRefactor',
		'newVariable',
		'smalltalkEditor',
		'methodNode',
		'oldVariableNode'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'exceptions' }
RenameTemporaryApplier class >> errorMessageForCanNotParseMethod: anError [

	^ String streamContents: [ :stream |
		stream
			nextPutAll: 'Method can not be parsed due to:';
			newLine;
			nextPutAll: anError messageText ]
]

{ #category : 'instance creation' }
RenameTemporaryApplier class >> on: aSmalltalkEditor for: aTemporaryNode at: aMethodNode [

	^self new initializeOn: aSmalltalkEditor for: aTemporaryNode at: aMethodNode
]

{ #category : 'refactoring - parameters request' }
RenameTemporaryApplier >> askNewVariableName [

	newVariable := (self request: 'Enter new name:' initialAnswer: oldVariableNode name) withBlanksTrimmed 
]

{ #category : 'refactoring - creation' }
RenameTemporaryApplier >> createRefactoring [

	^RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode
	
]

{ #category : 'initialization' }
RenameTemporaryApplier >> initializeOn: aSmalltalkEditor for: aTemporaryNode at: aMethodNode [

	smalltalkEditor := aSmalltalkEditor.
	classToRefactor := smalltalkEditor codeProvider selectedClassOrMetaClass.
	methodNode := aMethodNode.
	oldVariableNode := aTemporaryNode.
	
]

{ #category : 'refactoring - parameters request' }
RenameTemporaryApplier >> requestRefactoringParameters [

	self askNewVariableName
]

{ #category : 'refactoring - changes' }
RenameTemporaryApplier >> showChanges [

	smalltalkEditor actualContents: changes.
	smalltalkEditor hasUnacceptedEdits ifFalse: [
		smalltalkEditor 
			hasUnacceptedEdits: true;
			acceptContents ]
	
]
