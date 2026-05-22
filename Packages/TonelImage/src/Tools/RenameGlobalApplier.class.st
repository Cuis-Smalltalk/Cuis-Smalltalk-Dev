Class {
	#name : 'RenameGlobalApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'browser',
		'newName',
		'oldName'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
RenameGlobalApplier class >> on: aBrowser  [

	^self on: aBrowser for: ''
]

{ #category : 'instance creation' }
RenameGlobalApplier class >> on: aBrowser for: anOldName [

	^self new initializeOn: aBrowser for: anOldName
]

{ #category : 'refactoring - parameters request' }
RenameGlobalApplier >> askNewName [

	newName := self request: 'Enter new name:' initialAnswer: oldName asPlainString.
	newName := newName withBlanksTrimmed asSymbol.
	
]

{ #category : 'refactoring - parameters request' }
RenameGlobalApplier >> askOldName [

	oldName := self request: 'Enter global name to rename:' initialAnswer: oldName.
	oldName := oldName withBlanksTrimmed asSymbol.
	
]

{ #category : 'refactoring - creation' }
RenameGlobalApplier >> createRefactoring [

	^RenameGlobal from: oldName to: newName in: Smalltalk 
]

{ #category : 'initialization' }
RenameGlobalApplier >> initializeOn: aBrowser for: anOldName [

	browser := aBrowser.
	oldName := anOldName.
	
]

{ #category : 'refactoring - changes' }
RenameGlobalApplier >> openChangedMethods [

	changes ifNotEmpty: [ 
		MethodSetWindow openMessageList: changes label: 'Renamed references' autoHighlight: newName allOccurrences: false ]

]

{ #category : 'refactoring - parameters request' }
RenameGlobalApplier >> requestRefactoringParameters [

	oldName isEmpty ifTrue: [ self askOldName ].
	self askNewName
]

{ #category : 'refactoring - changes' }
RenameGlobalApplier >> showChanges [

	self openChangedMethods

]
