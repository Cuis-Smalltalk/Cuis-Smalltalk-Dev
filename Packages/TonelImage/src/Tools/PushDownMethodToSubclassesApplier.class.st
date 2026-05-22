Class {
	#name : 'PushDownMethodToSubclassesApplier',
	#superclass : 'TransferMethodApplier',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'as yet unclassified' }
PushDownMethodToSubclassesApplier >> informChangesToBrowser [

	browser messageListIndex: browser messageListIndex 
]

{ #category : 'refactoring - creation' }
PushDownMethodToSubclassesApplier >> refactoringClass [

	^PushDownMethodToSubclasses
]
