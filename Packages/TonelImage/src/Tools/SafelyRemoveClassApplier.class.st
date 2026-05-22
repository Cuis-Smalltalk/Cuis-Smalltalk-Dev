Class {
	#name : 'SafelyRemoveClassApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'classToRemove',
		'browser'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
SafelyRemoveClassApplier class >> on: aBrowser of: aClassToRemove [
	
	^self new initializeOn: aBrowser of: aClassToRemove 
]

{ #category : 'refactoring - parameters request' }
SafelyRemoveClassApplier >> confirmationMessageText [
	
	^'Are you sure you want to remove ', classToRemove name asPlainString, '?'
]

{ #category : 'refactoring - creation' }
SafelyRemoveClassApplier >> createRefactoring [
		
	^SafelyRemoveClass of: classToRemove 
]

{ #category : 'refactoring - changes' }
SafelyRemoveClassApplier >> informChangesToBrowser [

	browser classListIndex: 0
]

{ #category : 'initialization' }
SafelyRemoveClassApplier >> initializeOn: aBrowser of: aClassToRemove  [
	
	browser := aBrowser.
	classToRemove := aClassToRemove 
]

{ #category : 'refactoring - parameters request' }
SafelyRemoveClassApplier >> requestRefactoringParameters [

	(self confirm: self confirmationMessageText) ifFalse: [ self endRequest ].

	
]

{ #category : 'refactoring - changes' }
SafelyRemoveClassApplier >> showChanges [

	self informChangesToBrowser
]
