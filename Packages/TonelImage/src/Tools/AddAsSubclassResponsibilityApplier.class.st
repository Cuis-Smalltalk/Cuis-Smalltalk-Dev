Class {
	#name : 'AddAsSubclassResponsibilityApplier',
	#superclass : 'TransferMethodApplier',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'refactoring - changes' }
AddAsSubclassResponsibilityApplier >> informChangesToBrowser [


]

{ #category : 'refactoring - creation' }
AddAsSubclassResponsibilityApplier >> refactoringClass [

	^AddAsSubclassResponsibility 
]
