Class {
	#name : 'PushDownMethodToOneSubclassApplier',
	#superclass : 'TransferMethodApplier',
	#instVars : [
		'subclassToPushDownTo'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'refactoring - creation' }
PushDownMethodToOneSubclassApplier >> createRefactoring [

	^self refactoringClass for: methodToTransfer to: subclassToPushDownTo
	
]

{ #category : 'refactoring - creation' }
PushDownMethodToOneSubclassApplier >> refactoringClass [

	^PushDownMethodToOneSubclass
]

{ #category : 'refactoring - parameters request' }
PushDownMethodToOneSubclassApplier >> requestRefactoringParameters [

	subclassToPushDownTo := (SelectionMenu selections: methodToTransfer methodClass subclasses)
		startUpWithCaption: 'Select subclass to push down method to'.
	subclassToPushDownTo ifNil: [ self endRequest ]
]
