Class {
	#name : 'MoveToInstanceOrClassMethodApplier',
	#superclass : 'TransferMethodApplier',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'refactoring - parameters request' }
MoveToInstanceOrClassMethodApplier >> confirmationMessageText [
	
	^'This message has senders. Are you sure you want to move it?'
]

{ #category : 'refactoring - parameters request' }
MoveToInstanceOrClassMethodApplier >> methodHasSenders [

	^(methodToTransfer methodClass whichSelectorsReferTo: methodToTransfer selector) isEmpty not.
]

{ #category : 'refactoring - creation' }
MoveToInstanceOrClassMethodApplier >> refactoringClass [

	^MoveToInstanceOrClassMethod
]

{ #category : 'refactoring - parameters request' }
MoveToInstanceOrClassMethodApplier >> requestRefactoringParameters [

	self methodHasSenders ifTrue: [
		(self confirm: self confirmationMessageText) ifFalse: [ self endRequest ]
	]
]
