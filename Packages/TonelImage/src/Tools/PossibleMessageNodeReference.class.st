Class {
	#name : 'PossibleMessageNodeReference',
	#superclass : 'MessageNodeReference',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
PossibleMessageNodeReference class >> messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange [

	^self new messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange.
]

{ #category : 'printing' }
PossibleMessageNodeReference class >> possibleMessageSendStringVersionHeader [

	"TODO: duplicated code from PossibleSender"

	^'Possible'
]

{ #category : 'accesing' }
PossibleMessageNodeReference >> isPossibleMessageSend [

	^true
]

{ #category : 'initialization' }
PossibleMessageNodeReference >> messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange [

	super messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange.
	self prefixStringVersionWith: self class possibleMessageSendStringVersionHeader.
]
