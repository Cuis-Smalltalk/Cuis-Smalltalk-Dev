Class {
	#name : 'InlineMethodMessageSendsSet',
	#superclass : 'MethodSet',
	#instVars : [
		'applier',
		'selectedIndex'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
InlineMethodMessageSendsSet class >> applier: anInlineMethodApplier [

	^(self messageList: anInlineMethodApplier messageSendsToInline) initializeApplier: anInlineMethodApplier
]

{ #category : 'initialization' }
InlineMethodMessageSendsSet >> addMessageNodeReference: aMessageNodeReference ifIncluded: anIncludedBlock [

	(messageList includes: aMessageNodeReference)
		ifTrue: [anIncludedBlock value]
		ifFalse: [messageList add: aMessageNodeReference]
]

{ #category : 'initialization' }
InlineMethodMessageSendsSet >> initialize [

	selectedIndex := 0.
	super initialize 
]

{ #category : 'initialization' }
InlineMethodMessageSendsSet >> initializeApplier: anInlineMethodApplier [

	applier := anInlineMethodApplier.
]

{ #category : 'message list' }
InlineMethodMessageSendsSet >> messageListIndex [

	^selectedIndex 
]

{ #category : 'message list' }
InlineMethodMessageSendsSet >> messageListIndex: anIndex [

	selectedIndex := anIndex.
	^super messageListIndex: anIndex 
]
