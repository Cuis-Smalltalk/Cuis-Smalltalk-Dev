Class {
	#name : 'MessageNodeReference',
	#superclass : 'Object',
	#instVars : [
		'messageNode',
		'selector',
		'classReference',
		'completeSourceRange',
		'methodNode',
		'stringVersion'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'initialization' }
MessageNodeReference class >> messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange [

	^self new messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange.
]

{ #category : 'accesing' }
MessageNodeReference >> actualClass [

	^classReference
]

{ #category : 'comparing' }
MessageNodeReference >> asPossibleMessageNodeReference [

	^PossibleMessageNodeReference
		messageNode: messageNode
		selector: selector
		class: classReference
		completeSourceRange: completeSourceRange.
]

{ #category : 'accesing' }
MessageNodeReference >> compiledMethod [

	^classReference >> selector 
]

{ #category : 'accesing' }
MessageNodeReference >> completeSourceRange [

	^completeSourceRange
]

{ #category : 'printing' }
MessageNodeReference >> displayStringOrText [
	"To be used in the UI"

	^stringVersion ifNil: [String streamContents: [:aStream | self printOn: aStream ]].
]

{ #category : 'comparing' }
MessageNodeReference >> equivalentTo: anObject [

	^(anObject isKindOf: MessageNodeReference)
		and: [anObject actualClass = classReference]
		and: [anObject methodNode sourceText = methodNode sourceText ]
		and: [anObject messageNode equivalentTo: messageNode ]
		and: [anObject completeSourceRange = completeSourceRange		]
		
]

{ #category : 'testing' }
MessageNodeReference >> isMessageListTitle [

	^completeSourceRange = #()

]

{ #category : 'accesing' }
MessageNodeReference >> isPossibleMessageSend [

	"Implements polymorphism with PossbileMessageNodeReference"

	^false
]

{ #category : 'accesing' }
MessageNodeReference >> messageNode [

	^messageNode 
]

{ #category : 'initialization' }
MessageNodeReference >> messageNode: aMessageNode selector: aSelector class: aClass completeSourceRange: aCompleteSourceRange [

	messageNode := aMessageNode.
	selector := aSelector.
	classReference := aClass.
	completeSourceRange := aCompleteSourceRange.
	"When no source range is passed it means this is a dummy message node reference that works as a title in the message list window
	and the selector is not even defined in the class - Fernando"
	self isMessageListTitle ifFalse: [methodNode := (classReference >> selector) notOptimizedMethodNode ].
	
]

{ #category : 'accesing' }
MessageNodeReference >> messageSendsRangesOf: aSelector [

	"To use polimorphysm with MethodReference - Fernando"

	^self isMessageListTitle ifTrue: [#()] ifFalse: [Array with: completeSourceRange].
]

{ #category : 'accesing' }
MessageNodeReference >> methodNode [

	^methodNode
]

{ #category : 'accesing' }
MessageNodeReference >> methodReference [

	^MethodReference class: classReference selector: 	selector.
]

{ #category : 'accesing' }
MessageNodeReference >> methodSymbol [

	^selector
]

{ #category : 'printing' }
MessageNodeReference >> prefixStringVersionWith: aString [

	"TODO: duplicated code from MethodReference"

	self removeStringVersionPrefix.
	stringVersion := '[', aString, '] - ', (stringVersion ifNil: [self displayStringOrText]).
]

{ #category : 'printing' }
MessageNodeReference >> printOn: aStream [

	classReference printOn: aStream.
	aStream nextPutAll: '>>'.
	selector printOn: aStream.
	aStream newLine.
	messageNode printOn: aStream.
	aStream newLine.
	completeSourceRange printOn: aStream.
]

{ #category : 'printing' }
MessageNodeReference >> removeStringVersionPrefix [

	"TODO: duplicated code from MethodReference"

	stringVersion ifNotNil: [:string | | i prefixCoda |
		prefixCoda := '] - '.
		i := stringVersion findString: prefixCoda.
		i = 0 ifFalse: [stringVersion := stringVersion copyFrom: i + prefixCoda size to: stringVersion size ].
	].

	
]

{ #category : 'accesing' }
MessageNodeReference >> selectorOfMessageNode [

	^messageNode selector key
]

{ #category : 'printing' }
MessageNodeReference >> stringVersion [

	^stringVersion
]

{ #category : 'printing' }
MessageNodeReference >> stringVersion: aString [

	stringVersion := aString.
]
