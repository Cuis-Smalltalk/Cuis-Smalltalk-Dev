'From Cuis 5.0 of 7 November 2016 [latest update: #3763] on 24 May 2019 at 10:19:06 am'!

!Encoder methodsFor: 'source mapping' stamp: 'jmv 5/23/2019 09:49:51'!
rangeForNode: node ifAbsent: aBlock

	^sourceRanges at: node ifAbsent: aBlock! !


!MessageNode methodsFor: 'source ranges' stamp: 'jmv 5/23/2019 10:05:52'!
keywordAndParameterPositionAt: anIndex encodedWith: anEncoder ifAbsent: aBlock

	| keywordPosition parameterLastPosition |

	keywordPosition := keywordRanges at: anIndex.
	parameterLastPosition := anIndex = arguments size
		ifTrue: [ (anEncoder rangeForNode: self ifAbsent: aBlock) last ]
		ifFalse: [ (keywordRanges at: anIndex + 1) first - 1].

	^keywordPosition first to: parameterLastPosition! !


!MethodNode methodsFor: 'source mapping' stamp: 'jmv 5/23/2019 09:50:16'!
rangeForNode: node ifAbsent: aBlock

	^encoder rangeForNode: node ifAbsent: aBlock! !


!CompiledMethod methodsFor: 'source code ranges' stamp: 'HAW 5/24/2019 09:41:50'!
messageSendsRangesOf: aSentSelector

	| methodNode ranges |

	methodNode := self methodNode.
	ranges := OrderedCollection new.

	methodNode nodesDo: [ :aParseNode |
		(aParseNode isMessageNamed: aSentSelector) ifTrue: [
			ranges add: (methodNode 
				rangeForNode: aParseNode 
				ifAbsent: [ self error: 'should not happen. aParseNode is part of the methodNode'])]].

	^ranges ! !


!Encoder methodsFor: 'source mapping' stamp: 'jmv 5/23/2019 10:06:08'!
messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aBlock

	| positions |

	positions := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: [ :aMessageSendNode | aMessageSendNode keywordAndParameterPositionAt: anIndex encodedWith: self ifAbsent: aBlock].

	^ positions isEmpty
		ifTrue: aBlock
		ifFalse: [ positions ]
	! !

!Encoder methodsFor: 'source mapping' stamp: 'HAW 5/24/2019 09:36:14'!
parameterDefinitionPositionFor: aParameterNode

	^ (self rangeForNode: aParameterNode ifAbsent: [ self error: 'invalid parameter node' ]) first! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 5/24/2019 09:57:00'!
selectorLastPosition

	"If #DoIt selector, returns 0 - Hernan"

	^self selector isUnary
		ifTrue: [ selectorKeywordsRanges ifNil: [ 0 ] ifNotNil: [ selectorKeywordsRanges last last ]]
		ifFalse: [
			(encoder 
				rangeForNode: arguments last
				ifAbsent: [ self error: 'Should not happen. arguments is part of the encoder'  ]) first last ]! !


!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'jmv 5/23/2019 09:51:43'!
isAtClassName: anIndex

	^(classDefinitionNode rangeForNode: classCreationMessageNode arguments first ifAbsent: [ ^ false ]) first includes: anIndex! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing' stamp: 'jmv 5/23/2019 09:51:59'!
isAtSuperclass: anIndex

	^(classDefinitionNode rangeForNode: superClassNode ifAbsent: [ ^ false ]) first includes: anIndex! !

!ClassDefinitionNodeAnalyzer methodsFor: 'testing - private' stamp: 'jmv 5/23/2019 09:51:14'!
is: anIndex atStringParameterNumber: aParameterPosition

	| parameterRange |
	
	parameterRange := (classDefinitionNode rangeForNode: (classCreationMessageNode arguments at: aParameterPosition) ifAbsent: [ ^ false ]) first.
	
	^anIndex between: parameterRange first + 1 and: parameterRange last - 1! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 5/23/2019 10:09:52'!
selectMessageNode: aMessageNodeUnderCursor in: aMethodNode
	
	| messageRange |
	
	self 
		withReceiverRangeOf: aMessageNodeUnderCursor 
		in: aMethodNode 
		selectorPosition: self startIndex 
		do: [ :receiverRange |
			messageRange := aMethodNode rangeForNode: aMessageNodeUnderCursor ifAbsent: [ ^ self ].
			self selectFrom: receiverRange first to: messageRange last ]
		
	! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 5/23/2019 10:10:12'!
selectNodeRange: aNodeUnderCursor in: aMethodNode 
	
	| range ranges |
	
	ranges := aMethodNode rangeForNode: aNodeUnderCursor ifAbsent: [ ^ self ].
	range := (aMethodNode isMultipleRanges: ranges) 
		ifTrue: [ ranges detect: [ :aRange | aRange includes: self startIndex ] ifNone: [ ^self ]]
		ifFalse: [ ranges ].
		
	self selectFrom: range first to: range last
! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 5/23/2019 10:10:22'!
withReceiverRangeOf: aMessageNode in: aMethodNode selectorPosition: aSelectorPosition do: aBlock   
	
	| receiverRange receiverRangeOrRanges messageNodeReceiver |
	
	"If aMessageNode receiver isNil it means that it is a cascade receiver so this imposes the question on how to inspect
	a cascade message send. We could inspect the result of sending all the messages up to the cursor but the problem is
	that when looking for the cascade receiver range it does not find it because it is a different node that the used in the source
	ranges... we could do the trick of looking for printString in the sourceRanges keys, but that is too much - Hernan"
	aMessageNode isCascade ifFalse: [ 
		messageNodeReceiver := aMessageNode receiver.
		messageNodeReceiver isMessageNode ifTrue: [ 
			^self withReceiverRangeOf: messageNodeReceiver in: aMethodNode selectorPosition: (messageNodeReceiver keywordPositionAt: 1) first do: aBlock ].
		
		receiverRangeOrRanges := aMethodNode rangeForNode: messageNodeReceiver ifAbsent: [ ^ self ].
		
		receiverRange := (aMethodNode isMultipleRanges: receiverRangeOrRanges)
			ifTrue: [ | closestRange |
				closestRange := receiverRangeOrRanges first.
				receiverRangeOrRanges do: [ :aRange |  (aRange last < aSelectorPosition and: [ aRange last > closestRange last ]) ifTrue: [ closestRange := aRange ]].
				closestRange ]
			ifFalse: [ receiverRangeOrRanges ].

		aBlock value: receiverRange ]! !


!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 5/24/2019 10:14:31'!
assertIsValidParameterName: aName

	| scannedNames |

	scannedNames _ [ Scanner new scanFieldNames: aName ]
		on: Error
		do: [ :anError |  self signalInvalidParameterName: aName ].
	scannedNames size = 1 ifFalse: [ self signalInvalidParameterName: aName ].
	scannedNames first = aName ifFalse: [ self signalInvalidParameterName: aName ].
! !

!methodRemoval: MethodNode #sourceRangeFor:!
MethodNode removeSelector: #sourceRangeFor:!
!methodRemoval: MessageNode #keywordAndParameterPositionAt:encodedWith:!
MessageNode removeSelector: #keywordAndParameterPositionAt:encodedWith:!
!methodRemoval: Encoder #sourceRangeFor:!
Encoder removeSelector: #sourceRangeFor:!
