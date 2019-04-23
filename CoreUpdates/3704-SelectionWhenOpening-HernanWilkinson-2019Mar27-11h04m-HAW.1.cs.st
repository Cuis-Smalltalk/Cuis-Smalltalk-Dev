'From Cuis 5.0 of 7 November 2016 [latest update: #3688] on 18 April 2019 at 6:36:44 pm'!

!MessageSetWindow class methodsFor: 'opening' stamp: 'HAW 3/29/2019 11:36:08'!
open: model label: aString

	| window |
	
	window := super open: model label: aString.
	model messageListIndex: 1.
	
	^window! !


!MessageSet methodsFor: 'private' stamp: 'HAW 3/29/2019 11:32:39'!
initializeMessageList: anArray

	messageList _ anArray.
	messageList isEmpty
		ifTrue: [ selectedMessage _ nil ]
		ifFalse: [ self messageListIndex: 1 ]! !


!MessageSetWindow class reorganize!
('opening' open:label: openFullProtocolForClass: openMessageList:label: openMessageList:label:autoSelect: openSubProtocolForClass:)
('*LiveTyping' openMessageList:label:types:selector:)
!

