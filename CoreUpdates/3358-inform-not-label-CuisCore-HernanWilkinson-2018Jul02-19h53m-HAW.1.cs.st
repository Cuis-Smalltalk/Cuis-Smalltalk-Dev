'From Cuis 5.0 of 7 November 2016 [latest update: #3348] on 2 July 2018 at 7:56:47 pm'!

!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 7/2/2018 19:55:29'!
browseMessageList: messageList name: labelString autoSelect: autoSelectString
	| title aSize |
	"Create and schedule a MessageSet browser on the message list."

	messageList size = 0 ifTrue: 
		[^ PopUpMenu inform: ('There are no\' , labelString) withNewLines ].

	title _ (aSize _ messageList size) > 1
		ifFalse:	[ labelString ]
		ifTrue:	[ '[', aSize printString, '] ', labelString].

	MessageSetWindow
		openMessageList: messageList
		label: title
		autoSelect: autoSelectString! !

