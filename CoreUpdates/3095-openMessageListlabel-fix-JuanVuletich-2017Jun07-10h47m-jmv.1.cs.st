'From Cuis 5.0 of 7 November 2016 [latest update: #3095] on 7 June 2017 at 10:50:30 am'!

!MessageSetWindow class methodsFor: 'instance creation' stamp: 'jmv 6/7/2017 10:49:13'!
openMessageList: anArray label: aString
	"Create a standard system view for the message set on the list, anArray. 
	The label of the view is aString."

	^self open: (MessageSet messageList: anArray) label: aString! !

