'From Cuis 4.2 of 25 July 2013 [latest update: #1899] on 27 December 2013 at 8:42:45.83206 am'!

!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'sqr 12/15/2013 12:21'!
hasContents

	^self contents notEmpty! !


!MessageSetWindow class methodsFor: 'instance creation' stamp: 'sqr 12/16/2013 14:28'!
openMessageList: messageList label: labelString autoSelect: autoSelectString
	"Open a system view for a MessageSet on messageList. 
	 1/24/96 sw: the there-are-no msg now supplied by my sender"

	| messageSet |
	messageSet _ MessageSet messageList: messageList.
	messageSet autoSelectString: autoSelectString.
	MessageSetWindow open: messageSet label: labelString ! !

