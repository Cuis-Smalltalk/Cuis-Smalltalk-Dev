'From Cuis 6.0 [latest update: #5772] on 22 April 2023 at 7:26:21 pm'!

!MessageNamesWindow class methodsFor: 'instance creation' stamp: 'bp 4/22/2023 19:25:20'!
openMessageNames
	"Bring a MessageNames tool to the front"
	^self
		open: MessageNames new
		label: 'Message Names'.! !

