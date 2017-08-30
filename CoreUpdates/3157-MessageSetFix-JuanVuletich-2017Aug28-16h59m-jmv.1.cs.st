'From Cuis 5.0 of 7 November 2016 [latest update: #3156] on 28 August 2017 at 5:05:29 pm'!

!MessageSet methodsFor: 'private' stamp: 'jmv 8/28/2017 17:05:18'!
initializeMessageList: anArray

	messageList _ anArray.
	messageList isEmpty
		ifTrue: [ selectedMessage _ nil ]
		ifFalse: [
			selectedMessage _ messageList first.
			self editSelection: #editMessage ]! !
