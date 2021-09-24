'From Cuis 5.0 [latest update: #4884] on 24 September 2021 at 11:22:34 am'!

!Base64MimeConverter class methodsFor: 'services' stamp: 'NM 9/24/2021 15:05:45'!
mimeEncode: aCollectionOrStream to: outStream
	self new
		dataStream: ((aCollectionOrStream is: #Stream)
			ifTrue: [aCollectionOrStream]
			ifFalse: [ReadStream on: aCollectionOrStream]);
		mimeStream: outStream;
		multiLine: true;
		mimeEncode! !

