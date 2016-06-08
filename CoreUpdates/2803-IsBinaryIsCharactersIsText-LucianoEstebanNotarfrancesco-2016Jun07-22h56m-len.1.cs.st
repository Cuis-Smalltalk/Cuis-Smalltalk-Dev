'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 7 June 2016 at 11:05:05.136103 pm'!

!PositionableStream methodsFor: 'testing' stamp: 'len 6/7/2016 21:41'!
isText
	"Return true if the receiver is a Text stream"
	^collection class == Text! !


!TextStream methodsFor: 'testing' stamp: 'len 6/7/2016 21:41'!
isText
	^ true! !


!Text methodsFor: 'printing' stamp: 'len 6/7/2016 21:51'!
printOn: aStream
	aStream isText
		ifTrue: [aStream nextPutAll: self. ^ self].
	self printNameOn: aStream.
	aStream nextPutAll: ' for '; print: string! !

