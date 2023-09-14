'From Cuis 6.0 [latest update: #6025] on 13 September 2023 at 5:17:15 pm'!

!PositionableStream methodsFor: 'mode' stamp: 'jmv 9/12/2023 12:30:15'!
useBytes
	collection class = ByteArray ifFalse: [
		self error: 'Can not #useBytes' ]! !

