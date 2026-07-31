'From Cuis7.9 [latest update: #8115] on 30 July 2026 at 4:23:50 pm'!

!Association methodsFor: 'printing' stamp: 'jmv 30/Jul/2026, 6:44:33 pm (UTC)'!
storeOn: aStream
	"Store in the format (key->value)"

	key storeOn: aStream.
	aStream nextPutAll: ' -> '.
	value storeOn: aStream.
! !

