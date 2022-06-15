'From Cuis 6.0 [latest update: #5240] on 7 June 2022 at 4:47:19 pm'!

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/7/2022 16:43:43'!
sameAs: aString 
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	^ ('' compare: bytes with: aString asUtf8Bytes) = 2! !


!String methodsFor: 'comparing' stamp: 'jmv 6/7/2022 16:44:05'!
sameAs: aString 
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString sameAs: self ].

	^ (self compare: self with: aString collated: CaseInsensitiveOrder) = 2! !

