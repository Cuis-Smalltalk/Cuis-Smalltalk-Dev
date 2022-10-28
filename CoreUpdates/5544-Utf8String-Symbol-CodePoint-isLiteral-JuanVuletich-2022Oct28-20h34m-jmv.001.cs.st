'From Cuis 6.0 [latest update: #5543] on 28 October 2022 at 8:35:13 pm'!

!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 10/28/2022 19:01:27'!
isLiteral
	^ self asCharacter notNil! !

!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 10/28/2022 19:03:52'!
storeOn: aStream

	self asCharacter ifNotNil: [ :c |
		^ c storeOn: aStream ].

	aStream nextPutAll: '(UnicodeCodePoint codePoint: '.
	aStream print: value.
	aStream nextPut: $).! !


!Utf8String methodsFor: 'printing' stamp: 'jmv 10/28/2022 18:56:23'!
isLiteral
	^true! !


!Utf8Symbol methodsFor: 'printing' stamp: 'jmv 10/28/2022 18:56:32'!
isLiteral
	"Answer whether the receiver is a valid Smalltalk literal."

	^ true! !

