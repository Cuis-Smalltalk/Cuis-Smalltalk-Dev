'From Cuis 6.0 [latest update: #5207] on 31 May 2022 at 2:24:13 pm'!

!Object methodsFor: 'testing' stamp: 'jmv 5/30/2022 15:16:50'!
isUtf8String
	"Overridden to return true in Utf8String, natch"
	^ false! !


!UnicodeCodePoint methodsFor: 'accessing' stamp: 'jmv 5/27/2022 17:35:23'!
digitValue
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 
	otherwise. This is used to parse literal numbers of radix 2-36.
	$0 numericValue = 48
	$9 numericValue = 57
	$A numericValue = 65
	$Z numericValue = 90 
	$7 digitValue = 7 
	"

	| nv |
	nv _ value.
	(nv between: 48 and: 57)
		ifTrue: [ ^ nv - 48 ].
	(nv between: 65 and: 90)
		ifTrue: [ ^ nv - 55 ].
	^ -1! !

!UnicodeCodePoint methodsFor: 'accessing' stamp: 'jmv 5/31/2022 14:22:15'!
numericValue
self flag: #unicodeNeedsImprovement.
	^self iso8859s15Code ifNil: [255]! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/27/2022 18:25:29'!
asLowercase
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^UnicodeCodePoint codePoint: ch asUnaccented asLowercase ].
	^self! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/27/2022 18:25:34'!
asUnaccented
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^UnicodeCodePoint codePoint: ch asUnaccented codePoint ].
	^self! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 5/30/2022 16:31:05'!
asText
	^ (Utf8String with: self) asText! !


!Utf8String methodsFor: 'testing' stamp: 'jmv 5/30/2022 15:16:58'!
isUtf8String

	^true! !

!Utf8String methodsFor: 'initialization' stamp: 'jmv 5/30/2022 17:10:13'!
initialize
	bytes _ `#[]`! !

!Utf8String methodsFor: 'printing' stamp: 'jmv 5/27/2022 17:33:58'!
print
	Transcript show: self; newLine! !

!Utf8String methodsFor: 'printing' stamp: 'jmv 5/27/2022 17:33:50'!
printOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."

	self storeOn: aStream! !

!Utf8String methodsFor: 'printing' stamp: 'jmv 5/27/2022 17:33:44'!
storeOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."
	| x |
	aStream nextPut: $'.
	1 to: self size do: [ :i |
		aStream nextPut: (x _ self at: i).
		x = $' ifTrue: [aStream nextPut: x]].
	aStream nextPut: $'! !

!Utf8String methodsFor: 'compatibility' stamp: 'jmv 5/30/2022 15:11:24'!
contents
	^self! !


!Utf8String methodsFor: 'copying' stamp: 'jmv 5/30/2022 14:23:03'!
, aString
	^Utf8String withBytes: bytes, aString asUtf8Bytes.! !

