'From Cuis 6.0 [latest update: #5238] on 8 June 2022 at 12:03:47 pm'!

!CharacterSequence methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
string
	"Answer the receiver itself.  This is for compatibility with other text classes."
	^self! !

!CharacterSequence methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asSymbol
	"Answer the unique symbol whose characters are the characters of the string."
	^Symbol intern: self! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/8/2022 11:52:25'!
asUtf32String
	"Answer an instance of the UTF-32 encoded String class"

	^Utf32String fromUtf8Bytes: self asUtf8Bytes.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/8/2022 11:53:21'!
asUtf8String
	"Answer an instance of the UTF-8 encoded String class"

	^Utf8String fromUtf8Bytes: self asUtf8Bytes! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 1/7/2014 22:05'!
displayStringOrText
	"To be used in the UI"
	"Answer this string."

	^ self
! !

!CharacterSequence methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:48'!
print
	Transcript show: self; newLine! !

!CharacterSequence methodsFor: 'printing' stamp: 'sma 6/1/2000 09:48'!
printOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."

	self storeOn: aStream! !

!CharacterSequence methodsFor: 'printing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
storeOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."
	| x |
	aStream nextPut: $'.
	1 to: self size do:
		[:i |
		aStream nextPut: (x _ self at: i).
		x = $' ifTrue: [aStream nextPut: x]].
	aStream nextPut: $'! !


!Utf8Symbol methodsFor: 'printing' stamp: 'jmv 6/8/2022 11:58:13'!
print
	Transcript show: self printString; newLine! !

!Utf8Symbol methodsFor: 'printing' stamp: 'jmv 6/8/2022 12:02:18'!
storeOn: aStream 

	aStream nextPut: $#.
	(Scanner isLiteralSymbol: self)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]! !

!methodRemoval: Utf8String #displayStringOrText stamp: 'jmv 6/8/2022 11:55:04'!
Utf8String removeSelector: #displayStringOrText!
!methodRemoval: Utf8String #storeOn: stamp: 'jmv 6/8/2022 12:02:01'!
Utf8String removeSelector: #storeOn:!
!methodRemoval: Utf8String #asSymbol stamp: 'jmv 6/8/2022 11:48:48'!
Utf8String removeSelector: #asSymbol!
!methodRemoval: Utf8String #asUtf32String stamp: 'jmv 6/8/2022 11:52:35'!
Utf8String removeSelector: #asUtf32String!
!methodRemoval: Utf8String #print stamp: 'jmv 6/8/2022 11:58:00'!
Utf8String removeSelector: #print!
!methodRemoval: Utf8String #string stamp: 'jmv 6/8/2022 12:03:26'!
Utf8String removeSelector: #string!
!methodRemoval: Utf8String #printOn: stamp: 'jmv 6/8/2022 11:59:14'!
Utf8String removeSelector: #printOn:!
!methodRemoval: Utf32String #asUtf8String stamp: 'jmv 6/8/2022 11:53:31'!
Utf32String removeSelector: #asUtf8String!
!methodRemoval: String #displayStringOrText stamp: 'jmv 6/8/2022 11:55:00'!
String removeSelector: #displayStringOrText!
!methodRemoval: String #storeOn: stamp: 'jmv 6/8/2022 12:01:58'!
String removeSelector: #storeOn:!
!methodRemoval: String #asSymbol stamp: 'jmv 6/8/2022 11:48:43'!
String removeSelector: #asSymbol!
!methodRemoval: String #asUtf32String stamp: 'jmv 6/8/2022 11:52:36'!
String removeSelector: #asUtf32String!
!methodRemoval: String #print stamp: 'jmv 6/8/2022 11:57:53'!
String removeSelector: #print!
!methodRemoval: String #string stamp: 'jmv 6/8/2022 12:03:29'!
String removeSelector: #string!
!methodRemoval: String #asUtf8String stamp: 'jmv 6/8/2022 11:53:27'!
String removeSelector: #asUtf8String!
!methodRemoval: String #printOn: stamp: 'jmv 6/8/2022 11:59:10'!
String removeSelector: #printOn:!

!CharacterSequence reorganize!
('accessing' string)
('adding' add:)
('copying' appendToText:)
('comparing' commonPartWith:startAt:stopAt:applying:)
('converting' asByteString asPlainString asString asSymbol asText asUtf32String asUtf8Bytes asUtf8String contractTo: displayStringOrText)
('enumerating' collect: collectInArray:)
('printing' print printOn: storeOn:)
('testing' isString)
('user interface' edit editLabel:)
!


!Utf8Symbol reorganize!
('comparing' =)
('converting' asPlainString asString asSymbol)
('testing' isSymbol)
('printing' print storeOn:)
!

