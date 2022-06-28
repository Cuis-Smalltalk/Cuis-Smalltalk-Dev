'From Cuis 6.0 [latest update: #5325] on 28 June 2022 at 11:42:55 am'!

!String methodsFor: 'accessing' stamp: 'jmv 6/27/2022 20:27:24'!
indexOf: aCharacter startingAt: start ifAbsent: aBlock
	| ans |
	aCharacter isCharacter ifFalse: [ ^ aBlock value ].
	ans _ String indexOfByte: aCharacter iso8859s15Code inString: self startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]! !

!String methodsFor: 'converting' stamp: 'jmv 6/27/2022 20:34:45'!
translateFrom: start  to: stop  table: table
	"translate the characters in the string by the given table, in place"
	String translate: self from: start to: stop table: table! !

!methodRemoval: String #compare: stamp: 'jmv 6/28/2022 11:42:27'!
String removeSelector: #compare:!
!methodRemoval: String #compare:caseSensitive: stamp: 'jmv 6/28/2022 11:42:27'!
String removeSelector: #compare:caseSensitive:!
