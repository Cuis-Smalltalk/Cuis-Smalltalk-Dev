'From Cuis 6.0 [latest update: #5295] on 15 June 2022 at 3:41:26 pm'!

!CharacterSequence class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:28:13'!
readFrom: inStream
	"Answer a new String that is determined by reading the stream, inStream.  Embedded
	double quotes become the quote Character."

	| char done |
	^self streamContents: [ :outStream |
		"go to first quote"
		inStream skipTo: $'.
		done _ false.
		[done or: [inStream atEnd]]
			whileFalse: 
				[char _ inStream next.
				char = $'
					ifTrue: 
						[char _ inStream next.
						char = $'
							ifTrue: [outStream nextPut: char]
							ifFalse: [done _ true]]
					ifFalse: [outStream nextPut: char]]].! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:20:49'!
crString
	"Answer a string containing a carriage return.
	Utf8String crString
	"
	^ `Utf8String with: Character cr`! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:20:34'!
crlfString
	"Answer a string containing a carriage return and a linefeed.
	Utf8String crlfString
	"
	^ `Utf8String with: Character cr with: Character lf`! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:25:02'!
lfString
	"Answer a string containing a single Lf character.
	Utf8String lfString
	"
	^ `Utf8String with: Character lf`! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:25:35'!
newLineString
	"Answer a string containing a single newLine (i.e. Lf) character.
	Utf8String newLineString
	"
	^ `Utf8String with: Character newLineCharacter`! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:31:40'!
tab
	"Answer a string containing a single tab character.
	Utf8String tab
	"
	^ `Utf8String with: Character tab`! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:12:58'!
with: aCharacter with: otherCharacter with: thirdCharacter
	"Answer a new instance of me, containing utf bytes for aCharacter, otherCharacter and thirdCharacter.
	Utf8String with: $a
	Utf8String with: $á with: Character euro
	Utf8String with: $á with: Character euro with: $¿
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint),
			(Character utf8BytesOfUnicodeCodePoint: otherCharacter codePoint),
			(Character utf8BytesOfUnicodeCodePoint: thirdCharacter codePoint)
		codePointCount: 3.! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:17:25'!
withAll: aSequenceableCollection
	"Create a new instance containing all the elements from aSequenceableCollection."
	^ self newFrom: aSequenceableCollection! !


!Utf8Symbol class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:40:27'!
fromCollectionOfStrings: aCollectionOfStrings

	^self newFrom: aCollectionOfStrings concatenation ! !

!Utf8Symbol class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:39:08'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	^ (Utf8String newFrom: aCollection) asSymbol

"
	Utf8Symbol newFrom: {$P. $e. $n}
	{$P. $e. $n} as: Utf8Symbol
"! !

!Utf8Symbol class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:40:05'!
readFrom: strm
	"
	Utf8Symbol readFrom: '#abc' readStream
	"

	strm peek = $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Scanner new scan: strm) advance  "Just do what the code scanner does"! !


!Symbol class methodsFor: 'instance creation' stamp: 'jmv 6/15/2022 15:38:49'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	^ (String newFrom: aCollection) asSymbol

"
	Symbol newFrom: {$P. $e. $n}
	{$P. $e. $n} as: Symbol
"! !

!methodRemoval: String class #fromString: stamp: 'jmv 6/15/2022 15:23:26'!
String class removeSelector: #fromString:!
!methodRemoval: String class #stringHash:initialHash: stamp: 'jmv 6/15/2022 15:30:49'!
String class removeSelector: #stringHash:initialHash:!
!methodRemoval: String class #readFrom: stamp: 'jmv 6/15/2022 15:27:38'!
String class removeSelector: #readFrom:!

!Utf8Symbol class reorganize!
('instance creation' fromCollectionOfStrings: newFrom: readFrom:)
('private' basicFromString:)
!

