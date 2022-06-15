'From Cuis 6.0 [latest update: #5252] on 8 June 2022 at 4:15:34 pm'!

!CharacterSequence class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 15:45:45'!
new: size withAll: element 
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^ self streamContents: [ :strm |
		size timesRepeat: [ strm nextPut: element ]].! !


!String class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new: size withAll: value
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^(self new: size) atAllPut: value! !

!String class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 16:14:12'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."
	| newArray index |
	newArray _ self new: aCollection size.
	index _ 0.
	aCollection do: [ :each | newArray at: (index _ index + 1) put: each].
	^ newArray

"
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Utf8String
	{$c. $h. $r} as: Utf32String
"! !

!String class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
with: aCharacter 
	"Answer a new instance of me, containing only aCharacter.
	String with: $a
	String with: $á
	String with: Character euro
	"

	| newCollection |
	newCollection _ self new: 1.
	newCollection at: 1 put: aCharacter.
	^newCollection! !

!String class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
with: firstChar with: secondChar
	"Answer a new instance of me, containing firstChar and secondChar.
	String with: $a
	String with: $á with: Character euro
	"

	| newCollection |
	newCollection _ self new: 2.
	newCollection at: 1 put: firstChar.
	newCollection at: 2 put: secondChar.
	^newCollection! !

!String class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
with: firstObject with: secondObject with: thirdObject 
	"Answer a new instance of me, containing only these three objects."

	| newCollection |
	newCollection _ self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^newCollection! !

!String class methodsFor: 'instance creation' stamp: 'jmv 3/7/2015 23:10'!
withAll: aSequenceableCollection
	"Create a new collection containing all the elements from aSequenceableCollection."

	^ (self new: aSequenceableCollection size)
		replaceFrom: 1
		to: aSequenceableCollection size
		with: aSequenceableCollection! !

!String class methodsFor: 'stream creation' stamp: 'jmv 6/8/2022 16:15:00'!
writeStream
	^ WriteStream on: (self new: 100)! !


!Utf32String methodsFor: 'initialization' stamp: 'jmv 6/8/2022 15:39:27'!
initialize
	codePoints _ `WordArray new`! !


!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 15:37:14'!
new: sizeRequested
	^self withWords: (WordArray new: sizeRequested)! !

!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 15:47:59'!
new: size withAll: codePoint
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^ self withWords: (WordArray new: size withAll: codePoint codePoint)! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 15:48:16'!
new: size withAll: codePoint 
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^ self streamContents: [ :strm |
		size timesRepeat: [ strm nextPut: codePoint ]].! !


!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 16:14:16'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."
	| newArray index |
	newArray _ WordArray new: aCollection size.
	index _ 0.
	aCollection do: [ :each | newArray at: (index _ index + 1) put: each codePoint].
	^ self withWords: newArray

"
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Utf8String
	{$c. $h. $r} as: Utf32String
"! !

!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 16:04:56'!
with: aCharacter with: otherCharacter
	"Answer a new instance of me, containing utf words for aCharacter and otherCharacter.
	Utf32String with: $a
	Utf32String with: $á with: Character euro
	"
	^self withWords: (WordArray with: aCharacter codePoint with: otherCharacter codePoint).! !

!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 15:38:20'!
withWords: aWordArray
	^self basicNew words: aWordArray ! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 16:14:20'!
newFrom: aSimilarObject
	^Utf8String streamContents: [ :strm |
		aSimilarObject do: [ :codePoint |
			strm nextPut: codePoint ]]

"
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Utf8String
	{$c. $h. $r} as: Utf32String
"! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 16:05:02'!
with: aCharacter with: otherCharacter
	"Answer a new instance of me, containing utf bytes for aCharacter and otherCharacter.
	Utf8String with: $a
	Utf8String with: $á with: Character euro
	"
	^self withBytes: (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint),
		(Character utf8BytesOfUnicodeCodePoint: otherCharacter codePoint).! !


!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 16:14:27'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."
	| newArray index |
	newArray _ self new: aCollection size.
	index _ 0.
	aCollection do: [ :each | newArray at: (index _ index + 1) put: each].
	^ newArray

"
	Array newFrom: {1. 2. 3}
	{1. 2. 3} as: Array
	{1. 2. 3} as: ByteArray
	{$c. $h. $r} as: String
"! !

!methodRemoval: CharacterSequence class #with:with:with: stamp: 'jmv 6/8/2022 16:06:43'!
CharacterSequence class removeSelector: #with:with:with:!
!methodRemoval: CharacterSequence class #with:with: stamp: 'jmv 6/8/2022 16:05:48'!
CharacterSequence class removeSelector: #with:with:!
!methodRemoval: CharacterSequence class #withAll: stamp: 'jmv 6/8/2022 16:12:38'!
CharacterSequence class removeSelector: #withAll:!
!methodRemoval: CharacterSequence class #writeStream stamp: 'jmv 6/8/2022 16:15:14'!
CharacterSequence class removeSelector: #writeStream!
!methodRemoval: CharacterSequence class #newFrom: stamp: 'jmv 6/8/2022 16:00:32'!
CharacterSequence class removeSelector: #newFrom:!
!methodRemoval: CharacterSequence class #with: stamp: 'jmv 6/8/2022 16:03:35'!
CharacterSequence class removeSelector: #with:!

!CharacterSequence class reorganize!
('instance creation' new new:withAll: numElements)
!


!String class reorganize!
('instance creation' addUnicodeCodePoint:to:hex: crString crlfString fromString: fromUtf32Words: fromUtf32Words:hex:trimLastNull: fromUtf8Bytes: fromUtf8Bytes:hex:trimLastNull: lfString new:withAll: newFrom: newLineString readFrom: tab with: with:with: with:with:with: withAll:)
('initialization' initialize)
('primitives' findFirstInString:inSet:startingAt: indexOfByte:inString:startingAt: stringHash:initialHash: translate:from:to:table:)
('character collation' does:caseInsensitiveCollateBefore: does:collateAfter: does:collateBefore:)
('fileman-constants' pathSeparators)
('system' symbolClassToUse)
('stream creation' writeStream)
!


!Utf32String reorganize!
('accessing' at: isLastLineSeparator isLastSeparator size words words:)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with:)
('comparing' = hash)
('converting' asUtf32String asUtf32Words asUtf8Bytes)
('testing' is: isEmpty isUtf32String)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('as yet unclassified' doesNotUnderstand:)
('initialization' initialize)
!

