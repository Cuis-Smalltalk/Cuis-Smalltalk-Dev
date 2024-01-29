'From Cuis6.3 [latest update: #6168] on 6 January 2024 at 3:54:22 pm'!

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 1/4/2024 22:06:45'!
findFirstBasicAt: aBlock startingAt: firstIndex
	"Return the index of my first element for which aBlock evaluates as true.
	Start at firstIndex.
	Use #basicAt: for accessing elements, not #at:
	This means, for String, evaluate aBlock on each byte."

	| s index |
	s := self size.
	index := firstIndex.
	[
		index <= s ] whileTrue: [
			(aBlock value: (self basicAt: index)) ifTrue: [^index ].
			index := index + 1.
			].
	^ 0! !


!String methodsFor: 'accessing' stamp: 'jmv 1/6/2024 15:26:41'!
indexOfAnyIn: aByteSetByteArray startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start.
	aByteSetByteArray is a ByteArray of size 256 with a 1 for byte values considered to be included."

	| answer |
	answer := ByteArray findFirstInBytes: self inSet: aByteSetByteArray startingAt: start.

	^answer = 0 
		ifTrue: [ aBlock value ]
		ifFalse: [ answer]! !


!UnicodeString methodsFor: 'accessing' stamp: 'jmv 1/6/2024 15:31:57'!
indexOfAnyIn: aByteSetByteArray startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start."

	"Fast lane."
	| index |
	self isAscii ifTrue: [
		^ bytes indexOfAnyIn: aByteSetByteArray startingAt: start ifAbsent: aBlock ].

	"If only ASCII bytes are included in the map..."
	(ByteArray indexOfByte: 1 inBytes: aByteSetByteArray startingAt: 128) = 0 ifTrue: [
		|startBI foundBI|
		startBI := self byteIndexAt: start.
		foundBI := bytes indexOfAnyIn: aByteSetByteArray startingAt: startBI ifAbsent: aBlock.
		^ self codePointIndexAt: foundBI ].

	index := start.
	self do: [ :codePoint |
		codePoint iso8859s15Code ifNotNil: [ :ii |
			(aByteSetByteArray at: ii + 1) > 0 ifTrue: [ ^index ]].
		index := index + 1].

	^aBlock value! !


!ByteArray methodsFor: 'accessing' stamp: 'jmv 1/6/2024 15:32:59'!
indexOfAnyIn: aByteSetByteArray startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start.
	aByteSetByteArray is a ByteArray of size 256 with a 1 for byte values considered to be included."

	| answer |
	answer := ByteArray findFirstInBytes: self inSet: aByteSetByteArray startingAt: start.

	^answer = 0 
		ifTrue: [ aBlock value ]
		ifFalse: [ answer]! !


!String methodsFor: 'converting' stamp: 'jmv 1/6/2024 15:37:39'!
unescapePercents
	"change each %XY substring to the character with ASCII value XY in hex.
	This is the opposite of #percentEscapingNonAscii: and #percentEscapingNonAsciiAndThoseIn:bytes:
	(String percentEscapingNonAscii: '⊙' bytes) unescapePercents
	'%E2%8A%99' unescapePercents
	"

	| myBytes answerClass answerBytes c decodedByte pos oldPos specialCharsByteSet |
	myBytes := self asByteArray.
	specialCharsByteSet := `ByteArray new: 256 :: at: $+ asciiValue + 1 put: 1; at: $% asciiValue + 1 put: 1; yourself`.
	answerClass := String.
	answerBytes := ByteArray streamContents: [ :byteStream |
		oldPos := 1.

		[pos := myBytes indexOfAnyIn: specialCharsByteSet startingAt: oldPos ifAbsent: [ 0 ]. pos > 0 ]
		whileTrue: [
			byteStream nextPutAll: (myBytes copyFrom: oldPos to: pos - 1).
			c := self at: pos.
			c = $+ ifTrue: [byteStream nextPut: `$  asciiValue` ] ifFalse: [
				(c = $% and: [pos + 2 <= self size]) ifTrue: [
					decodedByte := (self at: pos+1) asUppercase digitValue * 16 +
						(self at: pos+2) asUppercase digitValue.
					pos := pos + 2.
					decodedByte > 255 ifTrue: [^self].	"not really an escaped string"
					decodedByte > 127 ifTrue: [ answerClass := UnicodeString ].
					byteStream nextPut: decodedByte ]
				ifFalse: [byteStream nextPut: c numericValue ]].
			oldPos := pos+1].
		byteStream nextPutAll: (myBytes copyFrom: oldPos to: self size).
	].
	^ answerClass fromUtf8Bytes: answerBytes! !


!String class methodsFor: 'services' stamp: 'jmv 1/6/2024 15:50:03'!
isAscii: aStringOrByteArray
	"Return true if all bytes in argument are less than 128.
	This means it is a pure 7-bit ASCII.
	Valid both for instances of String (possibly with accented latin letters or other Character)
	and for utf-8 encoded ByteArrays.
	In these cases, the bytes represent the same ascii string both as String and as Utf8.
	See also #nextUtf8BytesAndCodePointInto:into:, #ascii and #nonAscii"

	| nonAsciiBytesSetBA |
	nonAsciiBytesSetBA := `(ByteArray new: 128), ((ByteArray new: 128) + 1)`.
	^ (ByteArray
		findFirstInBytes: aStringOrByteArray
		inSet: nonAsciiBytesSetBA
		startingAt: 1)
			= 0.! !

!String class methodsFor: 'services' stamp: 'jmv 1/6/2024 15:39:20'!
string: aString lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| charSetLineEnders start end endWithoutDelimiters |
	charSetLineEnders := `ByteArray new: 256 :: at: Character cr asciiValue + 1 put: 1; at: Character lf asciiValue + 1 put: 1; yourself`.
	start := 1.
	[
		end := aString indexOfAnyIn: charSetLineEnders startingAt: start ifAbsent: [ 0 ].
		end = 0
			ifTrue: [
				"Last line was found. Evaluate and exit.
				Note. If last char in receiver is a line separator, there's an extra empty line"
				endWithoutDelimiters := end := aString size.
				aBlock value: start value: endWithoutDelimiters value: end.
				^aString ].

		"Account for CrLf sequence"
		endWithoutDelimiters := end - 1.
		(end < aString size
			and: [(aString basicAt: end + 1) = `Character lf asciiValue`
			and: [(aString basicAt: end) = `Character cr asciiValue` ]])
				ifTrue: [ end := end + 1].

		aBlock value: start value: endWithoutDelimiters value: end.
		start := end + 1 ] repeat! !

!String class methodsFor: 'services' stamp: 'jmv 1/6/2024 15:40:11'!
string: aByteStringOrByteArray withLineEndings: newLineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"
	| cr lf charSetLineEnders inPos outPos outString lineEndPos newOutPos lineEndingSize |
	cr := Character cr.
	lf := Character lf.
	charSetLineEnders := `ByteArray new: 256 :: at: Character cr asciiValue + 1 put: 1; at: Character lf asciiValue + 1 put: 1; yourself`.
	(aByteStringOrByteArray is: #ByteArray)
		ifTrue: [
			cr := cr asciiValue.
			lf := lf asciiValue ].

	inPos := 1.
	outPos := 1.
	lineEndingSize := newLineEndingString size.
	"This could be extremely inefficient if lineEndingString size > 1"
	outString := aByteStringOrByteArray class new: aByteStringOrByteArray size * lineEndingSize.

	[
		lineEndPos := aByteStringOrByteArray indexOfAnyIn: charSetLineEnders startingAt: inPos ifAbsent: [0].
		lineEndPos ~= 0 ] whileTrue: [
			newOutPos := outPos + (lineEndPos - inPos).
			outString replaceFrom: outPos to: newOutPos - 1 with: aByteStringOrByteArray startingAt: inPos.
			1 to: lineEndingSize do: [ :i |
				outString at: newOutPos put: (newLineEndingString at: i).
				newOutPos := newOutPos + 1 ].
			outPos := newOutPos.
			((aByteStringOrByteArray at: lineEndPos) = cr and: [
					lineEndPos < aByteStringOrByteArray size and: [ (aByteStringOrByteArray at: lineEndPos+1) = lf ] ]) ifTrue: [
				"CRLF ending"
				inPos := lineEndPos + 2 ]
			ifFalse: [ 
				"CR or LF ending"
				inPos := lineEndPos + 1 ]].

	"no more line endings.  copy the rest"
	newOutPos := outPos + (aByteStringOrByteArray size - inPos).
	outString replaceFrom: outPos to: newOutPos with: aByteStringOrByteArray startingAt: inPos.

	^outString copyFrom: 1 to: newOutPos! !

!String class methodsFor: 'services' stamp: 'jmv 1/6/2024 15:47:19'!
substringsIn: aByteStringOrByteArray
	"Answer an array of the substrings that compose the argument.
	If argument is an UTF-8 encoded ByteArray, answer is an array of same."

	| end beginning charSetNonSeparators charSetSeparators |
	charSetSeparators := `ByteArray new: 256 ::
		at: Character space asciiValue + 1 put: 1;
		at: Character tab asciiValue + 1 put: 1;
		at: Character lf asciiValue + 1 put: 1;
		at: Character cr asciiValue + 1 put: 1;
		at: Character newPage asciiValue + 1 put: 1;
		yourself`.
	charSetNonSeparators := `ByteArray new: 256 :: += 1;
		at: Character space asciiValue + 1 put: 0;
		at: Character tab asciiValue + 1 put: 0;
		at: Character lf asciiValue + 1 put: 0;
		at: Character cr asciiValue + 1 put: 0;
		at: Character newPage asciiValue + 1 put: 0;
		yourself`.
	end := 0.
	^Array streamContents: [ :result |
		"find one substring each time through this loop"
		[ 
			"find the beginning of the next substring"
			beginning := aByteStringOrByteArray indexOfAnyIn: charSetNonSeparators startingAt: end+1 ifAbsent: nil.
			beginning notNil ] 
		whileTrue: [
			"find the end"
			end := aByteStringOrByteArray indexOfAnyIn: charSetSeparators startingAt: beginning ifAbsent: [ aByteStringOrByteArray size + 1 ].
			end := end - 1.
			result nextPut: (aByteStringOrByteArray copyFrom: beginning to: end).
		]
	].! !


!ByteArray class methodsFor: 'services' stamp: 'jmv 1/6/2024 15:50:15'!
findFirstInBytes: aByteArrayOrByteString inSet: aByteSetByteArray startingAt: start
	| i stringSize |
	<primitive: 'primitiveFindFirstInString' module: 'MiscPrimitivePlugin'>

	aByteSetByteArray size ~= 256 ifTrue: [ ^0 ].

	i := start.
	stringSize := aByteArrayOrByteString size.
	[ i <= stringSize and: [ (aByteSetByteArray at: (aByteArrayOrByteString basicAt: i) +1) = 0 ] ] whileTrue: [ 
		i := i + 1 ].

	i > stringSize ifTrue: [ ^0 ].
	^i! !

!methodRemoval: ByteArray #indexOfAnyOf:startingAt:ifAbsent: stamp: 'jmv 1/6/2024 15:47:43'!
ByteArray removeSelector: #indexOfAnyOf:startingAt:ifAbsent:!
!methodRemoval: UnicodeString #indexOfAnyOf:startingAt:ifAbsent: stamp: 'jmv 1/6/2024 15:47:34'!
UnicodeString removeSelector: #indexOfAnyOf:startingAt:ifAbsent:!
!methodRemoval: String class #basicCharSetNonSeparators stamp: 'jmv 1/6/2024 15:48:22'!
String class removeSelector: #basicCharSetNonSeparators!
!methodRemoval: String class #charSetSeparators stamp: 'jmv 1/6/2024 15:48:20'!
String class removeSelector: #charSetSeparators!
!methodRemoval: String class #charSetLineEnders stamp: 'jmv 1/6/2024 15:40:24'!
String class removeSelector: #charSetLineEnders!
!methodRemoval: String class #charSetNonSeparators stamp: 'jmv 1/6/2024 15:48:17'!
String class removeSelector: #charSetNonSeparators!
!methodRemoval: String #indexOfAnyOf:startingAt:ifAbsent: stamp: 'jmv 1/6/2024 15:47:42'!
String removeSelector: #indexOfAnyOf:startingAt:ifAbsent:!
!methodRemoval: Collection #asCharacterSet stamp: 'jmv 1/6/2024 15:51:03'!
Collection removeSelector: #asCharacterSet!
!classRemoval: #CharacterSet stamp: 'jmv 1/6/2024 15:52:27'!
Smalltalk removeClassNamed: #CharacterSet!

