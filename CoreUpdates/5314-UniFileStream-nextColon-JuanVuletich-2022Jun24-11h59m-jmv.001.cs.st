'From Cuis 6.0 [latest update: #5313] on 24 June 2022 at 12:02:06 pm'!

!UniFileStream methodsFor: 'private' stamp: 'jmv 6/24/2022 11:24:06'!
nextBytes: n
	"Read n bytes, or until the end of the file."

	| bytes s |
	s _ n min: self size.
	bytes _ ByteArray new: s.
	^ self nextBytes: s into: bytes startingAt: 1.! !


!UniStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 6/24/2022 11:21:14'!
next: n
	"Read n elements, or until the end of the file."

	self subclassResponsibility! !


!ByteStringReadAdaptor methodsFor: 'reading' stamp: 'jmv 6/24/2022 11:32:23'!
next: n
	"Read n bytes, or until the end of the file.
	Answer them as seen from our perspective:
	Stream bytes as Cuis / ISO 8859-15 Characters.
	Disregard the fact that stream contents are most likely UTF-8.
	This is valid only if contents are ASCII, or Cuis / ISO-8859-15 byte Characters.
	Answer is a String (i.e. a byte oriented ISO 8859-15 String)"

	^(stream nextBytes: n) asString! !


!BytesReadAdaptor methodsFor: 'reading' stamp: 'jmv 6/24/2022 11:27:57'!
next: n
	"Read n bytes, or until the end of the file.
	Answer them as a ByteArray."

	^stream nextBytes: n! !


!Utf32ReadAdaptor methodsFor: 'reading' stamp: 'jmv 6/24/2022 11:33:16'!
next: n
	"Read n UnicodeCodePoints, or until the end of the file.
	Answer them, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-15 if invalid UTF-8).
	Answer is an instance of Utf32String."

	| utf32 codePointCount |
	codePointCount _ 0.
	utf32 _ WordArray streamContents: [ :utf32Stream |
		[ codePointCount = n or: [stream atEnd] ] whileFalse: [
			stream nextUtf8BytesAndCodePointInto: nil into: [ :codePoint |
				utf32Stream nextPut: codePoint.
				codePointCount _ codePointCount + 1 ]]].
	^Utf32String withWords: utf32! !


!Utf8ReadAdaptor methodsFor: 'reading' stamp: 'jmv 6/24/2022 11:56:07'!
next: n
	"Read n UnicodeCodePoints, or until the end of the file.
	Answer them, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-15 if invalid UTF-8).
	Answer is an instance of Utf8String."

	| utf8 codePointCount |
	codePointCount _ 0.
	utf8 _ ByteArray streamContents: [ :utf8Stream |
		[ codePointCount = n or: [stream atEnd] ] whileFalse: [
			stream
				nextUtf8BytesAndCodePointInto: [ :byte1 :byte2 :byte3 :byte4 |
					utf8Stream nextPut: byte1.
					byte2 ifNotNil: [
						utf8Stream nextPut: byte2.
						byte3 ifNotNil: [
							utf8Stream nextPut: byte3.
							byte4 ifNotNil: [
								utf8Stream nextPut: byte4 ]]].
					codePointCount _ codePointCount + 1]
				into: nil ]].
	^Utf8String fromUtf8Bytes: utf8 codePointCount: codePointCount.! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 11:35:30'!
next: n
	"Answer the next n charaters (or bytes) from this file, or nil if at the end of the file.
	Answer will be
	- an Utf8String, Utf32String or String or size n (or until the end of the file if shorter)
	- a ByteArray of size n (or until the end of the file if shorter)"

	^readAdaptor next: n! !


!Utf8ReadAdaptor methodsFor: 'reading' stamp: 'jmv 6/24/2022 11:56:17'!
streamUpToEnd
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-15 if invalid UTF-8).
	Leave stream at the end position."

	| utf8 codePointCount |
	codePointCount _ 0.
	utf8 _ ByteArray streamContents: [ :utf8Stream |
		[ stream atEnd ] whileFalse: [
			stream
				nextUtf8BytesAndCodePointInto: [ :byte1 :byte2 :byte3 :byte4 |
					utf8Stream nextPut: byte1.
					byte2 ifNotNil: [
						utf8Stream nextPut: byte2.
						byte3 ifNotNil: [
							utf8Stream nextPut: byte3.
							byte4 ifNotNil: [
								utf8Stream nextPut: byte4]]].
					codePointCount _ codePointCount + 1]
				into: nil ]].
	^Utf8String fromUtf8Bytes: utf8 codePointCount: codePointCount.! !

