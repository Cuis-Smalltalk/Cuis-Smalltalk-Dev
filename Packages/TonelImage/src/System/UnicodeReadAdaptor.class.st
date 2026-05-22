Class {
	#name : 'UnicodeReadAdaptor',
	#superclass : 'UniStreamReadAdaptor',
	#category : 'System-Files',
	#package : 'System',
	#tag : 'Files'
}

{ #category : 'accessing' }
UnicodeReadAdaptor >> collectionSpecies [
	^UnicodeString
]

{ #category : 'reading' }
UnicodeReadAdaptor >> next [
	^stream nextCodePoint
]

{ #category : 'reading' }
UnicodeReadAdaptor >> next: n [
	"Read n Characters (i.e. n Unicode CodePoints), or until the end of the file.
	Answer them, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-1 if invalid UTF-8).
	Answer is an instance of UnicodeString."

	| utf8 codePointCount |
	codePointCount := 0.
	utf8 := ByteArray streamContents: [ :utf8Stream |
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
					codePointCount := codePointCount + 1]
				into: nil ]].
	^UnicodeString fromUtf8Bytes: utf8 codePointCount: codePointCount.
]

{ #category : 'reading' }
UnicodeReadAdaptor >> streamUpToEnd [
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-1 if invalid UTF-8).
	Leave stream at the end position."

	| utf8 codePointCount |
	codePointCount := 0.
	utf8 := ByteArray streamContents: [ :utf8Stream |
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
					codePointCount := codePointCount + 1]
				into: nil ]].
	^UnicodeString fromUtf8Bytes: utf8 codePointCount: codePointCount.
]
