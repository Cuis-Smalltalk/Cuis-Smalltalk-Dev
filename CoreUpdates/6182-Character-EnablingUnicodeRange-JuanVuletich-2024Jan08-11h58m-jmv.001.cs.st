'From Cuis6.3 [latest update: #6171] on 8 January 2024 at 12:12:17 pm'!

!String class methodsFor: 'instance creation' stamp: 'jmv 1/8/2024 12:08:14'!
fromUtf8Bytes: aByteArray trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-8 to the internal encoding: ISO Latin 9 (ISO 8859-15).
	Discard any code points that can not be represented."

	^String streamContents: [ :strm | | s byteIndex n |
		s := aByteArray size.
		byteIndex := 1.
		[  byteIndex <= s ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					codePoint < 256 ifTrue: [
						strm nextPut: (Character codePoint: codePoint) ]].
			byteIndex := byteIndex + n
		].
		(doTrimLastNullChar and: [ aByteArray last = 0 ]) ifTrue: [
			strm position: strm position - 1 ]].! !

