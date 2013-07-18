'From Cuis 4.1 of 12 December 2012 [latest update: #1572] on 22 January 2013 at 4:19:59 pm'!

!String methodsFor: 'converting' stamp: 'hjh 1/22/2013 15:26'!
utf8ToISO8859s15
	"Convert the given string from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"Skip any unicode chars not in ISO Latin 9 (ISO 8859-15)"
	"
	self assert: (ByteArray readHexFrom: '41C2A2E282AC') asString utf8ToISO8859s15 = 'A¢¤'
	"
	^String streamContents: [ :strm | | bytes |
		bytes _ self readStream.
		[ bytes atEnd ] whileFalse: [
			(Integer nextUnicodeCodePointFromUtf8: bytes) ifNotNil: 
			[ :codePoint | | iso8859m15code |
				 iso8859m15code := Character unicodeCodePoint: codePoint. 
                         iso8859m15code isNil ifFalse: [ strm nextPut: iso8859m15code]
                             ifTrue: [ strm nextPutAll: '&#', codePoint asString, ';']

				]]] ! !

