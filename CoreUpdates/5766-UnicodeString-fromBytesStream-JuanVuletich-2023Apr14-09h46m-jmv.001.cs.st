'From Cuis 6.0 [latest update: #5764] on 14 April 2023 at 10:11:48 am'!

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 4/14/2023 10:10:44'!
fromBytesStream: aByteStream
	"Handle UTF-8 bytes, but convert invalid UTF-8 assuming it means ISO-8859-15.
	Argument can be, for example, a InflateStream."

	| utf8 codePointCount buffer readBytes leftover byteIndex n |
	buffer := ByteArray new: 10000.
	leftover := nil.
	codePointCount := 0.
	utf8 := ByteArray streamContents: [ :utf8Stream |
		[aByteStream atEnd] whileFalse: [ 
			readBytes := aByteStream nextInto: buffer.
			leftover ifNotNil: [
				readBytes := leftover, readBytes.
				leftover := nil ].
			byteIndex := 1.
			[ byteIndex <= (aByteStream atEnd
						ifFalse: [readBytes size-3] "To be sure we don't break a possible partially read multibyte code point."
						ifTrue: [readBytes size])   						"But do taks _all_ the bytes if they are the last ones."
			] whileTrue: [
				n := Character utf8BytesAndCodePointAt: byteIndex in: readBytes
					into: [ :byte1 :byte2 :byte3 :byte4 |
						utf8Stream nextPut: byte1.
						byte2 ifNotNil: [
							utf8Stream nextPut: byte2.
							byte3 ifNotNil: [
								utf8Stream nextPut: byte3.
								byte4 ifNotNil: [
									utf8Stream nextPut: byte4 ]]].
						codePointCount := codePointCount + 1]
					into: nil.
				byteIndex := byteIndex + n ].
			leftover := byteIndex > readBytes size ifFalse: [
				readBytes copyFrom: byteIndex to: readBytes size ].
		].
	].
	^UnicodeString fromUtf8Bytes: utf8 codePointCount: codePointCount.! !

