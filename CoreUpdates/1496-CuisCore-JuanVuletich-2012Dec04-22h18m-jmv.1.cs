'From Cuis 4.0 of 21 April 2012 [latest update: #1495] on 4 December 2012 at 10:20:41 pm'!
!classDefinition: #Base64MimeConverter category: #'Collections-Streams'!
MimeConverter subclass: #Base64MimeConverter
	instanceVariableNames: 'data multiLine '
	classVariableNames: 'FromCharTable ToCharTable '
	poolDictionaries: ''
	category: 'Collections-Streams'!

!Base64MimeConverter methodsFor: 'accessing' stamp: 'ar 4/15/2008 17:58'!
multiLine
	"Determines whether we allow multi-line encodings (the default) or force everything into a single line (for use with URLs etc. where the continuation marker and the line break cause problems)"
	^multiLine! !

!Base64MimeConverter methodsFor: 'accessing' stamp: 'ar 4/15/2008 17:58'!
multiLine: aBool
	"Determines whether we allow multi-line encodings (the default) or force everything into a single line (for use with URLs etc. where the continuation marker and the line break cause problems)"
	multiLine := aBool! !


!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'ar 3/9/2010 22:17'!
mimeEncode: aStream multiLine: aBool
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."

	^self mimeEncode: aStream multiLine: aBool atStart: true! !

!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'ar 3/9/2010 22:16'!
mimeEncode: aStream multiLine: aBool atStart: resetInput
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."

	| me |
	resetInput ifTrue:[aStream position: 0].
	me := self new dataStream: aStream.
	me multiLine: aBool.
	me mimeStream: (ReadWriteStream on: (String new: aStream size + 20 * 4 // 3)).
	me mimeEncode.
	me mimeStream position: 0.
	^ me mimeStream! !

!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'ar 3/9/2010 22:17'!
mimeEncodeContinue: aStream
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."
	^self mimeEncode: aStream multiLine: true atStart: false! !


!StringTest methodsFor: 'tests - converting' stamp: 'ul 10/30/2012 02:14'!
testBase64

	self 
		assert: 'SGVsbG8gV29ybGQ=' base64Decoded = 'Hello World';
		assert: 'Hello World' base64Encoded = 'SGVsbG8gV29ybGQ=';
		assert: (String new: 100 withAll: $x) base64Encoded  = 'eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eA=='! !


!Base64MimeConverter methodsFor: 'conversion' stamp: 'jmv 12/4/2012 22:15'!
mimeEncode
	"Convert from data to 6 bit characters."

	| phase1 phase2 raw nib lineLength |
	phase1 := phase2 := false.
	lineLength := 0.
	[dataStream atEnd] whileFalse: [
		(multiLine and:[lineLength >= 70]) ifTrue: [ mimeStream newLine.  lineLength := 0. ].
		data := raw := dataStream next asInteger.
		nib := (data bitAnd: 16rFC) bitShift: -2.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(raw := dataStream next) ifNil: [raw := 0. phase1 := true].
		data := ((data bitAnd: 3) bitShift: 8) + raw asInteger.
		nib := (data bitAnd: 16r3F0) bitShift: -4.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(raw := dataStream next) ifNil: [raw := 0. phase2 := true].
		data := ((data bitAnd: 16rF) bitShift: 8) + (raw asInteger).
		nib := (data bitAnd: 16rFC0) bitShift: -6.
		mimeStream nextPut: (ToCharTable at: nib+1).
		nib := (data bitAnd: 16r3F).
		mimeStream nextPut: (ToCharTable at: nib+1).

		lineLength := lineLength + 4.].
	phase1 ifTrue: [mimeStream skip: -2; nextPut: $=; nextPut: $=.
			^ mimeStream].
	phase2 ifTrue: [mimeStream skip: -1; nextPut: $=.
			^ mimeStream].

! !

!Base64MimeConverter methodsFor: 'conversion' stamp: 'ul 6/17/2011 12:36'!
nextValue
	"The next six bits of data char from the mimeStream, or nil.  Skip all other chars"
	| raw num |
	[raw := mimeStream next.
	raw ifNil: [^ nil].	"end of stream"
	raw == $= ifTrue: [^ nil].
	num := FromCharTable at: raw asciiValue + 1.
	num ifNotNil: [^ num].
	"else ignore space, return, tab, ..."
	] repeat! !


!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'ar 3/9/2010 22:17'!
mimeEncode: aStream
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."
	^self mimeEncode: aStream multiLine: true atStart: true! !


!String methodsFor: 'converting' stamp: 'ul 10/30/2012 02:09'!
base64Encoded
	"Encode the receiver as base64"
	"'Hello World' base64Encoded"

	^(Base64MimeConverter
		mimeEncode: (ReadStream on: self)
		multiLine: false) contents! !

!classDefinition: #Base64MimeConverter category: #'Collections-Streams'!
MimeConverter subclass: #Base64MimeConverter
	instanceVariableNames: 'data multiLine'
	classVariableNames: 'FromCharTable ToCharTable'
	poolDictionaries: ''
	category: 'Collections-Streams'!
