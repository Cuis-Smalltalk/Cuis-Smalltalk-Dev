'From Cuis 4.2 of 25 July 2013 [latest update: #2938] on 18 September 2016 at 10:24:02 pm'!

!Base64MimeConverter methodsFor: 'conversion' stamp: 'jmv 9/18/2016 22:22:09'!
nextByte
	| datum |
	datum _ dataStream next.
	datum class == Character ifTrue: [
		datum _ datum numericValue ].
	^datum! !


!Base64MimeConverter methodsFor: 'conversion' stamp: 'jmv 9/18/2016 22:22:59'!
mimeEncode
	"Convert from data to 6 bit characters."

	| phase1 phase2 raw nib lineLength |
	ToCharTable ifNil: [ self class initializeTables ].
	phase1 := phase2 := false.
	lineLength := 0.
	[dataStream atEnd] whileFalse: [
		(multiLine and:[lineLength >= 70]) ifTrue: [ mimeStream newLine.  lineLength := 0. ].
		data := raw := self nextByte.
		nib := (data bitAnd: 16rFC) bitShift: -2.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(raw := self nextByte) ifNil: [raw := 0. phase1 := true].
		data := ((data bitAnd: 3) bitShift: 8) + raw asInteger.
		nib := (data bitAnd: 16r3F0) bitShift: -4.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(raw := self nextByte) ifNil: [raw := 0. phase2 := true].
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

