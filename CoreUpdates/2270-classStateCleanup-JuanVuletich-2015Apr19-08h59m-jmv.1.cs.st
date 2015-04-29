'From Cuis 4.2 of 25 July 2013 [latest update: #2257] on 19 April 2015 at 9:06:04.659912 am'!

!Base64MimeConverter class methodsFor: 'cached state access' stamp: 'jmv 4/19/2015 09:05'!
releaseClassCachedState
	FromCharTable _ ToCharTable _ nil! !

!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'jmv 4/19/2015 09:03'!
initializeTables

	FromCharTable _ Array new: 256.	"nils"
	ToCharTable _ Array new: 64.
	($A asciiValue to: $Z asciiValue) doWithIndex: [:val :ind | 
		FromCharTable at: val+1 put: ind-1.
		ToCharTable at: ind put: val asCharacter].
	($a asciiValue to: $z asciiValue) doWithIndex: [:val :ind | 
		FromCharTable at: val+1 put: ind+25.
		ToCharTable at: ind+26 put: val asCharacter].
	($0 asciiValue to: $9 asciiValue) doWithIndex: [:val :ind | 
		FromCharTable at: val+1 put: ind+25+26.
		ToCharTable at: ind+26+26 put: val asCharacter].
	FromCharTable at: $+ asciiValue + 1 put: 62.
	ToCharTable at: 63 put: $+.
	FromCharTable at: $/ asciiValue + 1 put: 63.
	ToCharTable at: 64 put: $/.
	! !


!Base64MimeConverter methodsFor: 'conversion' stamp: 'jmv 4/19/2015 09:04'!
mimeEncode
	"Convert from data to 6 bit characters."

	| phase1 phase2 raw nib lineLength |
	ToCharTable ifNil: [ self class initializeTables ].
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

!Base64MimeConverter methodsFor: 'conversion' stamp: 'jmv 4/19/2015 09:04'!
nextValue
	"The next six bits of data char from the mimeStream, or nil.  Skip all other chars"
	| raw num |
	FromCharTable ifNil: [ self class initializeTables ].
	[
		raw := mimeStream next.
		raw ifNil: [^ nil].	"end of stream"
		raw == $= ifTrue: [^ nil].
		num := FromCharTable at: raw asciiValue + 1.
		num ifNotNil: [^ num].
		"else ignore space, return, tab, ..."
	] repeat! !

!methodRemoval: Base64MimeConverter class #initialize!
Base64MimeConverter class removeSelector: #initialize!
Base64MimeConverter initialize!

!Base64MimeConverter class reorganize!
('cached state access' releaseClassCachedState)
('as yet unclassified' decodeInteger: encodeInteger: initializeTables mimeDecodeToBytes: mimeDecodeToChars: mimeEncode: mimeEncode:multiLine: mimeEncode:multiLine:atStart: mimeEncodeContinue:)
!

