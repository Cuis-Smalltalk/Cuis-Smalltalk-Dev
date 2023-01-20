'From Cuis 6.0 [latest update: #5641] on 20 January 2023 at 2:19:49 pm'!

!UniFileStream methodsFor: 'private' stamp: 'jmv 1/20/2023 14:15:02'!
nextCodePoint
	| answer |
	answer _ nil.
	self nextUtf8BytesAndCodePointInto: nil into: [ :codePoint |
		answer _ UnicodeCodePoint codePoint: codePoint ].

	"If we'd skipped BOM, try again. See comment at
	utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into:
	Feel free to blame Microsoft for this nonsense."
	answer ifNil: [
		(self position = 3 and: [self atEnd not]) ifTrue: [
			answer _ self nextCodePoint ]].

	^answer! !

