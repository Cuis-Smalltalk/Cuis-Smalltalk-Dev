'From Cuis 6.0 [latest update: #5866] on 16 June 2023 at 3:18:27 pm'!

!SmalltalkEditor methodsFor: 'clipboard' stamp: 'jmv 6/16/2023 15:13:46'!
clipboardStringOrText
	| clipContents |
	clipContents := super clipboardStringOrText.
	self replaceUnicodeArrowsWithSmalltalkArrows ifTrue: [
		clipContents := clipContents copyReplaceAll: `(UnicodeCodePoint codePoint: 16r2190) asString` with: `(Character smalltalkLeftArrow ) asString`.
		clipContents := clipContents copyReplaceAll: `(UnicodeCodePoint codePoint: 16r2191) asString` with: `(Character smalltalkUpArrow ) asString`.
		].
	^clipContents.! !

