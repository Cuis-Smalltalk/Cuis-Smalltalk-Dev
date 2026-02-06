'From Cuis7.7 [latest update: #7830] on 6 February 2026 at 4:44:34 pm'!

!AbstractFont methodsFor: 'measuring' stamp: 'jmv 6/Feb/2026 15:51:32'!
boundsLeftOf: aCharacter
	"Return the left bounds of the given character"
	^0! !


!TrueTypeFont methodsFor: 'accessing' stamp: 'jmv 6/Feb/2026 16:03:58'!
boundsLeftOf: aCharacter
	"Return the left bounds of the given character"

	^ (ttFontDescription boundsLeftOf: aCharacter) * pointSize! !


!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 6/Feb/2026 15:47:46'!
boundsLeftOf: aCharacter

	Character
		evaluateOnce: [ :byte1 :byte2OrNil :byte3OrNil :byte4OrNil |
			^self boundsLeftOfUtf8Byte1: byte1 b2: byte2OrNil b3: byte3OrNil b4: byte4OrNil ]
		withUtf8BytesOf: aCharacter codePoint.! !

!TTFontDescription methodsFor: 'glyph properties' stamp: 'jmv 6/Feb/2026 15:42:16'!
boundsLeftOfUtf8Byte1: byte1 b2: byte2OrNil b3: byte3OrNil b4: byte4OrNil

	| i bouldsLeft |
	
	i := self contourDataIndexForUtf8Byte1: byte1 byte2: byte2OrNil byte3: byte3OrNil byte4: byte4OrNil.
	
	"If glyph not found, take first glyph 'theUnknownGlyph'"
	i = 0 ifTrue: [ i := 1 ].
	bouldsLeft := contourDataForUtf8 at: i+1.
	^ bouldsLeft / letterMTopSideBearing.! !


!AbstractFont methodsFor: 'measuring' stamp: 'jmv 6/Feb/2026 16:43:31'!
widthOfString: aString from: startIndex to: stopIndex
	"Measure the length of the given string between start and stop index"
	| character resultX |
	aString isEmpty ifTrue: [^0].
	resultX := 0.
	(startIndex = 1 and: [ stopIndex > 0]) ifTrue: [
		resultX := (self boundsLeftOf: aString first) negated ].
	startIndex to: stopIndex do: [ :i |
		character := aString at: i.
		resultX := resultX + (self widthOf: character) ].
	^resultX! !

