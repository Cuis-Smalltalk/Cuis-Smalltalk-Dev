'From Cuis 6.0 [latest update: #5136] on 5 May 2022 at 12:17:29 pm'!

!String methodsFor: 'character scanner support' stamp: 'jmv 5/5/2022 11:14:55'!
scanCharactersFrom: startIndex to: stopIndex with: aCharacterScanner rightX: rightX stopConditions: stops
	"Double dispatch."

	^ aCharacterScanner scanStringCharsFrom: startIndex to: stopIndex in: self rightX: rightX stopConditions: stops! !


!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 5/5/2022 11:19:09'!
scanStringCharsFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value.
	Determine width of each character.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	| nextDestX |
	lastIndex _ startIndex.
	sourceString from: startIndex to: stopIndex do: [ :char |
		"stops are only defined for the first 128 characters
		(entries 128 to 255 are always nil)."
		(stops at: char numericValue + 1) ifNotNil: [ :stop | ^stop ].
		nextDestX _ destX + (font widthOf: char).
		nextDestX > rightX ifTrue: [
			^stops at: CharacterScanner crossedXCode ].
			destX _ nextDestX.
			lastIndex _ lastIndex + 1 ].
	lastIndex _ stopIndex.
	^ stops at: CharacterScanner endOfRunCode! !


!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 5/5/2022 11:15:21'!
scanCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value.
	Determine width of each character.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	^sourceString scanCharactersFrom: startIndex to: stopIndex with: self rightX: rightX stopConditions: stops! !

