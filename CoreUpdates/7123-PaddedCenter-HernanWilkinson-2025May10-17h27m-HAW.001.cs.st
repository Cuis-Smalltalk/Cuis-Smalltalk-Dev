'From Cuis7.3 [latest update: #7120] on 10 May 2025 at 6:00:11 pm'!

!CharacterSequence methodsFor: 'copying' stamp: 'HAW 5/10/2025 17:59:25'!
padded: leftRightOrCenter to: aLength with: aChar

	"
	'hi' padded: #left to: 5 with: $. -->  '...hi' 
	'hi' padded: #right to: 5 with: $. --> 'hi...' 
	'hi' padded: #center to: 5 with: $. --> '.hi..' 
	"
	| toPad |
	
	toPad := aLength - self size max: 0.
	
	leftRightOrCenter = #left ifTrue: [ ^ (self species new: toPad withAll: aChar) , self].
	leftRightOrCenter = #right ifTrue: [ ^ self , (self species new: toPad withAll: aChar)].
	leftRightOrCenter = #center ifTrue: [ | paddedLeft | 
		paddedLeft := self padded: #left to: toPad//2 + self size with: aChar.
		^ paddedLeft padded: #right to: aLength with: aChar].! !

