'From Cuis 5.0 of 7 November 2016 [latest update: #3708] on 25 April 2019 at 8:41:55 am'!

!StrikeFont methodsFor: 'accessing' stamp: 'jmv 4/24/2019 20:27:07'!
lineSpacing
	"Answer the height of the receiver including any additional line gap."

	^self ascent + self descent! !


!AbstractFont methodsFor: 'accessing' stamp: 'jmv 4/24/2019 18:11:27'!
ascent

	self subclassResponsibility ! !

!AbstractFont methodsFor: 'accessing' stamp: 'jmv 4/25/2019 08:40:28'!
height
	"Answer the line spacing. Prefer calling #lineSpacing, that is more explicit.
	Besides, #height is false polymorphism, Fonts are not interchangeable with Form or Rectangle!!"

	^self lineSpacing! !


!StrikeFont methodsFor: 'accessing' stamp: 'jmv 4/24/2019 20:25:57'!
ascent
	"Answer the receiver's maximum extent of characters above the baseline. Positive."

	self isSuperscript ifTrue: [ ^ ascent * 1.9 ].
	self isSubscript ifTrue: [ ^ ascent * 0.75 ].
	^ascent! !

!StrikeFont methodsFor: 'accessing' stamp: 'jmv 4/24/2019 20:25:44'!
descent
	"Answer the receiver's maximum extent of characters below the baseline. Positive."

	| answer |
	answer _ descent.
	self isSubscript ifTrue: [ answer _ answer * 2 ].
	^ answer! !


!AbstractFont reorganize!
('accessing' ascent baseKern characterToGlyphMap derivativeFonts height pointSize xTable)
('measuring' approxWidthOfText: normalizedWidthOf: widthOf: widthOfString: widthOfString:from:to: widthOfStringOrText:)
('caching' releaseCachedState)
('derivatives - emphasis' bold boldItalic emphasized: italic struckThrough underlined)
('displaying' on:displayString:from:to:at:color:)
!

