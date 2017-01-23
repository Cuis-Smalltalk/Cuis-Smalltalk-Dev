'From Cuis 5.0 of 7 November 2016 [latest update: #3040] on 22 January 2017 at 9:33:48 pm'!

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 1/22/2017 21:17:32'!
privateExtent: newExtent

	^ (super privateExtent: newExtent)
		ifTrue: [
			self buildMagnifiedBackgroundImage.
			worldState ifNotNil: [
				worldState clearCanvas ]];
		yourself! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 1/22/2017 21:25:10'!
drawOn: aCanvas

	| titleColor roundCorners |

	titleColor _ self widgetsColor.
	self isTopWindow
		ifTrue: [ titleColor _ titleColor lighter ].

	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			"Round corners. Optional title gradient."
			self drawRoundedFrameOn: aCanvas color: titleColor ]
		ifFalse: [
			"No round corners. No title gradient."
			self drawClassicFrameOn: aCanvas color: titleColor ].
	Theme current minimalWindows
		ifFalse: [
			labelString ifNotNil: [self drawLabelOn: aCanvas]]! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 1/22/2017 21:31:40'!
popUpInWorld: aWorld
	"Present this menu under control of the given hand."
	"Needed if not the real world but an inner PasteUpMorph"
	| positionInWorld |
	positionInWorld _ aWorld internalizeFromWorld: aWorld activeHand morphPosition.
	^self
		popUpAt: positionInWorld
		forHand: aWorld activeHand
		in: aWorld
! !

