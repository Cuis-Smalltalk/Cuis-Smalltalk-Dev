'From Cuis 5.0 [latest update: #4286] on 23 July 2020 at 3:43:50 pm'!

!TextDoIt class methodsFor: 'as yet unclassified' stamp: 'jmv 7/23/2020 15:42:42'!
evalString: str
	"
	('Some text. ',
	(Text string: '<click here>' attribute: (TextDoIt evalString: '123456 print')), 
	' more regular text') edit
	"
	^ self new evalString: str! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 7/23/2020 15:42:02'!
drawOn: aCanvas
	Transcript
		showOnDisplay: true;
		displayOn: form in: self morphLocalBounds.
	aCanvas image: form at: self morphTopLeft.
	self displayBounds ifNotNil: [ :r |
		Transcript bounds:r ].
	Transcript
		showOnDisplay: doImmediateUpdates.
	self updateWorkspace! !


!TextComposition methodsFor: 'editing' stamp: 'jmv 7/23/2020 15:43:30'!
clickAt: clickPoint
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	| startBlock action target range boxes box t |
	action _ false.
	startBlock _ self characterBlockAtPoint: clickPoint.
	t _ model actualContents.
	(t attributesAt: startBlock stringIndex) do: [ :att | 
		att mayActOnClick ifTrue: [
				(target _ model) ifNil: [ target _ editor morph].
				range _ t rangeOf: att startingAt: startBlock stringIndex.
				boxes _ self selectionRectsFrom: (self characterBlockForIndex: range first) 
							to: (self characterBlockForIndex: range last+1).
				box _ boxes detect: [ :each | each containsPoint: clickPoint] ifNone: nil.
				box ifNotNil: [
					box _ editor morph displayBounds.
					editor morph allOwnersDo: [ :m | box _ box intersect: (m displayBounds) ].
					Utilities
						awaitMouseUpIn: box
						repeating: nil
						ifSucceed: [(att actOnClickFor: target in: self at: clickPoint editor: editor) ifTrue: [action _ true]].
				]]].
	^ action! !

