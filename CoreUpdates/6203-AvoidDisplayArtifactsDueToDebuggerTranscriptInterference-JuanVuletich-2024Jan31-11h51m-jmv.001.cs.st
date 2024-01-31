'From Cuis6.3 [latest update: #6202] on 31 January 2024 at 11:59:13 am'!

!Transcript class methodsFor: 'displaying' stamp: 'jmv 1/31/2024 11:55:48'!
display
	showOnDisplay ifTrue: [
		self displayOnDisplay.
		DisplayScreen screenUpdateRequired: bounds ].
	"So any morph in front of us is repaired when Morphic cycles.
	This includes, for instance, the TranscriptWindow that shows our contents if showOnDisplay is false"
	UISupervisor whenUIinSafeState: [ self triggerEvent: #redrawTranscriptArea ].! !


!TranscriptWindow class methodsFor: 'GUI building' stamp: 'jmv 1/31/2024 11:21:33'!
openTranscript
	"
	TranscriptWindow openTranscript
	"
	| win m |
	win := TranscriptWindow new.
	win
		setLabel: 'Transcript';
		model: Transcript.
	m := TranscriptMorph new.
	win layoutMorph addMorph: m proportionalHeight: 1.
	win model when: #redrawTranscriptArea send: #invalidateBounds to: win.
	^ win openInWorld.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
TranscriptWindow allInstancesDo: [ :tw |
	tw model removeActionsForEvent: #redraw.
	tw model when: #redrawTranscriptArea send: #invalidateBounds to: tw ]!

