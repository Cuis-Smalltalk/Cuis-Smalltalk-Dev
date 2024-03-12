'From Cuis6.3 [latest update: #6265] on 12 March 2024 at 10:59:13 am'!

!Transcript class methodsFor: 'displaying' stamp: 'jmv 3/12/2024 10:58:45'!
display
	showOnDisplay ifTrue: [
		self displayOnDisplay.
		"DisplayScreen screenUpdateRequired: bounds"
		"Better do it right now!!"
		Display forceToScreen: bounds. ].
	"So any morph in front of us is repaired when Morphic cycles.
	This includes, for instance, the TranscriptWindow that shows our contents if showOnDisplay is false"
	UISupervisor whenUIinSafeState: [ self triggerEvent: #redrawTranscriptArea ].! !

