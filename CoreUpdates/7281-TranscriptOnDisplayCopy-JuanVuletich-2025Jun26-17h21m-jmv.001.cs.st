'From Cuis7.5 [latest update: #7280] on 26 June 2025 at 5:21:44 pm'!

!Transcript class methodsFor: 'private' stamp: 'jmv 6/26/2025 16:11:15'!
displayEngine
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	"
	Transcript releaseClassCachedState
	"
	(displayEngine isNil or: [
		displayEngine class ~= BitBltCanvasEngine]) ifTrue: [
			displayEngine := BitBltCanvasEngine toForm: Display shallowCopy].
	^ displayEngine! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 6/26/2025 16:11:07'!
display
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	showOnDisplay ifTrue: [
		self displayOnDisplay.
		Display forceToScreen: bounds ].
	"So any morph in front of us is repaired when Morphic cycles.
	This includes, for instance, the TranscriptWindow that shows our contents if showOnDisplay is false"
	UISupervisor whenUIinSafeState: [ self triggerEvent: #redrawTranscriptArea ].! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 6/26/2025 16:11:02'!
displayUnfinishedEntry
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	showOnDisplay ifTrue: [
		self displayUnfinishedEntryOnDisplay ifNotNil: [ :damage |
			Display forceToScreen: bounds ]]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

	Transcript releaseClassCachedState!

