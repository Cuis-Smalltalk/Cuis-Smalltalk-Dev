'From Cuis7.1 [latest update: #6517] on 15 July 2024 at 11:41:07 am'!

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/15/2024 11:27:34'!
doClipSubmorphs
	self setProperty: #doClipSubmorphs toValue: true.! !

!LayoutMorph methodsFor: 'geometry testing' stamp: 'jmv 7/15/2024 11:34:26'!
clipsSubmorphs
	"LayoutMorphs usually not do not clip submorphs.
	A special case is Transcript. It doesn't include a ScrollPane (that usually does clipping).
	Note: for this to work, we can't be transparent, as transparent morphs can't clip submorphs.
		Senders may also call #doAdoptWidgetsColor.
	There may be other similar cases in the future."

	^self hasProperty: #doClipSubmorphs.! !


!Transcript class methodsFor: 'ui' stamp: 'jmv 7/15/2024 11:33:02'!
open
	"
	Transcript open
	"
	| win m |
	win := TranscriptWindow new.
	win
		setLabel: 'Transcript';
		model: Transcript.
	m := TranscriptMorph new.
	win layoutMorph
		"This is needed so the Layout correctly clips the TranscriptMorph"
		doAdoptWidgetsColor;
		doClipSubmorphs;
		addMorph: m proportionalHeight: 1.
	win model when: #redrawTranscriptArea send: #invalidateBounds to: win.
	^ win openInWorld! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| found |
found := false.
self runningWorld submorphsDo: [ :m | m class == TranscriptWindow ifTrue: [ m delete. found := true ]].
found ifTrue: [ Transcript open ].!

