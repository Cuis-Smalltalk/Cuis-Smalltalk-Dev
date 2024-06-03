'From Cuis7.1 [latest update: #6427] on 3 June 2024 at 11:23:22 am'!

!LayoutMorph methodsFor: 'geometry testing' stamp: 'jmv 6/3/2024 11:22:43'!
clipsSubmorphs
	"LayoutMorphs are usually not drawn (doAdoptWidgetsColor is false, they are transparent) and do not clip submorphs.
	A special case is Transcript. It doesn't include a ScrollPane (that usually does clipping),
	so doAdoptWidgetsColor is set to true, and we also want it to clipSubmorphs.
	There may be other similar cases in the future."
	^doAdoptWidgetsColor! !


!Transcript class methodsFor: 'ui' stamp: 'jmv 6/3/2024 11:22:04'!
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
		"This is needed so the Layout is actually drawn and it actually correctly clips the TranscriptMorph"
		doAdoptWidgetsColor;
		addMorph: m proportionalHeight: 1.
	win model when: #redrawTranscriptArea send: #invalidateBounds to: win.
	^ win openInWorld! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 6/3/2024 11:07:50'!
drawOn: aCanvas
	aCanvas
		fillRectangle: self morphLocalBounds
		color: color.
	Transcript displayTextOn: aCanvas in: self morphLocalBounds.
	Transcript bounds: (self externalizeBoundsToWorld: self morphLocalBounds)
				encompassingIntegerRectangle.
	self updateWorkspace! !

!methodRemoval: TranscriptWindow #clipsSubmorphs stamp: 'jmv 6/3/2024 11:20:25'!
TranscriptWindow removeSelector: #clipsSubmorphs!

!TranscriptWindow reorganize!
('open/close' delete)
('GUI building' windowColor)
('drawing' visible:)
!

