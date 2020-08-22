'From Cuis 5.0 [latest update: #4356] on 21 August 2020 at 12:34:31 pm'!

!Morph methodsFor: 'updating' stamp: 'jmv 8/21/2020 12:31:46'!
invalidateBounds
	"Report that the area occupied by this morph should be redrawn, 
	but most likely we are not going to be redrawn on it, or maybe we'll not be fully visible. See senders."

	self invalidateDisplayRect: self displayBoundsOrBogus fromSubmorph: nil for: nil.! !


!TranscriptWindow class methodsFor: 'GUI building' stamp: 'jmv 8/21/2020 12:28:48'!
openTranscript
	"
	TranscriptWindow openTranscript
	"
	| win m |
	win _ TranscriptWindow new.
	win
		setLabel: 'Transcript';
		model: Transcript.
	m _ TranscriptMorph new.
	win layoutMorph addMorph: m proportionalHeight: 1.
	win model when: #redraw send: #invalidateBounds to: m.
	^ win openInWorld.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
TranscriptWindow allInstancesDo: [ :t | t delete ].
TranscriptWindow openTranscript.!
