'From Cuis 5.0 [latest update: #4459] on 17 November 2020 at 4:06:36 pm'!

!TranscriptMorph methodsFor: 'geometry testing' stamp: 'jmv 11/17/2020 16:06:01'!
clipsSubmorphs
	"Answer true if we clip the shape of our submorphs to our own.
	Answer true only when clipping by the canvas is needed.
	Morphs that can guarantee that submorphs don't protrude and answer false to 
	#submorphsMightProtrude should answer false for better performance"

	^ true! !


!TranscriptMorph reorganize!
('initialization' defaultBorderWidth initialize)
('drawing' drawOn:)
('event handling testing' handlesMouseDown:)
('geometry')
('geometry testing' clipsSubmorphs)
('menus' getMenu mouseButton2Activity)
('menu commands' clearAll clearFile clearInternal clearStdout doImmediateUpdates doRegularUpdates dontLogToFile dontLogToStdout editContents logToFile logToStdout)
('updating' updateWorkspace)
!

