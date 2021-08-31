'From Cuis 5.0 [latest update: #4819] on 30 August 2021 at 4:25:57 pm'!

!TranscriptWindow methodsFor: 'geometry testing' stamp: 'jmv 8/30/2021 16:25:26'!
clipsSubmorphs
	"Our contents are not inside a PluggableScrollPane like in other SystemWindows,
	so we do the clipping ourselves."

	^ true! !

!methodRemoval: TranscriptMorph #clipsSubmorphs stamp: 'jmv 8/30/2021 16:24:26'!
TranscriptMorph removeSelector: #clipsSubmorphs!

!TranscriptWindow reorganize!
('open/close' delete)
('GUI building' windowColor)
('drawing' visible:)
('geometry testing' clipsSubmorphs)
!


!TranscriptMorph reorganize!
('initialization' defaultBorderWidth initialize)
('drawing' drawOn:)
('event handling testing' handlesMouseDown:)
('menus' getMenu mouseButton2Activity)
('menu commands' clearAll clearFile clearInternal clearStdout doImmediateUpdates doRegularUpdates dontLogToFile dontLogToStdout editContents logToFile logToStdout)
('updating' updateWorkspace)
!

