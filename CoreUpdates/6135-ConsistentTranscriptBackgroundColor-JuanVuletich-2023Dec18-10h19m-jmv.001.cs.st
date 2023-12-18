'From Cuis6.0 [latest update: #6134] on 18 December 2023 at 10:22:03 am'!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay lastDisplayPosition bounds logToStdout displayEngine backgroundColor '!

!Transcript class methodsFor: 'accessing' stamp: 'jmv 12/18/2023 09:30:33'!
backgroundColor: aColor
	backgroundColor := aColor! !


!TranscriptMorph methodsFor: 'accessing' stamp: 'jmv 12/18/2023 10:01:24'!
color: aColor
	"Set the receiver's color. "
	super color: aColor.
	Transcript backgroundColor: color.! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 12/18/2023 09:57:23'!
displayOnDisplay
	"To be called directly, not from Morphic.
	See #displayOnCanvas:in:"
	"
	1 to: 2000 do: [ :i | i print ].
	1 to: 750 do: [ :i | i print ].
	Transcript displayOnDisplay.
	Display forceToScreen
	"
	self displayEngine.
	displayEngine clipRect: bounds.
	displayEngine
		fill: bounds fillColor: backgroundColor rule: Form over.
	self displayTextOn: self in: bounds.! !

!Transcript class methodsFor: 'class initialization' stamp: 'jmv 12/18/2023 09:59:18'!
initialize
	"
	self initialize
	"
	showOnDisplay := true.
	bounds := 20@20 extent: 300@500.
	logToFile := false.
	logToStdout := false.
	entries := Array new: self maxEntries.
	unfinishedEntry := String writeStream.
	lastDisplayPosition := 0.
	accessSemaphore := Semaphore forMutualExclusion.
	backgroundColor := `Color veryVeryLightGray`.
	self clear! !


!Morph methodsFor: 'user interface' stamp: 'jmv 12/18/2023 09:21:37'!
flash

	self displayBounds ifNotNil: [ :r |
		10 timesRepeat: [ Delay forSeconds: 0.1 :: wait. Display flash: r ]]! !


!TranscriptMorph methodsFor: 'initialization' stamp: 'jmv 12/18/2023 10:01:48'!
initialize
	super initialize.
	doImmediateUpdates := true.
	Transcript backgroundColor: color.
	Transcript showOnDisplay: doImmediateUpdates.! !

!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 12/18/2023 09:30:45'!
adoptWidgetsColor: paneColor
	color = self defaultColor ifTrue: [
		color := Theme current paneBackgroundFrom: paneColor.
		Transcript backgroundColor: color.
	].
	super adoptWidgetsColor: paneColor.! !

Transcript initialize!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay lastDisplayPosition bounds logToStdout displayEngine backgroundColor'!

!TranscriptMorph reorganize!
('accessing' color:)
('initialization' defaultBorderWidth initialize)
('drawing' adoptWidgetsColor: drawOn:)
('event handling testing' handlesMouseDown:)
('menus' getMenu mouseButton2Activity)
('menu commands' clearAll clearFile clearInternal clearStdout doImmediateUpdates doRegularUpdates dontLogToFile dontLogToStdout editContents logToFile logToStdout)
('updating' updateWorkspace)
!

