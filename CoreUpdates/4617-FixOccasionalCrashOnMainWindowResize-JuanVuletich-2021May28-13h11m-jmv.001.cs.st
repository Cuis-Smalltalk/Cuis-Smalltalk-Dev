'From Cuis 5.0 [latest update: #4613] on 28 May 2021 at 1:20:12 pm'!

!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 5/28/2021 11:13:51'!
isDisplayExtentOk
	"False if Cuis main window size has changed, but Display hasn't been updated yet."

	^Display extent = DisplayScreen actualScreenSize! !

!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 5/28/2021 11:38:25'!
terminateScreenUpdater

	ScreenUpdaterProcess ifNotNil: [
		ScreenUpdaterProcess terminate.
		ScreenUpdaterProcess _ nil ]! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 5/28/2021 11:14:54'!
endEntry
	| c d cb |
	c _ self contents.
	DisplayScreen isDisplayExtentOk ifFalse: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame _ frame intersect: Display boundingBox.
		^ self clear; show: c].
	textComposition
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll.
	d _ textComposition usedHeight - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ textComposition characterBlockAtPoint:
			`0@0` + (0@(d+FontFamily defaultLineSpacing)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		textComposition: textComposition
		bounds: (`4@4` + frame topLeft extent: Display extent)
		color: `Color black`
		selectionColor: `Color blue`.
	DisplayScreen screenUpdateRequired: nil! !


!UISupervisor class methodsFor: 'services' stamp: 'jmv 5/28/2021 13:17:39'!
restoreDisplay
	self ui ifNotNil: [ :guiRootObject |
		DisplayScreen isDisplayExtentOk ifFalse: [
			"Deallocate before allocating could mean less memory stress."
			guiRootObject clearCanvas ]].
	DisplayScreen startUp.
	self ui ifNotNil: [ :guiRootObject |
		guiRootObject restoreDisplay ].
	self whenUIinSafeState: [ Cursor defaultCursor activateCursor ].! !


!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 5/28/2021 11:14:20'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	self isDisplayExtentOk ifFalse: [
		UISupervisor restoreDisplay ]! !

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 5/28/2021 11:38:55'!
shutDown 
	"Minimize Display memory saved in image"
	Display setExtent: `240@120` depth: Display nativeDepth.
	self terminateScreenUpdater! !

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 5/28/2021 11:39:19'!
startUp 
	"
	DisplayScreen startUp
	"
	self terminateScreenUpdater.
	Display setExtent: self actualScreenSize depth: Display nativeDepth.
	Display beDisplay.
	self installScreenUpdater! !

!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 5/28/2021 11:39:06'!
installScreenUpdater
	"Initialize the event tickler process. Terminate the old process if any."
	"
	DisplayScreen installScreenUpdater
	"

	self terminateScreenUpdater.
	ScreenUpdaterProcess _ [ self screenUpdater ] newProcess.
	ScreenUpdaterProcess priority: Processor lowIOPriority.
	ScreenUpdaterProcess name: 'Background Screen updater'.
	ScreenUpdaterProcess resume! !

!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 5/28/2021 11:33:49'!
screenUpdater
	| delay |
	delay _ Delay forMilliseconds: 50.
	ScreenUpdaterSemaphore _ Semaphore new.
	Damage _ nil.
	[
		delay wait.
		ScreenUpdaterSemaphore wait.
		Display forceToScreen: Damage.
		ScreenUpdaterSemaphore initSignals.
		Damage _ nil.
	] repeat! !


!WorldMorph methodsFor: 'canvas' stamp: 'jmv 5/28/2021 13:16:50'!
setCanvas
	self clearCanvas.						"Deallocate before allocating could mean less memory stress."
	self setCanvas: Display getCanvas.
	damageRecorder _ DamageRecorder new.
	self redrawNeeded! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	DisplayScreen installScreenUpdater!

