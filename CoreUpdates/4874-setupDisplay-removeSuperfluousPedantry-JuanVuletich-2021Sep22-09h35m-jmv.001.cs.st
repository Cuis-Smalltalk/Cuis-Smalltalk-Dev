'From Cuis 5.0 [latest update: #4871] on 22 September 2021 at 9:39:51 am'!

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 9/22/2021 09:37:38'!
setupDisplay
	"
	DisplayScreen setupDisplay.
	Display forceToScreen.
	"

	self terminateScreenUpdater.
	Display setExtent: self actualScreenSize depth: Display nativeDepth.
	Display beDisplay.
	self installScreenUpdater.! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 9/22/2021 09:37:45'!
startUp
	"
	DisplayScreen startUp.
	Display forceToScreen.
	"
	self setupDisplay.! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/22/2021 09:38:22'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	DisplayScreen isDisplayExtentOk ifFalse: [
		"Minimize the risk of going out of memory:
		- First clear existing canvas, to free the memory it uses.
		- Then, setup the display.
		- Then set up new canvas."
		self clearCanvas.
		DisplayScreen setupDisplay.
		self setMainCanvas.
		self whenUIinSafeState: [ Cursor defaultCursor activateCursor ]].! !

!methodRemoval: DisplayScreen class #setupDisplay: stamp: 'jmv 9/22/2021 09:38:37'!
DisplayScreen class removeSelector: #setupDisplay:!
