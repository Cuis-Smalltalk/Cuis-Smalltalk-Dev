'From Cuis 5.0 [latest update: #4856] on 13 September 2021 at 4:08:19 pm'!

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 9/13/2021 16:07:58'!
setMainCanvas
	"Deallocate before allocating could mean less memory stress."

	self clearCanvas.
	self setCanvas: Display getMainCanvas.
	self restoreDisplay.! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/13/2021 16:03:02'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	DisplayScreen isDisplayExtentOk ifFalse: [
		self clearCanvas.
		DisplayScreen startUp.
		self setMainCanvas.
		self whenUIinSafeState: [ Cursor defaultCursor activateCursor ]].! !

