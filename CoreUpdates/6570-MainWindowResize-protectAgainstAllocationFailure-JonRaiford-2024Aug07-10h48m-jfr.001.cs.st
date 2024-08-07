'From Cuis7.1 [latest update: #6517] on 7 August 2024 at 10:49:00 am'!

!WorldMorph methodsFor: 'update cycle' stamp: 'jfr 8/7/2024 10:48:53'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e p |
	DisplayScreen isDisplayExtentOk ifFalse: [
		"If host OS scale factor has changed, try to recover our previous Display extent"
		newScaleFactor := DisplayScreen hostOSScaleFactor.
		newScaleFactor = ScaleFactor ifFalse: [
			ScaleFactor := newScaleFactor.
			e := DisplayScreen hostOSWindowExtent ifNotNil: [ :ee |
				ee * ScaleFactor - DisplayScreen actualScreenSize + Display extent ].
			p := DisplayScreen hostOSWindowPosition.
			e ifNotNil: [DisplayScreen hostOSWindowExtent: e].
			p ifNotNil: [DisplayScreen hostOSWindowPosition: p].
		].
		[
			"This allocates new memory for Display and is critical."
			DisplayScreen setupDisplay.
			UISupervisor newUIProcess.
			self whenUIinSafeState: [ Cursor defaultCursor activateCursor ]
		] on: OutOfMemory do: [
			"In case of allocation failure, don't allocate Display right now,
			but start a new UI process. This will retry the Display resize real soon."
			'Display allocation failed'  print.
			UISupervisor newUIProcess 		]].! !

