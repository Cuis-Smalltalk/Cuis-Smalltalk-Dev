'From Cuis 6.0 [latest update: #5803] on 24 May 2023 at 3:40:14 pm'!

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 5/24/2023 15:38:08'!
mainLoop
	| d |

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime := Time localMillisecondClock.
	lastAlarmTime := 0.

	canvas isNil ifTrue: [
		self setMainCanvas ].
	d := Delay forMilliseconds: 50.
	[
		self displayWorld.
		[
			self doOneCycle: d.
			UISupervisor isUIProcessRunning ]
				whileTrue: []
	] on: Error, Halt do: [ :ex |
		"Handle a drawing error"
		(ex signalerContext findContextSuchThat: [ :context |
				(context receiver is: #Morph) and: [ context  selector = #drawOn: ]])
			ifNotNil: [ :context |
				context receiver drawingFails.
				self clearCanvas.
				"Install the old error handler, so we can re-raise the error.
				This means, open a Debugger.
				This will also create a new Morphic process. if the user closes the debugger."
				ex receiver error: ex description.
				"Create a new UI process, if the user proceeds in the debugger."
				UISupervisor newUIProcess ]
			ifNil: [ ex pass ]
	].! !

