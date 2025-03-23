'From Cuis7.3 [latest update: #7091] on 23 March 2025 at 4:05:25 pm'!

!StepMessage methodsFor: 'evaluating' stamp: 'jmv 3/23/2025 16:01:57'!
valueAtTime: millisecondClock

	|  nArgs millisecondsSinceLast answer |
	numArgs ifNil: [numArgs := selector numArgs].
	nArgs := arguments ifNil: [0] ifNotNil: [arguments size].
	lastEvaluationTime ifNil: [ lastEvaluationTime := millisecondClock ].
	millisecondsSinceLast := millisecondClock - lastEvaluationTime.
	lastEvaluationTime := millisecondClock.
	nArgs = numArgs ifTrue: [
		"Ignore extra argument"
		^self value ].
	[
		arguments
			ifNil: [ answer := receiver perform: selector with: millisecondsSinceLast]
			ifNotNil: [ answer := receiver perform: selector withArguments: (arguments copyWith: millisecondsSinceLast) ]
	] ifError: [ :err :rcvr |
		receiver
			stopStepping;
			setProperty: #errorOnStep toValue: true.
		{ 'Error while stepping: '. self. rcvr. err } print.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	].
	^answer! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 3/23/2025 15:57:49'!
debugMenuOptions

	^`{
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		10.
				#label 				-> 		'Inspect World'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#inspect.
				#icon 				-> 		#inspectIcon.
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		20.
				#label 				-> 		'Explore World'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#explore.
				#icon 				-> 		#exploreIcon.
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		10.
				#itemOrder 		-> 		30.
				#label 				-> 		'MessageTally all Processes'.
				#selector 			-> 		#startMessageTally.
				#icon 				-> 		#systemMonitorIcon.
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		10.
				#label 				-> 		'Start drawing all again'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#removeAllKnownFailing.
				#icon 				-> 		#graphicsIcon.
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		20.
				#label 				-> 		'Start stepping again'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#resumeAfterStepError.
				#icon 				-> 		#mediaPlaybackStartIcon.
			} asDictionary.
			{
				#submenuOf  		-> 		TheWorldMenu debugLabel.
				#itemGroup 		-> 		20.
				#itemOrder 		-> 		30.
				#label 				-> 		'Close all Debuggers'.
				#object 			-> 		DebuggerWindow.
				#selector 			-> 		#closeAllDebuggers.
				#icon 				-> 		#closeIcon.
			} asDictionary.
		}`! !

