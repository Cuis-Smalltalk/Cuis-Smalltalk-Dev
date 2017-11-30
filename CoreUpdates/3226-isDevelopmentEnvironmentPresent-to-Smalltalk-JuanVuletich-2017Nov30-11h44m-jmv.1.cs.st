'From Cuis 5.0 of 7 November 2016 [latest update: #3225] on 30 November 2017 at 11:45:29 am'!

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 11/30/2017 11:44:27'!
isDevelopmentEnvironmentPresent
	"Or we can't open a Smalltalk debugger"

	^ Smalltalk isHeadless not and: [Smalltalk includesKey: #Debugger]! !


!UnhandledError methodsFor: 'priv handling' stamp: 'jmv 11/30/2017 11:44:53'!
defaultAction
	"The current computation is terminated. The cause of the error should be logged or reported to the user. If the program is operating in an interactive debugging environment the computation should be suspended and the debugger activated."

	Smalltalk isDevelopmentEnvironmentPresent
		ifTrue: [ self devDefaultAction ]
		ifFalse: [ self standaloneAppDefaultAction ]! !

!methodRemoval: UnhandledError #isDevelopmentEnvironmentPresent!
UnhandledError removeSelector: #isDevelopmentEnvironmentPresent!
