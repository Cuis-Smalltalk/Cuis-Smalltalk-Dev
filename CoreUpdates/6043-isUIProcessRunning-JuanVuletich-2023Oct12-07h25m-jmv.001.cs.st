'From Cuis 6.0 [latest update: #6042] on 12 October 2023 at 7:28:42 am'!

!UISupervisor class methodsFor: 'services' stamp: 'jmv 10/12/2023 07:28:31'!
isUIProcessRunning
	"Is the currently running process the UI Process?"

	^UIProcess notNil and: [ UIProcess isRunning ].! !

