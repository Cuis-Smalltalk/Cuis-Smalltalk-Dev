'From Cuis6.3 [latest update: #6225] on 11 February 2024 at 3:50:10 pm'!

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 2/11/2024 15:48:37'!
logStartupNotice: stuff
	"
	Smalltalk logStartupNotice: 'stuff'
	"
	[
		Transcript
			logToFile: true;
			logToStdout: true.
		stuff print.
	] forkAt: Processor activePriority - 1! !


!SystemDictionary methodsFor: 'startup' stamp: 'jmv 2/11/2024 15:49:27'!
logStartupDebugAid: stuff
	"
	Transcript
		showOnDisplay: false;
		logToFile: true;
		logToStdout: true.
	Smalltalk at: #LogStartupDebugAids put: true.
	Smalltalk logStartupDebugAid: 'stuff'
	"
	self logStartupDebugAids ifTrue: [
		self logStartupNotice: stuff ].! !

