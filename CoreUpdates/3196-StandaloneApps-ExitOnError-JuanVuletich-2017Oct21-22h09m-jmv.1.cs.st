'From Cuis 5.0 of 7 November 2016 [latest update: #3195] on 21 October 2017 at 10:12:27 pm'!

!ContextPart methodsFor: 'debugger access' stamp: 'jmv 10/21/2017 22:04:04'!
shortErrorReportOn: strm
	"Write a short error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report. "

	| cnt aContext |
 	strm print: Date today; space; print: Time now; newLine.
	aContext _ self.
	cnt _ 0.
	[aContext notNil and: [(cnt _ cnt + 1) < 20]] whileTrue: [
		strm print: aContext; newLine.  "just class>>selector"	
		aContext _ aContext sender]! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 10/19/2017 23:20:22'!
isHeadless
	"Answer true if any of this VM options was specified in the commandline:
		-nodisplay
		-vm-display-null

	Smalltalk isHeadless
	"
	self vmOptionsDo: [ :vmOption :i |
		vmOption = '-vm-display-null' ifTrue: [ ^ true ].
		vmOption = '-nodisplay' ifTrue: [ ^ true ] ].
	^ false! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 10/19/2017 23:14:08'!
vmOptionsDo: aBlock
	"Repeatedly evaluate aBlock for each vm option specified by the commandline that started Cuis.
	aBlock has two arguments: the vm option itself and the index (position)
	
	Smalltalk vmOptionsDo: [ :option :i | {i. option} print ]
	"
	| i vmOption |
	i _ -1.
	[vmOption _ Smalltalk getSystemAttribute: i.
	vmOption notNil ] whileTrue: [
		aBlock value: vmOption value: i.
		i _ i-1 ]! !


!UnhandledError methodsFor: 'priv handling' stamp: 'jmv 10/21/2017 22:07:43'!
standaloneAppDefaultAction
	"Dump the stack trace to a log file, then exit the program (image)."
	
	Smalltalk logError: self description inContext: self signalerContext to: 'CuisDebug'.
	Smalltalk quitPrimitive: 1! !


!Debugger class methodsFor: 'class initialization' stamp: 'jmv 10/21/2017 21:54:51'!
openContext: aContext label: aString contents: contentsStringOrNil
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."
	<primitive: 19> "Simulation guard"
	self errorRecursion not & Preferences logDebuggerStackToFile ifTrue:
		[Smalltalk logError: aString inContext: aContext to: 'CuisDebug'].
	ErrorRecursion ifTrue: [
		ErrorRecursion _ false.
		contentsStringOrNil
			ifNil: [
				self primitiveError: 'Can not open debugger due to recursion error.', 
					String newLineString, aString]
			ifNotNil: [
				self primitiveError: 'Can not open debugger due to recursion error.', 
					String newLineString, aString, String newLineString, contentsStringOrNil ]].
	ErrorRecursion _ true.
	self informExistingDebugger: aContext label: aString.
	(Debugger context: aContext)
		openNotifierContents: contentsStringOrNil
		label: aString.
	ErrorRecursion _ false.
	Processor activeProcess suspend.
! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 10/21/2017 21:54:56'!
openInterrupt: aString onProcess: interruptedProcess
	"Open a notifier in response to an interrupt. An interrupt occurs when the user types the interrupt key (cmd-. on Macs, ctrl-c or alt-. on other systems) or when the low-space watcher detects that memory is low."
	| debugger |
	<primitive: 19> "Simulation guard"
	debugger _ self new.
	debugger
		process: interruptedProcess
		context: interruptedProcess suspendedContext.
	debugger externalInterrupt: true.

Preferences logDebuggerStackToFile ifTrue:
	[(aString includesSubString: 'Space') & 
		(aString includesSubString: 'low') ifTrue: [
			Smalltalk logError: aString inContext: debugger interruptedContext to: 'LowSpaceDebug']].

	^ debugger
		openNotifierContents: nil
		label: aString
! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 10/21/2017 21:55:00'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."
	| w |
	Preferences logDebuggerStackToFile ifTrue: [
		Smalltalk
			logError: title
			inContext: context
			to: 'CuisDebug' ].
	w := ProjectX newProcessIfUIX: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	WorldState addDeferredUIMessage: [
		[ | debugger |
		"In case an error in Morphic code got us here, ensure mouse focus has been released"
		true runningWorld ifNotNil: [ :rWorld |
			rWorld activeHand ifNotNil: [ :aHand |
				aHand releaseMouseFocus ]].
		debugger := self new
			process: process
			context: context.
		debugger interruptedProcessUI: w.
		bool
			ifTrue: [ debugger openFullMorphicLabel: title ]
			ifFalse: [
				PreDebugWindow
					open: debugger
					label: title
					message: nil ]]
			on: UnhandledError
			do: [ :exOuter | | errorDescription |
				errorDescription := 'Error while trying to open Debugger' , String newLineString , 'Orginal error: ' , title asString , '.' , String newLineString , '	Debugger error: ' ,
					([ exOuter description ]
						on: UnhandledError
						do: [ :exInner |
							exInner return: 'a ' , exInner class printString ]) , ':'.
				self primitiveError: errorDescription ]].
	process suspend.! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 10/21/2017 22:12:20'!
logError: errMsg inContext: aContext to: baseFilename
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	| localFilename file |
	localFilename _ Preferences debugLogTimestamp
		ifTrue: [ baseFilename, '-', Utilities dateTimeSuffix, '.log' ]
		ifFalse: [ baseFilename, '.log' ].
	file _ DirectoryEntry smalltalkImageDirectory // localFilename.
	[
		file forceWriteStreamDo: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"
	[
 	 	StdIOWriteStream stdout newLine; nextPutAll: errMsg.
		StdIOWriteStream stdout newLine; nextPutAll: 'See '; nextPutAll: file pathName.
		StdIOWriteStream stdout newLine.
		aContext shortErrorReportOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout flush
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"! !


!UnhandledError methodsFor: 'priv handling' stamp: 'jmv 10/21/2017 21:17:43'!
defaultAction
	"The current computation is terminated. The cause of the error should be logged or reported to the user. If the program is operating in an interactive debugging environment the computation should be suspended and the debugger activated."

	self isDevelopmentEnvironmentPresent
		ifTrue: [ self devDefaultAction ]
		ifFalse: [ self standaloneAppDefaultAction ]! !

!UnhandledError methodsFor: 'priv handling' stamp: 'jmv 10/19/2017 23:20:13'!
isDevelopmentEnvironmentPresent

	^ Smalltalk isHeadless not and: [Smalltalk includesKey: #Debugger]! !


!Preferences class methodsFor: 'standard queries' stamp: 'jmv 10/19/2017 23:30:02'!
debugLogTimestamp
	^ self
		valueOfFlag: #debugLogTimestamp
		ifAbsent: [true]! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 10/21/2017 21:52:54'!
dateTimeSuffix
	"Answer a string which indicates the date and time, intended for use in building fileout filenames, etc.

	Utilities dateTimeSuffix
	"
	| n |
	n _ DateAndTime now.
	^ String streamContents: [ :strm |
		n printYMDOn: strm withLeadingSpace: false.
		strm nextPut: $_.
		n printHMSOn: strm separator: $. ]! !


!DisplayScreen methodsFor: 'private' stamp: 'jmv 10/21/2017 21:55:10'!
findAnyDisplayDepth
	"Return any display depth that is supported on this system."
	^self findAnyDisplayDepthIfNone: [
		"Ugh .... now this is a biggie - a system that does not support
		any of the Squeak display depths at all."
		Smalltalk
			logError: 'Fatal error: This system has no support for any display depth at all.'
			inContext: thisContext
			to: 'CuisDebug'.
		Smalltalk quitPrimitive. "There is no way to continue from here"
	]! !

!methodRemoval: Utilities class #monthDayTime24StringFrom:!
Utilities class removeSelector: #monthDayTime24StringFrom:!
!methodRemoval: Utilities class #monthDayTimeStringFrom:!
Utilities class removeSelector: #monthDayTimeStringFrom:!
!methodRemoval: Preferences class #twentyFourHourFileStamps!
Preferences class removeSelector: #twentyFourHourFileStamps!
!methodRemoval: Debugger #storeLog!
Debugger removeSelector: #storeLog!
