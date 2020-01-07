'From Cuis 5.0 [latest update: #3994] on 26 December 2019 at 11:10:16 am'!

!Process methodsFor: 'changing suspended state' stamp: 'HAW 12/26/2019 10:59:25'!
canSearchForSignalerContextOf: anException

	^ (anException class includesBehavior: Exception)
		and: [anException canSearchForSignalerContext]! !

!Process methodsFor: 'changing suspended state' stamp: 'HAW 12/26/2019 10:55:07'!
completeStepUpTo: aContext
	
	[aContext == suspendedContext] 
		whileFalse: [self completeStep: suspendedContext].! !

!Process methodsFor: 'changing suspended state' stamp: 'HAW 12/26/2019 10:59:09'!
ifSuspendedContextIsUnhandledErrorDo: aBlock

	| unhandledError |
	
	self isSuspendedContextSignalUnhandledError ifTrue: [ 
		unhandledError := suspendedContext tempAt: 1.
		(self canSearchForSignalerContextOf: unhandledError) ifTrue: [ 
			aBlock value: unhandledError ]].! !

!Process methodsFor: 'changing suspended state' stamp: 'HAW 12/26/2019 10:56:50'!
isSuspendedContextSignalUnhandledError

	^ suspendedContext selector == #signalForException:
		and: [suspendedContext receiver isBehavior 
		and: [suspendedContext receiver includesBehavior: UnhandledError]]! !


!Notification methodsFor: 'debugger updating' stamp: 'HAW 12/26/2019 11:01:45'!
isToUpdateDebuggerOn: aContext

	^tag isArray
		and: [tag size = 2
		and: [(tag first == aContext or: [tag first hasSender: aContext])]]! !

!Notification methodsFor: 'debugger updating' stamp: 'HAW 12/26/2019 11:01:48'!
withNewDebuggerLabelOn: aContext do: labelBlock ifNone: noneBlok

	^(self isToUpdateDebuggerOn: aContext)
		ifTrue: [ labelBlock value: tag second description ]
		ifFalse: noneBlok ! !


!Notification class methodsFor: 'debugger updating - signaling' stamp: 'HAW 12/26/2019 11:02:18'!
signalToUpdateDebuggerOn: unhandledErrorSignalerContext dueTo: anError

	self new 
		tag: {unhandledErrorSignalerContext. anError};
		signal.! !


!Object methodsFor: 'testing' stamp: 'HAW 12/26/2019 10:04:01'!
isContext

	^false ! !


!Debugger methodsFor: 'context stack menu' stamp: 'HAW 12/26/2019 10:19:57'!
handleLabelUpdatesIn: aBlock whenExecuting: aContext
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns."
	
	^aBlock
		on: Notification
		do: [:aNotification|
			aNotification 
				withNewDebuggerLabelOn: aContext
				do: [ :aNewLabel | 
					self labelString: aNewLabel.
					aNotification resume]
				ifNone: [aNotification pass]]! !


!Debugger class methodsFor: 'class initialization' stamp: 'HAW 12/26/2019 10:05:14'!
openContext: aContext label: aString contents: contentsStringOrNil
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	<primitive: 19> "Simulation guard"

	(self errorRecursion not and: [Preferences logDebuggerStackToFile]) ifTrue:
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
	[self informExistingDebugger: aContext label: aString.
	(Debugger context: aContext)
		openNotifierContents: contentsStringOrNil
		label: aString.] ensure: [ ErrorRecursion _ false ].
	Processor activeProcess suspend.
! !


!Process methodsFor: 'changing suspended state' stamp: 'HAW 12/26/2019 11:09:25'!
stepToHome: aContext 
	"Resume self until the home of top context is aContext.  Top context may be a block context.
	 Catch any UnhandledErrors that are created while stepping, answering the relevant signalerContext
	 if so. Note that this will cause weird effects if using through to step through UnhandledError
	 code, but as the doctor ordered, don't do that; use over or into instead."

	| home |
	
	home := aContext home.
	[suspendedContext := suspendedContext step.
	home == suspendedContext home or: [home isDead]] whileFalse:
		[self ifSuspendedContextIsUnhandledErrorDo: [ :anError |
			anError signalerContext ifNotNil: [:unhandledErrorSignalerContext|
				self completeStepUpTo: unhandledErrorSignalerContext.

				"Give a debugger a chance to update its title to reflect the new exception"
				 Notification signalToUpdateDebuggerOn: unhandledErrorSignalerContext dueTo: anError.
				^unhandledErrorSignalerContext]]].
		
	^suspendedContext! !


!PasteUpMorph methodsFor: 'interaction loop' stamp: 'HAW 12/26/2019 10:05:45'!
runProcess
	
	| process |
	
	process _ [ self mainLoop ] newProcess.
	process
		priority: Processor userSchedulingPriority;
		name: 'Morphic UI';
		animatedUI: self.
	
	^ process! !


!Notification reorganize!
('exceptionDescription' defaultAction isResumable)
('debugger updating' isToUpdateDebuggerOn: withNewDebuggerLabelOn:do:ifNone:)
!


!Notification class reorganize!
('debugger updating - signaling' signalToUpdateDebuggerOn:dueTo:)
!

