'From Cuis 5.0 [latest update: #3996] on 26 December 2019 at 11:35:35 am'!

!PreDebugWindow class methodsFor: 'instance creation' stamp: 'HAW 12/26/2019 11:18:10'!
forceOpen: model label: aString message: messageString
		
	|  window |
	
	window _ self new.
	window
		model: model;
		buildMorphicWindowMessage: messageString print.
	aString ifNotNil: [ window setLabel: aString ].
	
	window openInWorld ! !


!Debugger methodsFor: 'initialization' stamp: 'HAW 12/26/2019 11:18:22'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor defaultCursor activateCursor.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self lowSpaceChoices , (msgString ifNil: [ '' ]) ]
		ifFalse: [ msgString ].
	interruptedProcessUI _ UISupervisor newProcessIfUI: interruptedProcess.
	UISupervisor whenUIinSafeState: [
		PreDebugWindow
			forceOpen: self
			label: label
			message: msg ].! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'HAW 12/26/2019 11:17:49'!
open: model label: aString message: messageString
	
	(Preferences usePreDebugWindow or: [messageString notNil])
		ifTrue: [self forceOpen: model label: aString message: messageString]
		ifFalse: [model openFullMorphicLabel: aString ]! !

