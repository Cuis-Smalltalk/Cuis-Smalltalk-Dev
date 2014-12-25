'From Cuis 4.2 of 25 July 2013 [latest update: #2128] on 12 December 2014 at 4:25:38.679899 pm'!

!Symbol class methodsFor: 'private' stamp: 'jmv 12/12/2014 16:24'!
shutDown

	SymbolTable addAll: NewSymbols.
	NewSymbols _ WeakSet new! !


!Behavior methodsFor: 'system startup' stamp: 'jmv 12/12/2014 16:24'!
shutDown: quitting
	"This message is sent on system shutdown to registered classes.
	If quitting is true, we are about to quit.
	If quitting is false, we are just snapshotting."

	^ self shutDown! !

!Behavior methodsFor: 'system startup' stamp: 'jmv 12/12/2014 16:25'!
startUp: resuming
	"This message is sent to registered classes, with resuming = true when the system is coming up,
	and with resuming = false after a snapshot (image save, no quit)."

	^ self startUp! !


!WeakArray class methodsFor: 'system startup' stamp: 'jmv 12/12/2014 16:25'!
startUp: resuming

	"If not a system startup (just a snapshot), no need for this."
	resuming ifFalse: [ ^self ].

	self restartFinalizationProcess! !

!methodRemoval: Symbol class #shutDown:!
Symbol class removeSelector: #shutDown:!
