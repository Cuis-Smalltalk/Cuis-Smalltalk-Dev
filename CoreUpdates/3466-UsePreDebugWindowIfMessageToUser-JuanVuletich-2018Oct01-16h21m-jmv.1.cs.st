'From Cuis 5.0 of 7 November 2016 [latest update: #3430] on 1 October 2018 at 4:22:14 pm'!

!PreDebugWindow class methodsFor: 'instance creation' stamp: 'jmv 10/1/2018 16:21:56'!
open: model label: aString message: messageString
	|  window |
	(Preferences usePreDebugWindow or: [messageString notNil])
		ifTrue: [
			window _ self new.
			window
				model: model;
				buildMorphicWindowMessage: messageString print.
			aString ifNotNil: [ window setLabel: aString ].
			 window openInWorld ]
		ifFalse: [
			model openFullMorphicLabel: aString ]! !

