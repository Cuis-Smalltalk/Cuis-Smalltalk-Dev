'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 22 March 2012 at 11:59:16 am'!

!SystemChangeNotifier methodsFor: 'private' stamp: 'jmv 3/22/2012 11:58'!
                         triggerEvent: anEventSelector withArguments: anArgumentList

	self isBroadcasting ifTrue: [
		^super triggerEvent: anEventSelector withArguments: anArgumentList ]! !

