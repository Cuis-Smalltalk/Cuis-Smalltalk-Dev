'From Cuis 5.0 [latest update: #4520] on 13 January 2021 at 10:30:53 am'!

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 1/13/2021 09:59:07'!
layoutNeeded: aBoolean

	self privateFlagAt: 4 put: aBoolean! !

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 1/13/2021 09:59:11'!
needsRedraw: aBoolean

	self privateFlagAt: 1 put: aBoolean! !

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 1/13/2021 09:59:15'!
submorphNeedsRedraw: aBoolean

	self privateFlagAt: 2 put: aBoolean! !

