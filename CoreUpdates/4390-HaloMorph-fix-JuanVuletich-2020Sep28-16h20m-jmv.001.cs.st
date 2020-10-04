'From Cuis 5.0 [latest update: #4389] on 28 September 2020 at 4:33:47 pm'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/28/2020 16:32:38'!
doDrag: evt with: dragHandle
	| thePoint |
	evt hand obtainHalo: self.
	thePoint _ evt eventPosition - positionOffset.
	target morphPositionInWorld: thePoint.
	self morphPositionInWorld: thePoint + target morphTopLeft! !

