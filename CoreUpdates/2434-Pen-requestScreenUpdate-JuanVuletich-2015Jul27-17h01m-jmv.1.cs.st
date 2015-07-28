'From Cuis 4.2 of 25 July 2013 [latest update: #2433] on 27 July 2015 at 5:01:29.262738 pm'!

!Pen methodsFor: 'private' stamp: 'jmv 7/27/2015 17:01'!
copyBits
	super copyBits.
	destForm == Display ifTrue: [
		DisplayScreen screenUpdateRequired: self clipRect ]! !

!Pen methodsFor: 'private' stamp: 'jmv 7/27/2015 17:01'!
drawLoopX: xDelta Y: yDelta
	super drawLoopX: xDelta Y: yDelta.
	destForm == Display ifTrue: [
		DisplayScreen screenUpdateRequired: self clipRect ]! !

