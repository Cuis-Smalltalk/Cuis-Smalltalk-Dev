'From Cuis 4.2 of 25 July 2013 [latest update: #2971] on 4 November 2016 at 5:16:34 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'KenD 11/2/2016 16:19:04'!
comeToFrontAndAddHalo
	self runningWorld taskbar ifNotNil: [ :tb | tb restore: self ].
	self comeToFront.
	self addHalo! !

