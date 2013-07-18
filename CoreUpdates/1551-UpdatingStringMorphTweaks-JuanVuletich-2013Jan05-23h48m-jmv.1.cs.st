'From Cuis 4.1 of 12 December 2012 [latest update: #1550] on 5 January 2013 at 11:49:56 pm'!

!UpdatingStringMorph commentStamp: 'jmv 1/5/2013 23:49' prior: 0!
UpdatingStringMorph new
     target: [self runningWorld activeHand morphPosition asString];
     getSelector: #value;
     stepTime: 10;
     openInWorld!


!UpdatingStringMorph methodsFor: 'stepping' stamp: 'jmv 1/5/2013 23:49'!
step
	self contents: (target perform: getSelector)! !

!UpdatingStringMorph methodsFor: 'initialization' stamp: 'jmv 1/5/2013 23:48'!
initialize
	super initialize.
	target _ self.
	getSelector printString.
	stepTime _ 50! !

