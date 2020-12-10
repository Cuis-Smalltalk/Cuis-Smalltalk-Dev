'From Cuis 5.0 [latest update: #4384] on 9 December 2020 at 9:23:41 pm'!

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'MJR 12/9/2020 21:23:01'!
request: queryString centeredAt: aPoint initialAnswer: defaultAnswer validationBlock: validationBlock acceptBlock: acceptBlock cancelBlock: cancelBlock
	| answer |
	answer _ self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock;
		acceptBlock: acceptBlock;
		cancelBlock: cancelBlock.
	self runningWorld addMorph: answer centeredNear: aPoint - self deltaToTextPane.
	Preferences focusFollowsMouse ifFalse: [answer textBox focusText].
	^ answer! !

