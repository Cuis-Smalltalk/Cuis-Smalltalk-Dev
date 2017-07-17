'From Cuis 5.0 of 7 November 2016 [latest update: #3121] on 15 July 2017 at 10:07:51 pm'!

!UpdatingStringMorph methodsFor: 'stepping' stamp: 'pb 7/15/2017 22:07:40'!
stepAt: millisecondSinceLast

	self contents: (target perform: getSelector) asString! !

