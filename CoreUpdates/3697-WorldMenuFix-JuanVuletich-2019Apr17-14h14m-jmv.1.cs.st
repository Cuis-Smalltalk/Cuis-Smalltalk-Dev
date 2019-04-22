'From Cuis 5.0 of 7 November 2016 [latest update: #3696] on 17 April 2019 at 2:14:28 pm'!

!PasteUpMorph methodsFor: 'events' stamp: 'jmv 4/17/2019 14:13:14'!
click: aMouseButtonEvent localPosition: localEventPosition
	^self whenUIinSafeState: [self mouseButton2Activity]! !

