'From Cuis 5.0 of 7 November 2016 [latest update: #3260] on 23 February 2018 at 3:00:53 pm'!

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 2/23/2018 15:00:36'!
selectAll: aKeyboardEvent 
	"select everything, invoked by cmd-a.  1/17/96 sw"

	self selectAll.
	^ true! !

!methodRemoval: TextEditor #selectAll:!
TextEditor removeSelector: #selectAll:!
