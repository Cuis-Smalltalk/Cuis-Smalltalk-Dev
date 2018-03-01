'From Cuis 5.0 of 7 November 2016 [latest update: #3263] on 1 March 2018 at 4:59:38 pm'!

!HandMorph methodsFor: 'private events' stamp: 'jmv 3/1/2018 16:59:14'!
shouldControlEmulateAltFor: keyValue
	"At least on Linux Windows, command key is usually ctrl, not alt.
	But not for arrow keys!! ctrl-left ~~ alt-left"
	^ keyValue > 32! !

