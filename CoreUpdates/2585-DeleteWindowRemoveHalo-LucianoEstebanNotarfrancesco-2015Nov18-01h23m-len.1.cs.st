'From Cuis 4.2 of 25 July 2013 [latest update: #2583] on 18 November 2015 at 1:25:13.763581 am'!

!SystemWindow methodsFor: 'open/close' stamp: 'len 11/17/2015 22:25'!
delete
	| thisWorld |
	self okToChange ifFalse: [^self].
	thisWorld _ self world.
	SystemWindow noteTopWindowIn: thisWorld but: self.
	self sendToBack.
	self removeHalo.
	super delete.
	model _ nil.
	Theme current windowClosed: self! !

