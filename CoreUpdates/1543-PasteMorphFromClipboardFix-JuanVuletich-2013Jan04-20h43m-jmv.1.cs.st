'From Cuis 4.1 of 12 December 2012 [latest update: #1539] on 4 January 2013 at 8:45:09 pm'!

!HandMorph methodsFor: 'paste buffer' stamp: 'jmv 1/4/2013 15:53'!
pasteMorph

	| pastee |
	pastee _ Clipboard retrieveMorph.
	pastee ifNil: [^ self inform: 'Nothing to paste.'].
	self attachMorph: pastee! !

