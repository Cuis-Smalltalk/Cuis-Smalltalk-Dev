'From Cuis 4.0 of 21 April 2012 [latest update: #1367] on 14 August 2012 at 5:09:45 pm'!

!Symbol methodsFor: 'accessing' stamp: 'jmv 8/14/2012 17:09'!
precedence
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise.  The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	| c |
	self size = 0 ifTrue: [^ 0].
	"Consider selectors starting with an underscore $_ as unary, even if Preferences allowUnderscoreSelectors is not set."
	c _ self first.
	(c isLetter not and: [ c ~= $_ ]) ifTrue: [^ 2].
	self last = $: ifTrue: [^ 3].
	^ 1! !

