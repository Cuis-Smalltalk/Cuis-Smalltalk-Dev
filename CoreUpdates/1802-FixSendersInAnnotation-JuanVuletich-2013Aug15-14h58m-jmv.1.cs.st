'From Cuis 4.2 of 25 July 2013 [latest update: #1801] on 15 August 2013 at 2:58:42.122311 pm'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 8/14/2013 11:19'!
selectedSelector
	"Try to make a selector out of the current text selection"
	"Error handler needed if you do alt-n on a method annotation"
	^[ self selection string findSelector ] on: Error do: []! !

