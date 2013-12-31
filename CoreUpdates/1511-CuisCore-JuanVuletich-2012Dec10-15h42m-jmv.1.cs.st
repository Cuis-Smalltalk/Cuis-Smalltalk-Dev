'From Cuis 4.0 of 21 April 2012 [latest update: #1510] on 10 December 2012 at 3:42:56 pm'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'jmv 11/20/2012 16:20'!
indexOfMax
	"Answer the index of the maximum value in me."
	
	| answer max e |
	max _ self at: 1.
	answer _ 1.
	2 to: self size do: [ :i |
		e _ self at: i.
		e > max ifTrue: [
			max _ e.
			answer _ i ]].
	^answer! !

