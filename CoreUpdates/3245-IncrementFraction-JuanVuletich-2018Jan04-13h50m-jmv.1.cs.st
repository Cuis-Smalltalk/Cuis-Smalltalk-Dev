'From Cuis 5.0 of 7 November 2016 [latest update: #3244] on 4 January 2018 at 3:38:22 pm'!

!SequenceableCollection methodsFor: 'math functions' stamp: 'jmv 1/4/2018 15:37:17'!
incrementFraction
	"
	#(10 12.5 15 20) incrementFraction
	"
	| displaced answer |
	displaced _ self class new: self size.
	displaced replaceFrom: 2 to: self size with: self startingAt: 1.
	displaced at: 1 put: self first.
	answer _ self copy.
	answer -= displaced.
	^answer / displaced! !

