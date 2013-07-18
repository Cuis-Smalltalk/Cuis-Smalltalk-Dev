'From Cuis 4.1 of 12 December 2012 [latest update: #1707] on 29 May 2013 at 9:34:37.186014 am'!

!SequenceableCollection methodsFor: 'nil' stamp: 'jmv 5/28/2013 11:12'!
copyFrom: start count: n
	"Answer a copy of a subset of the receiver, starting from element at 
	index start and of size n."

	^(self species new: n)
		replaceFrom: 1
		to: n
		with: self
		startingAt: start! !

