'From Cuis7.7 [latest update: #7963] on 25 May 2026 at 11:22:15 am'!

!SequenceableCollection methodsFor: 'enum. subcollections' stamp: 'jmv 25/May/2026 11:21:15'!
groupsOf: n atATimeDo: aBlock 
	"Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end.
	Allows use of a flattened 
	array for things that naturally group into groups of n.
	If aBlock has a single argument, and groups are at least of size 2, pass it 
	an array of n items, otherwise, pass the items as separate arguments.
	See also pairsDo:"
	| passArray args |
	passArray := aBlock numArgs = 1.
	passArray ifFalse: [
		args := Array new: n ].
	n
		to: self size
		by: n
		do: [ :index |
			passArray
				ifTrue: [
					aBlock value: (self copyFrom: index - n + 1 to: index) asArray ]
				ifFalse: [
					args  replaceFrom: 1 to: n with: self startingAt: index - n + 1.
					aBlock valueWithArguments: args ]].! !

