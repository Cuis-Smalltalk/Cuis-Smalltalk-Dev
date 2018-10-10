'From Cuis 5.0 of 7 November 2016 [latest update: #3476] on 10 October 2018 at 3:58:36 pm'!

!Float methodsFor: 'comparing' stamp: 'jmv 10/10/2018 15:53:22'!
hash
	"Hash is reimplemented because = is implemented. Both words of the float are used. (The bitShift:'s ensure that the intermediate results do not become a large integer.) Care is taken to answer same hash as an equal Integer."

	| asInteger |
	self isFinite ifTrue: [
		asInteger _ self truncated.
		(asInteger = self and: [ asInteger abs < Float maxExactInteger ]) ifTrue: [
			^ asInteger hash ]].
	^ ((self basicAt: 1) bitShift: -4) +
	   ((self basicAt: 2) bitShift: -4)! !

