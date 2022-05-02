'From Cuis 6.0 [latest update: #5117] on 1 May 2022 at 1:28:07 pm'!

!Bag methodsFor: 'enumerating' stamp: 'len 5/1/2022 13:24:53'!
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. Collect the resulting values into a collection that is like the receiver and nswer the new collection."
	| answer |
	answer _ self species new.
	self withOccurrencesDo: [:each :times| answer add: (aBlock value: each) withOccurrences: times].
	^ answer! !

!Bag methodsFor: 'enumerating' stamp: 'len 5/1/2022 13:25:01'!
count: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.  Return the number that answered true."
	| count |
	count _ 0.
	self withOccurrencesDo: [:each :times| (aBlock value: each) ifTrue: [count _ count + times]].
	^ count! !

!Bag methodsFor: 'enumerating' stamp: 'len 5/1/2022 13:22:14'!
withOccurrencesDo: aBlock
	contents associationsDo: [:each| aBlock value: each key value: each value]! !


!Collection methodsFor: 'enumerating' stamp: 'len 5/1/2022 13:25:40'!
count: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.  Return the number that answered true."
	| count |
	count _ 0.
	self do: [:each| (aBlock value: each) ifTrue: [count _ count + 1]].
	^ count! !

