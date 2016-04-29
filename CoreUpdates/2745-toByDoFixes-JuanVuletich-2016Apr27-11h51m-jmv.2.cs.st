'From Cuis 4.2 of 25 July 2013 [latest update: #2743] on 27 April 2016 at 3:06:17.81966 pm'!

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 4/27/2016 15:04'!
to: anEnd by: aDuration do: aBlock
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	| schedule |
	schedule _ self to: anEnd by: aDuration.
	schedule scheduleDo: aBlock.
	"Timespan are open on the right side. But include requested end if appropriate, like implementation in Number
	See DateAndTimeTest>>#testToByDo
	"
	schedule end + DateAndTime clockPrecision = anEnd ifTrue: [
		aBlock value: anEnd ]! !


!Number methodsFor: 'intervals' stamp: 'jmv 4/27/2016 14:47'!
to: stop by: step do: aBlock 
	"Normally compiled in-line, and therefore not overridable.
	Evaluate aBlock for each element of the interval (self to: stop by: step).

	Warning: Might fail with Float numbers. For instance:
		0 to: 2.4 by: 0.1 do: [ :each | each print ]
	is wrong, while
		(0 to: 2.4 by: 0.1) do: [ :each | each print ]
	is right. Prefer explicit Interval creation if using Floats!!
	"
	| nextValue |
	nextValue _ self.
	step = 0 ifTrue: [self error: 'step must be non-zero'].
	step < 0
		ifTrue: [
			[stop <= nextValue]
				whileTrue: [
					aBlock value: nextValue.
					nextValue _ nextValue + step]]
		ifFalse: [
			[stop >= nextValue]
				whileTrue: [
					aBlock value: nextValue.
					nextValue _ nextValue + step]]! !


!Schedule methodsFor: 'enumerating' stamp: 'jmv 4/27/2016 13:02'!
between: aStart and: anEnd do: aBlock

	| element end i requestedStartDate |
	end _ self end min: anEnd.
	element _ self start.
	
	"Performance optimization. Avoid going through unnecesary days if easy."
	requestedStartDate _ aStart date.
	(requestedStartDate start > element and: [ self everyDayAtSameTimes ]) ifTrue: [
		element _ DateAndTime date: requestedStartDate time: element time ].

	i _ 1.
	[ element < aStart ] whileTrue: [
		element _ element + (schedule at: i).
		i _ i + 1.
		i > schedule size ifTrue: [i _ 1]].
	i _ 1.
	[ element <= end ] whileTrue: [
		aBlock value: element.
		element _ element + (schedule at: i).
		i _ i + 1.
		i > schedule size ifTrue: [i _ 1]]! !

