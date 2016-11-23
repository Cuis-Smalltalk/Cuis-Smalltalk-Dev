'From Cuis 5.0 of 7 November 2016 [latest update: #2974] on 21 November 2016 at 12:51:20 pm'!

!Collection methodsFor: 'statistics' stamp: 'pb 11/21/2016 12:50:03'!
product
	"Compute the product of all the elements in the receiver"
	^ self
		inject: 1
		into: [ :sub :next |
			sub * next ].! !

!Collection methodsFor: 'statistics' stamp: 'pb 11/21/2016 12:50:20'!
sampleVariance
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} sampleVariance
	"
	| sum mean |
	mean := self mean.
	sum := self
		inject: 0
		into: [ :sub :next |
			sub + (next - mean) squared ].
	^ sum / (self size - 1).! !

!Collection methodsFor: 'statistics' stamp: 'pb 11/21/2016 12:50:36'!
sum
	"Compute the sum of all the elements in the receiver"
	^ self
		inject: 0
		into: [ :sub :next |
			sub + next ].! !

!Collection methodsFor: 'statistics' stamp: 'pb 11/21/2016 12:51:07'!
variance
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} variance
	"
	| sum mean |
	mean := self mean.
	sum := self
		inject: 0
		into: [ :sub :next |
			sub + (next - mean) squared ].
	^ sum / self size.! !

