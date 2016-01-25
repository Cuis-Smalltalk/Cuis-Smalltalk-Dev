'From Cuis 4.2 of 25 July 2013 [latest update: #2355] on 9 June 2015 at 10:08:11.439162 am'!
'Description Please enter a description for this package'!
!provides: 'Probabilities-and-Statistics' 1 7!
!requires: 'Goodies' 1 0!
!classDefinition: #UnitaryMonotonicMap category: #'Probabilities-and-Statistics'!
FloatArray variableWordSubclass: #UnitaryMonotonicMap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Probabilities-and-Statistics'!
!classDefinition: 'UnitaryMonotonicMap class' category: #'Probabilities-and-Statistics'!
UnitaryMonotonicMap class
	instanceVariableNames: ''!

!classDefinition: #Histogram category: #'Probabilities-and-Statistics'!
Object subclass: #Histogram
	instanceVariableNames: 'tallies extraBinsForClampedValues lowBinCenter highBinCenter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Probabilities-and-Statistics'!
!classDefinition: 'Histogram class' category: #'Probabilities-and-Statistics'!
Histogram class
	instanceVariableNames: ''!

!classDefinition: #ProbabilityDistribution category: #'Probabilities-and-Statistics'!
Object subclass: #ProbabilityDistribution
	instanceVariableNames: 'uniformRandomGenerator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Probabilities-and-Statistics'!
!classDefinition: 'ProbabilityDistribution class' category: #'Probabilities-and-Statistics'!
ProbabilityDistribution class
	instanceVariableNames: ''!

!classDefinition: #ExponentialProbabilityDistribution category: #'Probabilities-and-Statistics'!
ProbabilityDistribution subclass: #ExponentialProbabilityDistribution
	instanceVariableNames: 'lambda'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Probabilities-and-Statistics'!
!classDefinition: 'ExponentialProbabilityDistribution class' category: #'Probabilities-and-Statistics'!
ExponentialProbabilityDistribution class
	instanceVariableNames: ''!

!classDefinition: #NormalProbabilityDistribution category: #'Probabilities-and-Statistics'!
ProbabilityDistribution subclass: #NormalProbabilityDistribution
	instanceVariableNames: 'mean standardDeviation cachedValue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Probabilities-and-Statistics'!
!classDefinition: 'NormalProbabilityDistribution class' category: #'Probabilities-and-Statistics'!
NormalProbabilityDistribution class
	instanceVariableNames: ''!

!classDefinition: #RayleighProbabilityDistribution category: #'Probabilities-and-Statistics'!
ProbabilityDistribution subclass: #RayleighProbabilityDistribution
	instanceVariableNames: 'sigma'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Probabilities-and-Statistics'!
!classDefinition: 'RayleighProbabilityDistribution class' category: #'Probabilities-and-Statistics'!
RayleighProbabilityDistribution class
	instanceVariableNames: ''!


!UnitaryMonotonicMap commentStamp: '<historical>' prior: 0!
We need a better name for this!!
My instances are continuous monotonic increasing functions. Domain is [0..1]. Function values are also in [0..1]. f(0) = 0. f(1) = 1. They have inverse, and the inverse has the same properties.!

!Histogram commentStamp: '<historical>' prior: 0!
Histograms to be used for statistical purposes. Useful, for example, for many image processing algorithms. See http://en.wikipedia.org/wiki/Histogram .
Counts are currently held in a WordArray, and limited to about 4 billion counts (32 bit unsigned integers)

Should replace classes ImageHistogram, ImageHistogram2, ImageHistogram3 and AccumulatedImageHistogram in package SignalProcessing. Keep working towards that!!

A good number of bins can be n^(1/2) or n^(1/3), where n is the total number of samples. See Wikipedia article for rationale.!

!ProbabilityDistribution commentStamp: '<historical>' prior: 0!
ProbabilityDistributions can also generate random numbers!

!ExponentialProbabilityDistribution commentStamp: 'jmv 4/6/2015 12:18' prior: 0!
| r h |
r _ ExponentialProbabilityDistribution lambda: 3.
h _ Histogram binCount: 500 minValue: 0 maxValue: 120.
100000 timesRepeat: [ h addValue: r nextRandomNumber ].
h plot.!

!NormalProbabilityDistribution commentStamp: 'jmv 4/6/2015 12:19' prior: 0!
| r h |
r _ NormalProbabilityDistribution new.
h _ Histogram binCount: 500 minValue: -5 maxValue: 5.
100000 timesRepeat: [ h addValue: r nextRandomNumber  ].
h plot.

| r h |
r _ NormalProbabilityDistribution mean: 2.5 standardDeviation: 0.5.
h _ Histogram binCount: 500 minValue: -5 maxValue: 5.
100000 timesRepeat: [ h addValue: r nextRandomNumber  ].
h plot.!

!RayleighProbabilityDistribution commentStamp: 'jmv 4/6/2015 12:22' prior: 0!
| r h |
r _ RayleighProbabilityDistribution sigma: 3.
h _ Histogram binCount: 500 minValue: 0 maxValue: 20.
1000000 timesRepeat: [ h addValue: r nextRandomNumber  ].
h plot.!

!UnitaryMonotonicMap methodsFor: 'accessing' stamp: 'jmv 6/3/2015 22:22'!
inverseValueAt: y
	"y in [0.0 .. 1.0]"
	| fracIndex y0 y1 |
	self
		findBinaryIndex: [ :yi | y - yi ]
		do: [ :foundIndex | fracIndex _ foundIndex ]
		ifNone: [ :i0 :i1 |
			y0 _ self at: i0.
			y1 _ self at: i1.
			fracIndex _ i0 interpolateTo: i1 at: y-y0 / (y1-y0) ].
	^fracIndex - 1.0 / (self size-1)! !

!UnitaryMonotonicMap methodsFor: 'accessing' stamp: 'jmv 6/3/2015 22:06'!
valueAt: x
	"x in [0.0 .. 1.0]"
	| i i0 |
	i _ (x * (self size-1)) + 1.
	i0 _ i floor.
	^ i = i0
		ifTrue: [ self at: i0 ]
		ifFalse: [ (self at: i0) interpolateTo: (self at: i0+1) at: i-i0 ]! !

!Histogram methodsFor: 'building' stamp: 'jmv 4/10/2015 16:12'!
accumulate

	2 to: tallies size do: [ :i |
		tallies at: i put: (tallies at: i) + (tallies at: i-1) ]! !

!Histogram methodsFor: 'building' stamp: 'jmv 4/10/2015 16:13'!
accumulated

	^self copy accumulate! !

!Histogram methodsFor: 'building' stamp: 'jmv 6/4/2015 10:53'!
accumulatedAsMap

	| answer |
	answer _ UnitaryMonotonicMap new: tallies size+1.
	answer at: 1 put: 0.0.
	1 to: tallies size do: [ :i |
		answer at: i+1 put: (answer at: i) + (tallies at: i) ].
	answer /= answer last.
	^answer! !

!Histogram methodsFor: 'building' stamp: 'jmv 4/6/2015 11:37'!
addValue: aNumber

	| i |
	i _ self indexFor: aNumber.
	tallies at: i put: (tallies at: i) + 1! !

!Histogram methodsFor: 'accessing' stamp: 'jmv 4/10/2015 16:11'!
countAt: value
	^tallies at: (self indexFor: value)! !

!Histogram methodsFor: 'private' stamp: 'jmv 4/6/2015 11:43'!
indexFor: aValue
	| slotIndex |
	slotIndex _ ((aValue - lowBinCenter / (highBinCenter -lowBinCenter )) * (tallies size-1)) rounded + 1.
	^slotIndex min: tallies size max: 1.! !

!Histogram methodsFor: 'building' stamp: 'jmv 6/4/2015 09:46'!
inverted
	"For accumulated histograms, compute de inverse cummulative function.
	This is not an exact inverse, and is so far an experiment.
	Some tests to ensure good properties, and maybe more work, are in order. "
	| answer answerTallies n i k |
	n _ self regularTalliesCount.
	answerTallies _ WordArray new: n.
	answer _ Histogram basicNew
		tallies: answerTallies
		lowBinCenter: 0
		highBinCenter: n
		extraBinsForClampedValues: false.
	i _ 1.
	k _ 1.0 * n / tallies last.
	1 to: n do: [ :j |
		[ (tallies at: i) * k < j ] whileTrue: [ i _ i + 1 ].
		answerTallies at: j put: i
		].
	^answer! !

!Histogram methodsFor: 'aux' stamp: 'jmv 4/10/2015 15:45'!
plot
	| rect |
	rect _ 10@10 extent: 768@400.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	self plotIn: rect color: Color black min: 0.0 max: tallies max! !

!Histogram methodsFor: 'aux' stamp: 'jmv 4/14/2015 13:01'!
plotIn: rect color: aColor
	self plotIn: rect color: aColor min: 0.0 max: tallies max! !

!Histogram methodsFor: 'aux' stamp: 'jmv 4/10/2015 15:44'!
plotIn: rect color: aColor min: min max: max
	"Throw-away code just to check out a couple of examples"
	| x dx pen y |
	pen _ Pen new.
	pen color: aColor.
	pen up.
	x _ rect left.
	dx _ rect width asFloat / (tallies size-1).
	tallies do: [ :v |
		y _ (max-v) / (max-min) * rect height asFloat.
		y _ y min: rect height.
		y _ y max: 0.
		pen goto: x asInteger @ (rect top + y asInteger).
		pen down.
		x _ x + dx].
	max printString displayOn: Display at: (x+2) @ (rect top-9).
	min printString displayOn: Display at: (x+2) @ (rect bottom - 9)! !

!Histogram methodsFor: 'copying' stamp: 'jmv 4/10/2015 16:16'!
postCopy
	tallies _ tallies copy! !

!Histogram methodsFor: 'private' stamp: 'jmv 4/6/2015 11:18'!
regularTalliesCount

	^ extraBinsForClampedValues
		ifTrue: [ tallies size - 2 ]
		ifFalse: [ tallies size ]! !

!Histogram methodsFor: 'private' stamp: 'jmv 4/6/2015 11:21'!
tallies: anArray lowBinCenter: aNumber highBinCenter: anotherNumber extraBinsForClampedValues: aBoolean
	tallies _ anArray.
	lowBinCenter _ aNumber.
	highBinCenter _ anotherNumber.
	extraBinsForClampedValues _ aBoolean! !

!Histogram class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 11:57'!
binCount: binCount lowBinCenter: lowBinCenter highBinCenter: highBinCenter
	"lowBinCenter is the center value of the lowest bin.
	highBinCenter is the center value of the highest bin.
	
	Values lower than the lower bound of the first bin go in the fist bin,
	and values higher or equal than the upper bound of the last bin go in the last bin.
		(i.e., out of range values are simply clamped)
		
	For example, the following will give a histogram where first and last bins have half the counts as all the rest:
		| h r |
		h _ Histogram binCount: 10 lowBinCenter: 0 highBinCenter: 100.
		r _ Random new.
		10000 timesRepeat: [ h addValue: r next * 100  ].
		h plot
		
	Maybe what we wanted was:
		| h r |
		h _ Histogram binCount: 10 lowBinCenter: 5 highBinCenter: 95.
		r _ Random new.
		10000 timesRepeat: [ h addValue: r next  * 100 ].
		h plot
	
	Or more simply
		| h r |
		h _ Histogram binCount: 10 minValue: 0 maxValue: 100.
		r _ Random new.
		10000 timesRepeat: [ h addValue: r next  * 100 ].
		h plot
"

	^self basicNew
		tallies: (WordArray new: binCount)
		lowBinCenter: lowBinCenter
		highBinCenter: highBinCenter
		extraBinsForClampedValues: false! !

!Histogram class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 11:55'!
binCount: binCount minValue: min maxValue: max
	"min is the lower bound of the lowest bin.
	max is the upper bound of the highest bin.
	
	Values lower than min go in the fist bin, and values higher or equal than max go in the last bin.
		(i.e., out of range values are simply clamped)

		| h r |
		h _ Histogram binCount: 10 minValue: 0.0 maxValue: 1.0.
		r _ Random new.
		10000 timesRepeat: [ h addValue: r next  ].
		h plot

	"

	^self binCount: binCount minValue: min maxValue: max extraBinsForClampedValues: false! !

!Histogram class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 11:56'!
binCount: regularBinCount minValue: min maxValue: max extraBinsForClampedValues: aBoolean
	"min is the lower bound of the lowest bin.
	max is the upper bound of the highest bin.
	
	If aBoolean is false, values lower than min go in the fist bin, and values higher or equal than max go in the last bin.
		(i.e., out of range values are simply clamped)
	If aBoolean, then extra bins are added at each end for these 'out of range' values

		| h r |
		h _ Histogram binCount: 10 minValue: 0.0 maxValue: 1.0 extraBinsForClampedValues: true.
		r _ Random new.
		10000 timesRepeat: [ h addValue: r next  ].
		h plot
	"

	| tallies highBinCenter lowBinCenter halfBinWidth |
	halfBinWidth _ max - min / regularBinCount / 2.0.
	aBoolean
		ifTrue: [
			tallies _ WordArray new: regularBinCount + 2.
			lowBinCenter _ min - halfBinWidth.
			highBinCenter _ max + halfBinWidth ]
		ifFalse: [
			tallies _ WordArray new: regularBinCount.
			lowBinCenter _ min + halfBinWidth.
			highBinCenter _ max - halfBinWidth ].
	^self basicNew
		tallies: tallies
		lowBinCenter: lowBinCenter
		highBinCenter: highBinCenter
		extraBinsForClampedValues: aBoolean! !

!ProbabilityDistribution methodsFor: 'accessing' stamp: 'jmv 4/6/2015 12:03'!
uniformRandomGenerator
	^uniformRandomGenerator ifNil: [ uniformRandomGenerator _ Random new ]! !

!ExponentialProbabilityDistribution methodsFor: 'initialization' stamp: 'jmv 4/6/2015 12:16'!
lambda: aNumber

	lambda _ aNumber! !

!ExponentialProbabilityDistribution methodsFor: 'random number generation' stamp: 'jmv 4/6/2015 12:16'!
nextRandomNumber
	"Answer the next value"

	^(1 / (1 - self uniformRandomGenerator next)) ln * 2 * lambda squared! !

!ExponentialProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:14'!
lambda: lambda
	^self basicNew lambda: lambda! !

!ExponentialProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:15'!
new
	^self lambda: 1.0! !

!NormalProbabilityDistribution methodsFor: 'initialization' stamp: 'jmv 4/6/2015 12:08'!
mean: mu standardDeviation: sigma
	mean _ mu.
	standardDeviation _ sigma! !

!NormalProbabilityDistribution methodsFor: 'random number generation' stamp: 'jmv 4/6/2015 12:06'!
nextRandomNumber
	"Two random values are generated. So, answer one and cache the other."

	^self nextStandardNormalRandomNumber * standardDeviation  + mean! !

!NormalProbabilityDistribution methodsFor: 'random number generation' stamp: 'jmv 4/6/2015 12:06'!
nextStandardNormalRandomNumber
	"Two random values are generated. So, answer one and cache the other."

	| x1 x2 t1 t2 y1 y2 |
	cachedValue notNil
		ifTrue: [
			y1 _ cachedValue.
			cachedValue _ nil.
			^ y1 ].
	x1 _ self uniformRandomGenerator next.
	x2 _ self uniformRandomGenerator next.
	t1 _ (-2 * x1 ln) sqrt.
	t2 _ 2 * Float pi * x2.
	y1 _ t1 * t2 cos.
	y2 _ t1 * t2 sin.
	cachedValue _ y2.
	^ y1! !

!NormalProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:08'!
mean: mu standardDeviation: sigma
	^self basicNew mean: mu standardDeviation: sigma! !

!NormalProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:09'!
new
	^self mean: 0.0 standardDeviation: 1.0! !

!RayleighProbabilityDistribution methodsFor: 'random number generation' stamp: 'jmv 4/6/2015 12:21'!
nextRandomNumber
	"Answer the next value"

	^((1 / (1 - self uniformRandomGenerator next)) ln * 2 * sigma squared) sqrt! !

!RayleighProbabilityDistribution methodsFor: 'initialization' stamp: 'jmv 4/6/2015 12:21'!
sigma: aNumber

	sigma _ aNumber! !

!RayleighProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:20'!
new
	^self sigma: 1.0! !

!RayleighProbabilityDistribution class methodsFor: 'instance creation' stamp: 'jmv 4/6/2015 12:20'!
sigma: sigma
	^self basicNew sigma: sigma! !
