'From Cuis 5.0 of 7 November 2016 [latest update: #3528] on 18 December 2018 at 12:54:27 pm'!
'Description Please enter a description for this package'!
!provides: 'Statistics' 1 12!
!requires: 'Pen' 1 0 nil!
SystemOrganization addCategory: #Statistics!


!classDefinition: #UnitaryMonotonicMap category: #Statistics!
FloatArray variableWordSubclass: #UnitaryMonotonicMap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Statistics'!
!classDefinition: 'UnitaryMonotonicMap class' category: #Statistics!
UnitaryMonotonicMap class
	instanceVariableNames: ''!

!classDefinition: #Histogram category: #Statistics!
Object subclass: #Histogram
	instanceVariableNames: 'tallies extraBinsForClampedValues lowBinCenter highBinCenter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Statistics'!
!classDefinition: 'Histogram class' category: #Statistics!
Histogram class
	instanceVariableNames: ''!


!UnitaryMonotonicMap commentStamp: '<historical>' prior: 0!
We need a better name for this!!
My instances are continuous monotonic increasing functions. Domain is [0..1]. Function values are also in [0..1]. f(0) = 0. f(1) = 1. They have inverse, and the inverse has the same properties.!

!Histogram commentStamp: '<historical>' prior: 0!
Histograms to be used for statistical purposes. Useful, for example, for many image processing algorithms. See http://en.wikipedia.org/wiki/Histogram .
Counts are currently held in a WordArray, and limited to about 4 billion counts (32 bit unsigned integers)

Should replace classes ImageHistogram, ImageHistogram2, ImageHistogram3 and AccumulatedImageHistogram in package SignalProcessing. Keep working towards that!!!!!!

A good number of bins can be n^(1/2) or n^(1/3), where n is the total number of samples. See Wikipedia article for rationale.!

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

!Histogram methodsFor: 'building' stamp: 'jmv 10/14/2016 16:44:01'!
zeroCountAt: aNumber

	| i |
	i _ self indexFor: aNumber.
	tallies at: i put: 0! !

!Histogram methodsFor: 'statistics' stamp: 'jmv 7/19/2016 10:08:17'!
average
	^self mean! !

!Histogram methodsFor: 'statistics' stamp: 'jmv 7/19/2016 10:17:07'!
max
	^ self percentile: 1! !

!Histogram methodsFor: 'statistics' stamp: 'jmv 7/19/2016 12:48:15'!
mean
	"Computing statistics from histograms instead of collections or images is much faster.
	Precision is good if histogram bins reflect accurately population values.

	FloatImage lena histogram percentile: 0.5. 0.50390625.
	FloatImage lena histogram median. 0.50390625.
	FloatImage lena histogram mean. 0.487265408039093.
	FloatImage lena histogram mode. 0.607421875.

	FloatImage lena elements mean. 0.487265408039093.
	"
	| count sum countForThisSlot |
	sum _ 0.
	count _ 0.
	1 to: tallies size do: [ :i |
		countForThisSlot _ tallies at: i.
		sum _ (self binCenter: i) * countForThisSlot + sum.
		count _ count + countForThisSlot ].
	^ sum asFloat / count! !

!Histogram methodsFor: 'statistics' stamp: 'jmv 7/19/2016 10:04:06'!
median
	"Computing statistics from histograms instead of collections or images is much faster.
	Precision is good if histogram bins reflect accurately population values.

	FloatImage lena histogram percentile: 0.5. 0.50390625.
	FloatImage lena histogram median. 0.50390625.
	FloatImage lena histogram mean. 0.487265408039093.
	FloatImage lena histogram mode. 0.607421875.

	FloatImage lena elements median. 0.50390625.
	"
	^ self percentile: 0.5! !

!Histogram methodsFor: 'statistics' stamp: 'jmv 7/19/2016 10:23:38'!
min
	^ self percentile: 0! !

!Histogram methodsFor: 'statistics' stamp: 'jmv 7/19/2016 12:48:18'!
mode
	"Computing statistics from histograms instead of collections or images is much faster.
	Precision is good if histogram bins reflect accurately population values.

	FloatImage lena histogram percentile: 0.5. 0.50390625.
	FloatImage lena histogram median. 0.50390625.
	FloatImage lena histogram mean. 0.487265408039093.
	FloatImage lena histogram mode. 0.607421875.
	"

	^ self binCenter: tallies indexOfMax! !

!Histogram methodsFor: 'statistics' stamp: 'jmv 7/19/2016 12:47:03'!
percentile: percentile
	"Computing statistics from histograms instead of collections or images is much faster.
	Precision is good if histogram bins reflect accurately population values.

	percentile in [0.0 .. 1.0]

	FloatImage lena histogram percentile: 0.0. 0.0546875 .
	FloatImage lena histogram percentile: 0.1. 0.2109375 .
	FloatImage lena histogram percentile: 0.5. 0.50390625 .
	FloatImage lena histogram percentile: 0.9. 0.734375 .
	FloatImage lena histogram percentile: 1.0. 0.9609375 .

	FloatImage lena elements percentile: 0.0. 0.0546875 .
	FloatImage lena elements percentile: 0.1. 0.2109375 .
	FloatImage lena elements percentile: 0.5. 0.50390625 .
	FloatImage lena elements percentile: 0.9. 0.734375 .
	FloatImage lena elements percentile: 1.0. 0.9609375 .
	"
	^ self accumulated basicPercentile: percentile! !

!Histogram methodsFor: 'statistics' stamp: 'jmv 7/19/2016 14:12:34'!
standardDeviation
	"Computing statistics from histograms instead of collections or images is much faster.
	Precision is good if histogram bins reflect accurately population values

	FloatImage lena histogram standardDeviation. 0.1824342268042488.

	FloatImage lena elements standardDeviation. 0.1824342268043369.
	This implementation is optimized, and a bit faster than #standardDeviationAlt
	"
	| count sum countForThisSlot mean |
	mean _ self mean.
	sum _ 0.
	count _ 0.
	1 to: tallies size do: [ :i |
		countForThisSlot _ tallies at: i.
		sum _ (self binCenter: i) squared * countForThisSlot + sum.
		count _ count + countForThisSlot ].
	^ ((sum asFloat / count) - mean squared) sqrt! !

!Histogram methodsFor: 'statistics' stamp: 'jmv 7/19/2016 14:12:06'!
standardDeviationAlt
	"Computing statistics from histograms instead of collections or images is much faster.
	Precision is good if histogram bins reflect accurately population values

	FloatImage lena histogram standardDeviation. 0.1824342268042488.

	FloatImage lena elements standardDeviation. 0.1824342268043369.
	
	This implementation follows the SD definition directly but is slightly slower.
	"
	| count sum countForThisSlot mean |
	mean _ self mean.
	sum _ 0.
	count _ 0.
	1 to: tallies size do: [ :i |
		countForThisSlot _ tallies at: i.
		sum _ ((self binCenter: i) - mean) squared * countForThisSlot + sum.
		count _ count + countForThisSlot ].
	^ (sum asFloat / count) sqrt! !

!Histogram methodsFor: 'statistics' stamp: 'jmv 7/19/2016 12:48:22'!
sum
	"Computing statistics from histograms instead of collections or images is much faster.
	Precision is good if histogram bins reflect accurately population values.
	"
	| sum countForThisSlot |
	sum _ 0.
	1 to: tallies size do: [ :i |
		countForThisSlot _ tallies at: i.
		sum _ (self binCenter: i) * countForThisSlot + sum ].
	^ sum! !

!Histogram methodsFor: 'private' stamp: 'jmv 7/19/2016 10:03:36'!
basicIndexForPercentile: percentile
	"Only meaningful for the accumulated histogram of a collection (such as a FloatImage)
	percentile in [0.0 .. 1.0]

	FloatImage lena histogram percentile: 0.0. 0.0546875.
	FloatImage lena histogram percentile: 0.1. 0.2109375 .
	FloatImage lena histogram percentile: 0.5. 0.50390625 .
	FloatImage lena histogram percentile: 0.9. 0.734375 .
	FloatImage lena histogram percentile: 1.0. 0.9609375 .
	"
	| samplesToTheLeft indexForMedianOrRightSample indexForLeftSample accumulatedBefore |

	samplesToTheLeft _ tallies last * percentile.
	indexForMedianOrRightSample _ tallies quickFindFirst: [ :x | x > samplesToTheLeft ].

	"Percentile in first slot"
	indexForMedianOrRightSample = 1 ifTrue: [
		^ 1 ].

	"Percentile in last occupied slot"
	indexForMedianOrRightSample = 0 ifTrue: [
		^ tallies quickFindFirst: [ :x | x >= samplesToTheLeft ] ].
	
	accumulatedBefore _ tallies at: indexForMedianOrRightSample - 1.

	"Percentile In this slot."
	accumulatedBefore < samplesToTheLeft ifTrue: [
		^ indexForMedianOrRightSample ].

	"Percentile is smallest sample, i.e. first occupied slot.."
	samplesToTheLeft = 0 ifTrue: [
		^ indexForMedianOrRightSample ].

	"Between slots. Look for last slot with actual values, to the left of indexForMedianOrRightSample"
	indexForLeftSample _ tallies quickFindFirst: [ :x | x >= accumulatedBefore ].

	"And answer mean of them."
	^indexForMedianOrRightSample + indexForLeftSample * 0.5! !

!Histogram methodsFor: 'private' stamp: 'jmv 7/19/2016 12:51:18'!
basicPercentile2: percentile
	"Only meaningful for the accumulated histogram of a collection (such as a FloatImage)
	percentile in [0.0 .. 1.0]
	
	See http://math.stackexchange.com/questions/879052/how-to-find-mean-and-median-from-histogram
	Theorically better than #basicPercentile:
	Actually better if possible values any Float (conceptually infinite possible values per bin), or value quantization much finer than bin width.
	With limited depth images (for example, just 256 possible values) a well chosen bin width together with #basicPercentile: gives exact (and therefore better!!) results.
	"

	| m c fm fm_1 lm n2 |
	m _ self basicIndexForPercentile: percentile.
	n2 _ tallies last * percentile.
	fm_1 _ m > 1 ifTrue: [tallies at: m-1] ifFalse: [0].
	fm _ (tallies at: m) - fm_1.
	lm _ (self binCenter: m) + (self binCenter: m-1) * 0.5.
	c _ (highBinCenter -lowBinCenter ) / (tallies size-1).
	^ lm + ((n2-fm_1/fm)*c)! !

!Histogram methodsFor: 'private' stamp: 'jmv 12/18/2018 09:21:14'!
basicPercentile: percentile
	"Only meaningful for the accumulated histogram of a collection (such as a FloatImage)
	percentile in [0.0 .. 1.0]
	"

	^ self binCenter: (self basicIndexForPercentile: percentile)! !

!Histogram methodsFor: 'private' stamp: 'jmv 5/25/2018 10:48:33'!
indexFor: aValue
	| slotIndex |
	tallies size = 1 ifTrue: [ ^1 ].
	slotIndex _ ((aValue - lowBinCenter / (highBinCenter -lowBinCenter )) * (tallies size-1)) rounded + 1.
	^slotIndex min: tallies size max: 1.! !

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

!Histogram methodsFor: 'private' stamp: 'jmv 3/7/2018 10:26:33'!
talliesCount

	^ tallies size! !

!Histogram methodsFor: 'accessing' stamp: 'jmv 12/18/2018 09:49:28'!
binCenter: binIndex
	"Answer the value for the center of the slot"
	| fraction |
	fraction _ tallies size = 1
		ifFalse: [ (binIndex-1) / (tallies size-1) ]
		ifTrue: [ 1/2 ].
	^ fraction * (highBinCenter - lowBinCenter ) + lowBinCenter.! !

!Histogram methodsFor: 'accessing' stamp: 'jmv 12/18/2018 10:03:40'!
binSize
	"Answer the number of bins"

	^tallies size! !

!Histogram methodsFor: 'accessing' stamp: 'jmv 12/18/2018 09:51:14'!
binWidth
	"Answer the width for each bin"

	^highBinCenter - lowBinCenter / (tallies size-1)! !

!Histogram methodsFor: 'accessing' stamp: 'jmv 4/10/2015 16:11'!
countAt: value
	^tallies at: (self indexFor: value)! !

!Histogram methodsFor: 'accessing' stamp: 'jmv 7/9/2017 20:00:50'!
counts
	^tallies! !

!Histogram methodsFor: 'accessing' stamp: 'jmv 7/9/2017 19:58:20'!
domain
	^ (1 to: tallies size) collect: [ :i | self binCenter: i ]! !

!Histogram methodsFor: 'accessing' stamp: 'jmv 12/18/2018 09:52:36'!
halfBinWidth
	"Answer the half the width for each bin.
	This is the maximum error for statistics such as mode, median and percentiles."

	^self binWidth / 2! !

!Histogram methodsFor: 'aux' stamp: 'jmv 3/7/2018 10:59:07'!
jetPlotOn: aForm y: y

	| count w max |
	w _ aForm width.
	count _ tallies size.
	max _ tallies max.
	1 to: w do: [ :j |
		aForm colorAt: j-1 @ y put: (Color jet: (tallies at: (j * count / w) ceiling) / max) ]! !

!Histogram methodsFor: 'aux' stamp: 'jmv 5/24/2018 09:00:29'!
percentile: percentile plotOn: aForm y: y
	"
	Assume percentile in [0.0 .. 1.0]
	"
	
	| x |
	x _ (self percentile: percentile) * aForm width.
	0 to: (aForm width / tallies size) ceiling do: [ :xx |
		aForm colorAt: x+xx @ y put: Color black ]! !

!Histogram methodsFor: 'aux' stamp: 'jmv 7/20/2016 15:39:36'!
plot
	| rect |
	rect _ 10@10 extent: 768@400.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	self plotIn: rect color: Color red min: 0.0 max: tallies max! !

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

!Histogram methodsFor: 'aux' stamp: 'jmv 7/18/2016 17:51:06'!
plotLogIn: rect color: aColor min: min max: max
	"Throw-away code just to check out a couple of examples"
	| x dx pen y |
	pen _ Pen new.
	pen color: aColor.
	pen up.
	x _ rect left.
	dx _ rect width asFloat / (tallies size-1).
	tallies do: [ :v |
		y _ v > 0 ifTrue: [(max-v log) / (max-min) * rect height asFloat] ifFalse: [rect height].
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

!Collection methodsFor: '*Statistics' stamp: 'jmv 8/24/2018 09:49:05'!
meanAndStandardDeviation
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} meanAndStandardDeviation
	"

	| meanAndVariance |
	meanAndVariance _ self meanAndVariance.
	^{meanAndVariance first.  meanAndVariance second sqrt}! !

!Collection methodsFor: '*Statistics' stamp: 'jmv 8/24/2018 09:48:43'!
meanAndVariance
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} meanAndVariance
	"

	| sum mean |
	mean _ self mean.
	sum _ self collect: [ :each | (each - mean) squared ] andFold: [ :a :b | a + b ].
	^{mean.  sum / self size}! !

!Collection methodsFor: '*Statistics' stamp: 'jmv 12/18/2018 09:18:58'!
median
	"See comment in SortedCollection.
	Very expensive on large collections. Consider using a Histogram."
	| sorted  center e0 e1 i0 |
	sorted _ self asArray sort.
	center _ sorted size+1 / 2.
	^ center isInteger
		ifTrue: [ sorted at: center ]
		ifFalse: [
			i0 _ center floor.
			e0 _ sorted at: i0.
			e1 _ sorted at: i0+1.
			e0 = e1
				ifTrue: [
					"Useful for collection with well defined order, but no arithmetic"
					e0 ]
				ifFalse: [ e0 + e1 / 2 ]]! !

!Collection methodsFor: '*Statistics' stamp: 'jmv 7/19/2016 12:26:25'!
mode
	"See comment in SortedCollection.
	Very expensive on large collections. Consider using a Histogram."
	^self asBag mode! !

!Collection methodsFor: '*Statistics' stamp: 'jmv 7/19/2016 11:14:02'!
percentile: percentile
	"See comment in SortedCollection.
	Very expensive on large collections. Consider using a Histogram."
	| sorted |
	sorted _ self asArray sort.
	^sorted at: ((sorted size * percentile) ceiling max: 1)! !

!Collection methodsFor: '*Statistics' stamp: 'jmv 7/19/2016 11:15:52'!
sampleStandardDeviation
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} sampleStandardDeviation

	Estimate standard deviation of a large population from a small sample.
	"

	^self sampleVariance sqrt! !

!Collection methodsFor: '*Statistics' stamp: 'jmv 7/19/2016 11:16:03'!
sampleVariance
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} sampleVariance

	Estimate variance of a large population from a small sample.
	"

	| sum mean |
	mean _ self mean.
	sum _ self collect: [ :each | (each - mean) squared ] andFold: [ :a :b | a + b ].
	^sum / (self size-1)! !

!Collection methodsFor: '*Statistics' stamp: 'jmv 8/24/2018 09:47:54'!
standardDeviation
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} standardDeviation
	"

	^ self meanAndVariance second sqrt! !

!Collection methodsFor: '*Statistics' stamp: 'jmv 8/24/2018 09:47:37'!
variance
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} variance
	"

	^ self meanAndVariance second! !

!SortedCollection methodsFor: '*Statistics' stamp: 'jmv 12/18/2018 09:17:32'!
median
	"Return the center element, or the average of 2 center elements (if size is even, there's no center element)."

	| center e0 e1 i0 |
	center _ self size+1 / 2.
	^ center isInteger
		ifTrue: [ self at: center ]
		ifFalse: [
			i0 _ center floor.
			e0 _ self at: i0.
			e1 _ self at: i0+1.
			e0 = e1
				ifTrue: [
					"Useful for collection with well defined order, but no arithmetic"
					e0 ]
				ifFalse: [ e0 + e1 / 2 ]]! !

!Bag methodsFor: '*Statistics' stamp: 'jmv 7/19/2016 12:29:42'!
mode
	| count maxRepeated maxRepeatedCount |
	maxRepeated _ nil.
	maxRepeatedCount _ 0.
	contents contents associationsDo: [ :assoc |
		count _ assoc value.
		count > maxRepeatedCount ifTrue: [
			maxRepeated _ assoc key.
			maxRepeatedCount _ count ]].
	^maxRepeated! !
