'From Cuis 5.0 [latest update: #4620] on 31 May 2021 at 4:40:37 pm'!

!Form methodsFor: 'converting' stamp: 'jmv 5/31/2021 11:07:39'!
as8BitStandardPaletteColorForm
	"Simple conversion of zero pixels to transparent. Force it to 8 bits."

	| f |
	f _ ColorForm extent: self extent depth: 8.
	f colors: Color indexedColors copy.
	self displayOn: f at: self offset negated.
	f offset: self offset.
	^ f! !

!Form methodsFor: 'converting' stamp: 'jmv 5/31/2021 16:28:18'!
asColorFormOfDepth: destDepth
	"
	Answer a ColorForm with a custom optimized palette of up to 256, 16 or 4 entries.
	self runningWorld backgroundImage asColorFormOfDepth: 8 :: display
	self runningWorld backgroundImage orderedDither32To16 asColorFormOfDepth: 8 :: display
	self runningWorld backgroundImage orderedDither32To16 asColorFormOfDepth: 4 :: display
	"
	| answer bitsPerColor clusterIndexToSplit clusterToSplit clusters colors desiredNumberOfClusters errors firstCluster map metricToSplitOn tally valueToSplitOn box hueWeightFactor brightnessWeightFactor saturationWeightFactor |
	self depth > 8 ifFalse: [
		^self error: 'Only for 16 bit and 32 bit Forms' ].
	desiredNumberOfClusters _ 1 bitShift: destDepth.

	"Wheights to balance error on each color metric"
	hueWeightFactor _ 8.0 / 360.0.
	saturationWeightFactor _ 1.0.
	brightnessWeightFactor _ 3.0.
	destDepth < 8 ifTrue: [
		brightnessWeightFactor _ 13.0.
		destDepth < 4 ifTrue: [
			hueWeightFactor _ 0.0.
			saturationWeightFactor _ 0.0. ]].
	"Assign all pixels to a single cluster"
	tally _ self tallyPixelValues.
	firstCluster _ OrderedCollection new.
	tally withIndexDo: [ :pixelCount :pixelValuePlusOne |
		pixelCount = 0 ifFalse: [ |c|
			c _ Color colorFromPixelValue: pixelValuePlusOne -1 depth: 16.
			firstCluster add: {c hue. c saturation. c brightness. pixelCount. pixelValuePlusOne } ]].
	clusters _ OrderedCollection with: firstCluster.

	"Pick the existing cluster with the largest error, and split it."
	[clusters size < desiredNumberOfClusters and: [ clusters anySatisfy: [:eachCluster | eachCluster size > 1]]] whileTrue: [ | cluster1 cluster2 |
		"Detect cluster with largest error, and split it"
		errors _ clusters collect: [ :eachCluster | | sum average error |
			sum _ (eachCluster sum: [ :e | {e first * e fourth. e second * e fourth. e third * e fourth. e fourth }]).
			average _ {sum first. sum second. sum third} / sum fourth.
			error _ eachCluster sum: [ :e | | hueError |
				hueError _ (e first - average first) abs. hueError > (360/2) ifTrue: [hueError _ 360 - hueError].
				{hueError * hueWeightFactor. (e second-average second) abs * saturationWeightFactor. (e third-average third) abs * brightnessWeightFactor} * e fourth ].
			{error max. error indexOfMax. average at: error indexOfMax} ].
		clusterIndexToSplit _ (errors collect: [ :e | e first]) indexOfMax.
		metricToSplitOn _ (errors at: clusterIndexToSplit) second.
		valueToSplitOn _ (errors at: clusterIndexToSplit) third.
		clusterToSplit _ clusters at: clusterIndexToSplit.
		cluster1 _ OrderedCollection new.
		cluster2 _ OrderedCollection new.
		clusterToSplit do: [ :pixelMetricsAndCount |
			(pixelMetricsAndCount at: metricToSplitOn) < valueToSplitOn
				ifTrue: [cluster1 add: pixelMetricsAndCount]
				ifFalse: [cluster2 add: pixelMetricsAndCount]].
		clusters at: clusterIndexToSplit put: cluster1.
		clusters add: cluster2.
	].

	colors _ clusters collect: [ :eachCluster | | sum average |
		average _ eachCluster average.
			sum _ (eachCluster sum: [ :a | {a first * a fourth. a second * a fourth. a third * a fourth. a fourth }]).
			average _ {sum first. sum second. sum third} / sum fourth.
		Color hue: average first saturation: average second brightness: average third ].
	answer _ ColorForm extent: self extent depth: destDepth.
	answer colors: colors.

	"Build colormap for displaying self on answer"
	bitsPerColor _ 5.				"To read 16bpp source"
	map _ Bitmap new: (1 bitShift: 3*bitsPerColor).
	clusters withIndexDo: [ :eachCluster :clusterIndex |
		eachCluster do: [ :pixMetrixsAndCount |
			map at: pixMetrixsAndCount fifth put: clusterIndex-1 ]].

	box _ self boundingBox.
	answer copyBits: box from: self at: 0@0 clippingBox: box rule: Form over map: map.
	^answer
! !


!Form methodsFor: 'converting' stamp: 'jmv 5/31/2021 16:27:21'!
as8BitColorForm
	"Answer an 8 bit ColorForm with an optimized palette with up to 256 entries."

	^self asColorFormOfDepth: 8! !


!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 5/31/2021 16:36:26'!
buildMagnifiedBackgroundImage
	| image |
	backgroundImageData
		ifNil: [ backgroundImage _ nil ]
		ifNotNil: [
			[image _ Form fromBinaryStream: backgroundImageData readStream.
			backgroundImage _ image magnifyTo: extent.
			backgroundImage _ backgroundImage orderedDither32To16 asColorFormOfDepth: 8.
			] on: Error do: [backgroundImage := nil]. "Can happen if JPEG plugin not built"
			self redrawNeeded
		]! !

!methodRemoval: WorldMorph #buildMagnifiedBackgroundImage stamp: 'jmv 5/31/2021 16:40:21'!
WorldMorph removeSelector: #buildMagnifiedBackgroundImage!
