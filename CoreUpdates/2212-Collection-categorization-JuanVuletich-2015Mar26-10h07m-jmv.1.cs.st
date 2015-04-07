'From Cuis 4.2 of 25 July 2013 [latest update: #2211] on 26 March 2015 at 10:10:03.611378 am'!

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 10:02'!
mean
	^ self sum / self size! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 10:10'!
sampleStandardDeviation
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} sampleStandardDeviation
	"

	^self sampleVariance sqrt! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 10:11'!
sampleVariance
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} sampleVariance
	"

	| sum mean |
	mean _ self mean.
	sum _ self collect: [ :each | (each - mean) squared ] andFold: [ :a :b | a + b ].
	^sum / (self size-1)! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 10:10'!
standardDeviation
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} standardDeviation
	"

	^self variance sqrt! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 10:11'!
variance
	"
	See
		http://en.wikipedia.org/wiki/Variance
		http://www.mathsisfun.com/data/standard-deviation.html
	{600. 470. 170. 430. 300} variance
	"

	| sum mean |
	mean _ self mean.
	sum _ self collect: [ :each | (each - mean) squared ] andFold: [ :a :b | a + b ].
	^sum / self size! !


!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 10:02'!
average
	^self mean! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 09:43'!
max
	^ self inject: self anyOne into: [:max :each | max max: each]! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 09:43'!
median
	"See comment in SortedCollection"
	| sorted |
	sorted _ self asArray sort.
	^sorted at: sorted size + 1 // 2! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 09:43'!
min
	^ self inject: self anyOne into: [:min :each | min min: each]! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 09:43'!
range
	^ self max - self min! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 09:43'!
sum
	"Compute the sum of all the elements in the receiver"

	^self collect: [ :each | each ] andFold: [ :a :b | a + b]! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 09:44'!
sum: aBlock
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 0 is a good initial value for the sum. 
	Consider a collection of measurement objects, 0 would be the unitless 
	value and would not be appropriate to add with the unit-ed objects."
	^self collect: aBlock andFold: [ :a :b | a + b ]! !


!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 3/25/2015 09:42'!
copyFrom: start count: n
	"Answer a copy of a subset of the receiver, starting from element at 
	index start and of size n."

	^(self species new: n)
		replaceFrom: 1
		to: n
		with: self
		startingAt: start! !


!ExponentialRandom class methodsFor: 'instance creation' stamp: 'jmv 3/25/2015 10:23'!
newWithParameter: aNumber

	^self new
		parameter: aNumber;
		yourself! !


!RayleighRandom class methodsFor: 'instance creation' stamp: 'jmv 3/25/2015 10:23'!
newWithParameter: aNumber

	^self new
		parameter: aNumber;
		yourself! !


!SequenceableCollection reorganize!
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: anyOne at:ifAbsent: at:incrementBy: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: customizeExplorerContents eighth fifth first first: fourth from:to:put: identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: integerAt: integerAt:put: last last: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate replaceAll:with: replaceFrom:to:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: third)
('comparing' = hasEqualElements: hash)
('converting' asArray asByteArray asColorArray concatenation isSequenceable printStringWithNewline readStream reverse reversed writeStream @)
('copying' , copy copyAfter: copyAfterLast: copyFrom:count: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: forceTo:paddingStartWith: forceTo:paddingWith: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: collectWithIndex: combinations:atATimeDo: do: do:separatedBy: do:without: doWithIndex: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findLast: from:to:do: groupsOf:atATimeDo: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: replace: reverseDo: reverseWith:do: select: upTo: with:collect: with:do: with:reverseDo: withIndexCollect: withIndexDo:)
('private' asDigitsAt:in:do: checkedAt: combinationsAt:in:after:do: copyReplaceAll:with:asTokens: errorFirstObject: errorOutOfBounds permutationsStartingAt:do:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: includes: isSequenceableCollection)
('gui' do:displayingProgress:)
!


!Collection reorganize!
('accessing' anyOne atRandom size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // raisedTo: \\)
('comparing' hash)
('converting' asArray asBag asByteArray asCharacterSet asFloatArray asIdentitySet asIdentitySkipList asIntegerArray asOrderedCollection asPointArray asSet asSkipList asSkipList: asSortedArray asSortedCollection asSortedCollection: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll:)
('enumerating' allSatisfy: anySatisfy: associationsDo: collect: collect:andFold: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: detectSum: difference: do: do:separatedBy: do:without: explorerContents explorerContentsWithIndexCollect: fold: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: select: select:thenCollect: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log negated reciprocal roundTo: rounded sign sin sqrt squared tan truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck errorCollectionToSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species toBraceStack:)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('sorting' sorted:)
('statistics' average max mean median min range sampleStandardDeviation sampleVariance standardDeviation sum sum: variance)
!

