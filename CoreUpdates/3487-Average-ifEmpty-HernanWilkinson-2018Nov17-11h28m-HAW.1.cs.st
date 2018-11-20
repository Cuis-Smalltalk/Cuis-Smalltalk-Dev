'From Cuis 5.0 of 7 November 2016 [latest update: #3487] on 17 November 2018 at 11:46:04 am'!

!Collection methodsFor: 'private' stamp: 'HAW 11/17/2018 11:40:56'!
emptyCollectionDescription

	^self class emptyCollectionDescription ! !


!Collection class methodsFor: 'error descriptions' stamp: 'HAW 11/17/2018 11:41:14'!
emptyCollectionDescription

	^'this collection is empty'! !


!Collection methodsFor: 'private' stamp: 'HAW 11/17/2018 11:39:51'!
errorEmptyCollection

	self error: self emptyCollectionDescription! !

!Collection methodsFor: 'statistics' stamp: 'HAW 11/17/2018 11:34:23'!
average: aBlock

	^ self average: aBlock ifEmpty: [ self errorEmptyCollection ]! !

!Collection methodsFor: 'statistics' stamp: 'HAW 11/17/2018 11:38:13'!
average: aBlock ifEmpty: emptyBlock 
	
	^ (self sum: aBlock ifEmpty: [ ^emptyBlock value ]) / self size! !


!Collection reorganize!
('accessing' anyOne atRandom size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = hash)
('converting' asArray asBag asByteArray asCharacterSet asDictionary asFloat64Array asFloatArray asIdentitySet asIntegerArray asNewArray asOrderedCollection asSet asSortedCollection asSortedCollection: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll:)
('enumerating' allSatisfy: anySatisfy: associationsDo: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: detectSum: difference: do: do:separatedBy: do:without: explorerContents explorerContentsWithIndexCollect: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: select: select:thenCollect: select:thenDo: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck emptyCollectionDescription errorCollectionToSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
('*Collections-CompactArrays' asPointArray)
('*Statistics' meanAndStandardDeviation meanAndVariance median mode percentile: sampleStandardDeviation sampleVariance standardDeviation variance)
!


!Collection class reorganize!
('instance creation' newFrom: with: with:with: with:with:with: with:with:with:with: with:with:with:with:with: with:with:with:with:with:with: withAll:)
('error descriptions' emptyCollectionDescription)
!

