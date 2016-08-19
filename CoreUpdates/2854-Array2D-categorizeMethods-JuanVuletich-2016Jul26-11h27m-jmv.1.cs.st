'From Cuis 4.2 of 25 July 2013 [latest update: #2853] on 26 July 2016 at 11:53:58 am'!

!Array2D methodsFor: 'iterating' stamp: 'jmv 7/26/2016 11:27:42'!
replaceValues: aBlock
	"Replace each value with the result of evaluating aBlock, with i, j and the previous value as the arguments"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			self i: i j: j put:
				(aBlock value: i value: j value: (self i: i j: j)) ] ]! !

!Array2D methodsFor: 'iterating' stamp: 'jmv 7/26/2016 11:27:49'!
withIndexesDo: aBlock
	"Replace each value with the result of evaluating aBlock, with the previous value as the argument"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: i value: j value: (self i: i j: j) ] ]! !


!Array2D reorganize!
('accessing' at: at:put: extent height i:j: i:j:put: size width x:y: x:y:put:)
('copying' postCopy)
('filling' fillWith: fillWithArrayOfArrays:)
('initialization' initHeight:width: initializeElements)
('printing' print printOn:)
('private' elementsIndexForI:j:)
('testing' isSquare)
('comparing' = hash)
('iterating' replaceValues: withIndexesDo:)
!

