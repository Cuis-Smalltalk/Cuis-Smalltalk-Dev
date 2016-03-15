'From Cuis 4.2 of 25 July 2013 [latest update: #2703] on 15 March 2016 at 12:43:24.33248 pm'!

!Array2D methodsFor: 'comparing' stamp: 'jmv 3/15/2016 12:41'!
= another
	self == another ifTrue: [ ^ true ].
	self class = another class ifFalse: [ ^false ].
	width = another width ifFalse: [ ^false ].
	height = another height ifFalse: [ ^false ].
	^elements = another elements! !

!Array2D methodsFor: 'comparing' stamp: 'jmv 3/15/2016 12:43'!
hash
	^elements hash! !


!Array2D reorganize!
('accessing' at: at:put: extent height i:j: i:j:put: size width x:y: x:y:put:)
('copying' postCopy)
('filling' fillWith: fillWithArrayOfArrays:)
('initialization' initHeight:width: initializeElements)
('misc' replaceValues: withIndexesDo:)
('printing' print printOn:)
('private' elementsIndexForI:j:)
('testing' isSquare)
('comparing' = hash)
!

