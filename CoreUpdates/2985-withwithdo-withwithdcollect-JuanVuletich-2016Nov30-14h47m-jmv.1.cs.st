'From Cuis 5.0 of 7 November 2016 [latest update: #2984] on 30 November 2016 at 2:56:21 pm'!

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 11/30/2016 14:52:08'!
with: otherCollection with: thirdCollection collect: threeArgBlock
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."
	| n result |
	n _ self size.
	otherCollection size = n ifFalse: [ self error: 'otherCollection must be the same size' ].
	thirdCollection size = n ifFalse: [ self error: 'thirdCollection must be the same size' ].
	result _ self species new: n.
	1 to: n do: [ :index | 
		result at: index put:
			(threeArgBlock
				value: (self at: index)
				value: (otherCollection at: index)
				value: (thirdCollection at: index) )].
	^ result! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 11/25/2016 12:15:27'!
with: otherCollection with: thirdCollection do: threeArgBlock 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	| n |
	n _ self size.
	otherCollection size = n ifFalse: [self error: 'otherCollection must be the same size'].
	thirdCollection size = n ifFalse: [self error: 'thirdCollection must be the same size'].
	1 to: n do: [ :index |
		threeArgBlock
			value: (self at: index)
			value: (otherCollection at: index)
			value: (thirdCollection at: index)]! !


!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 11/30/2016 14:51:19'!
with: otherCollection with: thirdCollection collect: threeArgBlock
	"Collect and return the result of evaluating twoArgBlock with 
	corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	result _ self species new: self size.
	1 to: self size do: [ :index |
		result addLast:
			(threeArgBlock
				value: (self at: index)
				value: (otherCollection at: index)
				value: (thirdCollection at: index) )].
	^ result! !

