'From Cuis6.3 [latest update: #6222] on 9 February 2024 at 3:44:50 pm'!

!Text methodsFor: 'copying' stamp: 'len 2/9/2024 10:46:51'!
copyBlank: requestedSize
	"Answer an instance like me, of size n, but without content.
	The underlying string of the new instance matches the type of the receiver's string (for example String or UnicodeString)."
	^ Text fromString: (string class new: requestedSize)! !


!SequenceableCollection methodsFor: 'converting' stamp: 'len 2/9/2024 10:52:40'!
reversed
	"Answer a copy of the receiver with element order reversed."
	"Example: 'frog' reversed"

	| n result src |
	n := self size.
	result := self copyBlank: n.
	src := n + 1.
	1 to: n do: [:i | result at: i put: (self at: (src := src - 1))].
	^ result
! !

!SequenceableCollection methodsFor: 'copying' stamp: 'len 2/9/2024 11:03:28'!
copyWith: newElement
	"Answer a copy of the receiver that is 1 bigger than the receiver and has 
	newElement at the last element."

	| answer |
	answer := self copyBlank: self size + 1.
	answer replaceFrom: 1 to: self size with: self startingAt: 1.
	answer at: answer size put: newElement.
	^answer! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'len 2/9/2024 10:51:44'!
collect: aBlock
	"Refer to the comment in Collection|collect:."
	| result |
	result := self copyBlank: self size.
	1 to: self size do:
		[:index | result at: index put: (aBlock value: (self at: index))].
	^ result! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'len 2/9/2024 10:51:55'!
collect: aBlock from: firstIndex to: lastIndex
	"Refer to the comment in Collection|collect:."

	| size result j |
	size := lastIndex - firstIndex + 1.
	result := self copyBlank: size.
	j := firstIndex.
	1 to: size do: [:i | result at: i put: (aBlock value: (self at: j)). j := j + 1].
	^ result! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'len 2/9/2024 10:53:11'!
with: otherCollection collect: twoArgBlock
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	result := self copyBlank: self size.
	1 to: self size do:
		[:index | result at: index put:
		(twoArgBlock
			value: (self at: index)
			value: (otherCollection at: index))].
	^ result! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'len 2/9/2024 10:53:27'!
with: otherCollection with: thirdCollection collect: threeArgBlock
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."
	| n result |
	n := self size.
	otherCollection size = n ifFalse: [ self error: 'otherCollection must be the same size' ].
	thirdCollection size = n ifFalse: [ self error: 'thirdCollection must be the same size' ].
	result := self copyBlank: n.
	1 to: n do: [ :index | 
		result at: index put:
			(threeArgBlock
				value: (self at: index)
				value: (otherCollection at: index)
				value: (thirdCollection at: index) )].
	^ result! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'len 2/9/2024 10:53:35'!
withIndexCollect: elementAndIndexBlock
	"Just like with:collect: except that the iteration index supplies the second argument to the block."
	| result |
	result := self copyBlank: self size.
	1 to: self size do:
		[:index | result at: index put:
		(elementAndIndexBlock
			value: (self at: index)
			value: index)].
	^ result! !


!OrderedCollection methodsFor: 'copying' stamp: 'len 2/9/2024 11:07:47'!
copyFrom: startIndex to: endIndex
	"Answer a copy of the receiver that contains elements from position startIndex to endIndex."
	self flag: #fix. "TODO: copy the underlying array, that uses a primitive and its much faster; or implement replaceFrom:to:with:startingAt: in OrderedCollection in a smart way"
	^ super copyFrom: startIndex to: endIndex! !

!methodRemoval: Text #grownTo: stamp: 'len 2/9/2024 10:46:58'!
Text removeSelector: #grownTo:!
!methodRemoval: OrderedCollection #collect: stamp: 'len 2/9/2024 10:59:27'!
OrderedCollection removeSelector: #collect:!
!methodRemoval: OrderedCollection #collect:from:to: stamp: 'len 2/9/2024 11:00:24'!
OrderedCollection removeSelector: #collect:from:to:!
!methodRemoval: OrderedCollection #reversed stamp: 'len 2/9/2024 11:08:46'!
OrderedCollection removeSelector: #reversed!
!methodRemoval: OrderedCollection #with:with:collect: stamp: 'len 2/9/2024 11:09:10'!
OrderedCollection removeSelector: #with:with:collect:!
!methodRemoval: OrderedCollection #copyWith: stamp: 'len 2/9/2024 11:08:10'!
OrderedCollection removeSelector: #copyWith:!
!methodRemoval: OrderedCollection #withIndexCollect: stamp: 'len 2/9/2024 11:09:34'!
OrderedCollection removeSelector: #withIndexCollect:!
!methodRemoval: OrderedCollection #with:collect: stamp: 'len 2/9/2024 11:08:59'!
OrderedCollection removeSelector: #with:collect:!
