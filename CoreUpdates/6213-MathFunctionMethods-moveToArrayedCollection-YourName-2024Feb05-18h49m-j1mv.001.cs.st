'From Cuis6.3 [latest update: #6211] on 5 February 2024 at 6:57:27 pm'!

!ArrayedCollection methodsFor: 'math functions' stamp: 'jmv 2/5/2024 18:54:09'!
derivative
	| displaced answer |
	displaced := self class new: self size.
	displaced replaceFrom: 2 to: self size with: self startingAt: 1.
	displaced at: 1 put: self first - self first.	"Some reasonable zero"
	answer := self copy.
	answer -= displaced.
	^answer! !

!ArrayedCollection methodsFor: 'math functions' stamp: 'jmv 2/5/2024 18:54:42'!
incrementFraction
	"
	#(10 12.5 15 20) incrementFraction
	"
	| displaced answer |
	displaced := self class new: self size.
	displaced replaceFrom: 2 to: self size with: self startingAt: 1.
	displaced at: 1 put: self first.
	answer := self copy.
	answer -= displaced.
	^answer / displaced! !

!ArrayedCollection methodsFor: 'math functions' stamp: 'jmv 2/5/2024 18:54:52'!
integral
	| answer |
	answer := self copy.
	2 to: answer size do: [ :i |
		answer at: i put: (answer at: i) + (answer at: i-1) ].
	^answer! !


!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 2/5/2024 18:52:28'!
copyAfter: anElement
	"Answer a copy of the receiver from after the first occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self indexOf: anElement ifAbsent: [^ self species new ])! !

!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 2/5/2024 18:52:24'!
copyAfterLast: anElement
	"Answer a copy of the receiver from after the last occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self species new ])! !

!methodRemoval: SequenceableCollection #integral stamp: 'jmv 2/5/2024 18:54:55'!
SequenceableCollection removeSelector: #integral!
!methodRemoval: SequenceableCollection #derivative stamp: 'jmv 2/5/2024 18:54:14'!
SequenceableCollection removeSelector: #derivative!
!methodRemoval: SequenceableCollection #incrementFraction stamp: 'jmv 2/5/2024 18:54:56'!
SequenceableCollection removeSelector: #incrementFraction!

!ArrayedCollection reorganize!
('accessing' size)
('adding' add:)
('printing' printNameOn: storeOn:)
('private' defaultElement storeElementsFrom:to:on:)
('sorting' isSorted isSortedBy: mergeFirst:middle:last:into:by: mergeSortFrom:to:by: mergeSortFrom:to:src:dst:by: quickSortFrom:to:by: sort sort:)
('objects from disk' byteSize bytesPerBasicElement bytesPerElement restoreEndianness writeOn:)
('inserting' insert:shiftingRightAt:)
('testing' isEmpty)
('inspecting' explorerContents)
('math functions' derivative incrementFraction integral)
!

