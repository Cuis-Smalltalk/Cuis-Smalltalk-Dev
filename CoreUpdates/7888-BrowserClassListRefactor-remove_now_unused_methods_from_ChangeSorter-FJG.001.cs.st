'From Cuis7.7 [latest update: #7850] on 4 April 2026 at 4:28:12 pm'!

!ChangeSorter methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 16:26:35'!
classListIndex: anInteger

	currentClassName := self classList at: anInteger
			ifPresent: [:aClass | aClass name]
			ifAbsent: nil.
	currentSelector := nil.
	self changed: #classListIndex.
	self changed: #messageList.
	self acceptedContentsChanged.! !

!methodRemoval: ChangeSorter #currentClassName stamp: 'FJG 4/Apr/2026 16:26:00'!
ChangeSorter removeSelector: #currentClassName!
!methodRemoval: ChangeSorter #currentClassName: stamp: 'FJG 4/Apr/2026 16:26:19'!
ChangeSorter removeSelector: #currentClassName:!
