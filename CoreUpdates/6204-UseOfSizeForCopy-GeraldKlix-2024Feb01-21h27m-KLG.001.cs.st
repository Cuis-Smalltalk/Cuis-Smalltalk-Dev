'From Haver6.3 [latest update: #6202] on 1 February 2024 at 9:37:36 pm'!

!SequenceableCollection methodsFor: 'copying' stamp: 'KLG 2/1/2024 21:27:41'!
copyFrom: start count: n
	"Answer a copy of a subset of the receiver, starting from element at 
	index start and of size n."

	^(self species ofSize: n)
		replaceFrom: 1
		to: n
		with: self
		startingAt: start! !

!SequenceableCollection methodsFor: 'copying' stamp: 'KLG 2/1/2024 21:30:13'!
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize |
	newSize := stop - start + 1 max: 0.
	^(self species ofSize: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start! !

!SequenceableCollection methodsFor: 'copying' stamp: 'KLG 2/1/2024 21:31:30'!
copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| newSequenceableCollection newSize endReplacement |
	newSize := self size - (stop - start + 1) + replacementCollection size.
	endReplacement := start - 1 + replacementCollection size.
	newSequenceableCollection := self species ofSize: newSize.
	start > 1 ifTrue:[
		newSequenceableCollection
			replaceFrom: 1
			to: start - 1
			with: self
			startingAt: 1].
	start <= endReplacement ifTrue:[
		newSequenceableCollection
			replaceFrom: start
			to: endReplacement
			with: replacementCollection
			startingAt: 1].
	endReplacement < newSize ifTrue:[
		newSequenceableCollection
			replaceFrom: endReplacement + 1
			to: newSize
			with: self
			startingAt: stop + 1].
	^newSequenceableCollection! !

