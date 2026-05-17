'From Cuis7.7 [latest update: #7941] on 17 May 2026 at 6:52:50 pm'!

!RunNotArray methodsFor: 'copying' stamp: 'jmv 17/May/2026 18:52:42'!
copyReplaceFrom: start to: stop with: replacementCollection
	"Similar to inherited implementation, but tolerating stop beyond the receiver size,
	like RunArray does."

	| newSequenceableCollection newSize endReplacement |
	newSize := self size - (stop - start + 1) + replacementCollection size max: start + replacementCollection size - 1.
	endReplacement := start - 1 + replacementCollection size.
	newSequenceableCollection := self copyBlank: newSize.
	start > 1 ifTrue: [
		newSequenceableCollection
			replaceFrom: 1
			to: start - 1
			with: self
			startingAt: 1 ].
	start <= endReplacement ifTrue: [
		newSequenceableCollection
			replaceFrom: start
			to: endReplacement
			with: replacementCollection
			startingAt: 1 ].
	endReplacement < newSize ifTrue: [
		newSequenceableCollection
			replaceFrom: endReplacement + 1
			to: newSize
			with: self
			startingAt: stop + 1 ].
	^newSequenceableCollection! !


!RunNotArray methodsFor: 'accessing' stamp: 'jmv 17/May/2026 18:21:27'!
at: index
	"RunArray states:
		Tolerates index=0 and index=size+1 for copyReplace:
	But it actually allows any integer index > size."

	| s |
	index = 0 ifTrue: [ ^ self at: 1 ].
	s := self size.
	index > s ifTrue: [ ^ self at: s ].
	^ super at: index! !

