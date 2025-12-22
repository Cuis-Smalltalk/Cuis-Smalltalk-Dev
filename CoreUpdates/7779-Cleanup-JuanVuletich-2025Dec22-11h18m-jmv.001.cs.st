'From Cuis7.7 [latest update: #7778] on 22 December 2025 at 11:18:48 am'!

!HierarchyBrowser methodsFor: 'initialization' stamp: 'jmv 22/Dec/2025 11:03:39'!
classListIndex: newIndex
	"Cause system organization to reflect appropriate category"

	| newClassName |
	(classList isInBounds: newIndex) ifTrue: [
		newClassName := (classList at: newIndex) withoutLeadingBlanks.
		selectedSystemCategory := systemOrganizer categoryOfElement: newClassName].
	super classListIndex: newIndex.
	self changed: #systemCategorySingleton.! !


!ExtractMethodMessageSet methodsFor: 'message list' stamp: 'jmv 22/Dec/2025 11:04:10'!
messageListIndex: anIndex

	selectedIndex := anIndex.
	super messageListIndex: anIndex.! !


!InlineMethodMessageSendsSet methodsFor: 'message list' stamp: 'jmv 22/Dec/2025 11:04:39'!
messageListIndex: anIndex

	selectedIndex := anIndex.
	super messageListIndex: anIndex.! !

