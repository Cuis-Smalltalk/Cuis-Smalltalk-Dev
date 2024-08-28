'From Cuis7.1 [latest update: #6646] on 28 August 2024 at 9:28:15 am'!

!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 8/28/2024 09:25:35'!
setSelectionIndex: anInteger
	"Change the model's selected item index to be anInteger."

	setIndexSelector ifNotNil: [
		model perform: setIndexSelector with: anInteger.
		self privateVisualSelectionIndex: self getCurrentSelectionIndex.
		^ true ].
	^ false! !


!PluggableListMorphByItem methodsFor: 'model access' stamp: 'jmv 8/28/2024 09:25:44'!
setSelectionIndex: anInteger
	"Change the model's selected item to be the one at the given index."

	| item |
	setIndexSelector ifNotNil: [
		item := itemList at: anInteger ifAbsent: [ nil ].
		model perform: setIndexSelector with: item.
		self privateVisualSelectionIndex: self getCurrentSelectionIndex.
		^ true ].
	^false
	! !


!PluggableListMorphByItem reorganize!
('model access' getCurrentSelectionIndex getList setSelectionIndex:)
!

