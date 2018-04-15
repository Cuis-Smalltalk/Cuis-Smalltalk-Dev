'From Cuis 5.0 of 7 November 2016 [latest update: #3285] on 14 April 2018 at 8:01:56 pm'!

!PluggableListMorphByItem methodsFor: 'model access' stamp: 'HAW 4/14/2018 19:27:47'!
setSelectionIndex: anInteger
	"Change the model's selected item to be the one at the given index."

	| item |
	setIndexSelector ifNotNil: [
		item _ itemList at: anInteger ifAbsent: [ nil ].
		model perform: setIndexSelector with: item.
		self update: getIndexSelector.
		^ true ].
	^false
	! !

