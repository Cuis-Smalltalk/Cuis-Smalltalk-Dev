'From Cuis7.5 [latest update: #7641] on 12 November 2025 at 5:25:38 pm'!

!MenuMorph methodsFor: 'keyboard control' stamp: 'AW 11/12/2025 17:25:23'!
displayFiltered: evt
	| matchStr allItems isMatch feedbackMorph |
	matchStr := self valueOfProperty: #matchString.
	allItems := self submorphs select: [ :m |
		m is: #MenuItemMorph ].
	allItems do: [ :m |
		isMatch := matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch ].
	feedbackMorph := self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph := LabelMorph new color: `Color darkGray`.
		self addMorphBack: feedbackMorph lock position: `0@ -20`.
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self removeProperty: #feedbackMorph ].
	self selectItem: nil.
	self adjustSubmorphsLayout.! !

