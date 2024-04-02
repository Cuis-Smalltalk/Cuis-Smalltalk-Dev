'From Cuis6.3 [latest update: #6306] on 2 April 2024 at 10:23:54 am'!

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 4/2/2024 10:22:20'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."

	owner hasMouseFocus ifFalse: [ ^self ].
	owner selectedItem == self ifFalse: [ ^self ].
	"This will happen if the menu has toggles in it. (for instance, the 'show...' button)
	Update the look, refresh the world and wait a bit,
	to give the user some visual feedback"
	contentString ifNotNil: [
		self contentsWithMarkers: contentString inverse: true.
		self world ifNotNil: [ :w | w displayWorldOn: w mainCanvas ].  "show myself"
		(Delay forMilliseconds: 200) wait].
	self deselect.
	self invokeWithEvent: aMouseButtonEvent! !


!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 4/2/2024 10:16:48'!
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
	self selectItem: nil.! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 8/5/2021 11:43:50'!
fitInWorld

	| delta trialRect |
	trialRect := Rectangle origin: self morphPosition extent: extent.
	delta := trialRect amountToTranslateWithin: owner displayBounds.
	self morphPosition: trialRect origin + delta.! !

