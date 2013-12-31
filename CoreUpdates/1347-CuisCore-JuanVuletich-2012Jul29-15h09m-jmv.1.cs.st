'From Cuis 4.0 of 21 April 2012 [latest update: #1344] on 29 July 2012 at 3:21:22 pm'!

!TextEditor methodsFor: 'accessing' stamp: 'jmv 7/29/2012 11:31'!
pointBlock
	^pointBlock! !


!InnerListMorph methodsFor: 'list management' stamp: 'jmv 7/29/2012 15:14'!
drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!!"
	| topLeft |

	topLeft _ bounds left @ (bounds top + ((row - 1) * (font height))).
	^topLeft extent: bounds width @ font height! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 7/29/2012 15:11'!
clickAndHalf: evt
	self handleInteraction: [
		editor clickAndHalf: (evt translatedBy: bounds topLeft negated) ].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 7/29/2012 15:10'!
doubleClickAndHalf: evt
	self handleInteraction: [
		editor doubleClickAndHalf: (evt translatedBy: bounds topLeft negated) ].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 7/29/2012 15:10'!
mouseMove: evt
	evt mouseButton1Pressed ifFalse: [^ self enterClickableRegion: evt].
	self handleInteraction: [ editor mouseMove: (evt translatedBy: bounds topLeft negated)].
	(evt eventPosition y between: owner bounds top and: owner bounds bottom) ifFalse: [
		owner scrollSelectionIntoView ]! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 7/29/2012 15:10'!
mouseUp: evt
	super mouseUp: evt.
	self pauseBlinking.
	self handleInteraction: [editor mouseUp: (evt translatedBy: bounds topLeft negated)].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'selection' stamp: 'jmv 7/29/2012 15:12'!
scrollSelectionIntoView

	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ]! !


!TextModelMorph methodsFor: 'editor access' stamp: 'jmv 7/29/2012 15:19'!
scrollSelectionIntoView
	"Scroll my text into view if necessary and return true, else return false"

	| rectToTest |
	rectToTest _ self editor pointBlock translateBy: self textMorph bounds topLeft.
	self scrollToShow: rectToTest! !

!methodRemoval: TextModelMorph #scrollSelectionIntoView:!
TextModelMorph removeSelector: #scrollSelectionIntoView:!
