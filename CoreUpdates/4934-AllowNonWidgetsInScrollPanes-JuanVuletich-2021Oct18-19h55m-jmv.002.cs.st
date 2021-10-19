'From Cuis 5.0 [latest update: #4933] on 18 October 2021 at 8:07:24 pm'!
!classDefinition: #PluggableScrollPane category: 'Morphic-Widgets'!
PluggableMorph subclass: #PluggableScrollPane
	instanceVariableNames: 'scrollBar scroller hScrollBar hideScrollBars drawKeyboardFocusIndicator scrollerRadians scrollerScale theScrollerExtent '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PluggableScrollPane commentStamp: 'jmv 10/18/2021 19:56:27' prior: 0!
Allows viewing just part of a larger Morph. The scroll values vary from 0.0 to 1.0.

| p |
p := PluggableScrollPane new.
p scroller: WidgetMorph new.
p openInWorld.

| p |
p := PluggableScrollPane new.
p scroller: Sample01Star new.
p openInWorld.!


!Morph methodsFor: 'geometry' stamp: 'jmv 10/18/2021 14:52:24'!
topLeftInOwner

	^self fullBoundsInOwner origin! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 10/18/2021 14:53:02'!
topLeftInOwner

	^self morphPosition! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 10/18/2021 14:53:07'!
topLeftInOwner

	^self morphPosition! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 10/18/2021 19:50:20'!
extentChanged: oldExtent
	"Our extent changed. Reflow text."
	super extentChanged: oldExtent.
	model ifNotNil: [
		wrapFlag ifTrue: [
			extent x = oldExtent x ifFalse: [
				self resetTextComposition.
				self editor recomputeSelection.	
				owner
					updateScrollBarsBounds;
					setScrollDeltas ]]]! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/18/2021 20:02:53'!
minimumExtent
	| minW minH h w |
	w _ theScrollerExtent ifNotNil: [ theScrollerExtent x ] ifNil: [ 0 ].
	h _ theScrollerExtent ifNotNil: [ theScrollerExtent y ] ifNil: [ 0 ].
	"Figure out the minimum extent for this pane so that either content, or at least required scrollbars, will fit"
	minW _ self xtraBorder * 2 + w min: ScrollBar scrollbarThickness * 2.
	self vIsScrollbarShowing
		ifTrue: [
			minW _ minW + ScrollBar scrollbarThickness].
	minH _ self xtraBorder * 2 + h.
	self hIsScrollbarShowing
		ifTrue: [
			minH _ minH + ScrollBar scrollbarThickness].
	minH _ minH min: ScrollBar scrollbarThickness * 2.
	^ (minW + (borderWidth * 2)) @ (minH + (borderWidth * 2))! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/18/2021 14:53:34'!
scrollerOffset

	^ scroller topLeftInOwner negated + self viewableAreaTopLeft! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/18/2021 15:07:28'!
scrollerOffset: newOffset

	| delta |
	delta _ scroller topLeftInOwner - scroller morphPosition.
	scroller morphPosition: self viewableAreaTopLeft - newOffset - delta! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 10/18/2021 20:07:16'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	super someSubmorphPositionOrExtentChanged.
	scroller ifNotNil: [ | scrollerLocation doIt |
		doIt _ false.
		scroller hasVariableExtent ifTrue: [
			theScrollerExtent = scroller morphExtentInOwner ifFalse: [
				theScrollerExtent _ scroller morphExtentInOwner.
				doIt _ true ]].
		scrollerLocation _ scroller location.
		scrollerScale = scrollerLocation scale ifFalse: [
			scrollerScale _ scrollerLocation scale.
			doIt _ true ].
		scrollerRadians = scrollerLocation radians ifFalse: [
			scrollerRadians _ scrollerLocation radians.
			doIt _ true ].
		doIt ifTrue: [ self setScrollDeltas ]].! !

!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 10/18/2021 19:47:13'!
initialize
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars _ #showIfNeeded.

	"initialize the receiver's scrollBars"
	scrollBar _ ScrollBar new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar _ ScrollBar new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator _ Preferences drawKeyboardFocusIndicator.
	self addMorph: scrollBar.
	self addMorph: hScrollBar.
	self updateScrollBarsBounds.
	self innerMorphClass ifNotNil: [ :contentsClass |
		self scroller: contentsClass new ].! !

!classDefinition: #PluggableScrollPane category: 'Morphic-Widgets'!
PluggableMorph subclass: #PluggableScrollPane
	instanceVariableNames: 'scrollBar scroller hScrollBar hideScrollBars drawKeyboardFocusIndicator scrollerScale scrollerRadians theScrollerExtent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
