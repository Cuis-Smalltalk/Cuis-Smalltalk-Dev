'From Cuis 5.0 [latest update: #4451] on 15 November 2020 at 10:26:39 am'!

!PluggableScrollPane methodsFor: 'access options' stamp: 'jmv 11/15/2020 09:30:29'!
alwaysShowVerticalScrollbar

	hideScrollBars _ #alwaysShowVertical.
	self vShowScrollBar.! !


!SystemWindow class methodsFor: 'instance creation' stamp: 'jmv 11/15/2020 10:25:36'!
editWordWrapText: aTextModel label: labelString
	| textMorph window |
	textMorph _ TextModelMorph withModel: aTextModel.
	textMorph wrapFlag: true.
	textMorph alwaysShowVerticalScrollbar.
	window _ self new model: aTextModel.
	window setLabel: labelString.
	window layoutMorph
		addMorph: textMorph
		proportionalHeight: 1.
	^ window openInWorld! !


!Workspace methodsFor: 'gui' stamp: 'jmv 11/15/2020 10:25:48'!
openLabel: aString 
	"Create a standard system view of the model, me, and open it."
	| win |
	win _ WorkspaceWindow editWordWrapText: self label: aString.
	self changed: #actualContents.
	^win! !


!PluggableScrollPane methodsFor: 'access options' stamp: 'jmv 11/15/2020 09:28:38'!
hideScrollBarsIndefinitely

	hideScrollBars _ #hide.
	self vHideScrollBar.
	self hHideScrollBar.! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 11/15/2020 09:28:06'!
updateScrollBarsBounds
	
	| t |
	hideScrollBars = #hide ifTrue: [^self].
	t _ self scrollBarClass scrollbarThickness.
	scrollBar
		morphPosition: extent x - t - borderWidth @ borderWidth
		extent: t @ self vScrollBarHeight.
	hScrollBar
		morphPosition: borderWidth @ (extent y - t - borderWidth)
		extent: self hScrollBarWidth@t! !

!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 11/15/2020 09:28:59'!
initialize
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars _ #showIfNeeded.

	"initialize the receiver's scrollBars"
	scrollBar _ self scrollBarClass new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar _ self scrollBarClass new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator _ true.

	scroller _ self innerMorphClass new.
	self addMorph: scroller.
	self scrollerOffset: `0@ 0`.
	self addMorph: scrollBar.
	self addMorph: hScrollBar! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 11/15/2020 09:28:22'!
hIsScrollbarNeeded
	"Return whether the horz scrollbar is needed"

	self mightNeedHorizontalScrollBar ifFalse: [ ^false ].
	
	"Don't show it if we were told not to."
	hideScrollBars = #hide ifTrue: [ ^false ].

	^self hLeftoverScrollRange > 0! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 11/15/2020 09:31:36'!
vIsScrollbarNeeded
	"Return whether the vertical scrollbar is needed"
	
	"Don't show it if we were told not to."
	hideScrollBars = #hide ifTrue: [ ^false ].

	hideScrollBars = #alwaysShowVertical ifTrue: [ ^true ].

	^self vLeftoverScrollRange > 0! !

