'From Cuis7.3 [latest update: #6908] on 11 December 2024 at 10:54:30 am'!

!PluggableScrollPane methodsFor: 'access options' stamp: 'jmv 12/10/2024 17:36:50'!
alwaysHideHorizontalScrollbar

	hideScrollBars := #alwaysHideHorizontal.
	self hHideScrollBar.! !


!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 12/11/2024 10:43:12'!
hIsScrollbarNeeded
	"Return whether the horz scrollbar is needed"

	self mightNeedHorizontalScrollBar ifFalse: [ ^false ].
	
	"Don't show it if we were told not to."
	hideScrollBars = #hide ifTrue: [ ^false ].
	
	hideScrollBars = #alwaysHideHorizontal  ifTrue: [ ^false ].

	^self hLeftoverScrollRange > 0! !

