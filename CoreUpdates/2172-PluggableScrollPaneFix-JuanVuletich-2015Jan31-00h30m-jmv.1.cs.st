'From Cuis 4.2 of 25 July 2013 [latest update: #2171] on 31 January 2015 at 12:46:48.067933 am'!

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 1/31/2015 00:44'!
hideOrShowScrollBars

	"Assume for a moment we don't need an horizontal scrollbar"
	self hHideScrollBar.

	"Add or remove vertical scrollbar, asuming for a monent there's no horizontal scrollbar,
	to determine need of horizontal scrollbar..."
	self vIsScrollbarNeeded
		ifTrue: [ self vShowScrollBar ]
		ifFalse: [ self vHideScrollBar ].

	"If we need an horizontal scrollbar, add it."
	self hIsScrollbarNeeded ifTrue: [
		self hShowScrollBar.

		"If horizontal scrollbar is needed, maybe vertical scrollbar will be needed too (even if we previously thoutht it wouldn't be needed)."	
		"Note that there is no chance of modifying the need of horizontal scrollbar: it was already needed. Therefore, there is no circularity here."
		self vIsScrollbarNeeded  ifTrue: [
			self vShowScrollBar ]].

	"Ensure that if no scrollbars are needed, whole contents are visible"
	self vIsScrollbarShowing ifFalse: [
		scrollBar setValue: 0 ].
	self hIsScrollbarShowing ifFalse: [
		hScrollBar setValue: 0 ].

	self updateScrollBarsBounds! !

