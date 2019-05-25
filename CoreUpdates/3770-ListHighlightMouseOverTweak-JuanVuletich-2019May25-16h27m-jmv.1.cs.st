'From Cuis 5.0 of 7 November 2016 [latest update: #3769] on 25 May 2019 at 4:48:20 pm'!

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:34:46'!
focusIndicatorBottom
	^ self hIsScrollbarShowing
		ifTrue: [ extent y - borderWidth - self scrollBarClass scrollbarThickness ]
		ifFalse: [ extent y - borderWidth ]! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:31:00'!
focusIndicatorLeft
	^ borderWidth! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:34:00'!
focusIndicatorRight
	^ self vIsScrollbarShowing
		ifTrue: [ extent x - borderWidth - self scrollBarClass scrollbarThickness ]
		ifFalse: [ extent x - borderWidth ]! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:31:43'!
focusIndicatorTop
	^ borderWidth! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:43:21'!
viewableArea
	^ self viewableAreaTopLeft corner: self viewableAreaRight @ self viewableAreaBottom! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:30:36'!
viewableAreaBottom
	^ self focusIndicatorBottom - self xtraBorder! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:31:05'!
viewableAreaLeft
	^ self focusIndicatorLeft + self xtraBorder! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:31:23'!
viewableAreaRight
	^ self focusIndicatorRight - self xtraBorder! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:32:00'!
viewableAreaTop
	^ self focusIndicatorTop + self xtraBorder! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:40:16'!
viewableAreaTopLeft
	^ self viewableAreaLeft @ self viewableAreaTop! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:38:15'!
focusIndicatorExtent
	^ self focusIndicatorRectangle extent! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:37:59'!
focusIndicatorRectangle

	^ self focusIndicatorLeft @ self focusIndicatorTop corner: self focusIndicatorRight @ self focusIndicatorBottom! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:46:16'!
hScrollBarWidth
	"Return the width of the horizontal scrollbar"

	^ self focusIndicatorRight - self focusIndicatorLeft! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:42:08'!
scrollerOffset

	^ scroller morphPosition negated + self viewableAreaTopLeft! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:42:01'!
scrollerOffset: newOffset

	scroller morphPosition: self viewableAreaTopLeft - newOffset! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:36:45'!
viewableExtent

	^ self viewableWidth @ self viewableHeight! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:35:29'!
viewableHeight
	"Viewable height.
	Leave room for horizontal scrollbar if present"

	^ self viewableAreaBottom - self viewableAreaTop ! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 5/25/2019 16:35:54'!
viewableWidth
	"Viewable width.
	Leave room for vertical scrollbar if present"

	^ self viewableAreaRight - self viewableAreaLeft! !


!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 5/25/2019 16:43:26'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	self listMorph highlightedRow: (
		(self viewableArea containsPoint: localEventPosition) ifTrue: [
			self rowAtLocation: localEventPosition ifNone: []]).
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !


!TextModelMorph methodsFor: 'drawing' stamp: 'jmv 5/25/2019 16:44:45'!
drawOn: aCanvas
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"
	| bw bc |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	super drawOn: aCanvas.
	bw _ Preferences focusIndicatorWidth.
	bc _ nil.
	self wantsFrameAdornments ifTrue: [
		model refusesToAccept
			ifTrue: [ "Put up feedback showing that code cannot be submitted in this state"
				bc _ Color tan ]
			ifFalse: [
				self textMorph hasEditingConflicts
					ifTrue: [
						bw _ 3.
						bc _ Color red ]
					ifFalse: [
						self textMorph hasUnacceptedEdits ifTrue: [ bc _ Color red ]]]].
	(drawKeyboardFocusIndicator and: [ self textMorph hasKeyboardFocus ])
		ifTrue: [ bc ifNil: [ bc _ Theme current focusIndicator ]]
		ifFalse: [
			bc ifNotNil: [
				bc _ bc
					alphaMixed: 0.4
					with: Color white ]].
	bc ifNotNil: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: bw
			color: bc ].
	model actualContents isEmpty ifTrue: [
		self
			valueOfProperty: #emptyTextDisplayMessage
			ifPresentDo: [ :msg |
				aCanvas
					drawString: msg
					at: self viewableAreaTopLeft
					font: nil
					color: Theme current textEmptyDisplayMessage ]].! !

