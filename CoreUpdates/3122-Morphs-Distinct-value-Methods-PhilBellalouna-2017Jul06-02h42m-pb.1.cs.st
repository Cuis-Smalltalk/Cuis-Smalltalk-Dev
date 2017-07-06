'From Cuis 5.0 of 7 November 2016 [latest update: #3121] on 6 July 2017 at 3:13:37 am'!

!ScrollBar methodsFor: 'access' stamp: 'pb 7/6/2017 02:44:45'!
scrollValue
	^ value! !

!ScrollBar methodsFor: 'model access' stamp: 'pb 7/6/2017 02:53:19'!
internalScrollValue: newValue
	"Called internally for propagation to model"
	self scrollValue: newValue.
	setValueSelector ifNotNil: [
		model perform: setValueSelector with: value ]! !

!ScrollBar methodsFor: 'model access' stamp: 'pb 7/6/2017 02:45:15'!
scrollValue: newValue
	"Drive the slider position externally..."
	value _ newValue min: 1.0 max: 0.0.
	self computeSlider! !


!ProgressBarMorph methodsFor: 'accessing' stamp: 'pb 7/6/2017 02:42:04'!
progressValue
	^value! !

!ProgressBarMorph methodsFor: 'accessing' stamp: 'pb 7/6/2017 02:42:12'!
progressValue: aValue
	value _ aValue.
	self redrawNeeded! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 10/10/2015 23:26'!
fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :bar | 
			[ self atEnd ] whileFalse: [
					bar value: self position.
					self skipSeparators.
					
					[
						val := (self peekFor: $!!) 
								ifTrue: [
									chunk := self nextChunk.
									"These are the ones that should do nothing, 
									because next line is a doit that does the stuff
									(or because it is handled elsewhere)"
									(chunk beginsWith: 'description:  ')
									| (chunk beginsWith: 'provides: ')
									| (chunk beginsWith: 'requires: ')
									| (chunk beginsWith: 'classDefinition: ')
									| (chunk beginsWith: 'classRemoval: ')
									| (chunk beginsWith: 'methodRemoval: ')
									| (chunk beginsWith: 'classMoveToSomePackage: ')
									| (chunk beginsWith: 'methodMoveToSomePackage: ')
										ifFalse: [(Compiler evaluate: chunk logged: false) scanFrom: self]]
								ifFalse: [
									chunk := self nextChunk.
									self checkForPreamble: chunk.
									[ Compiler evaluate: chunk logged: true ]
										on: Error
										do: [ :ex |
											ex print.
											('while evaluating: ', chunk) print.
											ex pass ]
										]] 
							on: InMidstOfFileinNotification
							do: [ :ex | ex resume: true ] ].
			].
	"Note:  The main purpose of this banner is to flush the changes file."
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !

!PositionableStream methodsFor: 'gui' stamp: 'pb 7/6/2017 02:51:46'!
untilEnd: aBlock displayingProgress: aString
	aString
		displayProgressAt: Sensor mousePoint
		from: 0 to: self size
		during: [ :bar |
			[ self atEnd ] whileFalse: [
				bar scrollValue: self position.
				aBlock value ]]! !


!ProgressInitiationException class methodsFor: 'examples and tests' stamp: 'pb 7/6/2017 02:52:33'!
testInnermost

	"test the progress code WITHOUT special handling"

	^'Now here''s some Real Progress'
		displayProgressAt: Sensor mousePoint
		from: 0 
		to: 10
		during: [ :bar |
			1 to: 10 do: [ :x | 
				bar scrollValue: x.
				(Delay forMilliseconds: 500) wait.
				x = 5 ifTrue: [1/0].	"just to make life interesting"
			].
			'done'
		].

! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'pb 7/6/2017 02:46:41'!
hSetScrollDelta
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."
	| range delta w |
	
	delta _ self scrollDeltaWidth * 1.0.		"avoid Fraction arithmetic"
	range _ self hLeftoverScrollRange.
	range = 0 ifTrue: [
		^hScrollBar scrollDelta: 0.02 pageDelta: 0.2; interval: 1.0; internalScrollValue: 0 ].

	"Set up for one line (for arrow scrolling), or a full pane less one line (for paging)."
	w _ self viewableWidth * 1.0.		"avoid Fraction arithmetic"
	hScrollBar scrollDelta: delta / range pageDelta: w - delta / range.
	hScrollBar interval: w / self hTotalScrollRange.
	hScrollBar internalScrollValue: hScrollBar scrollValue! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'pb 7/6/2017 02:48:25'!
vSetScrollDelta
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."
	| range delta h |
	
	delta _ self scrollDeltaHeight * 1.0.	"avoid Fraction arithmetic"
	range _ self vLeftoverScrollRange.
	range = 0 ifTrue: [
		^ scrollBar scrollDelta: 0.02 pageDelta: 0.2; interval: 1.0; internalScrollValue: 0 ].

	"Set up for one line (for arrow scrolling), or a full pane less one line (for paging)."
	h _ self viewableHeight * 1.0. 		"avoid Fraction arithmetic"
	scrollBar scrollDelta: delta / range pageDelta: h - delta / range.
	scrollBar interval: h / self vTotalScrollRange.
	scrollBar internalScrollValue: scrollBar scrollValue! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'pb 7/6/2017 02:48:00'!
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
		scrollBar internalScrollValue: 0 ].
	self hIsScrollbarShowing ifFalse: [
		hScrollBar internalScrollValue: 0 ].

	self updateScrollBarsBounds! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'pb 7/6/2017 02:51:24'!
scrollBy: delta
	"Move the contents in the direction delta."

	| newYoffset r newXoffset |
	
	"Set the offset on the scroller"
	newYoffset _ self scrollerOffset y - delta y max: 0.
	newXoffset _ self scrollerOffset x - delta x max: 0.
	
	self scrollerOffset: newXoffset@ newYoffset.

	"Update the scrollBars"
	(r _ self vLeftoverScrollRange) = 0
		ifTrue: [ scrollBar scrollValue: 0.0 ]
		ifFalse: [ scrollBar scrollValue: newYoffset asFloat / r ].
	(r _ self hLeftoverScrollRange) = 0
		ifTrue: [ hScrollBar scrollValue: 0.0 ]
		ifFalse: [ hScrollBar scrollValue: newXoffset asFloat / r ]! !


!PluggableListMorph methodsFor: 'selection' stamp: 'pb 7/6/2017 02:47:04'!
scrollSelectionIntoView
	"make sure that the current selection is visible"
	| row r |
	row _ self getCurrentSelectionIndex.
	row = 0 
		ifTrue: [
			"Value is 0, but we need to propagate it to model"
			scrollBar internalScrollValue: scrollBar scrollValue ]
		ifFalse: [
			self flag: #jmvVer2.
			r _ self listMorph drawBoundsForRow: row.
			r _ ((self listMorph externalize: r origin) extent: r extent).
			self scrollToShow: r ]! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'pb 7/6/2017 02:56:44'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row |
	row _ (localEventPosition y < 0 and: [ scrollBar scrollValue > 0.0 ])
		ifTrue: [
			scrollBar scrollUp: 1.
			"Leave at least one visible item unaffected, for better visual feedback to the user."
			(self rowAtLocation: `0@0`) + 2 ]
		ifFalse: [
			(localEventPosition y > extent y and: [ scrollBar scrollValue < 1.0 ])
				ifTrue: [
					scrollBar scrollDown: 1.
					"Leave at least one visible item unaffected, for better visual feedback to the user."
					(self rowAtLocation: 0@extent y) - 3 ]
				ifFalse: [ self rowAtLocation: localEventPosition ]].
	row = 0 ifTrue: [ ^ self ].

	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].

	dragOnOrOff ifNil: [
		"Was not set at mouse down, which means the mouse must have gone down in an area where there was no list item"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row ].

	"Set meaning for subsequent dragging of selection"
	oldIndex _ self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [ oldVal _ self listSelectionAt: oldIndex ].

	"Set or clear new primary selection (listIndex)"
	dragOnOrOff 
		ifTrue: [ self changeModelSelection: row ]
		ifFalse: [ self changeModelSelection: 0 ].

	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [ self listSelectionAt: oldIndex put: oldVal ].
	
	"Select all in between if drag was too fast"
	"self listSelectionAt: row put: dragOnOrOff."
	(row min: dragStartRow) to: (row max: dragStartRow) do: [ :r |
		self listSelectionAt: r put: dragOnOrOff ]! !


!ScrollBar methodsFor: 'scrolling' stamp: 'pb 7/6/2017 02:48:48'!
scrollByPage
	"Scroll automatically while mouse is down"
	nextPageDirection
		ifTrue: [self internalScrollValue: (value + pageDelta min: 1.0)]
		ifFalse: [self internalScrollValue: (value - pageDelta max: 0.0)]
! !

!ScrollBar methodsFor: 'scrolling' stamp: 'pb 7/6/2017 02:48:59'!
scrollDown: count
	self internalScrollValue: (value + (scrollDelta * count) + 0.000001 min: 1.0)! !

!ScrollBar methodsFor: 'scrolling' stamp: 'pb 7/6/2017 02:49:09'!
scrollTo: handPositionRelativeToSlider
	| v handPositionRelativeToUs |
	grabPosition ifNotNil: [
		handPositionRelativeToUs _ slider externalize: handPositionRelativeToSlider.
		v _ (self isHorizontal
			ifTrue: [ handPositionRelativeToUs x - grabPosition x ]
			ifFalse: [ handPositionRelativeToUs y - grabPosition y ])
				- borderWidth - self buttonExtent * 1.0
					/ self freeSliderRoom.
		self internalScrollValue: v ]! !

!ScrollBar methodsFor: 'scrolling' stamp: 'pb 7/6/2017 02:49:21'!
scrollUp: count
	self internalScrollValue: (value - (scrollDelta * count) - 0.000001 max: 0.0)! !


!ProgressBarMorph methodsFor: 'menu' stamp: 'pb 7/6/2017 02:57:29'!
changeProgressValue: evt
	| answer |
	answer _ FillInTheBlankMorph
		request: 'Enter new value (0 - 1.0)'
		initialAnswer: self progressValue contents asString.
	answer isEmptyOrNil ifTrue: [^ self].
	self progressValue: answer asNumber! !


!ProgressMorph methodsFor: 'accessing' stamp: 'pb 7/6/2017 02:57:41'!
done
	^progress progressValue! !

!ProgressMorph methodsFor: 'accessing' stamp: 'pb 7/6/2017 02:52:47'!
done: amountDone
	progress progressValue: ((amountDone min: 1.0) max: 0.0)! !


!InnerTextMorph methodsFor: 'editing' stamp: 'pb 7/6/2017 02:47:26'!
acceptContents
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| accepted prevSelection prevScrollValue |
	
	prevSelection _ self editor selectionInterval copy.
	prevScrollValue _ owner verticalScrollBar scrollValue.
	
	(self canDiscardEdits and: [(self hasProperty: #alwaysAccept) not]) ifTrue: [self flash. ^true].
	(self hasEditingConflicts and: [ self confirmAcceptAnyway not ]) ifTrue: [self flash. ^false].
	
	accepted _ model acceptContentsFrom: owner.
	"During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the code-holding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
	
	"some implementors of acceptContentsFrom: answer self :("
	^accepted == true 
		ifTrue: [
			model refetch.
			self editor selectFrom: prevSelection first to: prevSelection last.
			WorldState addDeferredUIMessage: [
				self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ].
				owner verticalScrollBar internalScrollValue: prevScrollValue].
			true]
		ifFalse: [ false ]! !

!methodRemoval: ProgressBarMorph #value!
ProgressBarMorph removeSelector: #value!
!methodRemoval: ProgressBarMorph #value:!
ProgressBarMorph removeSelector: #value:!
!methodRemoval: ScrollBar #setValue:!
ScrollBar removeSelector: #setValue:!
!methodRemoval: ScrollBar #value!
ScrollBar removeSelector: #value!
!methodRemoval: ScrollBar #value:!
ScrollBar removeSelector: #value:!
