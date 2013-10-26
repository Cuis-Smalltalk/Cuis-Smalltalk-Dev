'From Cuis 4.0 of 21 April 2012 [latest update: #1384] on 21 August 2012 at 10:37:28 am'!

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 8/21/2012 10:03'!
zdrawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!!"
	| topLeft |
	self flag: #jmvVer2. "y drawBoundsForRow: tambien"
	topLeft _ self morphPositionInWorld + (0 @ (row - 1 * font height)).
	^ topLeft extent: self morphExtentInWorld x @ font height! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/21/2012 09:25'!
focusIndicatorExtent
	| e |
	e _ self morphExtentInWorld - borderWidth - borderWidth.
	self vIsScrollbarShowing ifTrue: [
		e _ e x - self scrollBarClass scrollbarThickness @ e y ].
	self hIsScrollbarShowing ifTrue: [
		e _ e x @ (e y - self scrollBarClass scrollbarThickness) ].
	^e! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/21/2012 09:25'!
viewableExtent

	^self focusIndicatorExtent - (self xtraBorder * 2)! !


!InnerListMorph methodsFor: 'list management' stamp: 'jmv 8/21/2012 10:04'!
drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!!"
	| topLeft |
	self flag: #jmvVer2. "y drawBoundsForRow: tambien"
	"revisar, limpiar?"
	topLeft _ self externalize: (0 @ (row - 1 * font height)).
	^ topLeft extent: self morphExtentInOwner x @ font height! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 10:02'!
draw: item atRow: row on: canvas
	"display the given item at row row"
	| drawBounds f |
	drawBounds _ self zdrawBoundsForRow: row.
	drawBounds _ drawBounds intersect: self morphBoundsInWorld.
	f _ (item is: #Text) ifTrue: [ font emphasized: (item emphasisAt: 1) ] ifFalse: [ font ].
	canvas drawString: item in: drawBounds font: f color: (self colorForRow: row)! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 10:02'!
drawBackgroundForMulti: row on: aCanvas
	| selectionDrawBounds c |
	"shade the background darker, if this row is selected"

	selectionDrawBounds _ self zdrawBoundsForRow: row.
	selectionDrawBounds _ selectionDrawBounds intersect: self morphBoundsInWorld.
	c _ (selectedRow notNil and: [ row = selectedRow])
		ifTrue: [ Theme current listHighlightFocused: owner hasKeyboardFocus ]
		ifFalse: [ Theme current listMultiHighlightFocused: owner hasKeyboardFocus ].
	aCanvas fillRectangle: selectionDrawBounds colorOrInfiniteForm: c! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 10:02'!
drawSelectionOn: aCanvas
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self zdrawBoundsForRow: selectedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: self morphBoundsInWorld.
	aCanvas
		fillRectangle: selectionDrawBounds
		colorOrInfiniteForm: (Theme current listHighlightFocused: owner hasKeyboardFocus)! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 10:03'!
doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self handleInteraction: [
		editor doubleClickAndHalf ].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 09:36'!
adjustExtent
	"This is just a suggestion. If we do wordwrap, the width will be honored.
	But the height is whatever is appropriate for the contents!!"
	self morphExtent: owner viewableExtent! !


!MenuMorph methodsFor: 'modal control' stamp: 'jmv 8/21/2012 10:31'!
invokeModalAt: aPoint in: aWorld allowKeyboard: aBoolean
	"Invoke this menu and don't return until the user has chosen a value.
	See senders of this method for finding out how to use modal menu morphs."
	| w oldFocus actHand |
	actHand _ aWorld activeHand.
	oldFocus _ actHand keyboardFocus.
	w _ aWorld outermostWorldMorph. "containing hand"
	w doOneSubCycle.
	self	
		popUpAt: aPoint
		forHand: actHand 
		in: aWorld 
		allowKeyboard: aBoolean.
	self isModalInvokationDone: false.
	[self isInWorld & self isModalInvokationDone not] whileTrue: [w doOneSubCycle].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ self modalSelection! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 8/21/2012 10:32'!
invokeAt: aPoint in: aWorld allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	|actHand w oldFocus |
	self flag: #bob.		"is <aPoint> global or local?"
	self flag: #arNote.	"<aPoint> is local to aWorld"
	actHand _ aWorld activeHand.
	oldFocus _ actHand keyboardFocus.
	w _ aWorld outermostWorldMorph. "containing hand"
	w doOneSubCycle.
	self
		popUpAt: aPoint
		forHand: actHand
		in: aWorld 
		allowKeyboard: aBoolean.
	done _ false.
	[self isInWorld & done not] whileTrue: [w doOneSubCycle].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ mvcSelection ! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/21/2012 09:22'!
focusIndicatorRectangle

	| topLeft bottomRight |
	self flag: #jmvVer. "Prefer #focusIndicatorExtent"
	topLeft _ self morphPositionInWorld.
	bottomRight _ topLeft + self morphExtentInWorld.
	topLeft _ topLeft + borderWidth.
	bottomRight _ bottomRight - borderWidth.
	self vIsScrollbarShowing ifTrue: [
		bottomRight _ scrollBar morphPositionInWorld x -1@ bottomRight y].
	self hIsScrollbarShowing ifTrue: [
		bottomRight _ bottomRight x @ 
			(bottomRight y - self scrollBarClass scrollbarThickness)].
	^topLeft corner: bottomRight! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/21/2012 09:44'!
scrollerOffset
	^(scroller morphPositionInOwner negated + borderWidth + self xtraBorder)! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/21/2012 09:36'!
viewableHeight
	"Viewable height.
	Leave room for horizontal scrollbar if present"

	^self viewableExtent y! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/21/2012 09:36'!
viewableWidth
	"Viewable width.
	Leave room for vertical scrollbar if present"

	^self viewableExtent x! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 8/21/2012 09:33'!
scrollToShow: aRectangle
	"scroll to include as much of aRectangle as possible, where aRectangle is in the scroller's local space.
	This means that 0@0 is scrolling all the way top and all the way left"
	| delta |
	(aRectangle top >= 0 and: [
		aRectangle bottom <= self viewableHeight ])
		ifTrue: [
			"already visible"
			^self ].

	"Scroll end of selection into view if necessary"
	delta _ aRectangle amountToTranslateWithin: (0@0 extent: self viewableExtent).
	delta y ~= 0 ifTrue: [
		self scrollBy: 0@delta y ]! !


!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 8/21/2012 09:59'!
scrollSelectionIntoView

	selectedMorph ifNotNil: [
		self flag: #jmvVer2.	"traducir mejor el rectangulo..."
		self scrollToShow: ((scroller externalize: selectedMorph morphPositionInOwner) extent: selectedMorph morphExtentInOwner) ]! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 10:37'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	| row |
	"First check for option (menu) click"
	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		^ self mouseButton2Activity ].
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		self listMorph noSelection].
	row _ self rowAtLocation: aMouseButtonEvent eventPosition.
	row = 0  ifTrue: [ ^super mouseDown: aMouseButtonEvent localPosition: localEventPosition ].
	"self dragEnabled ifTrue: [aMorph highlightForMouseDown]."
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 8/21/2012 10:05'!
scrollSelectionIntoView
	"make sure that the current selection is visible"
	| row |
	row _ self getCurrentSelectionIndex.
	row = 0 ifTrue: [
		^ scrollBar setValue: scrollBar value ].
	self scrollToShow: (self listMorph drawBoundsForRow: row)! !


!SystemWindow methodsFor: 'top window' stamp: 'jmv 8/21/2012 10:36'!
activateAndSendTopToBack: aBoolean
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	self owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	oldTop _ TopWindow.
	TopWindow _ self.

	oldTop ifNotNil: [
		oldTop passivate.
		aBoolean ifTrue: [
			oldTop owner addMorphBack: oldTop ]].

	self owner firstSubmorph == self 
		ifFalse: [
			"Bring me to the top if not already"
			self owner addMorphFront: self].
	self redrawNeeded.

	"Set keyboard focus"
	self submorphToFocusKeyboard ifNotNil: [ :m |
		self world ifNotNil: [ :w | w activeHand newKeyboardFocus: m ]]! !


!TextModelMorph methodsFor: 'editor access' stamp: 'jmv 8/21/2012 09:32'!
scrollSelectionIntoView
	"Scroll my text into view if necessary and return true, else return false"

	self scrollToShow: (self editor pointBlock translatedBy: self textMorph morphPositionInOwner)! !

!methodRemoval: PluggableScrollPane #viewableBounds!
PluggableScrollPane removeSelector: #viewableBounds!
