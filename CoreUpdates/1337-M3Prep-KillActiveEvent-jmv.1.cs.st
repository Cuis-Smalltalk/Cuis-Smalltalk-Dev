'From Cuis 4.0 of 3 April 2012 [latest update: #4218] on 20 April 2012 at 2:50:57 pm'!

!Morph methodsFor: 'meta-actions' stamp: 'jmv 4/20/2012 14:45'!
dismissMorph
	| w |
	w _ self world ifNil:[^self].
	w deleteAllHalos; stopStepping: self.
	self delete
! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 4/20/2012 14:46'!
resizeMorph
	| handle |
	handle := HandleMorph new 
				forEachPointDo: [:newPoint | self morphExtent: newPoint - bounds topLeft].
	self activeHand attachMorph: handle.
	handle startStepping! !


!Morph methodsFor: 'meta-actions' stamp: 'jmv 4/20/2012 14:46'!
resizeFromMenu
	"Commence an interaction that will resize the receiver"

	self resizeMorph! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 4/20/2012 14:45'!
dismissViaHalo
	"The user has clicked in the delete halo-handle.  This provides a hook in case some concomitant action should be taken, or if the particular morph is not one which should be put in the trash can, for example."

	^ self dismissMorph! !


!HandMorph methodsFor: 'event handling' stamp: 'jmv 4/20/2012 14:47'!
processEvents
	"Process user input events from the local input devices."

	| evt evtBuf type hadAny |

	hadAny := false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		evt := nil.	"for unknown event types"
		type := evtBuf first.
		type = EventSensor eventTypeMouse
			ifTrue: [ evt := self generateMouseEvent: evtBuf ].
		type = EventSensor eventTypeKeyboard 
			ifTrue: [ evt := self generateKeyboardEvent: evtBuf ].
		type = EventSensor eventTypeWindow
			ifTrue: [ evt _ self generateWindowEvent: evtBuf ].
		"All other events are ignored"
		evt
			ifNil: [
				^hadAny]
			ifNotNil: [
				"Finally, handle it"
				self handleEvent: evt.
				hadAny := true.
				"For better user feedback, return immediately after a mouse event has been processed."
				evt isMouse ifTrue: [ ^hadAny ]]].
	"note: if we come here we didn't have any mouse events"
	mouseClickState 
		ifNotNil: [ 
			"No mouse events during this cycle. Make sure click states time out accordingly"
			mouseClickState
				handleEvent: (lastMouseEvent asMouseMove: (Time millisecondClockValue - lastMouseEventTime max: 0))
				from: self ].
	^hadAny! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 4/20/2012 14:47'!
sendEvent: anEvent
	"Send the event to the morph currently holding the focus, or if none to the owner of the hand."

	^owner processEvent: anEvent! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 4/20/2012 14:48'!
sendFocusEvent: anEvent to: focusHolder in: world
	"Send the event to focusHolder, the morph currently holding the focus"
	| result |
	world becomeActiveDuring: [
		ActiveHand _ self.
		result _ focusHolder handleFocusEvent: anEvent ].
	^result! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 4/20/2012 14:48'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth widths l usableHeight boundsTop boundsRight r t b |
	xSep _ self xSeparation.
	ySep _ self ySeparation.
	usableWidth _ boundsForLayout width - ((submorphs size + 1) * xSep).
	sumOfFixed _ submorphs sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth _ usableWidth - sumOfFixed.
	padding ifNil: [	"shrink"
		availableForPropWidth = 0 ifFalse: [
			self flag: #jmvVer2.
			self width: self width - availableForPropWidth.
			^ self layoutSubmorphsAndComputeFullBounds ]].
	normalizationFactor _ self proportionalWidthNormalizationFactor.
	availableForPropWidth _ availableForPropWidth * normalizationFactor.
	widths _ submorphs collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	l _ ((usableWidth - widths sum) * (padding ifNil: [0]) + xSep max: 0) +  boundsForLayout left.
	usableHeight _ boundsForLayout height - (2*ySep) max: 0.
	boundsTop _ boundsForLayout top.	
	boundsRight _ boundsForLayout right.
	submorphs size to: 1 by: -1 do: [ :index | | m w h ls |
		m _ submorphs at: index.
		w _ widths at: index.
		"major direction"
		r _ l + w min: boundsRight.
		"minor direction"
		ls _ m layoutSpec.
		h _ (ls heightFor: usableHeight) min: usableHeight.
		t _ (usableHeight - h) * ls minorDirectionPadding + ySep + boundsTop.
		b _ t + h.
		"Set bounds and adjust major direction for next step"
		m bounds: (l rounded @ t rounded corner: r rounded @ b rounded).
		w > 0 ifTrue: [
			l _ r + xSep min: boundsRight ]]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 4/20/2012 14:48'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight heights t usableWidth boundsLeft boundsBottom b l r |
	xSep _ self xSeparation.
	ySep _ self ySeparation.
	usableHeight _ boundsForLayout height - ((submorphs size + 1) * ySep).
	sumOfFixed _ submorphs sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight _ usableHeight - sumOfFixed.
	padding ifNil: [	"shrink"
		availableForPropHeight = 0 ifFalse: [
			self flag: #jmvVer2.
			self height: self height - availableForPropHeight.
			^ self layoutSubmorphsAndComputeFullBounds ]].
	normalizationFactor _ self proportionalHeightNormalizationFactor.
	availableForPropHeight _ availableForPropHeight * normalizationFactor.
	heights _ submorphs collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	t _ ((usableHeight - heights sum) * (padding ifNil: [0]) + ySep max: 0) +  boundsForLayout top.
	usableWidth _ boundsForLayout width - (2*xSep) max: 0.
	boundsLeft _ boundsForLayout left.	
	boundsBottom _ boundsForLayout bottom.
	submorphs size to: 1 by: -1 do: [ :index | | m h w ls |
		m _ submorphs at: index.
		h _ heights at: index.
		"major direction"
		b _ t + h min: boundsBottom.
		"minor direction"
		ls _ m layoutSpec.
		w _ (ls widthFor: usableWidth) min: usableWidth.
		l _ (usableWidth - w) * ls minorDirectionPadding + xSep + boundsLeft.
		r _ l + w.
		"Set bounds and adjust major direction for next step"
		m bounds: (l rounded @ t rounded corner: r rounded @ b rounded).
		h > 0 ifTrue: [
			t _ b + ySep min: boundsBottom ]]! !


!PasteUpMorph methodsFor: 'initialization' stamp: 'jmv 4/20/2012 14:49'!
becomeActiveDuring: aBlock
	"Make the receiver the ActiveWorld during the evaluation of aBlock.
	Note that this method does deliberately *not* use #ensure: to prevent
	re-installation of the world on project switches."
	| priorWorld priorHand |
	priorWorld _ ActiveWorld.
	priorHand _ ActiveHand.
	ActiveWorld _ self.
	ActiveHand _ self hands first. "default"
	aBlock
		on: Error
		do: [:ex | 
			ActiveWorld _ priorWorld.
			ActiveHand _ priorHand.
			ex pass]! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 4/20/2012 14:50'!
install
	owner := nil.	"since we may have been inside another world previously"
	ActiveWorld := self.
	ActiveHand := self hands first.	"default"
	submorphs do: [:ss | ss owner ifNil: [ss privateOwner: self]].
	"Transcript that was in outPointers and then got deleted."
	self viewBox: Display boundingBox.
	Sensor flushAllButDandDEvents.
	worldState handsDo: [:h | h initForEvents].
	self borderWidth: 0.	"default"
	SystemWindow noteTopWindowIn: self.
	self displayWorldSafely! !


!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 4/20/2012 14:50'!
interruptNameX: labelString
	"Create a Notifier on the active scheduling process with the given label."
	| preemptedProcess projectProcess |
	
	ActiveHand ifNotNil:[ActiveHand interrupted].
	ActiveWorld _ World. "reinstall active globals"
	ActiveHand _ World activeHand.
	ActiveHand interrupted. "make sure this one's interrupted too"

	projectProcess _ ProjectX uiProcessX.	"we still need the accessor for a while"
	preemptedProcess _ Processor preemptedProcess.
	"Only debug preempted process if its priority is >= projectProcess' priority"
	preemptedProcess priority < projectProcess priority ifTrue:[
		projectProcess suspend.
		preemptedProcess _ projectProcess.
	] ifFalse:[
		preemptedProcess suspend.
	].
	Debugger openInterrupt: labelString onProcess: preemptedProcess
! !

!methodRemoval: Morph #dismissMorph:!
Morph removeSelector: #dismissMorph:!
!methodRemoval: Morph #resizeMorph:!
Morph removeSelector: #resizeMorph:!
