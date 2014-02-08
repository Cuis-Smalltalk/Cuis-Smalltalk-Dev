'From Cuis 4.2 of 25 July 2013 [latest update: #1940] on 30 January 2014 at 12:56:50.357407 am'!

!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 1/29/2014 23:57'!
stepAt: millisecondClockValue in: aSystemWindow

	self updateListsAndCodeIn: aSystemWindow! !


!ChangeSorter methodsFor: 'self-updating' stamp: 'jmv 1/29/2014 23:57'!
stepAt: millisecondClockValue in: aSystemWindow

	self updateIfNecessary! !


!Morph methodsFor: 'events' stamp: 'jmv 1/30/2014 00:25'!
mouseStillDown
	"Handle a mouse move event."! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/30/2014 00:41'!
processMouseStillDown
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	self hasMouseFocus
		ifFalse: [
			^self stopSteppingSelector: #processMouseStillDown ].
	self mouseStillDown! !

!Morph methodsFor: 'focus handling' stamp: 'jmv 1/30/2014 00:39'!
hasMouseFocus

	self world ifNotNil: [ :w |
		w activeHand ifNotNil: [ :h |
			^ h mouseFocus == self ]].
	^ false! !

!Morph methodsFor: 'stepping and presenter' stamp: 'jmv 1/30/2014 00:47'!
startStepping: aSelector at: scheduledTime stepTime: stepTime
	"Start stepping the receiver"
	| w |
	w _ self world.
	w ifNotNil: [
		w startStepping: self at: scheduledTime selector: aSelector stepTime: stepTime.
		self redrawNeeded ]! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 1/29/2014 23:40'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| delta tryToPlace selectedOffset |
	self world startSteppingSubmorphsOf: self.
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition.
	sourceItem world addMorphFront: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self morphFullBoundsInWorld
			amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPointInWorld first value: false;
		value: rightOrLeftPointInWorld last - (extent x @ 0) value: false;
		value: rightOrLeftPointInWorld first value: true! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 1/29/2014 23:43'!
selectItem: aMenuItem
	selectedItem ifNotNil: [ selectedItem deselect ].
	selectedItem _ aMenuItem.
	selectedItem ifNotNil: [ selectedItem select ]! !


!PasteUpMorph methodsFor: 'stepping' stamp: 'jmv 1/30/2014 00:47'!
startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTime
	worldState startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTime.! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 1/30/2014 00:25'!
mouseStillDown
	"Acting when down (instead of waiting until releasing the button)
	also means that the button actin is repeated if the button is kept pressed.
	See #handlesMouseStillDown:"
	self performAction! !


!ScrollBar methodsFor: 'events' stamp: 'jmv 1/30/2014 00:26'!
mouseStillDown
	self scrollByPage! !


!HaloMorph methodsFor: 'stepping' stamp: 'jmv 1/30/2014 00:01'!
stepX
	| newBounds |
	target
		ifNil: [^ self].
	newBounds _ target isWorldMorph
				ifTrue: [target morphBoundsInWorld]
				ifFalse: [target worldBoundsForHalo truncated].
	newBounds = self morphBoundsInWorld
		ifTrue: [^ self].
	newBounds extent = extent
		ifTrue: [^ self morphPositionInWorld: newBounds origin].
	growingOrRotating ifFalse: [
		submorphs size > 1
			ifTrue: [self addHandles]].
	"adjust halo bounds if appropriate"
	self morphBoundsInWorld: newBounds! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:46'!
deselectTimeOut
	"Deselect timout. Now really deselect"
	owner selectedItem == self ifTrue: [
		owner selectItem: nil ]! !

!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 1/29/2014 23:46'!
deselect

	self isSelected: false.
	subMenu ifNotNil: [
		owner ifNotNil:[ owner activeSubmenu: nil ].
		self removeAlarm: #deselectTimeOut ]! !

!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 1/29/2014 23:44'!
select
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu
			popUpAdjacentTo: (Array with: self morphBoundsInWorld topRight + (10@0)
									with: self morphBoundsInWorld topLeft)
			from: self.
		subMenu selectItem: nil ]! !


!WorldState methodsFor: 'stepping' stamp: 'jmv 1/30/2014 00:46'!
startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTime
	"Add the given morph to the step list. Do nothing if it is already being stepped."
	"Aca y en todos los relacionados, selector puede ser un bloque"
	"Los arguments, no se si sirven de algo. revisar y limpiar."
	"yo quisiera que nunca haya argumentos... se pasan eventos bogus, basura."

	self stopStepping: aMorph selector: aSelector.
	stepList add:(
		StepMessage 
			scheduledAt: scheduledTime
			stepTime: stepTime
			receiver: aMorph
			selector: aSelector
			arguments: nil)! !


!Object methodsFor: 'testing' stamp: 'jmv 1/30/2014 00:06'!
stepAt: millisecondClockValue in: aWindow
"todo esto de stepping en object deberia morir!!!!!!!!!!"
	^ self step! !


!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 1/30/2014 00:47'!
monitor: anObjectExplorerWrapper
	"Start stepping and watching the given wrapper for changes."
	anObjectExplorerWrapper ifNil: [ ^self ].
	self monitorList at: anObjectExplorerWrapper put: anObjectExplorerWrapper asString.
	"guau... pedimos que nos steppeen, pero no somos un morph..."
	"tratar de mover esto a la ventana!!"
	"Esto venia al tratar de eliminar #step y #stepIn:,
	usando siempre #stepAt: y #stepAt:in:
	La pregunta ahora es, alguien que no es un morph, puede tener #stepAt:?
	O no tendria que ntener nada de stepping?????"
	self runningWorld startStepping: self at: Time localMillisecondClock selector: #step stepTime: 200! !


!Debugger methodsFor: 'private' stamp: 'jmv 1/29/2014 23:17'!
process: aProcess controller: aController context: aContext
"eliminar el segundo keyword y argumento!!!!!!!!!!!!"
	super initialize.
	Smalltalk at: #MessageTally ifPresentAndInMemory: [ :c | c terminateTimerProcess ].
	interruptedProcess _ aProcess.
	contextStackTop _ aContext.
	self newStack: (contextStackTop stackOfSize: 1).
	contextStackIndex _ 1.
	externalInterrupt _ false.
	selectingPC _ true! !


!Inspector methodsFor: 'stepping' stamp: 'jmv 1/29/2014 23:58'!
stepAt: millisecondClockValue in: aSystemWindow
	| newText |

	(Preferences smartUpdating and: [(millisecondClockValue - self timeOfLastListUpdate) > 2000]) "Not more often than once every 2 seconds"
		ifTrue: [
			self updateListsAndCodeIn: aSystemWindow.
			timeOfLastListUpdate := millisecondClockValue].

	newText := self contentsIsString
		ifTrue: [self selection]
		ifFalse: ["keep it short to reduce time to compute it"
			self selectionPrintString ].
	newText = acceptedContentsCache ifFalse: [
		acceptedContentsCache _ newText.
		self acceptedContentsChanged ]! !


!MorphicAlarm methodsFor: 'evaluating' stamp: 'jmv 1/30/2014 00:46'!
value: anArgument
"aca esta la sanata fea para ponerle el argumento a #stepAt:"
"al reconvertir, intentar evitar el falso polimorfismo. Llamar quizas #valueAt: o algo asi..."
"creo que en todos los casos va a ser nil..."
	| nArgs |
	numArgs ifNil:[numArgs _ selector numArgs].
	nArgs _ arguments ifNil:[0] ifNotNil:[arguments size].
	nArgs = numArgs ifTrue:[
		"Ignore extra argument"
		^self value].
	^arguments
		ifNil: [ receiver perform: selector with: anArgument]
		ifNotNil: [ receiver perform: selector withArguments: (arguments copyWith: anArgument)]! !


!Morph methodsFor: 'copying' stamp: 'jmv 1/30/2014 00:18'!
duplicate
	"Make and return a duplicate of the receiver"

	| newMorph |
	self okayToDuplicate ifFalse: [^ self].
	newMorph _ self copy.

"aunque yo no este stepping?????"
	newMorph startStepping.

	^ newMorph! !

!Morph methodsFor: 'event handling testing' stamp: 'jmv 1/30/2014 00:27'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp"
	^ false! !

!Morph methodsFor: 'event handling' stamp: 'jmv 1/30/2014 00:27'!
mouseStillDownStepRate
	"At what rate do I want to receive #mouseStillDown notifications?"
	^1! !

!Morph methodsFor: 'event handling' stamp: 'jmv 1/30/2014 00:27'!
mouseStillDownThreshold
	"Return the number of milliseconds after which mouseStillDown should be sent"
	^200! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/29/2014 23:20'!
processKeyDown: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."
	"localEventPosition?????"

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyDown: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/29/2014 23:21'!
processKeyUp: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."
		"localEventPosition?????"

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyUp: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/29/2014 23:21'!
processKeystroke: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."
		"localEventPosition?????"

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyStroke: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/30/2014 00:41'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."
	aMouseButtonEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		^self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition ].

	self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue:[
		self startStepping: #processMouseStillDown
			at: Time localMillisecondClock + self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/30/2014 00:52'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"System level event handling."

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [^self].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown
			at: Time localMillisecondClock
			stepTime: 1]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/30/2014 00:52'!
processMouseOver: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	 self hasMouseFocus ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		(self containsPoint: localEventPosition event: aMouseEvent) ifFalse: [
			^self ]].
	aMouseEvent hand noticeMouseOver: self event: aMouseEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/30/2014 00:41'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."

	aMouseButtonEvent wasHandled ifTrue: [^self]. "not interested"
	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		^self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		^self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition ].
	
	self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self stopSteppingSelector: #processMouseStillDown! !

!Morph methodsFor: 'initialization' stamp: 'jmv 1/30/2014 00:21'!
intoWorld: aWorld
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	aWorld ifNil: [ ^self ].
	self wantsSteps ifTrue: [ self  startStepping ].
	self submorphsDo: [ :m | m intoWorld: aWorld ]! !

!Morph methodsFor: 'stepping and presenter' stamp: 'jmv 1/30/2014 00:49'!
startStepping
	"Start getting sent the 'step' message."
	"que feo. le digo stepAt: que lleva un argumento... Cuando hay un argumento, se le pasa automaticamente el tiempo.
	mhhhhh"
	self startStepping: #stepAt: at: Time localMillisecondClock stepTime: nil! !

!Morph methodsFor: 'stepping and presenter' stamp: 'jmv 1/30/2014 00:50'!
startSteppingSelector: aSelector
	"Start getting sent the 'step' message.
	| m |
	m _ RectangleLikeMorph new..
	m openInWorld.
	m startSteppingSelector: [self print]
	Esto va a requerir que pueda pasar un bloque....
	"
	self startStepping: aSelector at: Time localMillisecondClock stepTime: nil! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:45'!
keyStroke: aKeyboardEvent 
	| matchString char asc selectable help |
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self world activeHand.
	char := aKeyboardEvent keyCharacter.
	asc := char asciiValue.
	aKeyboardEvent isReturnKey
		ifTrue: [
			selectedItem ifNotNil: [
					selectedItem hasSubMenu 
						ifTrue: [
							aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
							^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
						ifFalse: [
							"self delete."
							^selectedItem invokeWithEvent: aKeyboardEvent]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: aKeyboardEvent].
			^self].
	asc = 27 
		ifTrue: [
			"escape key"
			self
				valueOfProperty: #matchString
				ifPresentDo: [ :str | 
					str isEmpty 
						ifFalse: [
							"If filtered, first ESC removes filter"
							self setProperty: #matchString toValue: String new.
							self selectItem: nil.
							^self displayFiltered: aKeyboardEvent]].
			"If a stand-alone menu, just delete it"
			popUpOwner ifNil: [^self delete].
			"If a sub-menu, then deselect, and return focus to outer menu"
			self selectItem: nil.
			aKeyboardEvent hand newMouseFocus: popUpOwner owner.
			^aKeyboardEvent hand newKeyboardFocus: popUpOwner owner].
	(asc = 28 or: [asc = 29]) 
		ifTrue: [
			"left or right arrow key"
			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: [
					aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]].
	asc = 30 ifTrue: [^self moveSelectionDown: -1 event: aKeyboardEvent].	"up arrow key"
	asc = 31 ifTrue: [^self moveSelectionDown: 1 event: aKeyboardEvent].	"down arrow key"
	asc = 11 ifTrue: [^self moveSelectionDown: -5 event: aKeyboardEvent].	"page up key"
	asc = 12 ifTrue: [^self moveSelectionDown: 5 event: aKeyboardEvent].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsent: [String new].
	matchString := char = Character backspace 
				ifTrue: [
					matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: aKeyboardEvent keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: aKeyboardEvent.
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self world activeHand! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 1/29/2014 23:44'!
displayFiltered: evt
	| matchStr allItems isMatch matches feedbackMorph |
	matchStr _ self valueOfProperty: #matchString.
	allItems _ self submorphs select: [ :m |
		m is: #MenuItemMorph ].
	matches _ allItems select: [ :m |
		isMatch _ matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch.
		isMatch ].
	feedbackMorph _ self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph _ StringMorph new color: Color veryDarkGray.
		self addMorphBack: feedbackMorph lock.
		feedbackMorph morphPosition: (0@ -20).
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self removeProperty: #feedbackMorph ].
	matches notEmpty ifTrue: [
		self selectItem: matches first ]! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 1/29/2014 23:45'!
moveSelectionDown: direction event: evt
	"Move the current selection up or down by one, presumably under keyboard control.
	direction = +/-1"

	| index m |
	index _ (submorphs indexOf: selectedItem ifAbsent: [1-direction]) + direction.
	submorphs do: "Ensure finite"
		[:unused | m _ submorphs atWrap: index.
		((m is: #MenuItemMorph) and: [m isEnabled]) ifTrue: [
			^ self selectItem: m ].
		"Keep looking for an enabled item"
		index _ index + direction sign].
	^ self selectItem: nil! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 1/30/2014 00:20'!
startSteppingSubmorphsOf: aMorph
	"Ensure that all submorphs of the given morph that want to be stepped 
	are added to the step list. Typically used after adding a morph to the world."

	aMorph allMorphsDo: [ :m |
		m wantsSteps ifTrue: [ m startStepping ]]! !


!PluggableButtonMorph methodsFor: 'event handling testing' stamp: 'jmv 1/30/2014 00:27'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp"
	"Acting when down (instead of waiting until releasing the button)
	also means that the button action is repeated if the button is kept pressed"
	^actWhen == #buttonStillDown! !

!PluggableButtonMorph methodsFor: 'event handling' stamp: 'jmv 1/30/2014 00:27'!
mouseStillDownStepRate
	"At what rate do I want to receive #mouseStillDown notifications?"
	^40! !


!HierarchicalListMorph methodsFor: 'events-processing' stamp: 'jmv 1/30/2014 00:52'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown
			at: Time localMillisecondClock
			stepTime: 1]! !


!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 1/30/2014 00:52'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown
			at: Time localMillisecondClock
			stepTime: 1]! !


!ScrollBar methodsFor: 'event handling testing' stamp: 'jmv 1/30/2014 00:28'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp"
	^true! !


!SystemWindow methodsFor: 'stepping' stamp: 'jmv 1/29/2014 23:55'!
stepAt: millisecondClockValue
	"If the receiver is not collapsed, step it, after first stepping the model."

	(self isCollapsed not or: [ self wantsStepsWhenCollapsed ]) ifTrue: [
		model ifNotNil: [ model stepAt: millisecondClockValue in: self ].
		super stepAt: millisecondClockValue ]! !


!HaloHandleMorph methodsFor: 'event handling testing' stamp: 'jmv 1/30/2014 00:26'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp"
	^(super handlesMouseStillDown: evt) | keyStrokeSelector notNil! !


!HaloMorph methodsFor: 'stepping' stamp: 'jmv 1/30/2014 00:01'!
step
	self stepX! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 1/30/2014 00:01'!
doDup: evt with: dupHandle 
	"Ask hand to duplicate my target."

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self setTarget: (target duplicateMorph: evt).
	evt hand grabMorph: target.
	self stepX.	"update position if necessary"! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 1/30/2014 00:01'!
doGrab: evt with: grabHandle
	"Ask hand to grab my target."

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	evt hand grabMorph: target.
	self stepX. "update position if necessary"! !


!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 1/29/2014 23:21'!
processKeystroke: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."
		"localEventPosition?????"

	aKeyboardEvent wasHandled ifTrue:[^self].
	self handlesKeyboard ifFalse:	[^ self].
	aKeyboardEvent wasHandled: true.
	self keyStroke: aKeyboardEvent! !

!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 1/30/2014 00:52'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Re-implemented to allow for mouse-up move events"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown
			at: Time localMillisecondClock
			stepTime: 1]! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 1/30/2014 00:23'!
startBlinking
	"And show the cursor"
	pauseBlinking _ true.
	"Start blinking in a short while"
	textComposition ifNotNil: [ textComposition showTextCursor: true ].
	self
		startStepping: #onBlinkCursor
		at: Time localMillisecondClock
		stepTime: 500! !


!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 1/30/2014 00:23'!
startBlinking
	"And show the cursor"
	pauseBlinking _ true.
	"Start blinking in a short while"
	showTextCursor _ true.
	self
		startStepping: #onBlinkCursor
		at: Time localMillisecondClock
		stepTime: 500! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:46'!
activateSubmenu: evt
	"Activate our submenu; e.g., pass control to it"
	subMenu ifNil: [ ^false ]. "not applicable"
	(subMenu morphContainsPoint: (subMenu internalizeFromWorld: evt eventPosition)) ifFalse:[^false].
	subMenu activate: evt.
	self removeAlarm: #deselectTimeOut.
	^true! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:44'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	aMouseButtonEvent shiftPressed ifTrue: [
		^ super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].  "enable label editing" 
	aMouseButtonEvent hand newMouseFocus: owner. "Redirect to menu for valid transitions"
	owner selectItem: self! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/30/2014 00:53'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	owner hasMouseFocus ifFalse: [ ^self ].
	"This will happen if the menu has toggles in it. (for instance, the 'show...' button)
	Update the look, refresh the world and wait a bit,
	to give the user some visual feedback"
	self contentString ifNotNil: [
		self contents: self contentString withMarkers: true inverse: true.
		self refreshWorld.
		(Delay forMilliseconds: 200) wait].
	self deselect.
	self invokeWithEvent: aMouseButtonEvent! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:46'!
mouseEnter: evt
	"The mouse entered the receiver"
	owner popUpOwner ifNotNil: [ :parentItem |
		parentItem removeAlarm: #deselectTimeOut ].
	owner ifNil: [ ^self ].
	owner selectItem: self! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:47'!
mouseLeave: evt 
	"The mouse left the receiver. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	owner ifNil: [^self].
	"If we have a submenu, make sure we've got some time to enter it before actually leaving the menu item"
	subMenu 
		ifNil: [ owner selectItem: nil ]
		ifNotNil: [
			self addAlarm: #deselectTimeOut after: 500 ]! !

!methodRemoval: WorldState #startStepping:at:selector:arguments:stepTime:!
WorldState removeSelector: #startStepping:at:selector:arguments:stepTime:!
!methodRemoval: UpdatingMenuItemMorph #arrangeToStartSteppingIn:!
UpdatingMenuItemMorph removeSelector: #arrangeToStartSteppingIn:!
!methodRemoval: MenuItemMorph #deselect:!
MenuItemMorph removeSelector: #deselect:!
!methodRemoval: MenuItemMorph #deselectTimeOut:!
MenuItemMorph removeSelector: #deselectTimeOut:!
!methodRemoval: MenuItemMorph #select:!
MenuItemMorph removeSelector: #select:!
!methodRemoval: ScrollBar #mouseStillDown:!
ScrollBar removeSelector: #mouseStillDown:!
!methodRemoval: PluggableButtonMorph #mouseStillDown:!
PluggableButtonMorph removeSelector: #mouseStillDown:!
!methodRemoval: PasteUpMorph #startStepping:!
PasteUpMorph removeSelector: #startStepping:!
!methodRemoval: PasteUpMorph #startStepping:at:selector:arguments:stepTime:!
PasteUpMorph removeSelector: #startStepping:at:selector:arguments:stepTime:!
!methodRemoval: MenuMorph #popUpAdjacentTo:forHand:from:!
MenuMorph removeSelector: #popUpAdjacentTo:forHand:from:!
!methodRemoval: MenuMorph #selectItem:event:!
MenuMorph removeSelector: #selectItem:event:!
!methodRemoval: Morph #arrangeToStartStepping!
Morph removeSelector: #arrangeToStartStepping!
!methodRemoval: Morph #arrangeToStartSteppingIn:!
Morph removeSelector: #arrangeToStartSteppingIn:!
!methodRemoval: Morph #mouseStillDown:!
Morph removeSelector: #mouseStillDown:!
!methodRemoval: Morph #processMouseStillDown:localPosition:!
Morph removeSelector: #processMouseStillDown:localPosition:!
!methodRemoval: Morph #startStepping:at:arguments:stepTime:!
Morph removeSelector: #startStepping:at:arguments:stepTime:!
!methodRemoval: ChangeSorter #stepIn:!
ChangeSorter removeSelector: #stepIn:!
!methodRemoval: CodeProvider #stepIn:!
CodeProvider removeSelector: #stepIn:!
!methodRemoval: Object #stepIn:!
Object removeSelector: #stepIn:!
