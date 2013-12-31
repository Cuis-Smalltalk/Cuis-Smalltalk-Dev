'From Cuis 4.0 of 21 April 2012 [latest update: #1373] on 18 August 2012 at 6:18:48 pm'!
"Change Set:		1374-CuisCore-JuanVuletich-2012Aug18-18h16m
Date:			18 August 2012
Author:			Juan Vuletich

<your descriptive text goes here>"
World eventHandler: nil.
ActiveHand eventHandler: nil!

!classDefinition: #HaloSpec category: #'Morphic-Halos'!
Object subclass: #HaloSpec
	instanceVariableNames: 'addHandleSelector horizontalPlacement verticalPlacement color iconSymbol hoverHelp '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Halos'!
!classDefinition: #HaloHandleMorph category: #'Morphic-Halos'!
Morph subclass: #HaloHandleMorph
	instanceVariableNames: 'keyStrokeSelector mouseUpSelector mouseMoveSelector mouseDownSelector '
	classVariableNames: 'CircleForm '
	poolDictionaries: ''
	category: 'Morphic-Halos'!
!classDefinition: #HaloMorph category: #'Morphic-Halos'!
Morph subclass: #HaloMorph
	instanceVariableNames: 'target innerTarget positionOffset angleOffset growingOrRotating haloBox '
	classVariableNames: 'HandleSize Icons '
	poolDictionaries: ''
	category: 'Morphic-Halos'!

!HaloSpec methodsFor: 'accessing' stamp: 'jmv 8/18/2012 18:03'!
horizontalPlacement: hp verticalPlacement: vp color: col iconSymbol: is addHandleSelector: sel hoverHelp: aString
	horizontalPlacement _ hp.
	verticalPlacement _ vp.
	color _ col.
	iconSymbol _ is asSymbol.
	addHandleSelector _ sel.
	hoverHelp _ aString! !

!HaloSpec methodsFor: 'accessing' stamp: 'jmv 8/18/2012 18:04'!
hoverHelp
	^hoverHelp! !


!HaloHandleMorph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:22'!
handleMouseDown: aMorphicEvent

	super handleMouseDown: aMorphicEvent.
	self send: mouseDownSelector withEvent: aMorphicEvent! !

!HaloHandleMorph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:21'!
handleMouseMove: aMorphicEvent

	super handleMouseMove: aMorphicEvent.
	aMorphicEvent anyButtonPressed ifTrue: [
		self send: mouseMoveSelector withEvent: aMorphicEvent ]! !

!HaloHandleMorph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:28'!
handleMouseUp: aMorphicEvent

	super handleMouseUp: aMorphicEvent.
	self send: mouseUpSelector withEvent: aMorphicEvent! !

!HaloHandleMorph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:08'!
handlesKeyboard
	"Return true if the receiver wishes to handle keyboard events"
	^super handlesKeyboard! !

!HaloHandleMorph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:27'!
handlesMouseDown: evt
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^(super handlesMouseDown: evt) | 
		mouseDownSelector notNil | mouseMoveSelector notNil | mouseUpSelector notNil! !

!HaloHandleMorph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:08'!
handlesMouseOver: evt
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?"
	^super handlesMouseOver: evt! !

!HaloHandleMorph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:09'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"
	^(super handlesMouseStillDown: evt) | keyStrokeSelector notNil! !

!HaloHandleMorph methodsFor: 'accessing' stamp: 'jmv 8/18/2012 16:38'!
keyStrokeSelector: aSymbol
	keyStrokeSelector _ aSymbol! !

!HaloHandleMorph methodsFor: 'accessing' stamp: 'jmv 8/18/2012 16:38'!
mouseDownSelector: aSymbol
	mouseDownSelector _ aSymbol! !

!HaloHandleMorph methodsFor: 'accessing' stamp: 'jmv 8/18/2012 16:38'!
mouseMoveSelector: aSymbol
	mouseMoveSelector _ aSymbol! !

!HaloHandleMorph methodsFor: 'accessing' stamp: 'jmv 8/18/2012 16:38'!
mouseUpSelector: aSymbol
	mouseUpSelector _ aSymbol! !

!HaloHandleMorph methodsFor: 'act' stamp: 'jmv 8/18/2012 17:19'!
send: selector withEvent: aMorphicEvent
	| arity |
	owner ifNil: [ ^ self ].
	selector ifNil: [ ^ self ].
	arity _ selector numArgs.
	arity = 0 ifTrue: [ ^ owner perform: selector ].
	arity = 1 ifTrue: [
		^ owner
			perform: selector
			with: aMorphicEvent ].
	arity = 2 ifTrue: [
		^ owner
			perform: selector
			with: aMorphicEvent
			with: self ].
	self error: 'Event handling selectors must be Symbols and take 0-2 arguments'! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 8/18/2012 18:04'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint iconName colorToUse icon |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	handle morphBoundsInWorld: (Rectangle center: aPoint extent: HandleSize asPoint).
	(iconName _ handleSpec iconSymbol) ifNotNil: [
			| form |
			form _ Icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon.
				icon morphPositionInOwner: 0@0 ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^handle! !

!HaloMorph methodsFor: 'forward to target' stamp: 'jmv 8/18/2012 16:33'!
chooseEmphasisOrAlignment
	target chooseEmphasisOrAlignment! !

!HaloMorph methodsFor: 'forward to target' stamp: 'jmv 8/18/2012 16:34'!
chooseFont
	target chooseFont! !

!HaloMorph methodsFor: 'forward to target' stamp: 'jmv 8/18/2012 16:25'!
deleteBalloon
	target deleteBalloon! !

!HaloMorph methodsFor: 'forward to target' stamp: 'jmv 8/18/2012 16:25'!
mouseDownOnHelpHandle: anEvent
	target mouseDownOnHelpHandle: anEvent! !


!CustomMenu methodsFor: 'construction' stamp: 'jmv 8/18/2012 15:50'!
addList: listOfTuplesAndDashes
	"Add a menu item to the receiver for each tuple in the given list of the form (<what to show> <selector>). Add a line for each dash (-) in the list."

	listOfTuplesAndDashes do: [:aTuple |
		aTuple == #-
			ifTrue: [self addLine]
			ifFalse: [self add: aTuple first action: aTuple second]]

	"
	CustomMenu new addList: #(
		('apples' buyApples)
		('oranges' buyOranges)
		-
		('milk' buyMilk)); startUp
	"

! !


!FileStream class methodsFor: 'file reader services' stamp: 'jmv 8/18/2012 15:49'!
install: fullName
	"File in the entire contents of the file specified by the name provided.
	Do not affect the user change sets, store changes in separate one"

	| localName |
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: localName do: [ self fileIn: fullName ].
	('Installed ChangeSet: ', localName) print! !


!HaloSpec methodsFor: 'accessing' stamp: 'jmv 8/18/2012 18:01'!
addHandleSelector
	^ addHandleSelector! !

!HaloSpec methodsFor: 'accessing' stamp: 'jmv 8/18/2012 18:01'!
color
	^ color! !

!HaloSpec methodsFor: 'accessing' stamp: 'jmv 8/18/2012 18:02'!
horizontalPlacement
	^ horizontalPlacement! !

!HaloSpec methodsFor: 'accessing' stamp: 'jmv 8/18/2012 18:02'!
iconSymbol
	^ iconSymbol! !

!HaloSpec methodsFor: 'accessing' stamp: 'jmv 8/18/2012 18:02'!
verticalPlacement
	^ verticalPlacement! !


!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:51'!
click: evt
	"Handle a single-click event. This message is only sent to clients that request it by sending #waitForClicksOrDrag:event: to the initiating hand in their mouseDown: method. This default implementation does nothing."! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:51'!
doubleClick: evt
	"Handle a double-click event. This message is only sent to clients that request it by sending #waitForClicksOrDrag:event: to the initiating hand in their mouseDown: method. This default implementation does nothing."! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:51'!
handlesKeyboard
	"Return true if the receiver wishes to handle keyboard events"

	^ false
! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:52'!
handlesMouseDown: evt
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	"NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true." 

	^ false
! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:52'!
handlesMouseOver: evt
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?  The default response is false." 

	^ false! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:52'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"
	^ false
! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:55'!
keyStroke: anEvent
	"Handle a keystroke event."

	(self focusKeyboardFor: anEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: anEvent)
		ifTrue: [ ^ self ]! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:53'!
mouseDown: evt
	"Handle a mouse down event."! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:53'!
mouseEnter: evt
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed."! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:53'!
mouseLeave: evt
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed."! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:54'!
mouseMove: evt
	"Handle a mouse move event."! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:54'!
mouseStillDown: evt
	"Handle a mouse move event."! !

!Morph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:54'!
mouseUp: evt
	"Handle a mouse up event."! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/18/2012 16:22'!
addHandlesTo: aHaloMorph box: box
	"Add halo handles to the halo.  Apply the halo filter if appropriate"
	| wantIt |
	aHaloMorph haloBox: box.
	Preferences haloSpecifications do: [ :aSpec |
		wantIt _ Preferences selectiveHalos
			ifTrue: [
				self
					wantsHaloHandleWithSelector: aSpec addHandleSelector
					inHalo: aHaloMorph ]
			ifFalse: [ true ].
		wantIt ifTrue: [
			aHaloMorph
				perform: aSpec addHandleSelector
				with: aSpec ]].
	aHaloMorph target
		addOptionalHandlesTo: aHaloMorph
		box: box.! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/18/2012 16:22'!
addWorldHandlesTo: aHaloMorph box: box
	aHaloMorph haloBox: box.
	Preferences haloSpecificationsForWorld do: [ :aSpec |
		aHaloMorph
			perform: aSpec addHandleSelector
			with: aSpec ].
	aHaloMorph target
		addOptionalHandlesTo: aHaloMorph
		box: box.! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 8/18/2012 16:23'!
mouseButton3Down: anEvent
	"Special gestures (cmd-mouse on the Macintosh; Alt-mouse on Windows and Unix) allow a mouse-sensitive morph to be moved or bring up a halo for the morph."
	| h doNotDrag |
	h _ anEvent hand halo.
	"Prevent wrap around halo transfers originating from throwing the event back in"
	doNotDrag _ false.
	h ifNotNil:[
		(h target == self) ifTrue: [ doNotDrag _ true].
		(h target hasOwner: self) ifTrue: [ doNotDrag _ true].
		(self hasOwner: h target) ifTrue: [ doNotDrag _ true]].

	"cmd-drag on flexed morphs works better this way"
	h _ self addHalo: anEvent.
	doNotDrag ifTrue:[^self].
	"Initiate drag transition if requested"
	anEvent hand 
		waitForClicksOrDrag: h
		event: anEvent
		clkSel: nil
		dblClkSel: nil.
	"Pass focus explicitly here"
	anEvent hand newMouseFocus: h.! !


!HaloMorph methodsFor: 'accessing' stamp: 'jmv 8/18/2012 16:23'!
setTarget: aMorph
	"Private!! Set the target without adding handles."

	target _ aMorph

! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:40'!
addCollapseHandle: handleSpec
	"Add the collapse handle, with all of its event handlers set up, unless the target's owner is not the world or the hand."

	target owner
		ifNil: [ ^self ]	"nil happens, amazingly"
		ifNotNil: [ :to |
			(to isWorldMorph or: [ to is: #HandMorph ])
				ifFalse: [ ^self ]].
		
	(self addHandle: handleSpec)
		mouseDownSelector: #mouseDownInCollapseHandle:with:;
		mouseMoveSelector: #setDismissColor:with:;
		mouseUpSelector: #maybeCollapse:with:! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:37'!
addDebugHandle: handleSpec

	Preferences debugHaloHandle ifTrue: [
		(self addHandle: handleSpec)
			mouseDownSelector: #doDebug:with: ]
! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:31'!
addDismissHandle: handleSpec

	(self addHandle: handleSpec)
		mouseDownSelector: #setDismissColor:with:;
		mouseMoveSelector: #setDismissColor:with:;
		mouseUpSelector: #maybeDismiss:with:! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:32'!
addDragHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #startDrag:with:;
		mouseMoveSelector: #doDrag:with:! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:37'!
addDupHandle: haloSpec
	(self addHandle: haloSpec) mouseDownSelector:#doDup:with:! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:39'!
addFontEmphHandle: haloSpec

	(target is: #InnerTextMorph) ifTrue: [
		(self addHandle: haloSpec) mouseDownSelector: #chooseEmphasisOrAlignment ]! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:38'!
addFontSizeHandle: haloSpec

	(target is: #InnerTextMorph) ifTrue: [
		(self addHandle: haloSpec) mouseDownSelector: #chooseFont]! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:36'!
addGrabHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #doGrab:with:! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:23'!
addGrowHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #startGrow:with:;
		mouseMoveSelector: #doGrow:with:! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:29'!
addHelpHandle: haloSpec
	(self addHandle: haloSpec)
		mouseDownSelector: #mouseDownOnHelpHandle:;
		mouseMoveSelector: #deleteBalloon! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:36'!
addMenuHandle: haloSpec

	(self addHandle: haloSpec) mouseDownSelector: #doMenu:with:! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:35'!
addRecolorHandle: haloSpec
	"Add a recolor handle to the receiver, if appropriate"

	(self addHandle: haloSpec) mouseUpSelector: #doRecolor:with:! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 8/18/2012 17:35'!
addRotateHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #startRot:with:;
		mouseMoveSelector: #doRot:with:! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/18/2012 16:21'!
addHandlesForWorldHalos
	"Add handles for world halos, like the man said"

	| box w |
	w _ self world ifNil: [target world].
	self removeAllMorphs.  "remove old handles, if any"
	self morphBoundsInWorld: target morphBoundsInWorld.
	box _ w morphBoundsInWorld insetBy: 9.
	target addWorldHandlesTo: self box: box.

	self
		addNameBeneath: (box insetBy: (0@0 corner: 0@10))
		string: (target printStringLimitedTo: 40).
	growingOrRotating _ false.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/18/2012 16:21'!
doDebug: evt with: menuHandle
	"Ask hand to invoke the a debugging menu for my inner target.  If shift key is down, immediately put up an inspector on the inner target"

	| menu |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self world displayWorld.
	evt shiftPressed ifTrue: [
		self delete.
		^ target inspect].

	menu _ target buildDebugMenu: evt hand.
	menu addTitle: (target printStringLimitedTo: 40).
	menu popUpInWorld: self world! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/18/2012 16:21'!
doMenu: evt with: menuHandle
	"Ask hand to invoke the halo menu for my inner target."

	| menu |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self world displayWorld.
	menu _ target buildHandleMenu: evt hand.
	target addTitleForHaloMenu: menu.
	menu popUpInWorld: self world.
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/18/2012 16:21'!
doRecolor: evt with: aHandle
	"The mouse went down in the 'recolor' halo handle.  Allow the user to change the color of the innerTarget"

	evt hand obtainHalo: self.
	(aHandle containsPoint: evt eventPosition)
		ifFalse: [  "only do it if mouse still in handle on mouse up"
			self delete.
			target addHalo: evt]
		ifTrue: [
			target changeColor]! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:51'!
processKeyStroke: evt
	| action |

	(acceptOnCR and: [evt isReturnKey])
		ifTrue: [^ self acceptContents].

	self pauseBlinking.
	evt isReturnKey ifTrue: [	"Return - check for special action"
		action _ self crAction.
		action ifNotNil: [
			^action value]].
	self handleInteraction: [ editor processKeyStroke: evt ].
	self updateFromParagraph.

	self scrollSelectionIntoView! !


!MorphExtension methodsFor: 'other' stamp: 'jmv 8/18/2012 17:50'!
isDefault
	"Return true if the receiver is a default and can be omitted"
	layoutSpec ifNotNil: [ ^ false ].
	otherProperties ifNotNil: [
		otherProperties isEmpty ifFalse: [ ^ false ]].
	^ true.! !

!MorphExtension methodsFor: 'printing' stamp: 'jmv 8/18/2012 17:50'!
printOn: aStream
	"Append to the argument, aStream, a sequence of characters that 
	identifies the receiver."
	super printOn: aStream.
	aStream nextPutAll: ' (', self identityHash printString, ')'.
	layoutSpec ifNotNil: [ aStream nextPutAll: ' [layoutSpec = ] ' , layoutSpec printString , '] ' ].
	(otherProperties isNil or: [ otherProperties isEmpty ]) ifTrue: [ ^ self ].
	aStream nextPutAll: ' [other: '.
	otherProperties keysDo: [ :aKey |
		aStream nextPutAll: ' (' , aKey , ' -> ' , (otherProperties at: aKey) printString , ')' ].
	aStream nextPut: $].! !

!MorphExtension methodsFor: 'private' stamp: 'jmv 8/18/2012 17:50'!
sortedPropertyNames
	"answer the receiver's property names in a sorted way"
	| props |
	props _ WriteStream on: (Array new: 10).
	layoutSpec ifNotNil: [ props nextPut: #layoutSpec ].
	otherProperties ifNotNil: [
		otherProperties associationsDo: [ :a |
			props nextPut: a key ]].
	^ props contents sort: [ :s1 :s2 |
		s1 <= s2 ]! !


!PluggableButtonMorph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 15:58'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"
	"Acting when down (instead of waiting until releasing the button)
	also means that the button action is repeated if the button is kept pressed"
	^actWhen == #buttonStillDown! !


!Preferences class methodsFor: 'halos' stamp: 'jmv 8/18/2012 18:00'!
iconicHaloSpecifications
	"Answer an array that characterizes the locations, colors, icons, and selectors of the halo handles that may be used in the iconic halo scheme"

	"
	Preferences resetHaloSpecifications
	"

^ #(
	"selector						horiz				vert					color info						icon key 						balloon help
	 ---------						------				-----------			-------------------------------		---------------"
	(addCollapseHandle:		left				topCenter		(tan)							haloCollapseIcon 			'Collapse')
	(addDebugHandle:			right				topCenter		(orange)						haloDebugIcon 				'Debug')
	(addDismissHandle:			left				top				(red)							haloDismissIcon 				'Remove')
	(addRotateHandle:			left				bottom			(blue)							haloRotateIcon 				'Rotate')
	(addMenuHandle:			leftCenter		top				(blue lighter)					haloMenuIcon 				'Menu')
	(addGrabHandle:				center			top				(black)							haloGrabIcon 				'Pick up')
	(addDragHandle:				rightCenter		top				(brown)						haloDragIcon 				'Move')
	(addDupHandle:				right				top				(green)						haloDuplicateIcon 			'Duplicate')	
	(addHelpHandle:				center			bottom			(lightBlue)					haloHelpIcon 				'Help')
	(addGrowHandle:			right				bottom			(yellow)						haloScaleIcon 				'Change size')
	(addFontSizeHandle:		leftCenter		bottom			(lightGreen)					haloFontSizeIcon 			'Change font')
	(addFontEmphHandle:		rightCenter		bottom			(lightBrown darker)			haloFontEmphasisIcon 		'Emphasis & alignment')
	(addRecolorHandle:			right				bottomCenter	(magenta darker)			haloColorIcon 				'Change color')
)! !

!Preferences class methodsFor: 'halos' stamp: 'jmv 8/18/2012 18:04'!
installHaloSpecsFromArray: anArray

	| aColor |
	^ Parameters at: #HaloSpecs put: (anArray collect: [ :each |
				aColor _ Color.
				each fourth do: [ :sel | aColor _ aColor perform: sel].
				HaloSpec new 
					horizontalPlacement: each second
					verticalPlacement: each third 
					color: aColor
					iconSymbol: each fifth
					addHandleSelector: each first
					hoverHelp: each sixth])! !


!TextModelMorph methodsFor: 'event handling' stamp: 'jmv 8/18/2012 17:53'!
keyStroke: aKeyboardEvent
	"A keystroke was hit while the receiver had keyboard focus.  Pass the keywtroke on to my textMorph, and and also, if I have an event handler, pass it on to that handler"

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	self textMorph keyStroke: aKeyboardEvent! !

!methodRemoval: MorphExtension #eventHandler!
MorphExtension removeSelector: #eventHandler!
!methodRemoval: MorphExtension #eventHandler:!
MorphExtension removeSelector: #eventHandler:!
!methodRemoval: HierarchicalListMorph #initialize!
HierarchicalListMorph removeSelector: #initialize!
!methodRemoval: HaloMorph #addHandle:on:send:to:!
HaloMorph removeSelector: #addHandle:on:send:to:!
!methodRemoval: HaloMorph #innerTarget!
HaloMorph removeSelector: #innerTarget!
!methodRemoval: HaloMorph #mouseDownInDimissHandle:with:!
HaloMorph removeSelector: #mouseDownInDimissHandle:with:!
!methodRemoval: HaloMorph #strokeGrow:with:!
HaloMorph removeSelector: #strokeGrow:with:!
!classDefinition: #HaloMorph category: #'Morphic-Halos'!
Morph subclass: #HaloMorph
	instanceVariableNames: 'target positionOffset angleOffset growingOrRotating haloBox'
	classVariableNames: 'HandleSize Icons'
	poolDictionaries: ''
	category: 'Morphic-Halos'!
!classDefinition: #HaloHandleMorph category: #'Morphic-Halos'!
Morph subclass: #HaloHandleMorph
	instanceVariableNames: 'mouseDownSelector mouseUpSelector mouseMoveSelector keyStrokeSelector'
	classVariableNames: 'CircleForm'
	poolDictionaries: ''
	category: 'Morphic-Halos'!
!methodRemoval: Morph #balloonHelpTextForHandle:!
Morph removeSelector: #balloonHelpTextForHandle:!
!methodRemoval: Morph #eventHandler!
Morph removeSelector: #eventHandler!
!methodRemoval: Morph #eventHandler:!
Morph removeSelector: #eventHandler:!
!methodRemoval: Morph #on:send:to:!
Morph removeSelector: #on:send:to:!
!methodRemoval: Morph #on:send:to:withValue:!
Morph removeSelector: #on:send:to:withValue:!
!methodRemoval: HaloSpec #horizontalPlacement:verticalPlacement:color:iconSymbol:addHandleSelector:!
HaloSpec removeSelector: #horizontalPlacement:verticalPlacement:color:iconSymbol:addHandleSelector:!
!classDefinition: #HaloSpec category: #'Morphic-Halos'!
Object subclass: #HaloSpec
	instanceVariableNames: 'addHandleSelector horizontalPlacement verticalPlacement color iconSymbol hoverHelp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Halos'!

!HaloSpec reorganize!
('as yet unclassified')
('printing' printOn:)
('accessing' addHandleSelector color horizontalPlacement horizontalPlacement:verticalPlacement:color:iconSymbol:addHandleSelector:hoverHelp: hoverHelp iconSymbol verticalPlacement)
!

!classRemoval: #EventHandler!
Smalltalk removeClassNamed: #EventHandler!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences resetHaloSpecifications!

