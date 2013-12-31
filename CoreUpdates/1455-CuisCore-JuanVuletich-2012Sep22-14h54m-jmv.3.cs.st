'From Cuis 4.0 of 21 April 2012 [latest update: #1453] on 22 September 2012 at 3:42:33 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:31'!
morphPosition
	"Answer our position inside our owner, in owner's coordinates."

	^ location position! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:32'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	location position = aPoint ifTrue: [
		^ self ].		"Null change"

	self redrawNeeded.
	location setPosition: aPoint.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !


!AbstractSound methodsFor: 'file i/o' stamp: 'jmv 9/22/2012 15:33'!
storeSampleCount: samplesToStore bigEndian: bigEndianFlag on: aBinaryStream
	"Store my samples on the given stream at the current SoundPlayer sampling rate. If bigFlag is true, then each 16-bit sample is stored most-significant byte first (AIFF files), otherwise it is stored least-significant byte first (WAV files). If self isStereo is true, both channels are stored, creating a stereo file. Otherwise, only the left channel is stored, creating a mono file."

	| bufSize stereoBuffer reverseBytes  |
	self reset.
	bufSize _ (2 * self samplingRate rounded) min: samplesToStore.  "two second buffer"
	stereoBuffer _ SoundBuffer newStereoSampleCount: bufSize.
	reverseBytes _ bigEndianFlag ~= Smalltalk isBigEndian.

	'Storing audio...' displayProgressAt: World activeHand morphPosition
		from: 0 to: samplesToStore during: [:bar | | remaining out |
			remaining _ samplesToStore.
			[remaining > 0] whileTrue: [
				bar value: samplesToStore - remaining.
				stereoBuffer primFill: 0.  "clear the buffer"
				self playSampleCount: (bufSize min: remaining) into: stereoBuffer startingAt: 1.
				self isStereo
					ifTrue: [out _ stereoBuffer]
					ifFalse: [out _ stereoBuffer extractLeftChannel].
				reverseBytes ifTrue: [out reverseEndianness].
				(aBinaryStream isKindOf: StandardFileStream)
					ifTrue: [  "optimization for files: write sound buffer directly to file"
						aBinaryStream next: (out size // 2) putAll: out startingAt: 1]  "size in words"
					ifFalse: [  "for non-file streams:"
						1 to: out monoSampleCount do: [:i | aBinaryStream int16: (out at: i)]].
				remaining _ remaining - bufSize]].
! !


!AutoCompleter methodsFor: 'menu morph' stamp: 'jmv 9/22/2012 15:01'!
openCompletionMenu
	| theEditor |
	theEditor _ textMorph editor.
	position _ theEditor startIndex - 1.
	self closeMenu.
	self computeEntries.
	entries notEmpty
		ifTrue: [ 
			menuMorph _  AutoCompleterMorph 
				completer: self
				position: theEditor startBlock bottomLeft + textMorph morphPositionInWorld ]! !


!FormCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 9/22/2012 15:38'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	"

	| resizeFactor outerBox arrowMorph resizedForm f |
	resizeFactor _ 4.
	outerBox _ RectangleLikeMorph new.
	outerBox
		morphExtent: finalSizeInteger asPoint * resizeFactor;
		color: Color transparent.
	
	arrowMorph _ self buildArrowIn: outerBox morphBoundsInWorld.
	outerBox addMorphFront: arrowMorph.
	arrowMorph morphPosition: 12@8.	"not a clue why these numbers work..."
	
	
	f _ outerBox imageForm: 32.
	resizedForm _ f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.

	aSymbolDirection == #right ifTrue: [
		resizedForm _ resizedForm rotateBy: 90 ].
	aSymbolDirection == #down ifTrue: [
		resizedForm _ resizedForm rotateBy: 180 ].
	aSymbolDirection == #left ifTrue: [
		resizedForm _ resizedForm rotateBy:  270 ].
		
	aSymbolDirection == #up ifFalse: [
		resizedForm _ resizedForm
			copy: (resizedForm boundingBox insetBy: (resizedForm width - finalSizeInteger/ 2.0) rounded) ].
		
	^resizedForm! !


!Morph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:42'!
morphBoundsInWorld: newBounds
	| oldExtent newExtent |

	"remove senders and implementors"
	self flag: #jmvVer2.

	oldExtent _ self morphExtentInWorld.
	newExtent _ newBounds extent.
	"Moving stuff around is most likely the most common operation.
	Optimize it"
	oldExtent = newExtent ifTrue: [
		^self morphPositionInWorld: newBounds topLeft ].
	(oldExtent dotProduct: oldExtent) <= (newExtent dotProduct: newExtent) ifTrue:[
		"We're growing. First move then resize."
		self morphPositionInWorld: newBounds topLeft; morphExtent: newExtent.
	] ifFalse: [
		"We're shrinking. First resize then move."
		self morphExtent: newExtent; morphPositionInWorld: newBounds topLeft.
	].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:56'!
morphExtent
	"In our own coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^ 50 @ 40! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:41'!
morphPositionInWorld

	self flag: #jmvVer2.
	"Most likely we don't want to use global coordinates...
	In fact, we could be in many frames of reference at the same time...
	This method makes no sense at all!!"

	^self externalizeToWorld: 0@0! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:33'!
morphPositionInWorld: newPositionInWorld
	"Change the position of this morph."
	"El tema es, que tipo de coordenadas tenemos?
	En un mundo relativista, no hay un marco de referencia absoluto.
	No tiene sentido hablar de coordenadas del mundo... El mundo podria estar escalado... 
		Que tienen de especial las coordenadas del mundo?
	Coordenadas 'del hardware'? No deberia saber mucho sobre el... Puede haber multiples displays, hands de diverso tipo, remotas, virtuales...
	
	En ppio, un par de coordenadas pueden ser relativas a cualquier morph. Pareciera que necesito metodos de conversion de cualquier morph hacia mi, y de mi hacia cualquier morph... Como encontrar un marco de referencia comun????
	Dejar esto para despues. En realidad, para empezar, preciso menos: Solo preciso saber si las coordenadas estan en el morph o en su owner. Nada mas. Los eventos se iran transformando apropiadamente al moverse por el arbol, o al menos, llevaran consigo una transformacion (MatrixTransform2x3) que se ira actualizando"

	| newPositionInOwner |
	self flag: #jmvVer2.
	"This method MUST die"

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].

	location position = newPositionInOwner ifTrue: [
		^ self ].		"Null change".

	self redrawNeeded.
	location setPosition: newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/22/2012 15:37'!
editBalloonHelpContent: aString
	| reply |
	reply _ FillInTheBlank
		multiLineRequest: 'Edit the balloon help text for ' , (self printStringLimitedTo: 40)
		centerAt: self world activeHand morphPosition
		initialAnswer: (aString ifNil: [self noHelpString] ifNotNil: [aString])
		answerHeight: 200.
	reply ifNil: [^ self].  "User cancelled out of the dialog"
	(reply isEmpty or: [reply asString = self noHelpString])
		ifTrue: [self setBalloonText: nil]
		ifFalse: [self setBalloonText: reply]! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/22/2012 15:17'!
addMorphFrontFromWorldPosition: aMorph
	| positionInWorld |
	positionInWorld _ aMorph morphPositionInWorld.
	self addMorphFront: aMorph.
	aMorph morphPositionInWorld: positionInWorld! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/22/2012 15:27'!
placeEmbeddedObject: anchoredFormOrMorph

	"This method should be redone calling reasonable protocol on the canvas.
	Also check use of global coordinates..."
	self flag: #jmvVer2.
	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			anchoredFormOrMorph morphPositionInWorld:
				((destX - anchoredFormOrMorph morphWidth)@
				(lineY+ line baseline - anchoredFormOrMorph morphHeight)) -
					paraTopLeft ]
		ifFalse: [
			destY _ lineY.
			runX _ destX.
			anchoredFormOrMorph 
				displayOn: canvas grafPort destForm 
				at: destX - anchoredFormOrMorph width @ (destY + line baseline - anchoredFormOrMorph height)
				clippingBox: canvas grafPort clipRect
				rule: Form blend
				fillColor: nil ].
	^ true! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 9/22/2012 15:37'!
startUpWithoutKeyboard
	"Display and make a selection from the receiver as long as the button  is pressed. Answer the current selection.  Do not allow keyboard input into the menu"
	
	^ self startUpWithCaption: nil at: ActiveHand morphPosition allowKeyboard: false! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:57'!
basicExtent: aPoint

	self flag: #jmvVer2.
	"Lo dejo un rato porque se llama dde initialize... De ultima, es un problema? Es cierto que no se la escala aun..."
	"
	self validateOwnerNotNil.
	"
	
	"ver senders. Es en owner's o en world's ??? --- DEBE SER EN OWN COORDINATES"
	extent = aPoint ifTrue: [^ self].
	self redrawNeeded.
	extent _ aPoint.
	self someSubmorphPositionOrExtentChanged.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:56'!
morphExtent
	"In our own coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^ extent! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:56'!
morphHeight

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	^ extent y! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:57'!
morphHeight: aNumber

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	self morphExtent: extent x@aNumber! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:57'!
morphWidth

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	^ extent x! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:57'!
morphWidth: aNumber

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	self morphExtent: aNumber@extent y! !


!AutoCompleterMorph methodsFor: 'initialization' stamp: 'jmv 9/22/2012 15:38'!
setCompleter: anAutoCompleter position: aPoint 
	completer _ anAutoCompleter.
	self resetMenu.
	self openInWorld.
	self morphPosition: aPoint.! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 9/22/2012 15:38'!
createAcceptButton
	"create the [accept] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current acceptButton;
		 label: 'Accept';
		 action: #acceptClicked.
	self addMorph: result.
	result morphPosition: 29@90.
	result morphExtent: 93@27.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 9/22/2012 15:38'!
createCancelButton
	"create the [cancel] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current cancelButton;
		 label: 'Cancel';
		 action: #cancelClicked.
	self addMorph: result.
	result morphPosition: 149@90.
	result morphExtent: 93@27.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 9/22/2012 15:38'!
createQueryTextMorph: queryString 
	"create the queryTextMorph"
	| result |
	result _ StringMorph new contents: queryString.
	result lock.
	self addMorph: result.
	result morphPosition: 30@7.
	result morphExtent: 239@15.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 9/22/2012 15:38'!
createTextPaneExtent: answerExtent acceptBoolean: acceptBoolean
	"create the textPane"
	| result |
	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval
				allowStyler: true.
	result morphExtent: answerExtent.
	result borderWidth: 1; borderColor: Color lightGray.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	self addMorph: result.
	result morphPosition: 14@25.
	result morphExtent: extent-(28@62).
	^ result! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 9/22/2012 15:33'!
request: queryString
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph request: 'What is your favorite color?'"

	^ self
		request: queryString
		initialAnswer: ''
		centerAt: World activeHand morphPosition! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 9/22/2012 15:33'!
request: queryString initialAnswer: defaultAnswer 
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph
		request: 'What is your favorite color?'
		initialAnswer: 'red, no blue. Ahhh!!'"

	^ self
		request: queryString
		initialAnswer: defaultAnswer
		centerAt: World activeHand morphPosition! !


!HaloMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 9/22/2012 15:02'!
startDrag: evt with: dragHandle
	"Drag my target without removing it from its owner."

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	positionOffset _ dragHandle referencePosition - target morphPositionInWorld! !

!HaloMorph methodsFor: 'events' stamp: 'jmv 9/22/2012 15:01'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition
	"Transfer the halo to the next likely recipient"
	target ifNil:[^self delete].
	aMouseButtonEvent hand obtainHalo: self.
	positionOffset _ aMouseButtonEvent eventPosition - target morphPositionInWorld.
	"wait for drags or transfer"
	aMouseButtonEvent hand 
		waitForClicksOrDrag: self 
		event: aMouseButtonEvent
		clkSel: #transferHalo:localPosition:
		dblClkSel: nil! !

!HaloMorph methodsFor: 'events' stamp: 'jmv 9/22/2012 15:18'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Drag our target around"
	| thePoint |
	thePoint _ aMouseMoveEvent eventPosition - positionOffset.
	target morphPositionInWorld: thePoint! !

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 9/22/2012 15:01'!
popUpFor: aMorph event: aMorphicEvent
	"This message is sent by morphs that explicitly request the halo on a button click. Note: anEvent is in aMorphs coordinate frame."

	| hand anEvent |
	self flag: #workAround.	"We should really have some event/hand here..."
	anEvent _ aMorphicEvent
				ifNil: [
					hand _ aMorph world activeHand.
					hand ifNil: [ hand _ aMorph world firstHand ]. 
					hand lastEvent ]
				ifNotNil: [
					hand _ aMorphicEvent hand.
					aMorphicEvent ].
	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset _ anEvent eventPosition - aMorph morphPositionInWorld.
	self startStepping! !

!HaloMorph methodsFor: 'stepping' stamp: 'jmv 9/22/2012 15:19'!
step
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

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/22/2012 15:38'!
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
				icon morphPosition: 0@0 ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^handle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/22/2012 15:33'!
addNameBeneath: outerRectangle string: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition w nameBackground |
	w _ self world ifNil: [ target world ].
	nameBackground _ RectangleLikeMorph new
		color: (Color lightBlue alpha: 0.9).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color magenta.
	self addMorph: nameBackground.
	self addMorph: nameMorph.
	namePosition _ outerRectangle bottomCenter - ((nameMorph morphWidth // 2) @ (self handleSize negated // 2 - 1)).
	nameMorph morphPositionInWorld: (namePosition min: w viewBox bottomRight - nameMorph morphHeight + 5).
	nameBackground morphPosition: nameMorph morphPosition - 2.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/22/2012 15:18'!
doDrag: evt with: dragHandle
	| thePoint |
	evt hand obtainHalo: self.
	thePoint _ evt eventPosition - positionOffset.
	target morphPositionInWorld: thePoint! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/22/2012 15:18'!
doGrow: evt with: growHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
"Como podria andar el grow de un morph embebido en otro? andara ahora?"
newExtent _ evt eventPosition - positionOffset - target morphPositionInWorld.
	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target morphExtent: newExtent.
	growHandle morphPositionInWorld: evt eventPosition - (growHandle morphExtent // 2)! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/22/2012 15:18'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable.  Keep the relevant command object up to date."

	| degrees |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
	degrees _ (evt eventPosition - target referencePosition) degrees.
	degrees _ degrees - angleOffset degrees.
	degrees _ degrees detentBy: 10.0 atMultiplesOf: 90.0 snap: false.
	degrees = 0.0
		ifTrue: [rotHandle color: Color lightBlue]
		ifFalse: [rotHandle color: Color blue].
	rotHandle submorphsDo:
		[:m | m color: rotHandle color makeForegroundColor].
	self removeAllHandlesBut: rotHandle.

	target rotationDegrees: degrees.

	rotHandle morphPositionInWorld: evt eventPosition - (rotHandle morphExtent // 2)! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/22/2012 15:34'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		^self mouseFocus
			ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
			ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]].

	"any mouse event but mouseOver"
	lastMouseEvent _ aMouseEvent.	
	lastMouseEventTime _ Time millisecondClockValue.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			^self mouseOverHandler processMouseOver: lastMouseEvent  ]].

	aMouseEvent isMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
				ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]
		] ifFalse: [
			"Issue a synthetic move event if we're not at the position of the event"
			aMouseEvent eventPosition = self morphPosition ifFalse: [
				"Issue a mouse move event to make the receiver appear at the given position"
				self startMouseDispatch: (MouseMoveEvent new
					setType: #mouseMove
					position: aMouseEvent eventPosition
					buttons: aMouseEvent buttons
					hand: self
					stamp: aMouseEvent timeStamp) ].
			"Drop submorphs on button events"
			self hasSubmorphs
				ifTrue: [ self dropMorphs: aMouseEvent ]
				ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]]].
		self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 9/22/2012 15:34'!
attachMorph: m
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."
	| delta |
	self releaseMouseFocus. "Break focus"
	self addMorphBack: m.
	delta _ m morphExtentInWorld // 2.
	m morphPositionInWorld: (self morphPosition - delta)! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 9/22/2012 15:34'!
dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| dropEvent |
	(aMouseEvent isMouseUp and:[aMorph shouldDropOnMouseUp not]) ifTrue: [ ^self ].
	dropEvent _ DropEvent new setPosition: self morphPosition contents: aMorph hand: self.
	owner dispatchEvent: dropEvent localPosition: dropEvent eventPosition.
	dropEvent wasHandled ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self mouseOverHandler processMouseOver: aMouseEvent! !

!HandMorph methodsFor: 'paste buffer' stamp: 'jmv 9/22/2012 15:34'!
pasteMorph

	| pastee |
	pastee _ Clipboard retrieveMorph.
	pastee ifNil: [^ self inform: 'Nothing to paste.'].
	self attachMorph: pastee.
	pastee aligned: pastee referencePosition with: self morphPosition! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 9/22/2012 15:34'!
generateKeyboardEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| buttons modifiers type keyValue pressType stamp |
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [stamp _ Time millisecondClockValue].
	(evtBuf sixth <= 0 or: [
		(keyValue _ (Character iso8859s15CodeForUnicodeCodePoint: evtBuf sixth)) isNil ])
			ifTrue: [ keyValue _ Character macRomanToLatin1: evtBuf third ].
	modifiers _ evtBuf fifth.
	pressType _ evtBuf fourth.
	pressType = EventSensor eventKeyDown ifTrue: [
		type _ #keyDown.
		lastKeyDownValue _ keyValue].
	pressType = EventSensor eventKeyUp ifTrue: [type _ #keyUp].
	pressType = EventSensor eventKeyChar ifTrue: [
		type _ #keystroke.
		"If Control key pressed, and the VM answers a code below 27,
		 it means it did the translation, convert it back to regular character:
		We want to handle the meaning of ctrl ourselves."
		(modifiers anyMask: 2) ifTrue: [		"Control key pressed"
			keyValue < 27 ifTrue: [
				
				"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [		"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ 	"If Equal, Ctrl-alphabetic in Windows => do translate"
							lastKeyDownValue < 47 ]) ifTrue: [		"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue _ (modifiers anyMask: 1)
							ifFalse: [ keyValue + 96 ]	"shift not pressed: conver to lowercase letter"
							ifTrue: [ keyValue + 64 ]].	"shift pressed: conver to uppercase letter"
					]
				].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [
				modifiers _ modifiers bitOr: 8 ]
			]].
	buttons _ modifiers bitShift: 3.
	^KeyboardEvent new 
		setType: type
		buttons: buttons
		position: self morphPosition
		keyValue: keyValue
		hand: self
		stamp: stamp! !


!HandleMorph methodsFor: 'events' stamp: 'jmv 9/22/2012 15:34'!
keyStroke: aKeyboardEvent
	"Check for cursor keys"
	| keyValue |
	(owner is: #HandMorph) ifFalse: [ ^self ].
	keyValue _ aKeyboardEvent keyValue.
	keyValue = 28 ifTrue: [ ^self morphPosition: self morphPosition - (1@0) ].
	keyValue = 29 ifTrue: [ ^self morphPosition: self morphPosition + (1@0) ].
	keyValue = 30 ifTrue: [ ^self morphPosition: self morphPosition - (0@1) ].
	keyValue = 31 ifTrue: [ ^self morphPosition: self morphPosition + (0@1) ].
	"Special case for return"
	aKeyboardEvent isReturnKey ifTrue:[
		"Drop the receiver and be done"
	self flag: #arNote. "Probably unnecessary"
		owner releaseKeyboardFocus: self.
		self delete ]! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 9/22/2012 15:35'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| xcess |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [ ^self ].
	aHand world addMorphFront: self.
	self morphPosition: aHand morphPosition + (-6@20).
	xcess _ self morphPosition x + self morphExtent x - aHand world morphWidth.
	xcess > 0 ifTrue: [
		self morphPosition: self morphPosition - (xcess@0) ].
	aHand balloonHelp: self! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:38'!
adjustExtent
	"And reposition submorphs"
	| w h y |
	"make all items wide, so selection indicator is wide too"
	w _ self desiredWidth.
	y _ 0.
	self submorphsDo: [ :m |
		m
			morphPosition: 0@y;
			morphWidth: w.
		h _ m morphHeight.
		y _ y + h ].
	self morphExtent: w@y! !

!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:35'!
itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	| ptY last |
	self hasSubmorphs ifFalse: [ ^nil ].
	(aPoint > (0@0) and: [ aPoint < extent ]) ifFalse: [ ^nil ].
	ptY _ aPoint y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	self firstSubmorph morphPosition y > ptY ifTrue: [ ^nil ].
	last _ self lastSubmorph.
	last morphPosition y + last morphHeight < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^self 
		findSubmorphBinary: [ :m |
			(m morphPosition y <= ptY and: [ m morphPosition y + m morphHeight >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPosition y + (m morphHeight // 2) > ptY ifTrue: [-1] ifFalse: [1]]]! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/22/2012 15:35'!
drawLinesOn: aCanvas 
	| lColor bottomY topY tx |
	lColor _ Theme current line.
	tx _ aCanvas currentTransformation.
	topY _ (tx internalizePosition: aCanvas clipRect topLeft) y min: (tx internalizePosition: aCanvas clipRect topRight) y.
	bottomY _ (tx internalizePosition: aCanvas clipRect bottomLeft) y max: (tx internalizePosition: aCanvas clipRect bottomRight) y.
	self submorphs do: [ :submorph | 
		(submorph isExpanded or: [
			(submorph morphPosition y between: topY and: bottomY) or: [
				submorph nextSibling notNil and: [
					submorph nextSibling morphPosition y between: topY and: bottomY ] ] ])
		ifTrue: [
			self
				drawLinesFor: submorph
				on: aCanvas
				lineColor: lColor ]]
	! !


!InnerTextMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/22/2012 15:10'!
addMorphFrontFromWorldPosition: aMorph
	"Overridden for more specific re-layout and positioning"
	| positionInWorld |
	positionInWorld _ aMorph morphPositionInWorld.
	^self anchorMorph: aMorph at: positionInWorld! !


!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 9/22/2012 15:35'!
step
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [
			Cursor normal show ].
		^self stopStepping ].

	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPosition.
	hand lastEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p ]
		ifFalse: [
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: p)) ifFalse: [
				hand _ nil.
				Cursor normal show.
				self stopStepping ]]! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/22/2012 15:38'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth widths l usableHeight boundsTop boundsRight t |
	xSep _ self xSeparation.
	ySep _ self ySeparation.
	usableWidth _ boundsForLayout width - ((submorphs size + 1) * xSep).
	sumOfFixed _ submorphs sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth _ usableWidth - sumOfFixed.
	normalizationFactor _ self proportionalWidthNormalizationFactor.
	availableForPropWidth _ availableForPropWidth * normalizationFactor.
	widths _ submorphs collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	l _ ((usableWidth - widths sum) * (padding ifNil: [0]) + xSep max: 0) +  boundsForLayout left.
	usableHeight _ boundsForLayout height - (2*ySep) max: 0.
	boundsTop _ boundsForLayout top.	
	boundsRight _ boundsForLayout right.
	submorphs size to: 1 by: -1 do: [ :index | | m w h ls |
		m _ submorphs at: index.
		"major direction"
		w _ widths at: index.
		"minor direction"
		ls _ m layoutSpec.
		h _ (ls heightFor: usableHeight) min: usableHeight.
		t _ (usableHeight - h) * ls minorDirectionPadding + ySep + boundsTop.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		m
			morphPosition: l rounded @ t rounded;
			morphExtent: (w rounded min: boundsForLayout width)@ h rounded.
		w > 0 ifTrue: [
			l _ l + w + xSep min: boundsRight ]]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/22/2012 15:38'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight heights t usableWidth boundsLeft boundsBottom l |
	xSep _ self xSeparation.
	ySep _ self ySeparation.
	usableHeight _ boundsForLayout height - ((submorphs size + 1) * ySep).
	sumOfFixed _ submorphs sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight _ usableHeight - sumOfFixed.
	normalizationFactor _ self proportionalHeightNormalizationFactor.
	availableForPropHeight _ availableForPropHeight * normalizationFactor.
	heights _ submorphs collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	t _ ((usableHeight - heights sum) * (padding ifNil: [0]) + ySep max: 0) +  boundsForLayout top.
	usableWidth _ boundsForLayout width - (2*xSep) max: 0.
	boundsLeft _ boundsForLayout left.	
	boundsBottom _ boundsForLayout bottom.
	submorphs size to: 1 by: -1 do: [ :index | | m h w ls |
		m _ submorphs at: index.
		"major direction"
		h _ heights at: index.
		"minor direction"
		ls _ m layoutSpec.
		w _ (ls widthFor: usableWidth) min: usableWidth.
		l _ (usableWidth - w) * ls minorDirectionPadding + xSep + boundsLeft.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		m
			morphPosition: l rounded @ t rounded;
			morphExtent: w rounded @ (h rounded min: boundsForLayout height).
		h > 0 ifTrue: [
			t _ t + h + ySep min: boundsBottom ]]! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 9/22/2012 15:39'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane _ LayoutMorph newRow separation: 5. "1"
pane addMorph: (StringMorph contents: '1').

rect1 := BorderedRectMorph new color: (Color lightOrange); 
	morphWidth: 20; morphHeight: 30.
pane addMorph: rect1.
rect2 := BorderedRectMorph new color: (Color cyan); 
	morphWidth: 20; morphHeight: 30.
pane addMorph: rect2.
pane
	color: Color lightGreen;
	openInWorld;
	morphPosition: 120 @ 50;
	morphExtent: 180 @ 100.

pane _ LayoutMorph newRow separation: 5. "2"
pane addMorph: (StringMorph contents: '2').

rect1 := BorderedRectMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect1.
rect2 := BorderedRectMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane
	color: Color lightGreen;
	openInWorld;
	morphPosition: 320 @ 50;
	morphExtent: 180 @ 100.


pane _ LayoutMorph newRow separation: 5. "3"
pane addMorph: (StringMorph contents: '3').

rect1 := BorderedRectMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
rect2 := BorderedRectMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect2.
pane
	color: Color lightGreen;
	openInWorld;
	morphPosition: 520 @ 50;
	morphExtent: 180 @ 100! !


!MagnifierMorph methodsFor: 'menu' stamp: 'jmv 9/22/2012 15:35'!
chooseMagnification: evt
	| handle origin aHand currentMag |
	currentMag _ magnification.
	aHand _ evt ifNil: [ self world activeHand ] ifNotNil: [evt hand].
	origin _ aHand morphPosition y.
	handle _ HandleMorph new forEachPointDo:
		[ :newPoint | self magnification: (newPoint y - origin) / 8.0 + currentMag ].
	aHand attachMorph: handle.
	handle startStepping.
	self redrawNeeded. ! !

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 9/22/2012 15:36'!
sourcePoint
	"If we are being dragged use our center, otherwise use pointer position"
	^ (trackPointer not or: [owner notNil and: [owner is: #HandMorph]])
		ifTrue: [ self morphBoundsInWorld center ]
		ifFalse: [ self activeHand morphPosition ]! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 9/22/2012 15:38'!
contents: aString withMarkers: aBool inverse: inverse 
	"Set the menu item entry. If aBool is true, parse aString for embedded markers."

	| markerIndex marker |
	self contentString: nil.	"get rid of old"
	aBool ifFalse: [^super contents: aString].
	self removeAllMorphs.	"get rid of old markers if updating"
	self hasIcon ifTrue: [ self icon: nil ].
	(aString notEmpty and: [aString first = $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	self contentString: aString.	"remember actual string"
	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents:  (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph new image: marker.
	self addMorphFront: marker.
	marker morphPosition: (0@2)! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 9/22/2012 15:38'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w |
	
	titleMorph _ RectangleLikeMorph new.
	titleMorph color: Theme current menuTitleBar.
	pp _ 8@2.
	aString asString linesDo: [ :line |
		s _ StringMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s.
		s morphPosition: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphHeight: pp y; morphWidth: w + 8.
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 9/22/2012 15:36'!
popUpAdjacentTo: rightOrLeftPointInWorld forHand: hand from: sourceItem
	"Present this menu at the given point under control of the given hand."

	| delta tryToPlace selectedOffset |
	hand world startSteppingSubmorphsOf: self.
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition.
	sourceItem world addMorphFront: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self morphFullBoundsInWorld amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPointInWorld first value: false;
		value: rightOrLeftPointInWorld last - (extent x @ 0) value: false;
		value: rightOrLeftPointInWorld first value: true! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 9/22/2012 15:36'!
popUpForHand: hand in: aWorld
	| p |
	"Present this menu under control of the given hand."

	p _ hand morphPosition truncated.
	^self popUpAt: p forHand: hand in: aWorld
! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 9/22/2012 15:36'!
popUpInWorld: aWorld
	"Present this menu under control of the given hand."
	^self
		popUpAt: aWorld activeHand morphPosition
		forHand: aWorld activeHand
		in: aWorld
! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 9/22/2012 15:38'!
displayFiltered: evt
	| matchStr allItems isMatch matches feedbackMorph |
	matchStr _ self valueOfProperty: #matchString.
	allItems _ self submorphs select: [ :m |
		m isKindOf: MenuItemMorph ].
	matches _ allItems select: [ :m |
		isMatch _ matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch.
		isMatch ].
	feedbackMorph _ self valueOfProperty: #feedbackMorph.
	"warning: needs not to clip children!!"
	self flag: #jmvVer2.
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
		self submorphs last delete.
		self removeProperty: #feedbackMorph ].
	matches size = 1 ifTrue: [
		self
			selectItem: matches first
			event: evt ].! !

!MenuMorph methodsFor: 'menu' stamp: 'jmv 9/22/2012 15:38'!
sightTarget: event 
	| bullseye menu newTarget |
	owner
		ifNil: [^ self ].
	bullseye _ Point fromUserWithCursor: Cursor target.
	ActiveHand morphPosition: bullseye.
	menu _ CustomMenu new.
	(owner morphsAt: bullseye) do: [ :m |
		menu add: m printString action: m ].
	menu title: self printString, ' targets... '.
	newTarget _ menu startUp.
	newTarget
		ifNil: [^ self].
	self target: newTarget! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 9/22/2012 15:36'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu"

	^ self invokeModalAt: ActiveHand morphPosition in: ActiveWorld allowKeyboard: allowKeyboardControl! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 9/22/2012 15:38'!
adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w _ submorphs inject: 0 into: [ :prev :each |
		prev max: (
			(each respondsTo: #minItemWidth)
				ifTrue: [each minItemWidth]
				ifFalse: [each morphWidth])].

	w _ w + 4.
	p _ 5 @ 5.
	submorphs do: [ :m |
		m morphWidth: w.
		m morphPosition: p.
		p _ p + (0@(m morphHeight + 1)) ].

	self morphExtent: w @ p y + 5! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 9/22/2012 15:37'!
positionAt: aPoint relativeTo: aMenuItem
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| i yOffset sub delta |
	self adjustSubmorphsLayout.
	i _ 0.
	yOffset _ 0.
	[(sub _ self submorphs at: (i _ i + 1)) == aMenuItem]
		whileFalse: [ yOffset _ yOffset + sub morphHeight ].

	self morphPosition: aPoint - (2 @ (yOffset + 8)).

	"If it doesn't fit, show it to the left, not to the right of the hand."
	self morphBoundsInWorld right > owner world morphBoundsInWorld right
		ifTrue: [
			self moveRight: aPoint x + 1].

	"Make sure that the menu fits in the world."
	delta _ self morphBoundsInWorld amountToTranslateWithin:
		(owner world morphBoundsInWorld withHeight: ((owner world morphExtentInWorld y - 18) max: (ActiveHand morphPosition y) + 1)).
	delta = (0 @ 0) ifFalse: [ self morphPosition: self morphPosition + delta ]! !

!MenuMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:36'!
moveRight: aNumber
	self morphPosition: ((aNumber - extent x) @ self morphPosition y)! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 9/22/2012 15:35'!
informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString _ titleMorph submorphs first.
	self visible: false.
	w _ ActiveWorld.
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString width + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w displayWorld		 "show myself"
	]. 
	self delete.
	w displayWorld! !


!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 9/22/2012 15:38'!
acceptDroppingMorph: dropped event: evt 
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	| aMorph |
	aMorph := self morphToDropFrom: dropped.
	self isWorldMorph 
		ifTrue: [	"Add the given morph to this world and start stepping it if it wants to be."

			self addMorphFront: aMorph.
			(aMorph morphFullBoundsInWorld intersects: self viewBox) 
				ifFalse: [
					Beeper beep.
					aMorph morphPosition: extent // 2]]
		ifFalse: [super acceptDroppingMorph: aMorph event: evt].
	aMorph submorphsDo: [ :m | (m isKindOf: HaloMorph) ifTrue: [ m delete ]].
	self world startSteppingSubmorphsOf: aMorph! !

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:58'!
morphExtent: aPoint
	"In our own coordinates"

	self flag: #jmvVer2.
	extent = aPoint ifFalse: [
		self redrawNeeded.
		extent _ aPoint.
		self buildMagnifiedBackgroundImage.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = self morphBoundsInWorld ifFalse: [
				worldState canvas: nil.
				worldState viewBox: self morphBoundsInWorld ]]]! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 9/22/2012 15:28'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect _ Rectangle center: aPoint extent: aMorph morphFullBoundsInWorld extent.
	delta _ trialRect amountToTranslateWithin: self morphBoundsInWorld.
	self addMorph: aMorph.
	aMorph morphPositionInWorld: trialRect origin + delta.! !


!PluggableScrollPane methodsFor: 'access' stamp: 'jmv 9/22/2012 15:39'!
addToScroller: aMorph

	scroller addMorph: aMorph.
	aMorph morphPosition: 0@0! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:37'!
scrollerOffset
	^(scroller morphPosition negated + borderWidth + self xtraBorder)! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:39'!
scrollerOffset: newOffset
	| delta |
	delta _ borderWidth + self xtraBorder.
	scroller morphPosition: delta@delta - newOffset! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:39'!
updateScrollBarsBounds
	
	| t |
	hideScrollBars ifTrue: [^self].
	t _ self scrollBarClass scrollbarThickness.
	scrollBar
		morphPosition: extent x - t - borderWidth @ borderWidth;
		morphExtent: t @ self vScrollBarHeight.
	hScrollBar
		morphPosition: borderWidth @ (extent y - t - borderWidth);
		morphExtent: self hScrollBarWidth@t! !


!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 9/22/2012 15:35'!
scrollSelectionIntoView

	selectedMorph ifNotNil: [
		self flag: #jmvVer2.	"traducir mejor el rectangulo..."
		self scrollToShow: ((scroller externalize: selectedMorph morphPosition) extent: selectedMorph morphExtent) ]! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:39'!
computeSlider

	| delta |
	delta _ self buttonExtent + (self freeSliderRoom * value) asInteger.
	self isHorizontal
		ifTrue: [
			slider morphPosition: borderWidth +  delta @ borderWidth ]
		ifFalse: [
			slider morphPosition: borderWidth @ (borderWidth + delta) ] ! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 9/22/2012 15:39'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	self addMorph: downButton.
	downButton
		morphPosition: extent - borderWidth - e;
		morphExtent: e@e.
	self isHorizontal
		ifTrue: [ downButton updateRightButtonImage ]
		ifFalse: [ downButton updateDownButtonImage ]! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 9/22/2012 15:39'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ self buttonClass new.
	upButton model: self.
	self addMorph: upButton.
	upButton
		morphPosition: borderWidth@borderWidth;
		morphExtent: e@e.
	self isHorizontal
		ifTrue: [ upButton updateLeftButtonImage ]
		ifFalse: [ upButton updateUpButtonImage ].! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 9/22/2012 15:37'!
sliderGrabbedAt: handPositionRelativeToSlider

	grabPosition _ handPositionRelativeToSlider.
	sliderShadow
		morphPosition: slider morphPosition;
		morphExtent: slider morphExtent;
		show! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 9/22/2012 15:37'!
makeMeVisible 

	self world morphExtent > (0@0) ifFalse: [^ self].

	(self morphPosition >= (0@0) and: [ self morphPosition < (self world morphExtent-self labelHeight)]) ifTrue: [
		^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: extent world: self world) topLeft! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 9/22/2012 15:37'!
justDroppedInto: aMorph event: anEvent
	isCollapsed
		ifTrue: [
			self morphPosition: (self morphPosition max: 0@0) ]
		ifFalse: [
			TopWindow ~~ self ifTrue: [self activate]].
	^super justDroppedInto: aMorph event: anEvent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 9/22/2012 15:39'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| spacing box |
	spacing _ self boxExtent x + 2.

	box _ self createCloseBox.
	self addMorph: box.
	box morphPosition: 2@2.
	box morphExtent: self boxExtent.

	box _ self createCollapseBox.
	self addMorph: box.
	box morphPosition: spacing+2@2.
	box morphExtent: self boxExtent.

	box _ self createExpandBox.
	self addMorph: box.
	box morphPosition: spacing*2+2@2.
	box morphExtent: self boxExtent.

	box _ self createMenuBox.
	self addMorph: box.
	box morphPosition: spacing*3+2@2.
	box morphExtent: self boxExtent.! !

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 9/22/2012 15:40'!
openInWorld: aWorld extent: extent
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self morphPosition: (RealEstateAgent initialFrameFor: self world: aWorld) topLeft; morphExtent: extent.
	self activate.
	aWorld startSteppingSubmorphsOf: self.! !

!SystemWindow methodsFor: 'top window' stamp: 'jmv 9/22/2012 15:37'!
activateAndForceLabelToShow
	self activate.
	self morphPosition y < 0 ifTrue: [
		self morphPosition: (self morphPosition x @ 0)]! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 9/22/2012 15:40'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww b |
	thickness _ 4.
	cornerExtent _ 20.
	ww _ extent x.
	wh _ extent y.
	w _ ww - cornerExtent - cornerExtent.
	h _ wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster)
		morphPosition: cornerExtent@0;
		morphExtent: w@thickness.
	(adjusters at: #bottomAdjuster)
		morphPosition: cornerExtent@(wh-thickness);
		morphExtent: w@thickness.
	(adjusters at: #leftAdjuster)
		morphPosition: 0@cornerExtent;
		morphExtent: thickness@h.
	(adjusters at: #rightAdjuster)
		morphPosition: ww-thickness@cornerExtent;
		morphExtent: thickness@h.
	(adjusters at: #topLeftAdjuster)
		morphPosition: 0@0;
		morphExtent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster)
		morphPosition: 0@(wh-cornerExtent);
		morphExtent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster)
		morphPosition: ww-cornerExtent@0;
		morphExtent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster)
		morphPosition: ww@wh-cornerExtent;
		morphExtent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		b _ self layoutBounds.
		layoutMorph
			morphPosition: b origin;
			morphExtent: b extent ].
	
	layoutNeeded _ false! !


!TextModelMorph methodsFor: 'editor access' stamp: 'jmv 9/22/2012 15:37'!
scrollSelectionIntoView
	"Scroll my text into view if necessary and return true, else return false"

	self scrollToShow: (self editor pointBlock translatedBy: self textMorph morphPosition)! !

!methodRemoval: Morph #morphPositionInOwner!
Morph removeSelector: #morphPositionInOwner!
!methodRemoval: Morph #morphPositionInOwner:!
Morph removeSelector: #morphPositionInOwner:!
!methodRemoval: Morph #validateOwnerNotNil!
Morph removeSelector: #validateOwnerNotNil!
