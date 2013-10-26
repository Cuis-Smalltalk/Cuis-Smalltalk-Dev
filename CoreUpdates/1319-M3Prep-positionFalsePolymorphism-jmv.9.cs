'From Cuis 4.0 of 3 April 2012 [latest update: #1255] on 10 April 2012 at 2:34:25 pm'!

!DropEvent methodsFor: 'accessing' stamp: 'jmv 12/11/2011 23:10'!
eventPosition
	^position! !


!Morph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:42'!
morphPosition
"
Cuando tenga resueltos los setters (o quizas al mismo tiempo),
ver que senders del getter quieren en realidad #zzpositionInOwner y quienes #zzpositionInWorld.
Espero que pocos hablen en coordenadas del world!!
"
	^ bounds topLeft! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:43'!
morphPosition: aPoint
	"Change the position of this morph and and all of its submorphs."

	"Detesto el falso polimorfismo con streams. Ponerle otro nombre a esto!!
	Igual, todavia esta #position, el getter, que es igual de feo..."
	
	"
	VER SENDERS. Acomodar. el argumento es en coord del owner o del world?
	Convertir los senders a senders de #zzpositionInOwner: o #zzpositionInWorld
	Espero que pocos hablen en coordenadas del world!!
	"

	| delta |
	delta _ aPoint - bounds topLeft.
	(delta x = 0 and: [delta y = 0]) ifTrue: [^ self].  "Null change"
	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.
	self privateFullMoveBy: delta.

	"En realidad, quiero coordenadas relativas al owner!!"
"	position _ aPoint."

	self redrawNeeded
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !


!HaloMorph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:43'!
morphPosition: pos
	"Halos display imprefectly if their coordinates are non-integral
		-- especially the direction handles."

	^ super morphPosition: pos asIntegerPoint! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:44'!
morphPosition: aPoint
	"Prevent moving a world (e.g. via HandMorph>>specialGesture:)"

	"for now, let's allow it and see what happens"

	self isWorldMorph ifFalse: [ ^super morphPosition: aPoint ].
	super morphPosition: aPoint.
	self viewBox ifNotNil: [ self viewBox: (aPoint extent: self viewBox extent) ]! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:44'!
morphPosition: newPos
	super morphPosition: newPos.
	isCollapsed
		ifTrue: [ collapsedFrame _ bounds ]
		ifFalse: [ fullFrame _ bounds ]! !


!UserInputEvent methodsFor: 'accessing' stamp: 'jmv 12/11/2011 23:10'!
eventPosition
	^position! !


!MouseEvent methodsFor: 'accessing' stamp: 'jmv 12/11/2011 23:12'!
eventPosition
	"Answer the location of the cursor's hotspot when this event occured."
	^ position! !


!AbstractSound methodsFor: 'file i/o' stamp: 'jmv 12/12/2011 10:43'!
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


!AutoCompleter methodsFor: 'menu morph' stamp: 'jmv 12/12/2011 10:43'!
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
				position: theEditor startBlock bottomLeft + textMorph morphPosition ]! !


!CodePackage methodsFor: 'testing' stamp: 'jmv 4/10/2012 14:22'!
changeRecordForOverriddenMethod: aMethodReference
	| sourceFilesCopy method position |
	method := aMethodReference actualClass compiledMethodAt: aMethodReference methodSymbol.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x isNil ifTrue: [ nil ]
				ifFalse: [x readOnlyCopy]].
	[ | file prevPos prevFileIndex chunk stamp methodCategory tokens |
	method fileIndex = 0 ifTrue: [^ nil].
	file := sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil] whileTrue: [
		file position: (0 max: position-150).  "Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [ chunk _ file nextChunk ].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevPos := nil.
		stamp := ''.
		(chunk findString: 'methodsFor:' startingAt: 1) > 0
			ifTrue: [tokens := Scanner new scanTokens: chunk]
			ifFalse: [tokens := #()  "ie cant be back ref"].
		((tokens size between: 7 and: 8)
			and: [(tokens at: tokens size-5) = #methodsFor:])
			ifTrue:
				[(tokens at: tokens size-3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp := tokens at: tokens size-2.
						prevPos := tokens last.
						prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: prevPos.
						prevPos := sourceFilesCopy filePositionFromSourcePointer: prevPos]
				ifFalse: ["Old format gives no stamp; prior pointer in two parts"
						prevPos := tokens at: tokens size-2.
						prevFileIndex := tokens last].
				(prevPos = 0 or: [prevFileIndex = 0]) ifTrue: [prevPos := nil]].
		((tokens size between: 5 and: 6)
			and: [(tokens at: tokens size-3) = #methodsFor:])
			ifTrue:
				[(tokens at: tokens size-1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp := tokens at: tokens size]].
		methodCategory := (tokens after: #methodsFor:) ifNil: ['as yet unclassifed'].
		(self includesMethodCategory: methodCategory ofClass: aMethodReference actualClass) ifTrue:
			[methodCategory = (Smalltalk at: #Categorizer ifAbsent: [Smalltalk at: #ClassOrganizer]) default ifTrue: [methodCategory := methodCategory, ' '].
			^ ChangeRecord new file: file position: position type: #method
						class: aMethodReference classSymbol category: methodCategory meta: aMethodReference classIsMeta stamp: stamp].
		position := prevPos.
		prevPos notNil ifTrue: [
			file _ sourceFilesCopy at: prevFileIndex]].
		^ nil]
			ensure: [sourceFilesCopy do: [:x | x notNil ifTrue: [x close]]]
	! !


!DropEvent methodsFor: 'printing' stamp: 'jmv 12/11/2011 23:32'!
printOn: aStream

	aStream nextPut: $[.
	aStream nextPutAll: self eventPosition printString; space.
	aStream nextPutAll: self type.
	aStream nextPut: $].! !


!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/12/2011 10:49'!
aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand."
	self formerOwner: owner.
	self formerPosition: self morphPosition.
	^self "Grab me"! !

!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/12/2011 10:52'!
slideBackToFormerSituation: evt 
	| slideForm formerOwner formerPosition aWorld startPoint endPoint |
	formerOwner _ self formerOwner.
	formerPosition _ self formerPosition.
	aWorld _ evt hand world.
	slideForm _ self imageForm offset: 0 @ 0.
	startPoint _ evt hand fullBounds origin.
	endPoint _ formerPosition.
	owner removeMorph: self.
	aWorld displayWorld.
	slideForm 
		slideFrom: startPoint
		to: endPoint
		nSteps: 12
		delay: 15.
	formerOwner addMorph: self.
	self morphPosition: formerPosition.
	self justDroppedInto: formerOwner event: evt! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 12/11/2011 23:28'!
handleMouseOver: anEvent
	"System level event handling."
	anEvent hand mouseFocus == self ifTrue:[
		"Got this directly through #handleFocusEvent: so check explicitly"
		(self containsPoint: anEvent eventPosition event: anEvent) ifFalse:[^self]].
	anEvent hand noticeMouseOver: self event: anEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 12/11/2011 23:29'!
rejectDropEvent: anEvent
	"This hook allows the receiver to repel a drop operation currently executed. The method is called prior to checking children so the receiver must validate that the event was really designated for it.
	Note that the ordering of the tests below is designed to avoid a (possibly expensive) #fullContainsPoint: test. If the receiver doesn't want to repel the morph anyways we don't need to check after all."
	(self repelsMorph: anEvent contents event: anEvent) ifFalse:[^self]. "not repelled"
	(self fullContainsPoint: anEvent eventPosition) ifFalse:[^self]. "not for me"
	"Throw it away"
	anEvent wasHandled: true.
	anEvent contents rejectDropMorphEvent: anEvent.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:49'!
align: aPoint1 with: aPoint2
	"Translate by aPoint2 - aPoint1."

	^ self morphPosition: self morphPosition + (aPoint2 - aPoint1)! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:52'!
bounds: newBounds
	| oldExtent newExtent |
	oldExtent _ bounds extent.
	newExtent _ newBounds extent.
	"Moving stuff around is most likely the most common operation.
	Optimize it"
	oldExtent = newExtent ifTrue: [
		^self morphPosition: newBounds topLeft ].
	(oldExtent dotProduct: oldExtent) <= (newExtent dotProduct: newExtent) ifTrue:[
		"We're growing. First move then resize."
		self morphPosition: newBounds topLeft; extent: newExtent.
	] ifFalse: [
		"We're shrinking. First resize then move."
		self extent: newExtent; morphPosition: newBounds topLeft.
	].! !

!Morph methodsFor: 'geometry eToy' stamp: 'jmv 12/12/2011 10:52'!
referencePosition: aPoint
	"a rather ugly way to say #center: . Just for consistency with #referencePosition"
	self morphPosition: aPoint - (bounds extent // 2)! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 12/12/2011 10:49'!
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

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 12/11/2011 23:29'!
transferHalo: event from: formerHaloOwner
	"Progressively transfer the halo to the next likely recipient"
	| w |

	"Never transfer halo to top-most world"
	(self isWorldMorph and:[owner isNil]) ifFalse: [
		(self wantsHaloFromClick and:[formerHaloOwner ~~ self]) 
			ifTrue:[^self addHalo: event from: formerHaloOwner]].

	event shiftPressed ifTrue: [
		"Pass it outwards"
		owner ifNotNil:[^owner transferHalo: event from: formerHaloOwner].
		"We're at the top level; throw the event back in to find recipient"
		formerHaloOwner removeHalo.
		^self processEvent: event copy resetHandlerFields.
	].
	self submorphsDo: [ :m |
		(m fullContainsPoint: event eventPosition) 
			ifTrue:[^m transferHalo: event from: formerHaloOwner].
	].
	"We're at the bottom most level; throw the event back up to the root to find recipient"
	formerHaloOwner removeHalo.
	(w _ self world) ifNil: [ ^self ].
	^w processEvent: event copy resetHandlerFields.
! !

!Morph methodsFor: 'initialization' stamp: 'jmv 12/12/2011 10:49'!
openInWorld: aWorld
	"Add this morph to the requested World."
	(aWorld viewBox origin ~= (0@0) and: [self morphPosition = (0@0)]) ifTrue: [
		self morphPosition: aWorld viewBox origin].
	aWorld addMorph: self.
	aWorld startSteppingSubmorphsOf: self! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 12/12/2011 10:49'!
changeColorTarget: anObject selector: aSymbol originalColor: aColor hand: aHand
	"Put up a color picker for changing some kind of color.  May be modal or modeless, depending on #modalColorPickers setting"
	self flag: #arNote. "Simplify this due to anObject == self for almost all cases"
	^ ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: aHand;
		target: anObject;
		selector: aSymbol;
		originalColor: aColor;
		putUpFor: anObject near: ((anObject is: #Morph)
					ifTrue: [ Rectangle center: self morphPosition extent: 20 ]
					ifFalse: [ anObject == self world
								ifTrue: [ anObject viewBox bottomLeft + (20@-20) extent: 200 ]
								ifFalse: [ anObject fullBounds ]]);
		yourself! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 12/12/2011 10:51'!
addMorphFront: aMorph fromWorldPosition: aPoint

	self addMorphFront: aMorph.
	aMorph morphPosition: aPoint! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 12/12/2011 10:49'!
addMorphFrontFromWorldPosition: aMorph
	^self addMorphFront: aMorph fromWorldPosition: aMorph morphPosition.! !

!Morph methodsFor: 'testing' stamp: 'jmv 12/11/2011 23:36'!
shouldDropOnMouseUp
	| former |
	"
	former _ self formerPosition ifNil:[^false].
	^(former dist: self zzposition) > 10
	"^true! !


!AutoCompleterMorph methodsFor: 'initialization' stamp: 'jmv 12/12/2011 10:51'!
setCompleter: anAutoCompleter position: aPoint 
	completer _ anAutoCompleter.
	self morphPosition: aPoint.
	self resetMenu.
	self openInWorld! !

!AutoCompleterMorph methodsFor: 'event handling' stamp: 'jmv 12/11/2011 23:13'!
mouseUp: evt
	(self containsPoint: evt eventPosition)
		ifTrue: [
			self selected: 
				((evt eventPosition y - bounds top // self class itemHeight) + 
					self firstVisible).
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !


!FillInTheBlankMorph methodsFor: 'event handling' stamp: 'jmv 12/11/2011 23:14'!
mouseDown: evt
	(self containsPoint: evt eventPosition) ifFalse:[^ Beeper beep]. "sent in response to outside modal click"
	evt hand grabMorph: self. "allow repositioning"! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 12/12/2011 10:45'!
request: queryString
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph request: 'What is your favorite color?'"

	^ self
		request: queryString
		initialAnswer: ''
		centerAt: World activeHand morphPosition! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 12/12/2011 10:45'!
request: queryString initialAnswer: defaultAnswer 
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph
		request: 'What is your favorite color?'
		initialAnswer: 'red, no blue. Ahhh!!'"

	^ self
		request: queryString
		initialAnswer: defaultAnswer
		centerAt: World activeHand morphPosition! !


!HaloMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/12/2011 10:45'!
startDrag: evt with: dragHandle
	"Drag my target without removing it from its owner."

	self obtainHaloForEvent: evt andRemoveAllHandlesBut: dragHandle.
	positionOffset _ dragHandle referencePosition - target morphPosition! !

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 12/12/2011 10:51'!
mouseMove: evt
	"Drag our target around"
	| thePoint |
	thePoint _ evt eventPosition - positionOffset.
	target morphPosition: thePoint! !

!HaloMorph methodsFor: 'events' stamp: 'jmv 12/12/2011 10:45'!
popUpFor: aMorph event: evt
	"This message is sent by morphs that explicitly request the halo on a button click. Note: anEvent is in aMorphs coordinate frame."

	| hand anEvent |
	self flag: #workAround.	"We should really have some event/hand here..."
	anEvent _ evt
				ifNil: [
					hand _ aMorph world activeHand.
					hand ifNil: [ hand _ aMorph world firstHand ]. 
					hand lastEvent ]
				ifNotNil: [
					hand _ evt hand.
					evt ].
	self target: aMorph.
	hand halo: self.
	hand world addMorphFront: self.
	positionOffset _ anEvent eventPosition - aMorph morphPosition.
	self startStepping! !

!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 12/11/2011 23:27'!
containsPoint: aPoint event: anEvent
	"mouseButton3 events are handled by the halo"
	(anEvent isMouse and: [ anEvent isMouseDown and: [ anEvent mouseButton3Pressed ]])
		ifFalse:  [^super containsPoint: aPoint event: anEvent ].
	^self fullBounds containsPoint: anEvent eventPosition! !

!HaloMorph methodsFor: 'meta-actions' stamp: 'jmv 12/12/2011 10:45'!
mouseButton3Down: event
	"Transfer the halo to the next likely recipient"
	target ifNil:[^self delete].
	event hand obtainHalo: self.
	positionOffset _ event eventPosition - target morphPosition.
	"wait for drags or transfer"
	event hand 
		waitForClicksOrDrag: self 
		event: event
		clkSel: #transferHalo:
		dblClkSel: nil! !

!HaloMorph methodsFor: 'stepping' stamp: 'jmv 12/12/2011 10:51'!
step
	| newBounds |
	target
		ifNil: [^ self].
	newBounds _ target isWorldMorph
				ifTrue: [target bounds]
				ifFalse: [target worldBoundsForHalo truncated].
	newBounds = bounds
		ifTrue: [^ self].
	newBounds extent = bounds extent
		ifTrue: [^ self morphPosition: newBounds origin].
	growingOrRotating
		ifFalse: [submorphs size > 1
				ifTrue: [self addHandles]].
	"adjust halo bounds if appropriate"
	self bounds: newBounds! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/30/2011 23:40'!
addHandle: handleSpec on: eventName send: selector to: recipient 
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint iconName colorToUse aMorph |
	aPoint := self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	handle := HaloHandleMorph new.
	self addMorph: handle.
	handle 
		bounds: (Rectangle center: aPoint extent: HandleSize asPoint);
		color: (colorToUse := Color colorFrom: handleSpec color).
	(iconName := handleSpec iconSymbol) ifNotNil: [
			| form |
			form := Icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				aMorph _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				aMorph morphPosition: aPoint - (form extent // 2).
				handle addMorphFront: aMorph.
				]].
	handle 
		on: #mouseUp
		send: #endInteraction
		to: self.
	handle 
		on: eventName
		send: selector
		to: recipient.
	handle 
		setBalloonText: (target balloonHelpTextForHandle: handle).
	^handle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 10:51'!
addNameBeneath: outerRectangle string: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition w |
	w := self world ifNil: [target world].
	nameMorph := StringMorph contents: aString.
	nameMorph color: Color magenta.
	namePosition := outerRectangle bottomCenter 
				- ((nameMorph width // 2) @ (self handleSize negated // 2 - 1)).
	nameMorph 
		morphPosition: (namePosition min: w viewBox bottomRight - nameMorph extent y + 5).
	self addMorph: (RectangleMorph new
		bounds: (nameMorph bounds outsetBy: 2);
		borderWidth: 0;
		color: (Color lightBlue alpha: 0.9)).
	self addMorph: nameMorph.
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 10:51'!
doDrag: evt with: dragHandle
	| thePoint |
	evt hand obtainHalo: self.
	thePoint _ evt eventPosition - positionOffset.
	target morphPosition: thePoint! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 10:51'!
doGrow: evt with: growHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
self revisar.
	evt hand obtainHalo: self.
"Como podria andar el grow de un morph embebido en otro? andara ahora?"
newExtent _ evt eventPosition - positionOffset - target bounds topLeft.
	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target extent: newExtent.
	growHandle morphPosition: evt eventPosition - (growHandle extent // 2).
	self someSubmorphPositionOrExtentChanged
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/11/2011 23:15'!
doRecolor: evt with: aHandle
	"The mouse went down in the 'recolor' halo handle.  Allow the user to change the color of the innerTarget"

	evt hand obtainHalo: self.
	(aHandle containsPoint: evt eventPosition)
		ifFalse: [  "only do it if mouse still in handle on mouse up"
			self delete.
			target addHalo: evt]
		ifTrue: [
			innerTarget changeColor]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 10:51'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable.  Keep the relevant command object up to date."

	| degrees |
self revisar.
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

	rotHandle morphPosition: evt eventPosition - (rotHandle extent // 2).
	self someSubmorphPositionOrExtentChanged! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/11/2011 23:15'!
maybeCollapse: evt with: collapseHandle 
	"Ask hand to collapse my target if mouse comes up in it."

	evt hand obtainHalo: self.
	self delete.
	(collapseHandle containsPoint: evt eventPosition) 
		ifFalse: [
			target addHalo: evt ]
		ifTrue: [
			target collapse ]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/11/2011 23:16'!
maybeDismiss: evt with: dismissHandle
	"Ask hand to dismiss my target if mouse comes up in it."

	evt hand obtainHalo: self.
	(dismissHandle containsPoint: evt eventPosition)
		ifFalse: [
			self delete.
			target addHalo: evt]
		ifTrue: [
			target resistsRemoval ifTrue: [
				(PopUpMenu
					confirm: 'Really throw this away'
					trueChoice: 'Yes'
					falseChoice: 'Um, no, let me reconsider') ifFalse: [^ self]].

			self delete.
			target dismissViaHalo]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/11/2011 23:16'!
setDismissColor: evt with: dismissHandle
	"Called on mouseStillDown in the dismiss handle; set the color appropriately."

	| colorToUse |
	evt hand obtainHalo: self.
	colorToUse _  (dismissHandle containsPoint: evt eventPosition)
		ifFalse: [ Color red muchLighter ]
		ifTrue: [ Color lightGray ].
	dismissHandle color: colorToUse! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/11/2011 23:16'!
startGrow: evt with: growHandle
	"Initialize resizing of my target.  Launch a command representing it, to support Undo"

	| botRt |
	self obtainHaloForEvent: evt andRemoveAllHandlesBut: growHandle.
	botRt _ target bounds bottomRight.
	positionOffset _ (self world viewBox containsPoint: botRt)
		ifTrue: [evt eventPosition - botRt]
		ifFalse: [0@0]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/11/2011 23:16'!
startRot: evt with: rotHandle
	"Initialize rotation of my target if it is rotatable.  Launch a command object to represent the action"

	self obtainHaloForEvent: evt andRemoveAllHandlesBut: rotHandle.
	growingOrRotating _ true.

	self removeAllHandlesBut: rotHandle.  "remove all other handles"
	angleOffset _ evt eventPosition - target referencePosition.
	angleOffset _ Point
			r: angleOffset r
			degrees: angleOffset degrees - target rotationDegrees

! !


!HandMorph methodsFor: 'cursor' stamp: 'jmv 12/12/2011 10:46'!
cursorBounds

	^self morphPosition extent: CursorWithMask normal extent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 12/12/2011 10:46'!
handleEvent: anEvent
	| evt |
	owner ifNil: [ ^ self ].
	evt _ anEvent.
	evt isMouseOver ifTrue: [ ^ self sendMouseEvent: evt ].

	"Notify listeners"
	self sendListenEvent: evt to: self eventListeners.
	
	evt isWindowEvent ifTrue: [
		self sendEvent: evt.
		^ self mouseOverHandler processMouseOver: lastMouseEvent ].

	evt isKeyboard ifTrue: [
		self sendListenEvent: evt to: self keyboardListeners.
		self sendKeyboardEvent: evt.
		^ self mouseOverHandler processMouseOver: lastMouseEvent].

	evt isDropEvent ifTrue: [
		self sendEvent: evt.
		^ self mouseOverHandler processMouseOver: lastMouseEvent].

	evt isMouse ifTrue: [
		self sendListenEvent: evt to: self mouseListeners.
		lastMouseEvent _ evt.	
		lastMouseEventTime _ Time millisecondClockValue].

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: evt from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			^self mouseOverHandler processMouseOver: lastMouseEvent  ]].

	evt isMove ifTrue: [
		self morphPosition: evt eventPosition.
		self sendMouseEvent: evt.
	] ifFalse: [
		"Issue a synthetic move event if we're not at the position of the event"
		evt eventPosition = self morphPosition ifFalse: [ self moveToEvent: evt ].
		"Drop submorphs on button events"
		self hasSubmorphs
			ifTrue: [ self dropMorphs: evt ]
			ifFalse: [ self sendMouseEvent: evt ].
	].
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/12/2011 10:46'!
newMouseFocus: aMorph event: event 
	aMorph ifNotNil: [ 
		targetOffset _ event eventPosition - aMorph morphPosition].
	^self newMouseFocus: aMorph! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/12/2011 10:46'!
attachMorph: m
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."
	| delta |
	self releaseMouseFocus. "Break focus"
	delta _ m bounds extent // 2.
	m morphPosition: (self morphPosition - delta).
	m formerPosition: m morphPosition.
	targetOffset _ m morphPosition - self morphPosition.
	self addMorphBack: m.! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/12/2011 10:46'!
dropMorph: aMorph event: anEvent
	"Drop the given morph which was carried by the hand"
	| event |
	(anEvent isMouseUp and:[aMorph shouldDropOnMouseUp not]) ifTrue:[^self].

	"Note: For robustness in drag and drop handling we remove the morph BEFORE we drop him, but we keep his owner set to the hand. This prevents system lockups when there is a problem in drop handling (for example if there's an error in #wantsToBeDroppedInto:). THIS TECHNIQUE IS NOT RECOMMENDED FOR CASUAL USE."
	self privateRemove: aMorph.
	aMorph privateOwner: self.

	event _ DropEvent new setPosition: self morphPosition contents: aMorph hand: self.
	self sendEvent: event.
	event wasHandled ifFalse:[aMorph rejectDropMorphEvent: event].
	aMorph owner == self ifTrue:[aMorph delete].
	self mouseOverHandler processMouseOver: anEvent.! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/12/2011 10:46'!
grabMorph: aMorph from: formerOwner
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	self releaseMouseFocus. "Break focus"
	"And compute distance from hand's position"
	targetOffset _ aMorph morphPosition - self morphPosition.
	self addMorphBack: aMorph.
	aMorph justGrabbedFrom: formerOwner.! !

!HandMorph methodsFor: 'paste buffer' stamp: 'jmv 12/12/2011 10:47'!
pasteMorph

	| pastee |
	pastee _ Clipboard retrieveMorph.
	pastee ifNil: [^ self inform: 'Nothing to paste.'].
	self attachMorph: pastee.
	pastee align: pastee referencePosition with: self morphPosition
! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 12/12/2011 10:46'!
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

!HandMorph methodsFor: 'private events' stamp: 'jmv 12/12/2011 10:46'!
moveToEvent: anEvent
	"Issue a mouse move event to make the receiver appear at the given position"
	self handleEvent: (MouseMoveEvent new
		setType: #mouseMove 
		startPoint: self morphPosition 
		endPoint: anEvent eventPosition 
		trail: (Array with: self morphPosition with: anEvent eventPosition)
		buttons: anEvent buttons
		hand: self
		stamp: anEvent timeStamp)! !


!HandleMorph methodsFor: 'event handling' stamp: 'jmv 4/10/2012 14:29'!
keyStroke: evt
	"Check for cursor keys"
	| keyValue |
	(owner is: #HandMorph) ifFalse: [ ^self ].
	keyValue _ evt keyValue.
	keyValue = 28 ifTrue: [ ^self morphPosition: self morphPosition - (1@0) ].
	keyValue = 29 ifTrue: [ ^self morphPosition: self morphPosition + (1@0) ].
	keyValue = 30 ifTrue: [ ^self morphPosition: self morphPosition - (0@1) ].
	keyValue = 31 ifTrue: [ ^self morphPosition: self morphPosition + (0@1) ].
	"Special case for return"
	evt isReturnKey ifTrue:[
		"Drop the receiver and be done"
	self flag: #arNote. "Probably unnecessary"
		owner releaseKeyboardFocus: self.
		self delete ]! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 12/12/2011 10:47'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| xcess |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [ ^self ].
	self morphPosition: aHand morphPosition + (-6@20).
	xcess _ bounds right - aHand world bounds right.
	xcess > 0 ifTrue: [
		self morphPosition: self morphPosition - (xcess@0) ].
	aHand world addMorphFront: self.
	aHand balloonHelp: self! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 12/11/2011 23:17'!
enterClickableRegion: evt
	| index isLink |
	evt hand hasSubmorphs ifTrue: [ ^self ].
	paragraph ifNotNil:[
		index _ (paragraph characterBlockAtPoint: evt eventPosition) stringIndex.
		isLink _ (model actualContents attributesAt: index) 
					anySatisfy: [ :attr | attr mayActOnClick ].
		isLink ifTrue: [ Cursor webLink show ] ifFalse: [ Cursor normal show ]]! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 12/11/2011 23:17'!
mouseMove: evt
	evt mouseButton1Pressed ifFalse: [^ self enterClickableRegion: evt].
	self handleInteraction: [ editor mouseMove: (evt translatedBy: bounds topLeft negated)].
	(evt eventPosition y between: owner bounds top and: owner bounds bottom) ifFalse: [
		WorldState addDeferredUIMessage: [
			owner scrollSelectionIntoView: evt ]
	]! !


!LayoutAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 12/12/2011 10:47'!
handPoint
	^ hand morphPosition adhereTo: owner bounds! !

!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 12/12/2011 10:47'!
step
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		indicator ifNotNil: [
			indicator delete.
			indicator _ nil ].
		Cursor currentCursor == self cursor ifTrue: [
			Cursor normal show ].
		^self stopStepping ].

	"hasn't got the #mouseLeave: message (yet)"
	p _ self handPoint.
	hand lastEvent mouseButton1Pressed
		ifTrue: [
			indicator
				ifNil: [ self adjustOwnerAt: p ]
				ifNotNil: [ self adjustIndicatorAt: p ]]
		ifFalse: [
			indicator ifNotNil: [
				indicator delete.
				indicator _ nil.
				self adjustOwnerAt: p ].
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self containsPoint: hand morphPosition) ifFalse: [
				hand _ nil.
				Cursor normal show.
				self stopStepping ]]! !

!LayoutAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 12/12/2011 10:47'!
adjustIndicatorAt: aPoint
	owner direction = #vertical
		ifTrue: [
			indicator morphPosition: indicator morphPosition x @ (aPoint y-(indicator height//2)) ]
		ifFalse: [
			indicator morphPosition: (aPoint x-(indicator width//2)) @ indicator morphPosition y ]! !


!MagnifierMorph methodsFor: 'menu' stamp: 'jmv 12/12/2011 10:48'!
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

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 12/12/2011 10:48'!
sourcePoint
	"If we are being dragged use our center, otherwise use pointer position"
	^ (trackPointer not or: [owner notNil and: [owner is: #HandMorph]])
		ifTrue: [ bounds center ]
		ifFalse: [ self activeHand morphPosition ]! !

!MagnifierMorph methodsFor: 'round view' stamp: 'jmv 12/11/2011 23:35'!
toggleRoundness
	| |
"	w _ self world.
	self isRound
		ifTrue: [owner delete.
				w addMorph: self]
		ifFalse: [sm _ ScreeningMorph new position: self zzposition.
				sm addMorph: self.
				sm addMorph: (EllipseMorph newBounds: self bounds).
				w addMorph: sm]
			"! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 12/12/2011 10:51'!
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
	marker morphPosition: bounds left @ (bounds top + 2).
	self addMorphFront: marker! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 12/11/2011 23:17'!
activateOwnerMenu: evt
	"Activate our owner menu; e.g., pass control to it"
	owner ifNil:[^false]. "not applicable"
	(owner fullContainsPoint: evt eventPosition) ifFalse:[^false].
	owner activate: evt.
	^true! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 12/11/2011 23:17'!
activateSubmenu: evt
	"Activate our submenu; e.g., pass control to it"
	subMenu ifNil:[^false]. "not applicable"
	(subMenu fullContainsPoint: evt eventPosition) ifFalse:[^false].
	subMenu activate: evt.
	self removeAlarm: #deselectTimeOut:.
	^true! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/10/2012 14:30'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s p w |
	
	titleMorph _ RectangleMorph new.
	self setTitleParametersFor: titleMorph.
	p _ titleMorph morphPosition + (8@2).
	aString asString linesDo: [ :line | 
		s _ StringMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		s morphPosition: p.
		titleMorph addMorphBack: s.
		p _ p + (0@(s height+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each width ].
	titleMorph height: p y; width: w + 8.
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 12/12/2011 10:49'!
popUpAdjacentTo: rightOrLeftPoint forHand: hand from: sourceItem
	"Present this menu at the given point under control of the given hand."

	| delta tryToPlace selectedOffset |

	hand world startSteppingSubmorphsOf: self.
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil:[self items first]) morphPosition - self morphPosition.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self fullBounds amountToTranslateWithin: sourceItem world bounds.
		(delta x = 0 or: [ mustFit ]) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			sourceItem owner owner addMorphFront: self.
			^ self]].
	tryToPlace 
		value: rightOrLeftPoint first value: false;
		value: rightOrLeftPoint last  - (self width @ 0) value: false;
		value: rightOrLeftPoint first value: true

	! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 12/12/2011 10:49'!
popUpForHand: hand in: aWorld
	| p |
	"Present this menu under control of the given hand."

	p _ hand morphPosition truncated.
	^self popUpAt: p forHand: hand in: aWorld
! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 12/12/2011 10:49'!
popUpInWorld: aWorld
	"Present this menu under control of the given hand."
	^self
		popUpAt: aWorld activeHand morphPosition
		forHand: aWorld activeHand
		in: aWorld
! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 12/11/2011 23:17'!
mouseDown: evt
	"Handle a mouse down event."
	(stayUp or:[self fullContainsPoint: evt eventPosition]) 
		ifFalse:[^self deleteIfPopUp: evt]. "click outside"
	self isSticky ifTrue: [^self].
	"Grab the menu and drag it to some other place"
	evt hand grabMorph: self.! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 12/11/2011 23:17'!
mouseUp: evt
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self fullContainsPoint: evt eventPosition) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		evt hand releaseMouseFocus: self.
		^self deleteIfPopUp: evt].
	stayUp ifFalse:[
		"Still in pop-up transition; keep focus"
		evt hand newMouseFocus: self].! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 12/12/2011 10:48'!
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
	feedbackMorph ifNil: [
		feedbackMorph _ StringMorph new color: Color veryDarkGray.
		feedbackMorph morphPosition: self morphPosition - (0@20).
		self
			 addLine;
			 addMorphBack: feedbackMorph lock.
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

!MenuMorph methodsFor: 'menu' stamp: 'jmv 12/12/2011 10:51'!
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

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 12/12/2011 10:48'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu"

	^ self invokeModalAt: ActiveHand morphPosition in: ActiveWorld allowKeyboard: allowKeyboardControl! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 10:51'!
adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p tl |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w _ submorphs inject: 0 into: [ :prev :each |
		prev max: (
			(each respondsTo: #minItemWidth)
				ifTrue: [each minItemWidth]
				ifFalse: [each width])].

	w _ w + 4.
	tl _ bounds topLeft.
	p _ tl + 5.
	submorphs do: [ :m |
		m width: w.
		m morphPosition: p.
		p _ m bounds bottomLeft +(0@1) ].
	
	self extent: submorphs last bounds bottomRight - tl + 5! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 10:49'!
positionAt: aPoint relativeTo: aMenuItem inWorld: aWorld
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| i yOffset sub delta |
	self adjustSubmorphsLayout.
	i _ 0.
	yOffset _ 0.
	[(sub _ self submorphs at: (i _ i + 1)) == aMenuItem]
		whileFalse: [ yOffset _ yOffset + sub height ].

	self morphPosition: aPoint - (2 @ (yOffset + 8)).

	"If it doesn't fit, show it to the left, not to the right of the hand."
	bounds right > aWorld world bounds right
		ifTrue: [
			self moveRight: aPoint x + 1].

	"Make sure that the menu fits in the world."
	delta _ bounds amountToTranslateWithin:
		(aWorld world bounds withHeight: ((aWorld world bounds height - 18) max: (ActiveHand morphPosition y) + 1)).
	delta = (0 @ 0) ifFalse: [ self morphPosition: self morphPosition + delta ]! !

!MenuMorph methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:51'!
moveRight: aNumber
	self morphPosition: ((aNumber - bounds width) @ bounds top)! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 12/12/2011 10:48'!
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
		titleMorph width: titleString width + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w displayWorld		 "show myself"
	]. 
	self delete.
	w displayWorld! !


!MorphicEventDispatcher methodsFor: 'dispatching' stamp: 'jmv 12/11/2011 23:29'!
dispatchDefault: anEvent with: aMorph
	"Dispatch the given event. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| inside |
	"See if we're fully outside aMorphs bounds"
	(aMorph fullBounds containsPoint: anEvent eventPosition) ifFalse: [ ^#rejected ]. "outside"

	"Traverse children"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			(eachChild processEvent: anEvent using: self) == #rejected ifFalse: [
				"Not rejected. The event was in some submorph of the receiver"
				inside _ true
			]]].

	"Check for being inside the receiver"
	inside ifFalse: [ inside _ aMorph containsPoint: anEvent eventPosition event: anEvent ].
	inside ifTrue: [ ^aMorph handleEvent: anEvent ].
	^ #rejected! !

!MorphicEventDispatcher methodsFor: 'dispatching' stamp: 'jmv 12/11/2011 23:31'!
dispatchDropEvent: anEvent with: aMorph
	"Find the appropriate receiver for the event and let it handle it. The dispatch is similar to the default dispatch with one difference: Morphs are given the chance to reject an entire drop operation. If the operation is rejected, no drop will be executed."
	| inside |

	"Try to get out quickly"
	(aMorph fullBounds containsPoint: anEvent eventPosition)
		ifFalse: [ ^#rejected ].

	"Give aMorph a chance to repel the dropping morph"
	aMorph rejectDropEvent: anEvent.
	anEvent wasHandled ifTrue:[^self].

	"Go looking if any of our submorphs wants it"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			(eachChild processEvent: anEvent using: self) == #rejected ifFalse: [
				inside _ true
			]]].

	inside ifFalse: [ inside _ aMorph containsPoint: anEvent eventPosition event: anEvent ].
	inside ifTrue: [ ^aMorph handleEvent: anEvent ].
	^#rejected! !

!MorphicEventDispatcher methodsFor: 'dispatching' stamp: 'jmv 12/11/2011 23:31'!
dispatchMouseDown: anEvent with: aMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
"
	| globalPt handler inside lastHandler |
	"Try to get out quickly"
	globalPt _ anEvent eventPosition.
	(aMorph fullBounds containsPoint: globalPt) ifFalse: [ ^#rejected ].

	"Install the prospective handler for the receiver"
	lastHandler _ anEvent handler. "in case the mouse wasn't even in the receiver"
	handler _ aMorph handlerForMouseDown: anEvent.
	handler ifNotNil: [ anEvent handler: handler ].

	"Now give our submorphs a chance to handle the event"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			(eachChild processEvent: anEvent using: self) == #rejected ifFalse: [
				"Some child did contain the point so we're part of the top-most chain."
				inside _ true.
			]]].

	(inside or: [ aMorph containsPoint: anEvent eventPosition event: anEvent ]) ifTrue:[
		"Receiver is in the top-most unlocked, visible chain."
		handler ifNotNil: [ handler handleEvent: anEvent ].
		"Note: Re-installing the handler is not really necessary but good style."
		anEvent handler: lastHandler.
		^self ].

	"Mouse was not on receiver nor any of its children"
	anEvent handler: lastHandler.
	^#rejected! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 12/12/2011 10:52'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			anchoredFormOrMorph morphPosition:
				((destX - anchoredFormOrMorph width)@
				(lineY+ line baseline - anchoredFormOrMorph height)) -
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


!MouseClickState methodsFor: 'actions' stamp: 'jmv 12/30/2011 23:41'!
handleEvent: evt from: aHand
	"Process the given mouse event to detect a click, double-click, or drag.
	Return true if the event should be processed by the sender, false if it shouldn't.
	NOTE: This method heavily relies on getting *all* mouse button events."

	| timedOut distance |
	timedOut _ (evt timeStamp - lastClickDown timeStamp) > DoubleClickTimeout.
	distance _ (evt eventPosition - lastClickDown eventPosition) r.
	"Real action dispatch might be done after the triggering event, for example, because of waiting for timeout.
	So, count the button downs and ups(clicks), to be processed, maybe later, maybe in a mouseMove..."
	evt isMouseDown ifTrue: [
		lastClickDown _ evt.
		buttonDownCount _ buttonDownCount + 1 ].
	evt isMouseUp ifTrue: [
		buttonUpCount _ buttonUpCount + 1 ].

	"Simulate button 2 if timeout during first click (i.e. tap & hold). Useful for opening menus on pen computers."
	(buttonDownCount = 1 and: [ buttonUpCount = 0]) ifTrue: [
		(timedOut and: [ sendMouseButton2Activity and: [ distance = 0]]) ifTrue: [
			aHand dontWaitForMoreClicks.
			clickClient mouseButton2Activity.
			^ false ].
		"If we have already moved, then it won't be a double or triple click... why wait?"
		distance > 0 ifTrue: [
			aHand dontWaitForMoreClicks.
			self click.
			^ false ]].

	"If we're over triple click, or timed out, or mouse moved, don't allow more clicks."
	(buttonDownCount = 4 or: [ timedOut or: [ distance > 0 ]]) ifTrue: [
		aHand dontWaitForMoreClicks.
		^ false ].

	"Simple click."
	(buttonDownCount = 1 and: [ buttonUpCount = 1 ]) ifTrue: [
		self click ].

	"Click & hold"
	(buttonDownCount = 2 and: [ buttonUpCount = 1]) ifTrue: [
		self clickAndAHalf ].

	"Double click."
	(buttonDownCount = 2 and: [ buttonUpCount = 2]) ifTrue: [
		self doubleClick ].

	"Double click & hold."
	(buttonDownCount = 3 and: [ buttonUpCount = 2]) ifTrue: [
		self doubleClickAndHalf ].

	"Triple click"
	(buttonDownCount = 3 and: [ buttonUpCount = 3]) ifTrue: [
		self tripleClick ].

	"This means: if a mouseDown, then don't further process this event (so we can turn it into a double or triple click on next buttonUp)"
	^ evt isMouseDown! !


!OneLineEditorMorph methodsFor: 'event handling' stamp: 'jmv 12/30/2011 23:42'!
mouseDown: event
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	event hand newKeyboardFocus: self.

	self handleInteraction: [
		self editor mouseDown: event index: (self characterIndexAtPoint: event eventPosition) ].

	event hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: event
		clkSel: nil
		clkNHalf: #clickAndHalf:
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!OneLineEditorMorph methodsFor: 'event handling' stamp: 'jmv 12/30/2011 23:42'!
mouseMove: evt
	evt mouseButton1Pressed ifFalse: [^ self].
	self handleInteraction: [
		self editor mouseMove: evt index: (self characterIndexAtPoint: evt eventPosition) ]! !


!Parser methodsFor: 'public access' stamp: 'jmv 12/11/2011 22:42'!
parse: sourceStream class: class category: aCategory noPattern: noPattern context: ctxt notifying: req ifFail: aBlock
	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."
	| methNode repeatNeeded myStream s p |
	category _ aCategory.
	myStream _ sourceStream.
	[
		repeatNeeded _ false.
		p _ myStream position.
		s _ myStream upToEnd.
		myStream position: p.
		self encoder init: class context: ctxt notifying: self.
		self init: myStream notifying: req failBlock: [
			^ aBlock value ].
		doitFlag _ noPattern.
		failBlock _ aBlock.
		[ methNode _ self method: noPattern context: ctxt ]
			on: ReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded _ true.
				myStream _ sourceStreamGetter notNil 	"Cuis specific. Do not remove!!"
					ifTrue: [ requestor perform: sourceStreamGetter ]
					ifFalse: [ ReadStream on: requestor text string ]].
		repeatNeeded
	] whileTrue: [
		encoder _ self encoder class new ].
	methNode sourceText: s.
	^ methNode! !


!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/12/2011 10:52'!
acceptDroppingMorph: dropped event: evt 
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	| aMorph |
	aMorph := self morphToDropFrom: dropped.
	self isWorldMorph 
		ifTrue: [	"Add the given morph to this world and start stepping it if it wants to be."

			self addMorphFront: aMorph.
			(aMorph fullBounds intersects: self viewBox) 
				ifFalse: [
					Beeper beep.
					aMorph morphPosition: bounds center]]
		ifFalse: [super acceptDroppingMorph: aMorph event: evt].
	aMorph submorphsDo: [ :m | (m isKindOf: HaloMorph) ifTrue: [ m delete ]].
	self world startSteppingSubmorphsOf: aMorph! !

!PasteUpMorph methodsFor: 'event handling' stamp: 'jmv 12/11/2011 23:30'!
morphToGrab: event
	"Return the morph to grab from a mouse down event. If none, return nil."
	self submorphsDo: [ :m |
		((m rejectsEvent: event) not and:[m fullContainsPoint: event eventPosition]) ifTrue:[^m].
	].
	^nil! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 12/12/2011 10:52'!
viewBox: newViewBox
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState canvas: nil ].
		worldState viewBox: newViewBox ].
	super morphPosition: newViewBox topLeft.
	fullBounds _ bounds _ newViewBox.! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 12/12/2011 10:52'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect _ Rectangle center: aPoint extent: aMorph fullBounds extent.
	delta _ trialRect amountToTranslateWithin: bounds.
	aMorph morphPosition: trialRect origin + delta.
	self addMorph: aMorph.
! !


!PluggableButtonMorph methodsFor: 'event handling' stamp: 'jmv 12/11/2011 23:17'!
mouseUp: evt
	isPressed _ false.
	mouseIsOver _ false.
	(actWhen == #buttonUp and: [self containsPoint: evt eventPosition])
		ifTrue: [ self performAction ].
	self redrawNeeded! !


!PluggableScrollPane methodsFor: 'access' stamp: 'jmv 12/12/2011 10:50'!
addToScroller: aMorph

	aMorph morphPosition: scroller morphPosition.
	scroller addMorph: aMorph! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:50'!
scrollerOffset
	^self viewableBounds topLeft - scroller morphPosition! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:52'!
scrollerOffset: newOffset
	scroller morphPosition: self viewableBounds topLeft - newOffset! !


!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 12/30/2011 23:43'!
mouseDown: evt

	| aMorph |
	evt hand newKeyboardFocus: self.
	aMorph _ self itemFromPoint: evt eventPosition.
	(aMorph notNil and: [ aMorph inToggleArea: evt eventPosition ])
		ifTrue: [ ^self toggleExpandedState: aMorph event: evt ]. 
	evt mouseButton2Pressed  "First check for option (menu) click"
		ifTrue: [ ^ self mouseButton2Activity ].
	aMorph ifNil: [ ^super mouseDown: evt ].
	aMorph highlightForMouseDown.
	evt hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: evt 
		clkSel: #click:
		clkNHalf: nil
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 12/11/2011 23:28'!
mouseUp: event 
	| aMorph |
	aMorph := self itemFromPoint: event eventPosition.
	aMorph ifNil: [^self].
	aMorph highlightedForMouseDown ifFalse: [^self].
	aMorph highlightForMouseDown: false.
	"No change if model is locked"
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	((autoDeselect isNil or: [autoDeselect]) and: [aMorph == selectedMorph]) 
		ifTrue: [self setSelectedMorph: nil]
		ifFalse: [self setSelectedMorph: aMorph].
	Cursor normal show! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 12/11/2011 23:30'!
doubleClick: event
	| index |
	doubleClickSelector ifNil: [^super doubleClick: event].
	index _ self rowAtLocation: event eventPosition.
	index = 0 ifTrue: [^super doubleClick: event].
	"selectedMorph ifNil: [self setSelectedMorph: aMorph]."
	^ self model perform: doubleClickSelector! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 12/30/2011 23:43'!
mouseDown: evt

	| row |
	evt mouseButton2Pressed  "First check for option (menu) click"
		ifTrue: [^ self mouseButton2Activity].
	self hasKeyboardFocus ifFalse: [
		evt hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		self listMorph noSelection].
	row _ self rowAtLocation: evt eventPosition.
	row = 0  ifTrue: [^super mouseDown: evt].
	"self dragEnabled ifTrue: [aMorph highlightForMouseDown]."
	evt hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: evt
		clkSel: #click:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 12/11/2011 23:30'!
mouseUp: event
	"The mouse came up within the list; take appropriate action"

	| row |
	row _ self rowAtLocation: event eventPosition.
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	(autoDeselect == false and: [row = 0]) ifTrue: [^ self].  "work-around the no-mans-land bug"
	"No change if model is locked"
	((autoDeselect == nil or: [autoDeselect]) and: [row == self selectionIndex])
		ifTrue: [self changeModelSelection: 0]
		ifFalse: [self changeModelSelection: row].
	Cursor normal show.
! !


!PluggableListMorphOfMany methodsFor: 'event handling' stamp: 'jmv 12/30/2011 23:44'!
mouseDown: event

	| oldIndex oldVal row w |
	event mouseButton2Pressed ifTrue: [^ self mouseButton2Activity].

	self hasKeyboardFocus ifFalse: [
		event hand newKeyboardFocus: self ].

	row _ self rowAtLocation: event eventPosition.

	row = 0 ifTrue: [^super mouseDown: event].

	w _ self ownerThatIsA: SystemWindow.
	(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

		"Set meaning for subsequent dragging of selection"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row.
		oldIndex _ self getCurrentSelectionIndex.
		oldIndex ~= 0 ifTrue: [oldVal _ self listSelectionAt: oldIndex].

		"Set or clear new primary selection (listIndex)"
		dragOnOrOff
			ifTrue: [self changeModelSelection: row]
			ifFalse: [self changeModelSelection: 0].

		"Need to restore the old one, due to how model works, and set new one."
		oldIndex ~= 0 ifTrue: [self listSelectionAt: oldIndex put: oldVal].
		self listSelectionAt: row put: dragOnOrOff.
		"event hand releaseMouseFocus: aMorph."
		"aMorph changed"
	].
	event hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: event
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PluggableListMorphOfMany methodsFor: 'event handling' stamp: 'jmv 12/11/2011 23:30'!
mouseMove: event 
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row |
	row _ (event eventPosition y < bounds top and: [ scrollBar value > 0.0 ])
		ifTrue: [
			scrollBar scrollUp: 1.
			"Leave at least one visible item unaffected, for better visual feedback to the user."
			(self rowAtLocation: bounds topLeft) + 2 ]
		ifFalse: [
			(event eventPosition y > bounds bottom and: [ scrollBar value < 1.0 ])
				ifTrue: [
					scrollBar scrollDown: 1.
					"Leave at least one visible item unaffected, for better visual feedback to the user."
					(self rowAtLocation: bounds bottomLeft) - 3 ]
				ifFalse: [ self rowAtLocation: event eventPosition ]].
	row = 0 ifTrue: [ ^ self ].

	"No change if model is locked"
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].

	dragOnOrOff ifNil: [
		"Was not set at mouse down, which means the mouse must have gone down in an area where there was no list item"
		dragOnOrOff _ (self listSelectionAt: row) not ].

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


!PolygonMorph methodsFor: 'editing' stamp: 'jmv 12/11/2011 23:17'!
dragVertex: ix event: evt fromHandle: handle
	| p |
	p _ evt eventPosition.
	handle referencePosition: p.
	self verticesAt: ix put: p.
! !

!PolygonMorph methodsFor: 'editing' stamp: 'jmv 12/11/2011 23:18'!
mouseDown: evt

	^ evt shiftPressed
		ifTrue: [((owner isKindOf: PolygonMorph) and: [owner includesHandle: self])
					ifTrue: ["Prevent insertion handles from getting edited"
							^ super mouseDown: evt].
				self toggleHandles.
				handles ifNil: [^ self].
				vertices withIndexDo:  "Check for click-to-drag at handle site"
					[:vertPt :vertIndex |
					((handles at: vertIndex*2-1 ifAbsent: [ ^self ]) containsPoint: evt eventPosition) ifTrue:
						["If clicked near a vertex, jump into drag-vertex action"
						evt hand newMouseFocus: (handles at: vertIndex*2-1)]]]
		ifFalse: [super mouseDown: evt]! !

!PolygonMorph methodsFor: 'editing' stamp: 'jmv 12/11/2011 23:18'!
newVertex: ix event: evt fromHandle: handle
	"Insert a new vertex and fix everything up!! Install the drag-handle of the new vertex as recipient of further mouse events."

	| pt |
	(self hasProperty: #noNewVertices) ifFalse: [
		pt _ evt eventPosition.
		self setVertices: (vertices copyReplaceFrom: ix + 1 to: ix with: (Array with: pt)).
		evt hand newMouseFocus: (handles at: ((ix + 1) * 2) - 1)]
! !

!PolygonMorph methodsFor: 'editing' stamp: 'jmv 12/12/2011 10:52'!
updateHandles
	| newVert oldVert midPts nextVertIx tweens |
	smoothCurve
		ifTrue: [
			handles first referencePosition: vertices first.
			handles last referencePosition: vertices last.
			midPts _ OrderedCollection new.
			nextVertIx _ 2.
			tweens _ OrderedCollection new.
			self
				lineSegmentsDo: [:p1 :p2 | 
					tweens addLast: p2 asIntegerPoint.
					p2
							= (vertices atWrap: nextVertIx)
						ifTrue: ["Found endPoint."
							midPts addLast: (tweens at: tweens size // 2)
									+ (tweens at: tweens size + 1 // 2) // 2.
							tweens _ OrderedCollection new.
							nextVertIx _ nextVertIx + 1]].
			midPts
				withIndexDo: [:midPt :vertIndex | (closed
							or: [vertIndex < vertices size])
						ifTrue: [newVert _ handles at: vertIndex * 2.
							newVert referencePosition: midPt ]]]
		ifFalse: [vertices
				withIndexDo: [:vertPt :vertIndex | 
					oldVert _ handles at: vertIndex * 2 - 1.
					oldVert referencePosition: vertPt.
					(closed
							or: [vertIndex < vertices size])
						ifTrue: [newVert _ handles at: vertIndex * 2.
							newVert morphPosition: vertPt
									+ (vertices atWrap: vertIndex + 1) - newVert bounds extent // 2 + (1 @ -1)]]]! !

!PolygonMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 10:50'!
borderForm
	"A form must be created for drawing the border whenever the borderColor is translucent."

	| borderCanvas |
	borderForm ifNotNil: [^ borderForm].
	borderCanvas _ Display defaultCanvasClass forShadowOver: bounds.
	self drawBorderOn: borderCanvas.
	borderForm _ borderCanvas form.
	self arrowForms do:
		[:f |  "Eliminate overlap between line and arrowheads if transparent."
		borderForm copy: f boundingBox from: f to: f offset - self morphPosition rule: Form erase].
	^ borderForm! !

!PolygonMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 10:50'!
computeBounds
	| oldBounds delta excludeHandles |
	vertices ifNil: [^ self].

	self redrawNeeded.
	oldBounds _ bounds.
	self releaseCachedState.
	bounds _ self curveBounds truncated.
	self arrowForms do: [ :f |
		bounds _ bounds merge: (f offset extent: f extent)].
	handles ifNotNil: [ self updateHandles ].

	"since we are directly updating bounds, see if any ordinary submorphs exist and move them accordingly"
	(oldBounds notNil and: [(delta _ bounds origin - oldBounds origin) ~= (0@0)]) ifTrue: [
		excludeHandles _ IdentitySet new.
		handles ifNotNil: [excludeHandles addAll: handles].
		self submorphsDo: [ :each |
			(excludeHandles includes: each) ifFalse: [
				each morphPosition: each morphPosition + delta ] ] ].
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded.! !


!PolygonMorph class methodsFor: 'instance creation' stamp: 'jmv 12/12/2011 10:52'!
fromHand: hand
	"Let the user draw a polygon, clicking at each vertex, and ending
		by clicking within 5 of the first point..."
	| p1 poly oldVerts pN opposite |
	Cursor crossHair showWhile:
		[[Sensor anyButtonPressed] whileFalse:
			[self currentWorld displayWorldSafely; runStepMethods].
		p1 _ Sensor mousePoint].
	opposite _ (Display colorAt: p1) negated.
	opposite = Color transparent ifTrue: [opposite _ Color red].
	(poly _ LineMorph from: p1 to: p1 color: opposite width: 2) openInWorld.
	oldVerts _ {p1}.
	self currentWorld displayWorldSafely; runStepMethods.
	[true] whileTrue:
		[[Sensor anyButtonPressed] whileTrue:
			[pN _ Sensor mousePoint.
			poly setVertices: (oldVerts copyWith: pN).
			self currentWorld displayWorldSafely; runStepMethods].
		(oldVerts size > 1 and: [(pN dist: p1) < 5]) ifTrue: [
			hand morphPosition: Sensor mousePoint.  "Done -- update hand pos"
			^ (poly setVertices: (poly vertices copyWith: p1)) delete].
		oldVerts _ poly vertices.
		[Sensor anyButtonPressed] whileFalse:
			[pN _ Sensor mousePoint.
			poly setVertices: (oldVerts copyWith: pN).
			self currentWorld displayWorldSafely; runStepMethods]].
! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 12/12/2011 10:50'!
startUpWithoutKeyboard
	"Display and make a selection from the receiver as long as the button  is pressed. Answer the current selection.  Do not allow keyboard input into the menu"
	
	^ self startUpWithCaption: nil at: ActiveHand morphPosition allowKeyboard: false! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:52'!
computeSlider
	| r |
	r _ self roomToMove.
	slider morphPosition:
		(bounds isWide
			ifTrue: [r topLeft + ((r width * value) asInteger @ 0)]
			ifFalse: [r topLeft + (0 @ (r height * value)  asInteger)])! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 12/11/2011 23:31'!
setNextDirectionFromEvent: event

	nextPageDirection _ bounds isWide
		ifTrue: [ event eventPosition x >= slider referencePosition x ]
		ifFalse: [ event eventPosition y >= slider referencePosition y ]! !


!SketchMorph methodsFor: 'accessing' stamp: 'jmv 12/12/2011 10:50'!
form: aForm
	"Set the receiver's form"

	| oldForm |
	oldForm _ originalForm.
	originalForm _ aForm.
	self basicExtent: originalForm extent.

	oldForm ifNotNil: [ self morphPosition: self morphPosition + (oldForm extent - aForm extent // 2) ]! !


!ColorPickerMorph methodsFor: 'event handling' stamp: 'jmv 12/11/2011 23:14'!
mouseDown: evt
	| localPt |
	localPt _ evt eventPosition - bounds topLeft.
	self deleteAllBalloons.
	clickedTranslucency _ TransparentBox containsPoint: localPt.
	self inhibitDragging ifFalse: [
		(DragBox containsPoint: localPt)
			ifTrue: [^ evt hand grabMorph: self].
	].
	(RevertBox containsPoint: localPt)
		ifTrue: [^ self updateColor: originalColor feedbackColor: originalColor].
	self inhibitDragging ifFalse: [self comeToFront].
	sourceHand _ evt hand.
	self startStepping.
! !

!ColorPickerMorph methodsFor: 'menu' stamp: 'jmv 12/12/2011 10:45'!
pickUpColorFor: aMorph
	"Show the eyedropper cursor, and modally track the mouse through a mouse-down and mouse-up cycle"

      | aHand localPt oldCursor |
	aHand _ aMorph isNil
		ifTrue: [self world activeHand] 
		ifFalse: [ aMorph activeHand].
	self addToWorld: aHand world near: (aMorph ifNil: [aHand world]) fullBounds.
	self owner ifNil: [^ self].

	oldCursor _ Sensor currentCursor.
	ColorPickerMorph eyeDropperCursor show.

	self updateContinuously: false.
	[Sensor anyButtonPressed]
		whileFalse: 
			 [self trackColorUnderMouse].
	self deleteAllBalloons.

	localPt _World activeHand morphPosition - bounds topLeft.
	self inhibitDragging ifFalse: [
		(DragBox containsPoint: localPt) ifTrue:
			["Click or drag the drag-dot means to anchor as a modeless picker"
			^ self anchorAndRunModeless: aHand].
	].
	(clickedTranslucency _ TransparentBox containsPoint: localPt)
		ifTrue: [selectedColor _ originalColor].

	self updateContinuously: true.
	[Sensor anyButtonPressed]
		whileTrue:
			 [self updateTargetColorWith: self indicateColorUnderMouse].
	aHand newMouseFocus: nil;
		flushEvents.
	oldCursor show.
	self delete.
		 
 ! !

!ColorPickerMorph methodsFor: 'other' stamp: 'jmv 12/12/2011 10:51'!
addToWorld: world near: box
	| goodLocation |
	goodLocation _ self bestPositionNear: box inWorld: world.
	world allMorphsDo:
		[:p | (p isMemberOf: ColorPickerMorph) ifTrue:
		[(p ~~ self and: [p owner notNil and: [p target == target]]) ifTrue:
			[(p selector == selector and: [p argument == argument])
				ifTrue: [^ p comeToFront  "uncover existing picker"]
				ifFalse: ["place second picker relative to first"
						goodLocation _ self bestPositionNear: p bounds inWorld: world]]]].
	self morphPosition: goodLocation.
	world addMorphFront: self.
	self redrawNeeded! !

!ColorPickerMorph methodsFor: 'other' stamp: 'jmv 12/12/2011 10:45'!
indicateColorUnderMouse
	"Track the mouse with the special eyedropper cursor, and accept whatever color is under the mouse as the currently-chosen color; reflect that choice in the feedback box, and return that color."

	| pt |
	self pickColorAt: (pt _ World activeHand morphPosition ).
	isModal ifTrue: [
		self world activeHand morphPosition: pt.
		self world displayWorldSafely; runStepMethods].
	^ selectedColor	! !

!ColorPickerMorph methodsFor: 'other' stamp: 'jmv 12/12/2011 10:45'!
trackColorUnderMouse
	"Track the mouse with the special eyedropper cursor, and accept whatever color is under the mouse as the currently-chosen color; reflect that choice in the feedback box, and return that color."

	| pt |
	selectedColor _ originalColor.
	self trackColorAt: (pt _ World activeHand morphPosition ).
	isModal ifTrue: [
		self world activeHand morphPosition: pt.
		self world displayWorldSafely; runStepMethods.
		self modalBalloonHelpAtPoint: pt].
	^ selectedColor	! !

!ColorPickerMorph methodsFor: 'stepping and presenter' stamp: 'jmv 12/12/2011 10:45'!
step

	sourceHand ifNotNil: [
		self pickColorAt: sourceHand morphPosition ]! !

!ColorPickerMorph methodsFor: 'private' stamp: 'jmv 12/12/2011 10:51'!
anchorAndRunModeless: aHand
	"If user clicks on the drag-dot of a modal picker,
	anchor it, and change to modeless operation."

	self initializeModal: false; originalColor: originalColor.  "reset as modeless"
	aHand flushEvents.  "Drop any events gathered during modal loop"
	aHand morphPosition: Sensor mousePoint; grabMorph: self.  "Slip into drag operation"
! !


!Sonogram methodsFor: 'all' stamp: 'jmv 12/12/2011 10:50'!
plotColumn: dataArray

	| chm1 i normVal r |
	columnForm unhibernate.
	chm1 _ columnForm height - 1.
	0 to: chm1 do:
		[:y | 
		i _ y*(dataArray size-1)//chm1 + 1.
		normVal _ ((dataArray at: i) - minVal) / (maxVal - minVal).
		normVal < 0.0 ifTrue: [normVal _ 0.0].
		normVal > 1.0 ifTrue: [normVal _ 1.0].
		columnForm bits at: chm1-y+1 put: (pixValMap at: (normVal * 255.0) truncated + 1)].
	(lastX _ lastX + 1) > (image width - 1) ifTrue:
		[self scroll].
	image copy: (r _ (lastX@0 extent: 1@image height))
			from: (32//image depth-1)@0
			in: columnForm rule: Form over.
	"self changed."
	self invalidRect: (r translateBy: self morphPosition)! !


!StarMorph methodsFor: 'editing' stamp: 'jmv 12/12/2011 10:50'!
dragVertex: label event: evt fromHandle: handle
	| ext oldR pt center |
	label == #center ifTrue: [
		self morphPosition: self morphPosition + (evt eventPosition - handle referencePosition)].

	label == #outside ifTrue: [
		center _ handles first referencePosition.
		pt _ center - evt eventPosition.
		ext _ pt r.
		oldR _ ext.
		vertices _ (0 to: 359 by: (360//vertices size)) collect: [ :angle |
			(Point r: (oldR _ oldR = ext ifTrue: [ext*5//12] ifFalse: [ext])
					degrees: angle + pt degrees)
				+ center].
		handle align: handle referencePosition with: evt eventPosition ].

	self computeBounds.
! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 12/12/2011 10:50'!
makeMeVisible 

	self world extent > (0@0) ifFalse: [^ self].

	(self morphPosition >= (0@0) and: [ self morphPosition < (self world extent-self labelHeight)]) ifTrue: [
		^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self isCollapsed
		ifTrue: [ self morphPosition: (RealEstateAgent assignCollapsePointFor: self)]
		ifFalse: [ self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: bounds extent world: self world) topLeft].

! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 12/12/2011 10:50'!
justDroppedInto: aMorph event: anEvent
	isCollapsed
		ifTrue: [
			self morphPosition: (self morphPosition max: 0@0).
			collapsedFrame _ bounds]
		ifFalse: [
			fullFrame _ bounds.
			TopWindow ~~ self ifTrue: [self activate]].
	^super justDroppedInto: aMorph event: anEvent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 12/12/2011 10:52'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| spacing |
	spacing _ self boxExtent x + 2.

	self
		addMorph: (self createCloseBox morphPosition: 2@2);
		addMorph: (self createCollapseBox morphPosition: spacing+2@2);
		addMorph: (self createExpandBox morphPosition: spacing*2+2@2);
		addMorph: (self createMenuBox morphPosition: spacing*3+2@2)! !

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 12/12/2011 10:52'!
openInWorld: aWorld extent: extent
	"This msg and its callees result in the window being activeOnlyOnTop"
	self morphPosition: (RealEstateAgent initialFrameFor: self world: aWorld) topLeft; extent: extent.
	^self openAsIsIn: aWorld! !

!SystemWindow methodsFor: 'top window' stamp: 'jmv 12/12/2011 10:50'!
activateAndForceLabelToShow
	self activate.
	bounds top < 0 ifTrue: [
		self morphPosition: (self morphPosition x @ 0)]! !


!TextEditor methodsFor: 'events' stamp: 'jmv 12/30/2011 23:31'!
doubleClickAndHalf: evt

	| here b interval |
	b _ paragraph characterBlockAtPoint: evt eventPosition.
	here _ b stringIndex.
	interval _ self privateCurrentString encompassParagraph: (here to: here).
	self selectFrom: interval first to: interval last.

	doWordSelection _ false.
	doParagraphSelection _ true.
	initialSelectionStart _ self startBlock.
	initialSelectionStop _ self stopBlock! !

!TextEditor methodsFor: 'events' stamp: 'jmv 12/30/2011 23:44'!
mouseDown: evt 
	| clickPoint b |

	initialSelectionStart _ nil.
	initialSelectionStop _ nil.
	doWordSelection _ false.
	doParagraphSelection _ false.

	"Multiple selection of text.
	Windows uses Control, Mac uses Command (i.e. commandAlt)
	On the Mac, command-button1 is translated to command-button3 by the VM. do:
		Preferences disable: #commandClickOpensHalo
	to disable this behavior and make command-button1 work for multiple selection. "
	(evt controlKeyPressed or: [ evt commandAltKeyPressed ]) ifTrue: [
		self selectionInterval size > 0 ifTrue: [
			selectionStartBlocks _ selectionStartBlocks copyWith: self startBlock.
			selectionStopBlocks _ selectionStopBlocks copyWith: self stopBlock ]]
	ifFalse: [
		selectionStartBlocks _ #().
		selectionStopBlocks _ #() ].

	clickPoint _ evt eventPosition.
	b _ paragraph characterBlockAtPoint: clickPoint.

	(paragraph clickAt: clickPoint) ifTrue: [
		markBlock _ b.
		pointBlock _ b.
		evt hand releaseKeyboardFocus: self.
		^ self ].
	
	evt shiftPressed
		ifFalse: [
			(self markIndex = b stringIndex and: [ self pointIndex = b stringIndex ])
				ifTrue: [
					markBlock _ b.
					pointBlock _ b ]
				ifFalse: [
					markBlock _ b.
					pointBlock _ b.	
					self setEmphasisHereFromText ]]! !

!TextEditor methodsFor: 'events' stamp: 'jmv 12/30/2011 23:48'!
mouseMove: evt
	"Change the selection in response to mouse-down drag"

	| b interval i1 i2 |

	doWordSelection ifTrue: [
		pointBlock _ (paragraph characterBlockAtPoint: (evt eventPosition)).
		self selectWordLeftDelimiters: '' rightDelimiters: ''.
		markBlock _ self startBlock min: initialSelectionStart.
		pointBlock _ self stopBlock max: initialSelectionStop.
		self storeSelectionInParagraph.
		^self ].

	doParagraphSelection ifTrue: [
		b _ paragraph characterBlockAtPoint: evt eventPosition.
		i1 _ b stringIndex min: initialSelectionStart stringIndex.
		i2 _ b stringIndex max: initialSelectionStop stringIndex-1.
		interval _ self privateCurrentString encompassParagraph: (i1 to: i2).
		self selectFrom: interval first to: interval last.
		markBlock _ self startBlock min: initialSelectionStart.
		pointBlock _ self stopBlock max: initialSelectionStop.
		self storeSelectionInParagraph.
		^self ].

	pointBlock _ (paragraph characterBlockAtPoint: (evt eventPosition)).
	self storeSelectionInParagraph! !

!TextEditor methodsFor: 'events' stamp: 'jmv 12/11/2011 23:19'!
mouseUp: evt
	| cursorBlock cursorIndex startBlock startIndex stopBlock stopIndex |

	evt shiftPressed
		ifTrue: [
			"Squeak classic behavior for click, move, shift-click sequence "
			"pointBlock _(paragraph characterBlockAtPoint: (evt eventPosition))."

			"Mac behavior"
			cursorBlock _ paragraph characterBlockAtPoint: evt eventPosition.
			cursorIndex _ cursorBlock stringIndex.
			startBlock _ self startBlock min: cursorBlock.
			startIndex _ startBlock stringIndex.
			stopBlock _ self stopBlock max: cursorBlock.
			stopIndex _ stopBlock stringIndex.
			(stopIndex - cursorIndex) < (cursorIndex - startIndex)
				ifTrue: [
					markBlock _ startBlock.
					pointBlock _ cursorBlock ]
				ifFalse: [
					markBlock _ stopBlock.
					pointBlock _ cursorBlock ]].
	self storeSelectionInParagraph! !


!TextModelMorph methodsFor: 'editor access' stamp: 'jmv 12/11/2011 23:32'!
scrollSelectionIntoView: event
	"Scroll my text into view if necessary and return true, else return false"
	| selRects rectToTest cpHere |
	selRects _ self textMorph selectionRects.
	selRects isEmpty ifTrue: [ ^ self ].
	rectToTest _ selRects first merge: selRects last.
	(event notNil and: [ event anyButtonPressed ]) ifTrue: [  "Check for autoscroll"
		cpHere _ event eventPosition.
		cpHere y <= bounds top
			ifTrue:  [ rectToTest _ selRects first topLeft extent: 2@2 ]
			ifFalse: [
				cpHere y >= bounds bottom
					ifTrue: [ rectToTest _ selRects last bottomRight extent: 2@2 ]
					ifFalse: [ ^ self ]]].
	^ self scrollToShow: rectToTest! !


!MouseEvent methodsFor: 'comparing' stamp: 'jmv 12/11/2011 23:29'!
= aMorphicEvent
	super = aMorphicEvent ifFalse:[^false].
	position = aMorphicEvent eventPosition ifFalse: [^ false].
	buttons = aMorphicEvent buttons ifFalse: [^ false].
	^ true
! !

!MouseEvent methodsFor: 'printing' stamp: 'jmv 12/11/2011 23:37'!
printOn: aStream

	aStream nextPut: $[.
	aStream nextPutAll: self eventPosition printString; space.
	aStream nextPutAll: type; space.
	aStream nextPutAll: self modifierString.
	aStream nextPutAll: self buttonString.
	aStream nextPutAll: timeStamp printString.
	aStream nextPut: $].! !


!MouseMoveEvent methodsFor: 'comparing' stamp: 'jmv 12/11/2011 23:29'!
= aMorphicEvent
	super = aMorphicEvent ifFalse:[^false].
	position = aMorphicEvent eventPosition ifFalse: [^ false].
	startPoint = aMorphicEvent startPoint ifFalse: [^ false].
	buttons = aMorphicEvent buttons ifFalse: [^ false].
	^ true
! !


!WindowEdgeAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 12/12/2011 10:50'!
handPoint
	^ hand morphPosition! !

!methodRemoval: MouseEvent #position!
MouseEvent removeSelector: #position!
!methodRemoval: UserInputEvent #position!
UserInputEvent removeSelector: #position!
!methodRemoval: SystemWindow #position:!
SystemWindow removeSelector: #position:!
!methodRemoval: PasteUpMorph #position:!
PasteUpMorph removeSelector: #position:!
!methodRemoval: HaloMorph #position:!
HaloMorph removeSelector: #position:!
!methodRemoval: Morph #position!
Morph removeSelector: #position!
!methodRemoval: Morph #position:!
Morph removeSelector: #position:!
!methodRemoval: DropEvent #position!
DropEvent removeSelector: #position!
