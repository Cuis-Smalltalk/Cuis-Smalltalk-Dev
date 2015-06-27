'From Cuis 4.2 of 25 July 2013 [latest update: #2386] on 26 June 2015 at 4:13:10.690331 pm'!
!classDefinition: #DisplayScreen category: #'Graphics-Display Objects'!
Form subclass: #DisplayScreen
	instanceVariableNames: ''
	classVariableNames: 'ScreenUpdaterProcess ScreenUpdaterSemaphore Damage '
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 6/26/2015 16:01'!
installScreenUpdater
	"Initialize the event tickler process. Terminate the old process if any."
	"
	DisplayScreen installScreenUpdater
	"

	ScreenUpdaterProcess ifNotNil: [ ScreenUpdaterProcess terminate ].
	ScreenUpdaterProcess _ [ self screenUpdater ] newProcess.
	ScreenUpdaterProcess priority: Processor lowIOPriority.
	ScreenUpdaterProcess name: 'Background Screen updater'.
	ScreenUpdaterProcess resume! !

!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 6/26/2015 16:04'!
screenUpdateRequired: aRectangle
	
	Damage _ Damage ifNil: [ aRectangle ] ifNotNil: [ Damage merge: aRectangle ].
	ScreenUpdaterSemaphore signal! !

!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 6/26/2015 16:04'!
screenUpdater
	| delay |
	delay _ Delay forMilliseconds: 50.
	ScreenUpdaterSemaphore _ Semaphore new.
	Damage _ nil.
	[
		ScreenUpdaterSemaphore initSignals; wait.
		Display forceToScreen: Damage.
		delay wait.
	] repeat! !


!BitBlt class methodsFor: 'examples' stamp: 'jmv 6/26/2015 14:24'!
exampleColorMap	"BitBlt exampleColorMap"
	"This example shows what one can do with the fixed part of a color map. The color map, as setup below, rotates the bits of a pixel all the way around. Thus you'll get a (sometime strange looking ;-) animation of colors which will end up exactly the way it looked at the beginning. The example is given to make you understand that the masks and shifts can be used for a lot more than simply color converting pixels. In this example, for instance, we use only two of the four independent shifters."
	| cc bb |
	cc _ ColorMap masks: {
		1 << (Display depth-1). "mask out high bit of color component"
		1 << (Display depth-1) - 1. "mask all other bits"
		0.
		0}
		shifts: {
			1 - Display depth. "shift right to bottom most position"
			1. "shift all other pixels one bit left"
			0.
			0}.
	bb _ BitBlt toForm: Display.
	bb 
		sourceForm: Display;
		combinationRule: 3;
		colorMap: cc.
	1 to: Display depth do: [ :i |
		bb copyBits.
"		Display forceDisplayUpdate."
		Display forceToScreen.
	].
! !


!Form methodsFor: 'transitions' stamp: 'jmv 6/26/2015 14:21'!
pageWarp: otherImage at: topLeft forward: forward
	"Produce a page-turning illusion that gradually reveals otherImage
	located at topLeft in this form.
	forward == true means turn pages toward you, else away. [ignored for now]"
	| pageRect oldPage nSteps buffer p leafRect sourceQuad warp oldBottom d |
	pageRect _ otherImage boundingBox.
	oldPage _ self copy: (pageRect translatedBy: topLeft).
	(forward ifTrue: [oldPage] ifFalse: [otherImage])
		border: pageRect
		widthRectangle: (Rectangle
				left: 0
				right: 2
				top: 1
				bottom: 1)
		rule: Form over
		fillColor: Color black.
	oldBottom _ self copy: ((pageRect bottomLeft + topLeft) extent: (pageRect width@(pageRect height//4))).
	nSteps _ 8.
	buffer _ Form extent: otherImage extent + (0@(pageRect height//4)) depth: self depth.
	d _ pageRect topLeft + (0@(pageRect height//4)) - pageRect topRight.
	1 to: nSteps-1 do:
		[:i | forward
			ifTrue: [buffer copy: pageRect from: otherImage to: 0@0 rule: Form over.
					p _ pageRect topRight + (d * i // nSteps)]
			ifFalse: [buffer copy: pageRect from: oldPage to: 0@0 rule: Form over.
					p _ pageRect topRight + (d * (nSteps-i) // nSteps)].
		buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
		leafRect _ pageRect topLeft corner: p x @ (pageRect bottom + p y).
		sourceQuad _ Array with: pageRect topLeft
			with: pageRect bottomLeft + (0@p y)
			with: pageRect bottomRight
			with: pageRect topRight - (0@p y).
		warp _ (WarpBlt toForm: buffer)
				clipRect: leafRect;
				sourceForm: (forward ifTrue: [oldPage] ifFalse: [otherImage]);
				combinationRule: Form paint.
		warp copyQuad: sourceQuad toRect: leafRect.
		self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
"		Display forceDisplayUpdate."
		Display forceToScreen.
		].

	buffer copy: pageRect from: otherImage to: 0@0 rule: Form over.
	buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
	self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
"	Display forceDisplayUpdate."
	Display forceToScreen.
"
1 to: 4 do: [:corner | Display pageWarp:
				(Form fromDisplay: (10@10 extent: 200@300)) reverse
			at: 10@10 forward: false]
"
! !

!Form methodsFor: 'transitions' stamp: 'jmv 6/26/2015 14:25'!
slideImage: otherImage at: topLeft delta: delta
	"
	Display slideImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse
		at: 40@40 delta: 3@-4
	"
	| bb nSteps clipRect |
	bb _ otherImage boundingBox.
	clipRect _ topLeft extent: otherImage extent.
	nSteps _ 1.
	delta x = 0 ifFalse: [nSteps _ nSteps max: (bb width//delta x abs) + 1].
	delta y = 0 ifFalse: [nSteps _ nSteps max: (bb height//delta y abs) + 1].
	1 to: nSteps do: [ :i | 
			self 
				copyBits: bb from: otherImage
				at: delta*(i-nSteps) + topLeft
				clippingBox: clipRect rule: Form paint.
"			Display forceDisplayUpdate."
			Display forceToScreen.
		]! !

!Form methodsFor: 'transitions' stamp: 'jmv 6/26/2015 14:26'!
wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: rectForIndexBlock

	| i clipRect t rectsOrNil waitTime |
	i _ 0.
	clipRect _ topLeft extent: otherImage extent.
	clipBox ifNotNil: [ clipRect _ clipRect intersect: clipBox ].
	[ rectsOrNil _ rectForIndexBlock value: (i _ i + 1) ]
		whileNotNil: [
			t _ Time localMillisecondClock.
			rectsOrNil do: [ :r |
				self copyBits: r from: otherImage at: topLeft + r topLeft
					clippingBox: clipRect rule: Form over ].
"			Display forceDisplayUpdate."
			Display forceToScreen.
			waitTime _ 3 - (Time localMillisecondClock - t).
			waitTime > 0 ifTrue:
				["(Delay forMilliseconds: waitTime) wait"]]! !


!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 6/26/2015 14:24'!
flash: aRectangle 
	"Flash the area of the screen defined by the given rectangle."

	self reverse: aRectangle.
"	self forceDisplayUpdate."
	self forceToScreen.
	(Delay forMilliseconds: 100) wait.
	self reverse: aRectangle.
"	self forceDisplayUpdate."
	self forceToScreen.
! !

!DisplayScreen methodsFor: 'other' stamp: 'jmv 6/26/2015 14:33'!
forceDisplayUpdate
"dejamos de usarla, porque parece que con el deferUpdates: true, la papa es usar #forceToScreen
Probar en Win / Mac / Linux"
	"On platforms that buffer screen updates, force the screen to be updated immediately. On other platforms, or if the primitive is not implemented, do nothing."

	<primitive: 231>
	"do nothing if primitive fails"! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 6/26/2015 14:46'!
shutDown 
	"Minimize Display memory saved in image"
	Display setExtent: 240@120 depth: Display nativeDepth.
	ScreenUpdaterProcess ifNotNil: [
		ScreenUpdaterProcess terminate.
		ScreenUpdaterProcess _ nil ]! !

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 6/26/2015 14:46'!
startUp 
	"
	DisplayScreen startUp
	"
	Display setExtent: self actualScreenSize depth: Display nativeDepth.
	Display beDisplay.
	self installScreenUpdater! !


!BMPReadWriter class methodsFor: 'testing' stamp: 'jmv 6/26/2015 14:23'!
displayAllFrom: fd
	"BMPReadWriter displayAllFrom: FileDirectory smalltalkImageDirectory"
	fd fileNames do:[:fName|
		(fName endsWith: '.bmp') ifTrue:[
			[
				(Form fromBinaryStream: (fd readOnlyFileNamed: fName) binary) display.
				"Display forceDisplayUpdate."
				Display forceToScreen
			] on: Error do:[:nix|].
		].
	].
	fd directoryNames do:[:fdName|
		self displayAllFrom: (fd directoryNamed: fdName)
	].! !


!InputSensor methodsFor: 'user interrupts' stamp: 'jmv 6/26/2015 14:36'!
userInterruptWatcher
	"Wait for user interrupts and open a notifier on the active process when one occurs."

	[ true ] whileTrue: [
		InterruptSemaphore wait.
"		Display deferUpdates: false."
		Smalltalk at: #SoundPlayer ifPresent: [ :cls | cls shutDown ].
		Smalltalk handleUserInterrupt]! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 6/26/2015 14:27'!
fillRects: rectangleList color: aColor
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."
	"Details: Uses two reverses so that the display is restored to its original state. This is necessary when in deferred update mode."

	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: 0@0;
		clipRect: self viewBox;
		fillColor: aColor;
		combinationRule: Form over.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect"; forceDisplayUpdate" ].

	(Delay forMilliseconds: 150) wait! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 6/26/2015 14:28'!
flashRects: rectangleList color: aColor
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."
	"Details: Uses two reverses so that the display is restored to its original state. This is necessary when in deferred update mode."

	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: 0@0;
		clipRect: self viewBox;
		fillColor: aColor;
		combinationRule: Form reverse.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect"; forceDisplayUpdate "].

	(Delay forMilliseconds: 250) wait.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect"; forceDisplayUpdate "]! !


!TranscriptMorph methodsFor: 'initialization' stamp: 'jmv 6/26/2015 14:56'!
initialize
	super initialize.
	doImmediateUpdates _ true! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 6/26/2015 15:08'!
bounds: aRectangle

	innerRectangle _ aRectangle insetBy: self padding! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 6/26/2015 16:00'!
display
	showOnDisplay ifTrue: [
		self displayOn: Display.
		DisplayScreen screenUpdateRequired: innerRectangle ].
	self changed: #redraw	"So any morph in front of us is repaired when Morphic cycles"! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 6/26/2015 15:08'!
displayOn: aForm
	"
	Transcript displayOn: Display
	"
	| font count i string x y fh f r canvas |
	r _ innerRectangle outsetBy: self padding.
	aForm fill: r fillColor: Color white.
	font _ AbstractFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.
	y _ innerRectangle top.
	f _ firstIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	i _ (lastIndex - count max: f) \\ self maxEntries + 1.
	canvas _ aForm getCanvas.
	canvas newClipRect: innerRectangle.
	[
		string _ entries at: i.	
		canvas drawString: string at: x@y font: font color: Color veryDarkGray.
		y _ y + fh.
		i = lastIndex
	] whileFalse: [ i _ i \\ self maxEntries + 1 ].

	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: Color veryDarkGray! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 6/26/2015 15:59'!
displayUnfinishedEntry
	showOnDisplay ifTrue: [
		(self displayUnfinishedEntryOn: Display) ifNotNil: [ :damage |
			DisplayScreen screenUpdateRequired: damage ]]! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 6/26/2015 15:59'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas r |
	lastDisplayPosition < innerRectangle right ifTrue: [
		font _ AbstractFont default.
		fh _ font height.
		count _ innerRectangle height // fh-1.
		x _ innerRectangle left.
		string _ unfinishedEntry contents.
		y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerRectangle top.
		canvas _ aForm getCanvas.
		r _ innerRectangle left: lastDisplayPosition.
		canvas newClipRect: r.
		(canvas drawString: string at: x@y font: font color: Color veryDarkGray) ifNotNil: [ :lastPoint |
			lastDisplayPosition _ lastPoint x.
			^r ]].
	^nil! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 6/26/2015 14:36'!
handleFatalDrawingError: errMsg
	"Handle a fatal drawing error."

	self primitiveError: 
		'Fatal Morphic drawing error', String newLineString,
		errMsg.

	"Hm... we should jump into a 'safe' worldState here, but how do we find it?!!"! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 6/26/2015 14:36'!
tryDeferredUpdatingAndSetCanvasFor: aWorld
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, set a non-Disply canvas and answer false.."
	| properDisplay |
	
	"como es el unico sender, se puede hacer en el startup de morphic o de la imagen, y nunca mas..."
	(Display deferUpdates: true) ifNil: [
		"deferred updates not supported by the VM, do them in the image!!"
		self ensureNonDisplayCanvas.
		^ false].
	
	"if no canvas, or canvas was offscreen, from a platform that didnt support defers, then fix it"
	properDisplay _ canvas notNil and: [canvas drawsOnDisplay].
	properDisplay ifFalse: [
		aWorld viewBox: Display boundingBox.    "do first since it may clear canvas"
		self setCanvas: Display getCanvas.
	].
	^ true! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 6/26/2015 14:30'!
displayWorld: aWorld submorphs: submorphs
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvasFor: aWorld.

	"repair world's damage on canvas"
	worldDamageRects _ self drawInvalidAreasWorld: aWorld submorphs: submorphs.

	"Check which hands need to be drawn (they are not the hardware mouse pointer)"
	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"Draw hands (usually carying morphs) onto world canvas"
	canvas newClipRect: nil.
	handsToDraw reverseDo: [ :h | canvas fullDrawHand: h ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode
		ifTrue: [
			"Drawing was done on Display object. Just show it"
			Display forceDamageToScreen: allDamage.
			]
		ifFalse: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			canvas showAt: aWorld viewBox origin invalidRects: allDamage.
			].

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].
	"Dejemoslo en deferUpdates: true. Por que no? Si lo desactivo, la Mac se muere..."
	"Display deferUpdates: false."
"	Display forceDisplayUpdate"! !

!methodRemoval: Transcript class #borderWidth!
Transcript class removeSelector: #borderWidth!
!classDefinition: #DisplayScreen category: #'Graphics-Display Objects'!
Form subclass: #DisplayScreen
	instanceVariableNames: ''
	classVariableNames: 'Damage ScreenUpdaterProcess ScreenUpdaterSemaphore'
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

DisplayScreen installScreenUpdater.
TranscriptMorph allInstancesDo: [ :tr | tr doImmediateUpdates ]!

