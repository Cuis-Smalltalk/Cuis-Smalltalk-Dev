'From Cuis 4.1 of 12 December 2012 [latest update: #1669] on 12 April 2013 at 8:11:20 am'!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevBounds prevFullBounds '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!SystemWindow class methodsFor: 'top window' stamp: 'jmv 4/11/2013 08:46'!
noteTopWindowIn: aWorld but: aWindow
	| newTop |
	"TopWindow must be nil or point to the top window in this project."
	TopWindow _ nil.
	aWorld ifNil: [^ self].
	newTop := aWorld submorphs 
				detect: [:m | (m is: #SystemWindow) and: [m ~~ aWindow]]
				ifNone: [^nil].
	newTop activate.
	^newTop! !


!BitBlt methodsFor: 'accessing'!
clipRect
	"Answer the receiver's clipping area rectangle."

	^clipX @ clipY extent: clipWidth @ clipHeight! !


!Morph methodsFor: 'drawing' stamp: 'jmv 4/11/2013 07:19'!
drawErrorOn: aCanvas
	"The morph (or one of its submorphs) had an error in its drawing method."
	| br w w2 |
	br _ self morphExtent.
	w _ br > (20@20)
		ifTrue: [ 10 ]
		ifFalse: [ 1 ].
	aCanvas
		frameAndFillRectangle: (0@0 corner: br)
		fillColor: Color red
		borderWidth: w
		borderColor: Color yellow.
	w2 _ w//2.
	aCanvas line: 0@0 - w2 to: (br-w-w2) width: w color: Color yellow.
	aCanvas line: (br x-w-w2) @ (0-w2) to: 0-w2 @ (br y - w - w2) width: w color: Color yellow.! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/11/2013 09:00'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| fullBounds |
	fullBounds _ self morphFullBoundsInWorld.
	fullBounds _ fullBounds intersect: (0@0 extent: aCanvas extent).
	(savedPatch isNil or: [savedPatch extent ~= fullBounds extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: fullBounds extent depth: aCanvas depth ].
	aCanvas
		contentsOfArea: (fullBounds translatedBy: aCanvas canvasOrigin)
		into: savedPatch.
	savedPatch offset: fullBounds topLeft.
	prevFullBounds
		ifNil: [ aStream nextPut: fullBounds ]
		ifNotNil: [ aStream nextPut: (fullBounds merge: prevFullBounds)].
	prevFullBounds _ fullBounds! !


!SoundBuffer class methodsFor: 'instance creation' stamp: 'jm 9/17/97 12:44'!
newMonoSampleCount: anInteger
	"Return a SoundBuffer large enough to hold the given number of monaural samples (i.e., 16-bit words)."
	"Details: The size is rounded up to an even number, since the underlying representation is in terms of 32-bit words."

	^ self basicNew: (anInteger + 1) // 2
! !


!SystemWindow methodsFor: 'menu' stamp: 'jmv 4/11/2013 08:47'!
makeSecondTopmost
	| thisWorld |
	thisWorld _ self world.
	(SystemWindow noteTopWindowIn: thisWorld but: self)
		ifNotNil: [ :nextWindow |
			thisWorld addMorph: self behind: nextWindow ]! !

!SystemWindow methodsFor: 'menu' stamp: 'jmv 4/11/2013 08:48'!
sendToBack
	| thisWorld |
	thisWorld _ self world.
	(SystemWindow noteTopWindowIn: thisWorld but: self)
		ifNotNil: [ :nextWindow |
			thisWorld addMorphBack: self ]! !

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 4/11/2013 08:44'!
delete
	| thisWorld |
	self okToChange ifFalse: [^self].
	thisWorld _ self world.
	SystemWindow noteTopWindowIn: thisWorld but: self.
	self sendToBack.
	super delete.
	model _ nil! !

!SystemWindow methodsFor: 'top window' stamp: 'jmv 4/11/2013 08:37'!
activate
	"Bring me to the front and make me able to respond to mouse and keyboard"

	self activateAndSendTopToBack: false! !

!methodRemoval: SystemWindow class #noteTopWindowIn:!
SystemWindow class removeSelector: #noteTopWindowIn:!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevFullBounds'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
