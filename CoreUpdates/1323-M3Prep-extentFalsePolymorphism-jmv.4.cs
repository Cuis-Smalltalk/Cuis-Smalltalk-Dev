'From Cuis 4.0 of 3 April 2012 [latest update: #1255] on 10 April 2012 at 3:14:11 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 13:43'!
morphExtent
"assume it is always in owner's coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^ bounds extent! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 13:43'!
morphExtent: aPoint
"assume it is always in owner's coordinates!!"
	self flag: #jmvVer2.
	self basicExtent: aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:31'!
validateNotSent
	"To be removed. Just to check consistency"

	self flag: #jmvVer2.
	Count ifNil: [ Count _ 0 ].
	Count _ Count + 1.
	Count < 10 ifTrue: [
		'-----False polymorphism. Should not be sent!!-----' print.
		thisContext printStack: 10 ]! !


!ImageMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:21'!
morphExtent: aPoint
	"Do nothing; my extent is determined by my image Form."
! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:27'!
morphExtent: aPoint
	| newExtent |
	"Resist changing the extent if no wordwrap.. this should be checked."
	wrapFlag ifFalse: [ ^ self ].
	newExtent _ aPoint truncated max: self minimumExtent.
	
	"No change of wrap width"
	newExtent x = bounds extent x ifTrue: [ ^ self ].

	super morphExtent: newExtent.
	
	self resetParagraph.
	self editor recomputeSelection.	
	self updateFromParagraph.! !


!MagnifierMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:32'!
morphExtent: aPoint
	"Round to multiples of magnification"
	srcExtent _ (aPoint - (2 * borderWidth)) // magnification.
	^super morphExtent: self defaultExtent! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:32'!
morphExtent: aPoint

	self flag: #jmvVer2.
	bounds extent = aPoint ifFalse: [
		self redrawNeeded.
		bounds _ bounds topLeft extent: aPoint.
		extent _ aPoint.
		self validatePositionAndBounds.
		self buildMagnifiedBackgroundImage.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = bounds ifFalse: [
				worldState canvas: nil.
				worldState viewBox: bounds ]]]! !


!PluggableButtonMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:28'!
morphExtent: aPoint
	super morphExtent: aPoint.
	magnifiedIcon _ nil.
	self redrawNeeded! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:28'!
morphExtent: newExtent
	
	| minH minW |
	"Figure out the minimum width and height for this pane so that scrollbars will appear"
	minH _ Preferences scrollbarThickness * 2.
	minW _ minH.
	super morphExtent: (newExtent max: (minW@minH)).

	"Now reset widget sizes"
	scroller adjustExtent.
	self updateScrollBarsBounds.
	self setScrollDeltas! !


!LimitedHeightTextMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:27'!
morphExtent: aPoint
	maxHeight _ aPoint y.
	super morphExtent: aPoint.! !


!PolygonMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:21'!
morphExtent: newExtent 
	"Not really advisable, but we can preserve most of the geometry if we don't
	shrink things too small."
	| safeExtent center |
	center _ self referencePosition.
	safeExtent _ newExtent max: 20@20.
	self setVertices: (vertices collect:
		[:p | p - center * (safeExtent asFloatPoint / (bounds extent max: 1@1)) + center])! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 4/10/2012 15:10'!
morphExtent: newExtent
	| newExtentToUse |
	newExtent = bounds extent ifTrue: [^ self].
	newExtentToUse _ bounds isWide
		ifTrue: [ (newExtent x max: 14) @ newExtent y ]
		ifFalse: [ newExtent x @ (newExtent y max: 14) ].
	newExtentToUse = bounds extent ifTrue: [^ self].
	super morphExtent: newExtentToUse.
		
	self flag: #jmv.
	"Most times it is not necessary to recreate the buttons"
	self recreateSubmorphs! !


!Sonogram methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:22'!
morphExtent: newExtent
	super image: (Form extent: newExtent depth: Display depth).
	lastX _ -1.
	columnForm _ Form extent: (32//image depth)@(image height) depth: image depth.
	pixValMap _ ((1 to: 256) collect:
			[:i | columnForm pixelValueFor: (Color gray: (256-i)/255.0)])
		as: Bitmap.
! !


!StringMorph methodsFor: 'accessing' stamp: 'jmv 12/16/2011 10:26'!
setWidth: width

	self morphExtent: width @ (font ifNil: [StrikeFont default]) height! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:28'!
morphExtent: aPoint 
	"Set the receiver's extent to value provided. Respect my minimumExtent."

	| newExtent |
	newExtent _ self isCollapsed
		ifTrue: [aPoint]
		ifFalse: [aPoint max: self minimumExtent].
	newExtent = bounds extent ifTrue: [^ self].

	isCollapsed
		ifTrue: [super morphExtent: newExtent x @ (self labelHeight + 2)]
		ifFalse: [super morphExtent: newExtent].
	isCollapsed
		ifTrue: [collapsedFrame _ bounds]
		ifFalse: [fullFrame _ bounds]! !


!TranscriptMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:28'!
morphExtent: aPoint
	super morphExtent: aPoint.
	(form isNil or: [ form extent ~= aPoint ]) ifTrue: [
		form _ Form extent: aPoint depth: Display depth ]! !


!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:34'!
bounds: newBounds
	| oldExtent newExtent |

	"remove senders and implementors"
	self flag: #jmvVer2.
		
	oldExtent _ bounds extent.
	newExtent _ newBounds extent.
	"Moving stuff around is most likely the most common operation.
	Optimize it"
	oldExtent = newExtent ifTrue: [
		^self morphPosition: newBounds topLeft ].
	(oldExtent dotProduct: oldExtent) <= (newExtent dotProduct: newExtent) ifTrue:[
		"We're growing. First move then resize."
		self morphPosition: newBounds topLeft; morphExtent: newExtent.
	] ifFalse: [
		"We're shrinking. First resize then move."
		self morphExtent: newExtent; morphPosition: newBounds topLeft.
	].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 13:43'!
extent
"Needs to be turned into extentInOwner and extentInWorld"
"or better yet into morphExtent:, and assume it is always in owner's coordinates!!"
	self flag: #jmvVer2.
	self validateNotSent.
	^ bounds extent! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 14:57'!
extent: aPoint
"Needs to be turned into extentInOwner: and extentInWorld:"
"or better yet into morphExtent:, and assume it is always in owner's coordinates!!"
"(when deleting, delete all inheritance!!)"
	self flag: #jmvVer2.
	self validateNotSent.
	self basicExtent: aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 13:38'!
height
	self flag: #jmvVer2.
	"False polymorphism elimination: turn senders into morphHeight"
	self validateNotSent.
	^ bounds height! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 13:38'!
height: aNumber
	self flag: #jmvVer2.
	"False polymorphism elimination: turn senders into morphHeight:"
	self validateNotSent.
	self morphExtent: bounds width@aNumber asInteger.
! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:25'!
morphHeight: aNumber

"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	self morphExtent: bounds width@aNumber asInteger.
! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 13:38'!
morphPositionInOwner: newPositionInOwner
	"Change the position of this morph."
"Ojo aca. Ponemos position a nil pero es solo por el problema de bounds desenganchado de position..."
	| newPositionInWorld |
	self flag: #jmvVer2.

	newPositionInWorld _ owner
		ifNotNil: [ owner externalizeToWorld: newPositionInOwner ]
		ifNil: [ newPositionInOwner ].

	position = newPositionInOwner ifTrue: [
		newPositionInWorld = bounds topLeft ifFalse: [
			'---------feote2' print
		].
		^ self ].		"Null change"

	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.

	self validateOwnerNotNil.


"This won't be needed once we remove the bounds ivar in global coordinates!!
With this, we also delete implementors of #privateFullMoveBy: and #privateMoveBy: "
self privateFullMoveBy: newPositionInWorld - bounds topLeft.

	position _ newPositionInOwner.
	self validatePositionAndBounds.

	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 13:38'!
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
	self validateOwnerNotNil.

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].


	position = newPositionInOwner ifTrue: [
		"Al menos en algunos casos esto pasa porque al poner owner, no reajustamos bounds...
		Es necesario hacer que los globales sigan a los locales (manteniendo locales, moviendo morph)
		o que las locales sigan a las globales (manteniendo posicion en el mundo... si es que estabamos en un mundo!!)"
		self flag: #jmvVer2.
		newPositionInWorld - bounds topLeft = (0@0) ifFalse: [
			'-----feote' print.
			newPositionInWorld print.
			bounds print.
			position print.
			self print.
			owner print.
			owner morphPosition print.
			'--' print.
		].
"Quizas no un null change del todo... Es posible que bounds y position se desincronicen, al cambiar el owner (o cualquier owner en el medio). Todo esto es fulerote. espero sacarlo pronto!!"
		self privateFullMoveBy: newPositionInWorld - bounds topLeft.
		^ self ].		"Null change"


	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.

"This won't be needed once we remove the bounds ivar in global coordinates!!
With this, we also delete implementors of #privateFullMoveBy: and #privateMoveBy: "
self privateFullMoveBy: newPositionInWorld - bounds topLeft.

	position _ newPositionInOwner.
	self validatePositionAndBounds.

	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:25'!
morphWidth: aNumber
"Ensure everybody wants owner's coordinates!!"
	self flag: #jmvVer2.
	self morphExtent: aNumber asInteger@bounds height.
! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 13:36'!
width

	self flag: #jmvVer2.
	"False polymorphism elimination: turn senders into morphWidth"
	self validateNotSent.
	^ bounds width! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 13:37'!
width: aNumber

	self flag: #jmvVer2.
	"False polymorphism elimination: turn senders into morphWidth:"
	self validateNotSent.
	self morphExtent: aNumber asInteger@bounds height! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 12/16/2011 10:25'!
resizeMorph: evt 
	| handle |
	handle := HandleMorph new 
				forEachPointDo: [:newPoint | self morphExtent: newPoint - bounds topLeft].
	evt hand attachMorph: handle.
	handle startStepping! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 12/16/2011 10:27'!
createTextPaneExtent: answerExtent acceptBoolean: acceptBoolean
	"create the textPane"
	| result |
	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval.
	result morphExtent: answerExtent.
	result borderWidth: 1; borderColor: Color lightGray.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	self addMorph: result.
	result bounds: (14@25 corner: 257@84).
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 12/16/2011 10:23'!
initialize

	super initialize.
	self morphExtent: 271@121.
	responseUponCancel := ''
	! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 12/16/2011 10:36'!
addNameBeneath: outerRectangle string: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition w nameBackground |
	w _ self world ifNil: [ target world ].
	nameBackground _ RectangleMorph new
		borderWidth: 0;
		color: (Color lightBlue alpha: 0.9).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color magenta.
	self addMorph: nameBackground.
	self addMorph: nameMorph.
	namePosition _ outerRectangle bottomCenter - ((nameMorph morphWidth // 2) @ (self handleSize negated // 2 - 1)).
	nameMorph morphPosition: (namePosition min: w viewBox bottomRight - nameMorph morphExtent y + 5).
	nameBackground bounds: (nameMorph bounds outsetBy: 2).
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/16/2011 10:41'!
doGrow: evt with: growHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
"Como podria andar el grow de un morph embebido en otro? andara ahora?"
newExtent _ evt eventPosition - positionOffset - target bounds topLeft.
	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target morphExtent: newExtent.
	growHandle morphPosition: evt eventPosition - (growHandle extent // 2).
	self someSubmorphPositionOrExtentChanged
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/16/2011 10:43'!
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

	rotHandle morphPosition: evt eventPosition - (rotHandle morphExtent // 2).
	self someSubmorphPositionOrExtentChanged! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/16/2011 10:44'!
strokeGrow: evt with: growHandle
	| dir |
	evt keyValue = 28 ifTrue:[dir _ -1@0].
	evt keyValue = 29 ifTrue:[dir _ 1@0].
	evt keyValue = 30 ifTrue:[dir _ 0@-1].
	evt keyValue = 31 ifTrue:[dir _ 0@1].
	dir ifNil:[^self].
	evt hand obtainHalo: self.
	evt hand newKeyboardFocus: growHandle.
	target morphExtent: target morphExtent + dir.
	self someSubmorphPositionOrExtentChanged.! !


!HandleMorph methodsFor: 'initialization' stamp: 'jmv 12/16/2011 10:23'!
initialize
	"initialize the state of the receiver"
	super initialize.
	self morphExtent: 12 @ 12! !


!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 12/16/2011 10:23'!
contents: aString
	contents _ aString.
	paragraph _ Paragraph new.
	paragraph
		setModel: (TextModel withText: contents asText);
		extentForComposing: 9999999@9999999.
	paragraph composeAll.
	self morphExtent: paragraph usedExtent + 8! !


!ImageMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:33'!
extent: aPoint
	"Do nothing; my extent is determined by my image Form."

	self validateNotSent.! !


!InnerPluggableMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:24'!
adjustExtent

	self submorphBounds ifNotNil: [ :r |
		self morphExtent: r bottomRight - bounds topLeft ]! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:24'!
adjustExtent
	"And reposition submorphs"
	| w p0 h y |
	"make all items wide, so selection indicator is wide too"
	w _ self desiredWidth.
	p0 _ bounds topLeft..
	y _ 0.
	self submorphsDo: [ :m |
		h _ m morphHeight.
		m bounds: (p0 + (0@y) extent: w@h).
		y _ y + h ].
	self morphExtent: w@y! !


!InnerListMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:24'!
adjustExtent
	"Adjust our height to match the underlying list,
	but make it wider if neccesary to fill the available width in our PluggableListMorph
	(this is needed to make the selection indicator no narrower than the list)"
	self morphExtent:
		self desiredWidth @ ((listItems size max: 1) * font height)
! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:24'!
adjustExtent
	"This is just a suggestion. If we do wordwrap, the width will be honored.
	But the height is whatever is appropriate for the contents!!"
	self morphExtent: owner viewableBounds extent! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 12/16/2011 10:24'!
extentForComposing
	self flag: #jmvVer2.	"like #extent ..."
	^wrapFlag
		ifTrue: [ bounds width @ 9999999 ]
		ifFalse: [ 9999999@9999999 ]! !


!MagnifierMorph methodsFor: 'initialization' stamp: 'jmv 12/16/2011 10:25'!
initialize
	super initialize.
	trackPointer _ true.
	magnification _ 2.
	lastPos _ self sourcePoint.
	self morphExtent: 128@128! !

!MagnifierMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:28'!
borderWidth: anInteger
	"Grow outwards preserving innerBounds"
	| c |  
	c _ self referencePosition.
	super borderWidth: anInteger.
	super morphExtent: self defaultExtent.
	self referencePosition: c.! !

!MagnifierMorph methodsFor: 'menu' stamp: 'jmv 12/16/2011 10:25'!
chooseMagnification
	| result |
	result _ (SelectionMenu selections: #(1.5 2 4 8))
		startUpWithCaption: 'Choose magnification
(currently ', magnification printString, ')'.
	(result == nil or: [ result = magnification ]) ifTrue: [ ^ self ].
	magnification _ result.
	self morphExtent: bounds extent. "round to new magnification"
	self redrawNeeded. "redraw even if extent wasn't changed"! !

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 12/16/2011 10:28'!
magnification: aNumber
	| c |  
	magnification _ aNumber min: 8 max: 0.5.
	magnification _ magnification roundTo:
		(magnification < 3 ifTrue: [0.5] ifFalse: [1]).
	srcExtent _ srcExtent min: (512@512) // magnification. "to prevent accidents"
	c _ self referencePosition.
	super morphExtent: self defaultExtent.
	self referencePosition: c.! !


!MenuMorph methodsFor: 'private' stamp: 'jmv 12/16/2011 10:25'!
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
				ifFalse: [each morphWidth])].

	w _ w + 4.
	tl _ bounds topLeft.
	p _ tl + 5.
	submorphs do: [ :m |
		m morphWidth: w.
		m morphPosition: p.
		p _ m bounds bottomLeft +(0@1) ].
	
	self morphExtent: submorphs last bounds bottomRight - tl + 5! !


!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 12/16/2011 10:25'!
fitContents

	| newBounds boundsChanged |
	newBounds _ self measureContents.
	boundsChanged _ bounds extent ~= newBounds.
	self morphExtent: newBounds.		"default short-circuits if bounds not changed"
	boundsChanged ifFalse: [ self redrawNeeded ]! !


!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 12/16/2011 10:25'!
fitContents

	| newBounds boundsChanged |
	newBounds _ self measureContents.
	boundsChanged _ bounds extent ~= newBounds.
	self morphExtent: newBounds.		"default short-circuits if bounds not changed"
	boundsChanged ifFalse: [ self redrawNeeded ]! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:32'!
extent: aPoint

	self validateNotSent.
	self flag: #jmvVer2.
	bounds extent = aPoint ifFalse: [
		self redrawNeeded.
		bounds _ bounds topLeft extent: aPoint.
		extent _ aPoint.
		self validatePositionAndBounds.
		self buildMagnifiedBackgroundImage.
		self someSubmorphPositionOrExtentChanged.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = bounds ifFalse: [
				worldState canvas: nil.
				worldState viewBox: bounds ]]]! !


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 12/16/2011 10:25'!
initialize
	"initialize the state of the receiver"
	super initialize.

	roundButtonStyle _ nil.	"nil: honor Theme. true: draw as round button. false: draw as classic 3d border square button"
	model _ nil.
	getStateSelector _ nil.
	actionSelector _ nil.
	isPressed _ false.
	mouseIsOver _ false.
	actWhen _ #buttonUp.
	self morphExtent: 20 @ 15! !


!PolygonMorph methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:32'!
extent: newExtent 
	"Not really advisable, but we can preserve most of the geometry if we don't
	shrink things too small."
	| safeExtent center |
	self validateNotSent.
	center _ self referencePosition.
	safeExtent _ newExtent max: 20@20.
	self setVertices: (vertices collect:
		[:p | p - center * (safeExtent asFloatPoint / (bounds extent max: 1@1)) + center])! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 12/16/2011 10:39'!
initialize
	super initialize.
	self separation: 0.
	labelMorph _ StringMorph contents: '' font: AbstractFont default.
	subLabelMorph _ StringMorph contents: '' font: AbstractFont default.
	progress_ ProgressBarMorph new.
	progress morphExtent: 200 @ 15.
	self addMorph: labelMorph.
	self addMorph: subLabelMorph.
	self addMorph: progress fixedHeight: 15.! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:33'!
expandSlider
	"Compute the new size of the slider (use the old sliderThickness as a minimum)."
	| r |
	r _ self totalSliderArea.
	slider morphExtent: (bounds isWide
		ifTrue: [((r width * interval) asInteger max: 7) @ slider morphHeight]
		ifFalse: [slider morphWidth @ ((r height * interval) asInteger max: 7)])! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 4/10/2012 15:11'!
extent: newExtent
	| newExtentToUse |
	self validateNotSent.
	newExtent = bounds extent ifTrue: [^ self].
	newExtentToUse _ bounds isWide
		ifTrue: [ (newExtent x max: 14) @ newExtent y ]
		ifFalse: [ newExtent x @ (newExtent y max: 14) ].
	newExtentToUse = bounds extent ifTrue: [^ self].
	super extent: newExtentToUse.
		
	self flag: #jmv.
	"Most times it is not necessary to recreate the buttons"
	self recreateSubmorphs! !


!SketchMorph methodsFor: 'initialization' stamp: 'jmv 12/16/2011 10:26'!
initializeWith: aForm

	super initialize.
	originalForm _ aForm.
	self morphExtent: originalForm extent.
! !


!Sonogram methodsFor: 'all' stamp: 'jmv 12/16/2011 10:26'!
extent: extent minVal: min maxVal: max scrollDelta: d
	minVal _ min.
	maxVal _ max.
	scrollDelta _ d.
	self morphExtent: extent.

" try following with scrolldelta = 1, 20, 200
	| s data |
	s _ Sonogram new extent: 200@50
				minVal: 0.0 maxVal: 1.0 scrollDelta: 20.
	World addMorph: s.
	data _ (1 to: 133) collect: [:i | 0.0].
	1 to: 300 do:
		[:i | data at: (i\\133)+1 put: 1.0.
		s plotColumn: data.
		data at: (i\\133)+1 put: 0.0.
		World doOneCycleNow].
	s delete	
"! !

!Sonogram methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:33'!
extent: newExtent
	self validateNotSent.
	super image: (Form extent: newExtent depth: Display depth).
	lastX _ -1.
	columnForm _ Form extent: (32//image depth)@(image height) depth: image depth.
	pixValMap _ ((1 to: 256) collect:
			[ :i | columnForm pixelValueFor: (Color gray: (256-i)/255.0)])
		as: Bitmap.
! !


!StringMorph methodsFor: 'accessing' stamp: 'jmv 12/16/2011 10:26'!
fitContents

	| newBounds boundsChanged |
	newBounds _ self measureContents.
	boundsChanged _ bounds extent ~= newBounds.
	self morphExtent: newBounds.		"default short-circuits if bounds not changed"
	boundsChanged ifFalse: [self redrawNeeded]! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 12/16/2011 10:48'!
makeMeVisible 

	self world morphExtent > (0@0) ifFalse: [^ self].

	(self morphPosition >= (0@0) and: [ self morphPosition < (self world morphExtent-self labelHeight)]) ifTrue: [
		^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self isCollapsed
		ifTrue: [ self morphPosition: (RealEstateAgent assignCollapsePointFor: self)]
		ifFalse: [ self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: bounds extent world: self world) topLeft].

! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 12/16/2011 10:26'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	isCollapsed _ false.
	
	self wantsLabel ifTrue: [self initializeLabelArea].
	self morphExtent: 300 @ 200.
	updatablePanes _ #().

	adjusters _ Dictionary new.
	adjusters at: #topAdjuster put: WindowEdgeAdjustingMorph forTop.
	adjusters at: #bottomAdjuster put: WindowEdgeAdjustingMorph forBottom.
	adjusters at: #leftAdjuster put: WindowEdgeAdjustingMorph forLeft.
	adjusters at: #rightAdjuster put: WindowEdgeAdjustingMorph forRight.
	adjusters at: #topLeftAdjuster put: WindowEdgeAdjustingMorph forTopLeft.
	adjusters at: #bottomLeftAdjuster put: WindowEdgeAdjustingMorph forBottomLeft.
	adjusters at: #topRightAdjuster put: WindowEdgeAdjustingMorph forTopRight.
	adjusters at: #bottomRightAdjuster put: WindowEdgeAdjustingMorph forBottomRight.
	adjusters do: [ :m |
		self addMorph: m ].

	"by default"
	self beColumn! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 12/16/2011 10:35'!
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

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 12/16/2011 10:41'!
openInWorld: aWorld extent: extent
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self morphPosition: (RealEstateAgent initialFrameFor: self world: aWorld) topLeft; morphExtent: extent.
	self activate.
	aWorld startSteppingSubmorphsOf: self.! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 12/16/2011 10:26'!
buildMorphicWindow
	"TestRunner new openAsMorph"

	self layoutMorph
		addMorph: self buildUpperControls proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.75.
	self setLabel: 'SUnit Test Runner'.
	self refreshWindow.
	self morphExtent: 460 @ 400! !

!methodRemoval: FillInTheBlankMorph #morphExtent:!
FillInTheBlankMorph removeSelector: #morphExtent:!
