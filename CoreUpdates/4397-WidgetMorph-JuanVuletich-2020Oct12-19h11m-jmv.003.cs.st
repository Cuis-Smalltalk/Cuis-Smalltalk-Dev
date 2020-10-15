'From Cuis 5.0 [latest update: #4392] on 12 October 2020 at 8:38:23 pm'!
SystemOrganization removeSystemCategory: 'Tools-GUI'.!
SystemOrganization renameCategory: 'Morphic-Widgets' toBe: 'Morphic-Composite Widgets'.!
SystemOrganization renameCategory: 'Morphic-Views' toBe: 'Morphic-Widgets'.!
SystemOrganization renameCategory: 'Morphic-Tools' toBe: 'Morphic-Tool Windows'.!
!classDefinition: #WidgetMorph category: #'Morphic-Widgets'!
Morph subclass: #WidgetMorph
	instanceVariableNames: 'extent color borderWidth borderColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!WidgetMorph commentStamp: '<historical>' prior: 0!
Hierarchy for morphs that are rectangle like, and that occupy an area that can be specified as a Rectangle. This means that, besides being of rectangular shape, sides are aligned with local coordinate axes. Including rectangles with rounded corners and such. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape. Subclasses can add things like 'roundedCornerRadious' or such.!


!WidgetMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 16:12'!
borderColor
	^ borderColor! !

!WidgetMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 16:12'!
borderColor: aColor
	borderColor = aColor ifFalse: [
		borderColor _ aColor.
		self redrawNeeded]! !

!WidgetMorph methodsFor: 'accessing' stamp: 'gsa 12/28/2013 15:25'!
borderWidth
	^ borderWidth! !

!WidgetMorph methodsFor: 'accessing' stamp: 'jmv 7/30/2014 09:24'!
borderWidth: anInteger
	borderWidth = anInteger ifFalse: [
		borderWidth _ anInteger max: 0.
		self redrawNeeded ]! !

!WidgetMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 21:54'!
color

	^ color! !

!WidgetMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 21:59'!
color: aColor
	"Set the receiver's color. "
	color = aColor ifFalse: [
		color _ aColor.
		self redrawNeeded ]! !

!WidgetMorph methodsFor: 'initialization' stamp: 'jmv 3/10/2018 21:45:32'!
defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ `Color gray`! !

!WidgetMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 16:12'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 2! !

!WidgetMorph methodsFor: 'initialization' stamp: 'jmv 3/10/2018 21:56:39'!
defaultColor
	^ `Color orange`! !

!WidgetMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 20:38:00'!
initialize
	super initialize.
	extent _ `50@40`.
	color _ self defaultColor.
	borderColor _ self defaultBorderColor.
	borderWidth _ self defaultBorderWidth.! !

!WidgetMorph methodsFor: 'drawing' stamp: 'jmv 12/20/2014 14:58'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	"If you redefine this method in a subclass, please take a look at the comment at #isOpaqueMorph"
	aCanvas
		fillRectangle: self morphLocalBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 7/24/2020 10:22:57'!
morphBounds: aRectangle
	self morphPosition: aRectangle topLeft.
	self morphExtent: aRectangle extent! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/22/2020 16:35:34'!
morphExtent
	"In our own coordinates!!"

	^ extent! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 15:52:14'!
morphExtent: newExtent
	"assume it is always in our coordinates!!"

	extent = newExtent ifFalse: [
		(self privateExtent: newExtent) ifTrue: [
			self someSubmorphPositionOrExtentChanged.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 7/8/2014 11:41'!
morphExtentInWorld: newExtent
	"world coordinates"
	self flag: #jmvVer2.
	self morphExtent: (self internalizeDistanceFromWorld: newExtent)! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:56'!
morphHeight

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	^ extent y! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/22/2020 16:36:56'!
morphHeight: aNumber

	self morphExtent: extent x@aNumber! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 15:53:07'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| someChange |
	someChange _ false.
	(location isTranslation: newPos) ifFalse: [
		location _ location withTranslation: newPos.
		someChange _ true ].

	extent = newExtent ifFalse: [
		(self privateExtent: newExtent) ifTrue: [
			someChange _ true ]].

	someChange ifTrue: [
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2020 11:19:35'!
morphTopLeft
	"Our hierarchy occupies a rectangle specified by #morphTopLef and #morphExtent"
	^`0@0`! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:57'!
morphWidth

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	^ extent x! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/22/2020 16:37:29'!
morphWidth: aNumber

	self morphExtent: aNumber@extent y! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2014 09:59'!
privateExtent: aPoint
	"Answer whether extent was actually changed.
	If some subclass may reject the update, answer false in those cases."

	| newExtent |
	newExtent _ aPoint max: self minimumExtent.
	^extent = newExtent
		ifFalse: [ extent _ newExtent ]; not! !

!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:35:50'!
isOrthoRectangularMorph
	"True if we are of rectangular shape, aligned with local coordinate axes.
	Doesn't take into account possible rotation of our or some owner's local coordinates.
	Note: SystemWindow answers true, but implements #addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^true! !

!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 7/28/2020 12:16:53'!
morphContainsPoint: aLocalPoint
	"Answer true even if aLocalPoint is in a submorph, as long as it is in our shape"

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ self morphLocalBounds containsPoint: aLocalPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	"Can't do better. Please redefine in subclasses as appropriate!! (or finish Morphic 3!!)"
	"Mhhh. Podria usar el #imageForm: y ver si es transparente... deberia andar"
	^ self morphLocalBounds containsPoint: aLocalPoint! !

!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:35:08'!
requiresVectorCanvas
	"True if we use VectorCanvas protocol, or we are rotated or zoomed.
	If false, we occupy a Rectangle in local coordinates. Allows for many optimizations to be done.
	Note: If answer is false, but some owner requiresVectorCanvas, then we also do. Canvas takes care of this.
	Note: SystemWindow answers false, but implements #addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^location isPureTranslation not! !

!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:38:44'!
submorphsMightProtrude
	"Answered false by morphs that can guarantee that submorphs, and all morphs in their submorphs tree, will lie inside their bounds,
	either 'naturally' (by construction) or by being clipped by their owner (#clipsLastSubmorph). Allows for many optimizations to be done."

	^false! !

!WidgetMorph methodsFor: 'layout-properties' stamp: 'jmv 2/16/2016 13:05'!
layoutSpec: aLayoutSpec
	"Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout"
	self layoutSpec == aLayoutSpec ifTrue: [ ^self ].
	aLayoutSpec morph: self.
	layoutSpec := aLayoutSpec.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!WidgetMorph methodsFor: 'testing' stamp: 'jmv 3/17/2013 22:54'!
is: aSymbol
	^ aSymbol == #WidgetMorph or: [ super is: aSymbol ]! !


!WidgetMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:47:05'!
categoryInNewMorphMenu
	^ 'Kernel'! !

