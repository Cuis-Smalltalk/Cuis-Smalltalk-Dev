'From Cuis 5.0 [latest update: #4288] on 23 July 2020 at 4:05:24 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 7/23/2020 16:02:54'!
visible: aBoolean
	"set the 'visible' attribute of the receiver to aBoolean"

	self visible == aBoolean
		ifTrue: [ ^ self ].
	self setProperty: #visible toValue: aBoolean.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 15:59:54'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."
	(location isTranslation: aPoint) ifTrue: [ "Null change"
		^ self ].
	location _ location withTranslation: aPoint.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 15:58:19'!
morphPositionInWorld: newPositionInWorld
	"Change the position of this morph."
	"El tema es, que tipo de coordenadas tenemos?
	En un mundo relativista, no hay un marco de referencia absoluto.
	No tiene sentido hablar de coordenadas del mundo... El mundo podria estar escalado... 
		Que tienen de especial las coordenadas del mundo?
	Coordenadas 'del hardware'? No deberia saber mucho sobre el... Puede haber multiples displays, hands de diverso tipo, remotas, virtuales...
	
	En ppio, un par de coordenadas pueden ser relativas a cualquier morph. Pareciera que necesito metodos de conversion de cualquier morph hacia mi, y de mi hacia cualquier morph... Como encontrar un marco de referencia comun????
	Dejar esto para despues. En realidad, para empezar, preciso menos: Solo preciso saber si las coordenadas estan en el morph o en su owner. Nada mas. Los eventos se iran transformando apropiadamente al moverse por el arbol, o al menos, llevaran consigo una transformacion (AffineTransformation) que se ira actualizando"

	| newPositionInOwner |
	self flag: #jmvVer2.
	"This method MUST die"

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].

	(location isTranslation: newPositionInOwner) ifTrue: [
		^ self ].		"Null change".

	location _ location withTranslation: newPositionInOwner.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 16:01:24'!
rotateBy: radians
	"Change the scale of this morph. Argument is an angle."
	location _ location rotatedBy: radians.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 16:01:33'!
rotation: radians scale: scale
	"Change the scale of this morph. Arguments are an angle and a scale."
	location _ location withRotation: radians scale: scale.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 16:01:42'!
scaleBy: scaleFactor
	"Change the scale of this morph. Argument is a factor."
	location _ location scaledBy: scaleFactor.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/23/2020 16:00:57'!
removeAllMorphs
	| oldMorphs |
	submorphs isEmpty ifTrue: [ ^self ].
	submorphs do: [ :m |
		m privateOwner: nil ].
	oldMorphs _ submorphs.
	submorphs _ #().
	oldMorphs do: [ :m |
		self removedMorph: m ].
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded.! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/23/2020 16:01:05'!
removeAllMorphsIn: aCollection
	"greatly speeds up the removal of *lots* of submorphs"
	| set |
	aCollection isEmpty ifTrue: [ ^self ].
	set _ IdentitySet new: aCollection size * 4 // 3.
	aCollection do: [ :each | each owner == self ifTrue: [ set add: each ]].
	set isEmpty ifTrue: [ ^self ].
	set do: [ :m | m privateOwner: nil ].
	submorphs _ submorphs reject: [ :each | set includes: each].
	set do: [ :m | self removedMorph: m ].
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded.! !

!Morph methodsFor: 'updating' stamp: 'jmv 7/23/2020 16:00:42'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"these properties... better store flags in 'id' "
	self setProperty: #needsRedraw toValue: true.
	self allOwnersDo: [ :m | m setProperty: #submorphNeedsRedraw toValue: true ].
	self displayFullBounds
		ifNil: [
			"Dummy, so #updateIsNeeded answers true if some morph dirty"
			"Check if this is really needed. If so, find a cleaner way."
			self invalidateDisplayRect: (10@10 extent: 2@2) from: nil ]
		ifNotNil: [ :r |
			self invalidateDisplayRect: r from: nil ].

	self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 15:52:14'!
morphExtent: newExtent
	"assume it is always in our coordinates!!"

	extent = newExtent ifFalse: [
		(self privateExtent: newExtent) ifTrue: [
			self someSubmorphPositionOrExtentChanged.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 15:53:07'!
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


!ImageMorph methodsFor: 'accessing' stamp: 'jmv 7/23/2020 15:59:02'!
image: anImage 
	| newExtent |
	image _ anImage depth = 1 
		ifTrue: [ ColorForm mappingWhiteToTransparentFrom: anImage ]
		ifFalse: [ anImage ]. 
	newExtent _ image extent.
	extent = newExtent ifFalse: [
		extent _ newExtent.
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]].
	self redrawNeeded.! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 7/23/2020 15:57:32'!
model: aTextModel wrappedTo: width
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	| newExtent |
	wrapFlag _ true.
	newExtent _ width truncated@extent y.
	extent = newExtent ifFalse: [
		extent _ newExtent.
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].
	self model: aTextModel! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 7/23/2020 15:57:00'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth |
	newWidth _ extent x.
	"Adjust width only if we don't wrap text to own width!!"
	wrapFlag ifFalse: [
		newWidth _ self textComposition usedWidth max: 9 ].
	newHeight _ self textComposition usedHeight max: FontFamily defaultLineSpacing + 2.
	newExtent _ newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				extent _ newExtent.
				self someSubmorphPositionOrExtentChanged.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].

	owner innerHeight: newExtent y! !

!methodRemoval: MorphicCanvas #displayBoundsInWorldOf: stamp: 'jmv 7/23/2020 15:54:46'!
MorphicCanvas removeSelector: #displayBoundsInWorldOf:!
!methodRemoval: Morph #morphBoundsInWorld stamp: 'jmv 7/23/2020 15:54:34'!
Morph removeSelector: #morphBoundsInWorld!
