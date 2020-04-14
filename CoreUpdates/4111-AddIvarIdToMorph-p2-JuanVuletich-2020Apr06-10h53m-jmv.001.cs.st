'From Cuis 5.0 [latest update: #4106] on 6 April 2020 at 10:55:58 am'!
!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'owner submorphs location layoutNeeded layoutSpec properties id '
	classVariableNames: 'LastMorphId '
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Morph methodsFor: 'accessing' stamp: 'jmv 4/6/2020 10:48:47'!
morphId
	"Non zero. Zero id means no Morph."
	id isNil ifTrue: [
		LastMorphId isNil ifTrue: [ LastMorphId _ 0 ].
		LastMorphId _ LastMorphId + 1.
		id _ LastMorphId ].
	^id! !


!Morph class methodsFor: 'initialize-release' stamp: 'jmv 4/6/2020 10:48:10'!
releaseClassCachedState

	LastMorphId _ nil.
	self allSubInstancesDo: [ :each |
		each releaseCachedState ]! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 4/4/2020 19:32:35'!
clippingMorph: aMorph! !


!Morph methodsFor: 'caching' stamp: 'jmv 4/6/2020 10:45:56'!
releaseCachedState
	"Release any state that can be recomputed on demand, such as the pixel values for a color gradient or the editor state for a TextMorph. This method may be called to save space when a morph becomes inaccessible. Implementations of this method should do 'super releaseCachedState'."
	id _ nil.! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 4/4/2020 19:32:57'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	| oldClipRect |
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [ Preferences cheapWindowReframe ]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].

	"Draw current Morph"
	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self ].

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')"
	currentMorph hasSubmorphs ifTrue: [
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self clippingMorph: currentMorph.
			oldClipRect _ self clipRect.
			self setClipRect: (oldClipRect intersect: self clippingRectForCurrentMorph).
			self fullDraw: clipped.
			self setClipRect: oldClipRect.
			self clippingMorph: nil ].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullDraw: m ].
		].
	self outOfMorph! !

!methodRemoval: MorphicCanvas #currentOwnerIfClips: stamp: 'jmv 4/6/2020 10:54:47'!
MorphicCanvas removeSelector: #currentOwnerIfClips:!
!methodRemoval: PasteUpMorph class #releaseClassCachedState stamp: 'jmv 4/6/2020 10:54:40'!
PasteUpMorph class removeSelector: #releaseClassCachedState!
!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'owner submorphs location layoutNeeded layoutSpec properties id'
	classVariableNames: 'LastMorphId'
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
