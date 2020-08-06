'From Cuis 5.0 [latest update: #4335] on 5 August 2020 at 12:10:32 am'!

!Morph methodsFor: 'drawing' stamp: 'jmv 8/4/2020 11:10:14'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self.
	All areas that might possibly be uncovered must be included."
	" CREO QUE NO, que me conformo con optimizar ventanas. O quizas RectangleLike. Ver.""
	(self isOrthoRectangularMorph and: [ self isOpaqueMorph ]) ifTrue: [
		aRectangle areasOutside: self morphBoundsInWorld do: [ :r |  aCollection add: r ].
		^self ].
	"
	aCollection add: aRectangle! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/4/2020 11:09:53'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included."
	 | bounds radious |
	color mightBeTranslucent ifTrue: [
		aCollection add: aRectangle.
		^self ].

	bounds _ self displayBounds.
	bounds ifNil: [
		aCollection add: aRectangle.
		^self ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!!"
	Theme current roundWindowCorners ifFalse: [
		aRectangle areasOutside: bounds do: [ :rect |  aCollection add: rect ].
		^self ].

	"The solid rectangle does not include the corners.
	Report a couple of rows (top and bottom) or columns (left and right) as uncovered areas.
	We could also try to be more careful and answer each rounded corner...
	Right now, report top and bottom rows as uncovered areas"
	radious _ Theme current roundedWindowRadius.
	aRectangle areasOutside: (bounds insetBy: 0@radious) do: [ :rect |  aCollection add: rect ]! !


!DamageRecorder methodsFor: 'recording' stamp: 'jmv 8/4/2020 22:07:50'!
damageReportedNotVisibleAndCarriedByHand
	| answer |
	answer _ OrderedCollection new.
	self pvtAccessProtect critical: [
		damageByRoot keysAndValuesDo: [ :m :r |
			(m visible not or: [m owner isNil or: [m owner is: #HandMorph]]) ifTrue: [
				answer add: r]]].
	^answer! !

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 8/4/2020 19:33:01'!
reset
	self pvtAccessProtect critical: [
		self pvtReset ].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/4/2020 21:40:28'!
updateHandsDisplayBounds: aPasteUpMorph
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aPasteUpMorph handsDo: [ :hand |
		hand hasSubmorphs ifTrue: [
			self into: hand.
			hand submorphsDo: [ :morph |
				self updateDisplayBounds: morph ].
			self outOfMorph ]]! !


!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 8/4/2020 21:44:28'!
flash: aRectangle 
	"Flash the area of the screen defined by the given rectangle."

	self reverse: aRectangle.
	self forceToScreen: aRectangle.
	(Delay forMilliseconds: 100) wait.
	self reverse: aRectangle.
	self forceToScreen: aRectangle.
	(Delay forMilliseconds: 100) wait.! !


!TranscriptWindow class methodsFor: 'GUI building' stamp: 'jmv 8/4/2020 20:45:45'!
openTranscript
	"
	TranscriptWindow openTranscript
	"
	| win m |
	win _ TranscriptWindow new.
	win
		setLabel: 'Transcript';
		model: Transcript.
	m _ TranscriptMorph new.
	win layoutMorph addMorph: m proportionalHeight: 1.
	win model when: #redraw send: #redrawNeeded to: m.
	^ win openInWorld.
! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/4/2020 22:07:58'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rootMorphs rootMorphsDamage allDamage |
	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	self updatingMorphBoundsDo: [
		aPasteUpMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
		self updateHandsDisplayBounds: aPasteUpMorph ].

	rootMorphs _ aPasteUpMorph privateSubmorphs.
	rootMorphsDamage _ Array new: rootMorphs size.
	allDamage _ aDamageRecorder damageReportedNotVisibleAndCarriedByHand.

	"Iterate from front to back"
	1 to: rootMorphs size do: [ :i | | morph morphBounds morphDamage |
		morph _ rootMorphs at: i.
		morph visible ifTrue: [
			morphBounds _ morph displayFullBounds.
			morphDamage _ OrderedCollection new.
			(aDamageRecorder damageReportedFor: morph) ifNotNil: [ :r |
				morphDamage add: r ].
			allDamage do: [ :r | | ri |
				ri _ r intersect: morphBounds.
				ri hasPositiveExtent ifTrue: [
					morphDamage add: ri ]].
			1 to: i-1 do: [ :j | | morphBefore oldMorphDamage |
				oldMorphDamage _ morphDamage.
				morphDamage _ OrderedCollection new.
				morphBefore _ rootMorphs at: j.
				oldMorphDamage do: [ :r |
					morphBefore addPossiblyUncoveredAreasIn: r to: morphDamage ]].
			(Rectangle merging: morphDamage) ifNotNil: [ :morphDamageRect |
				rootMorphsDamage at: i put: morphDamageRect.
				morph addPossiblyUncoveredAreasIn: morphDamageRect to: allDamage ]]].

	"Draw World"
	allDamage do: [ :r |
		aPasteUpMorph drawOn: (self newClipRect: r) ].

	"Iterate from back to front."
	rootMorphs size to: 1 by: -1 do: [ :i | | morph morphBounds morphDamage |
		morph _ rootMorphs at: i.
		morphBounds _ morph displayFullBounds.
		morphDamage _ rootMorphsDamage at: i.
		allDamage do: [ :r | | intersection |
			intersection _ r intersect: morphBounds.
			intersection hasPositiveExtent ifTrue: [
				morphDamage _ intersection quickMerge: morphDamage ]].
		morphDamage ifNotNil: [
			self newClipRect: morphDamage.
			self fullDraw: morph.
			allDamage add: morphDamage ]].

	aDamageRecorder reset.
	^ allDamage! !

!methodRemoval: MorphicCanvas #updateHandsDisplayBounds:addDamageTo: stamp: 'jmv 8/5/2020 00:09:47'!
MorphicCanvas removeSelector: #updateHandsDisplayBounds:addDamageTo:!
!methodRemoval: DamageRecorder #damageReportedNotVisibleMorphs stamp: 'jmv 8/5/2020 00:09:47'!
DamageRecorder removeSelector: #damageReportedNotVisibleMorphs!
