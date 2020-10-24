'From Cuis 5.0 [latest update: #4417] on 21 October 2020 at 5:50:56 pm'!

!Morph commentStamp: 'jmv 10/21/2020 17:46:54' prior: 0!
A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://wiki.squeak.org/squeak/morph. 

Morphs exist in a tree, rooted at a World (a WorldMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets 
drawn. To hide a Morph and its submorphs, send the #visible: message. 

Every morph has a local coordinate system to interpret positions.
Local coordinates are used in the #drawOn: method (the Canvas understands positions in the local coordinate system), for the positions of submorphs (for example #morphPosition and #morphPosition:) and for positions carried by mouse events.

Events are delivered to morphs in Z-order, i.e. if a morph occludes another the event is only delivered to the foremost (just like physical objects). Events received by a morph carry positions in the local coordinate system.

Morphs can be translated by an offset, rotated around their center, orbited (rotated around the owner center), and zoomed (i.e. scaled).

Every morph has an associated transformation that defines the inner space where the morph is drawn and where the submorphs live. These transformations don't change anything from the internal point of view of the morph.

Structure:
instance var 				Type 						Description 
owner 				Morph or nil		 				My parent Morph, or nil for the top-level Morph, which is a WorldMorph
submorphs 				Array 						My child Morphs. 
location 				GeometryTransformation 						Specifies position (and possibly, angle of rotation and scale change) inside owner
												See comment at GeometryTransformation!


!PasteUpMorph commentStamp: '<historical>' prior: 0!
My instances are free areas where you can play with Morphs. Most important, the World is an instance of my subclass WorldMorph.!


!WorldMorph commentStamp: '<historical>' prior: 0!
A WorldMorph is a kind of PasteUpMorph that can act as the root morph of a Morphic system. Usually uses the whole Display.

activeHand 					-- a HandMorph
hands 					-- Array of HandMorphs (typically only one) representing the Cursor and its event queue.
canvas 					-- BitBlitCanvas on the DisplayScreen. (Display getCanvas)
damageRecorder 					-- the DamageRecorder for the Display
stepList 					-- a Heap of StepMessage.  Each morph which wants stepping has a StepMessage here.
								See comment in #Morph>>stepAt:
lastStepTime 					-- 'now' as last sampled at step. (Time localMillisecondClock)
lastStepMessage 					-- nil or a selector
lastCycleTime 					-- 'now' as last sampled (Time localMillisecondClock)
alarms 					-- a Heap of MorphicAlarm.  See comment in #Morph>>addAlarm:after:
								An _alarm_ is an action to be done once, vs a _step_ which is cycled.
lastAlarm 					-- 'now' as sampled at last alarm (Time localMillisecondClock).
drawingFailureMorphs 					-- a WeakIdentitySet of Morphs with drawing failures
waitDelay 					-- a Delay
pause 					-- A 'phase-locked loop' style value to help regularise the step/alarm/event service rate.
lastCycleHadAnyEvent 					-- a boolean
!


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 10/21/2020 17:40:03'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	WorldMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoMenuIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	BitBltCanvas releaseClassCachedState.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally DynamicTypingSmalltalkCompleter).
	AppLauncher appGlobalName ifNotNil: [ :any |
		keep add: any ].
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !


!Morph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 17:47:25'!
fontPreferenceChanged
	"Preferred fonts scale a number of window relations.
	Let morphs which rely on this updte themselves.
	
	Note that the fontPreferenceChanged message is typically
	sent to the current world.  As WorldMorph inherits from me
	the code below works fine for this."
	
	"I  do nothing myself but my submorphs may."
	
	self submorphsDo: [ :m | m fontPreferenceChanged. ]! !

!Morph methodsFor: 'testing' stamp: 'jmv 10/21/2020 17:38:51'!
isOwnedByWorld
	^owner isWorldMorph! !


!MorphicCanvas methodsFor: 'initialization' stamp: 'jmv 10/21/2020 17:49:39'!
world: aWorldMorph
	world _ aWorldMorph.
	self into: world! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/21/2020 17:48:51'!
computeDamage: aWorldMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
	"Iterate front to back while computing damage to actually repair for each morph, and for world background.
	Answer uncoveredDamage, the areas where World background needs to be redrawn."

	| uncoveredDamage morph morphDamage prevMorphDamage reuseInstance morphBefore |
	morphDamage _ OrderedCollection new.
	prevMorphDamage _ OrderedCollection new.
	uncoveredDamage _ aDamageRecorder damageReportedOther.
	"Iterate from front to back"
	1 to: rootMorphs size do: [ :i |
		morph _ rootMorphs at: i.
		morphDamage removeAll.
		(aDamageRecorder damageReportedFor: morph) ifNotNil: [ :r |
			morphDamage add: r ].
		1 to: i-1 do: [ :j |
			reuseInstance _ prevMorphDamage.
			prevMorphDamage _ morphDamage.
			morphDamage _ reuseInstance removeAll.
			morphBefore _ rootMorphs at: j.
			prevMorphDamage do: [ :r |
				morphBefore addPossiblyUncoveredAreasIn: r to: morphDamage ]].
		(Rectangle merging: morphDamage) ifNotNil: [ :morphDamageRect |
			rootMorphsDamage at: i put: morphDamageRect.
			morph addPossiblyUncoveredAreasIn: morphDamageRect to: uncoveredDamage ]].

	^ uncoveredDamage! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/21/2020 17:48:42'!
drawWorld: aWorldMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rootMorphs rootMorphsDamage worldDamage |
	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	self updatingMorphBoundsDo: [
		aWorldMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
		self updateHandsDisplayBounds: aWorldMorph ].

	rootMorphs _ aWorldMorph privateSubmorphs.
	rootMorphsDamage _ Array new: rootMorphs size.
	
	worldDamage _ self computeDamage: aWorldMorph repair: aDamageRecorder
		rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage.

	self drawWorldBackground: aWorldMorph rects: worldDamage.

	self drawWorld: aWorldMorph
		rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
		allDamage: worldDamage.

	aDamageRecorder reset.
	^ worldDamage! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/21/2020 17:49:10'!
drawWorld: aWorldMorph rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage allDamage: worldDamage
	"Redraw the damaged areas.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| morph morphDamage |

	"Iterate from back to front."
	rootMorphs size to: 1 by: -1 do: [ :i |
		morph _ rootMorphs at: i.
		morph visible ifTrue: [
			morphDamage _ rootMorphsDamage at: i.
			morph displayFullBounds ifNotNil: [ :morphFullBounds |
				worldDamage do: [ :r | | intersection |
					intersection _ r intersect: morphFullBounds.
					intersection hasPositiveExtent ifTrue: [
						morphDamage _ intersection quickMerge: morphDamage ]]].
			morphDamage ifNotNil: [
				self newClipRect: morphDamage.
				self fullDraw: morph.
				worldDamage add: morphDamage ]]].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/21/2020 17:49:27'!
drawWorldBackground: aWorldMorph rects: worldBackgroundDamage
	"Draw worldBackgroundDamage rects for aWorldMorph.
	Do not include submorphs."

	worldBackgroundDamage do: [ :r |
		aWorldMorph drawOn: (self newClipRect: r) ].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/21/2020 17:49:33'!
updateHandsDisplayBounds: aWorldMorph
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aWorldMorph handsDo: [ :hand |
		hand hasSubmorphs ifTrue: [
			self into: hand.
			hand submorphsDo: [ :morph |
				self updateDisplayBounds: morph ].
			self outOfMorph ]]! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 10/21/2020 17:39:15'!
activeSubclass: aMorphicCanvasSubclass
	ActiveSubclass _ aMorphicCanvasSubclass.
	WorldMorph allInstancesDo: [ :w | w setCanvas ]! !

!methodRemoval: PasteUpMorph #is: stamp: 'jmv 10/21/2020 17:50:43'!
PasteUpMorph removeSelector: #is:!
!classRemoval: #OldPasteUpMorph stamp: 'jmv 10/21/2020 17:41:05'!
Smalltalk removeClassNamed: #OldPasteUpMorph!

!classRemoval: #WorldState stamp: 'jmv 10/21/2020 17:41:11'!
Smalltalk removeClassNamed: #WorldState!

