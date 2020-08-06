'From Cuis 5.0 [latest update: #4337] on 5 August 2020 at 12:40:43 am'!
!classDefinition: #DamageRecorder category: #'Morphic-Support'!
Object subclass: #DamageRecorder
	instanceVariableNames: 'invalidRects totalRepaint drSemaphore damageByRoot '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!WorldState methodsFor: 'canvas' stamp: 'jmv 8/5/2020 00:37:39'!
clearCanvas
	canvas _ nil.
	damageRecorder _ DamageRecorder new.! !

!WorldState methodsFor: 'canvas' stamp: 'jmv 8/5/2020 00:37:47'!
setCanvas: aMorphicCanvas
	canvas _ aMorphicCanvas.
	canvas world: world.
	damageRecorder
		ifNil: [ damageRecorder _ DamageRecorder new].! !


!DamageRecorder methodsFor: 'initialization' stamp: 'jmv 8/5/2020 00:37:05'!
initialize
	super initialize.
	damageByRoot _ IdentityDictionary new.! !

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 8/5/2020 00:39:27'!
reset
	"Clear the damage list."
	self pvtAccessProtect critical: [
		damageByRoot removeAll ].! !

!DamageRecorder methodsFor: 'testing' stamp: 'jmv 8/5/2020 00:36:52'!
updateIsNeeded
	"Return true if the display needs to be updated."
	^ self pvtAccessProtect critical: [damageByRoot notEmpty]! !

!DamageRecorder methodsFor: 'private' stamp: 'jmv 8/5/2020 00:36:33'!
pvtInnerRecordInvalidRect: requestedRect for: aRootMorph
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle."
	| newRect |
	newRect _ requestedRect encompassingIntegerRectangle.
	damageByRoot at: aRootMorph put: 
		(damageByRoot at: aRootMorph
			ifPresent: [:r | r quickMerge: newRect]
			ifAbsent:[newRect])! !

!methodRemoval: DamageRecorder #pvtReset stamp: 'jmv 8/5/2020 00:39:28'!
DamageRecorder removeSelector: #pvtReset!
!methodRemoval: DamageRecorder #doFullRepaint stamp: 'jmv 8/5/2020 00:38:46'!
DamageRecorder removeSelector: #doFullRepaint!
!methodRemoval: DamageRecorder #invalidRectsFullBounds: stamp: 'jmv 8/5/2020 00:32:36'!
DamageRecorder removeSelector: #invalidRectsFullBounds:!
!methodRemoval: WorldState #doFullRepaint stamp: 'jmv 8/5/2020 00:38:47'!
WorldState removeSelector: #doFullRepaint!
!methodRemoval: PasteUpMorph #redrawNeeded stamp: 'jmv 8/5/2020 00:38:41'!
PasteUpMorph removeSelector: #redrawNeeded!
!classDefinition: #DamageRecorder category: #'Morphic-Support'!
Object subclass: #DamageRecorder
	instanceVariableNames: 'drSemaphore damageByRoot'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
