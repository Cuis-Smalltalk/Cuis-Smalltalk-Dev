'From Cuis 4.2 of 25 July 2013 [latest update: #2044] on 18 July 2014 at 3:03:45.759581 pm'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 7/18/2014 14:45'!
invalidateDisplayRect: damageRect from: aMorph

	| clippedRect |

	"warning. Senders are using global coordinates. Redesign!!"
	"local now!!!!!!!!!!"
	self flag: #jmvVer2.	"ok?"

	self visible ifFalse: [ ^self].

	clippedRect _ damageRect.
	aMorph ifNotNil: [
	 	aMorph == self clippedSubmorph
			ifTrue: [ clippedRect _ damageRect intersect: self morphBoundsInWorld ]].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect from: self ]! !

!Morph methodsFor: 'change reporting' stamp: 'jmv 7/18/2014 14:52'!
invalidateLocalRect: localRectangle
"los senders que quieran el rect completo, llamar a #morphBoundsInWorld
Los que quieran un pedacito, traducirlo hacia arriba (el canvas no me ayuda) y no tengo la currentTransformation. hacerlo paso a paso"

	self invalidateDisplayRect: (self externalizeDisplayBounds: localRectangle) from: nil! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/18/2014 14:59'!
externalizeDisplayBounds: r

	| inOwners |
	self flag: #jmvVer2.

	inOwners _ location displayBoundsOfTransformOf: r.
	^owner
		ifNotNil: [ owner externalizeDisplayBounds: inOwners ]
		ifNil: [ inOwners ]! !


!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 7/18/2014 14:42'!
invalidateDisplayRect: damageRect from: aMorph
        "Clip damage reports to my bounds, since drawing is clipped to my bounds."

        self == self world 
                ifTrue: [ worldState recordDamagedRect: (damageRect intersect: ( 0@0 extent: extent) ) ]
                ifFalse: [ super invalidateDisplayRect: damageRect from: aMorph ]
! !

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 7/14/2014 11:35'!
externalizeDisplayBounds: r

	^self isWorldMorph
		ifTrue: [ r ]
		ifFalse: [ super externalizeDisplayBounds: r ]! !


!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/18/2014 14:21'!
clippingRectForCurrentMorph
"En M3, es el obtenido al dibujarlo, recien.
Dejar eso claro en el nombre. Eliminar 'clipping'"
	^self displayBoundsOf: currentMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/18/2014 14:14'!
displayBoundsOf: aMorph
	"Answer a rectangle that completely bounds aMorph when drawn on our form.
	Might be larger than strictly required.
		- In Morphic 2, this could cause clipping artifacts. This doesn't usually happen because:
			a) Morphic 2 doesn't use scaling and rotation
			b) Most Morphic 2 morphs have rectangular shape.
		- In Morphic 3, clipping also considers the real shape of the owner morph. This avoids those artifacts."

	"Think about doing a BoundsFinderCanvas even for Morphic 2"
	self flag: #jmvVer2.

	^self externalizeDisplayBounds: (0@0 extent: aMorph morphExtent) from: aMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/18/2014 14:01'!
displayFullBoundsOf: aMorph
	"Answer a rectangle that completely bounds aMorph and submorphs when drawn on our form.
	Might be larger than strictly required. See comment at #displayBoundsOf:"

	| r |
	"Think about doing a BoundsFinderCanvas even for Morphic 2"

	self flag: #jmvVer2.

	r _ self displayBoundsOf: aMorph.
	aMorph unclippedSubmorphsReverseDo: [ :m |
		m visible ifTrue: [
			r _ r merge: (self displayFullBoundsOf: m) ]].

	^r! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/18/2014 15:02'!
morphBoundsInWorld
	"Return the bounds of this morph."
	
	"WORLD absolute bounds :("

	self flag: #jmvVer2.
	"Do a BoundsFinderCanvas
	This is a workaound for the mean time, see comment at #displayBoundsOf:"
	^FormCanvas new displayBoundsOf: self! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/18/2014 13:59'!
morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."
	"Warning:
		Not #visible morphs will not be considered in recursion, but will answer if asked directly"

	self flag: #jmvVer2.
	"Do a BoundsFinderCanvas
	This is a workaound for the mean time, and only for M2"
	^FormCanvas new displayFullBoundsOf: self! !

!Morph methodsFor: 'updating' stamp: 'jmv 7/18/2014 14:44'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Include submorphs if we don't clip!!
	Think about it. We don't to know about a specific display rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"

	self invalidateDisplayRect: self morphBoundsInWorld from: nil.
	"caro al pedo en casi todoso los casos..."
	self unclippedSubmorphsReverseDo: [ :m | m redrawNeeded ]! !


!SystemWindow methodsFor: 'change reporting' stamp: 'jmv 7/18/2014 14:53'!
invalidateTitleArea

	"not really pretty... also invalidating the top border, regardless of it being above or below the title area
	(Different themes use various looks, this covers them all)"
	self invalidateLocalRect: (0@0 extent: extent x @ (self labelHeight + borderWidth))! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/18/2014 15:00'!
restoreSavedPatchOn: aCanvas 
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."

	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas image: savedPatch at: savedPatch offset.
		submorphs notEmpty ifTrue: [ ^self ].

		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent + self shadowOffset) from: nil."
		self invalidateDisplayRect: (self morphPosition extent: extent) from: nil.
		Sensor currentCursor == Cursor normal ifFalse: [ Cursor normal show ].	"show hardware cursor"
		savedPatch _ nil ]! !


!InnerPluggableMorph methodsFor: 'geometry' stamp: 'jmv 7/18/2014 14:18'!
adjustExtent
	self flag: #jmvVer2.	"Do it just adding submorph extents!!"
	"
	self submorphBounds ifNotNil: [ :r |
		self morphExtent: r bottomRight ]
	"! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 7/18/2014 14:53'!
selectionChanged

	self textComposition selectionRects do: [:r | self invalidateLocalRect: r ]! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 7/18/2014 14:53'!
onBlinkCursor
	"Blink the cursor"
	textComposition ifNil: [ ^nil ].
	textComposition showTextCursor: textComposition showTextCursor not | pauseBlinking.
	pauseBlinking _ false.
	textComposition lastTextCursorRect ifNotNil: [ :r | self invalidateLocalRect: r].! !


!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 7/18/2014 14:53'!
onBlinkCursor
	"Blink the cursor"
	showTextCursor _ showTextCursor not | pauseBlinking.
	pauseBlinking _ false.
	textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r]! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 7/18/2014 14:53'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking _ true.
	"Show cursor right now if needed"
	showTextCursor ifFalse: [
		showTextCursor _ true.
		textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r ]]! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 7/18/2014 14:53'!
stopBlinking
	"And do not show cursor anymore."
	self stopStepping: #onBlinkCursor.
	"Hide cursor right now if needed"
	showTextCursor ifTrue: [
		showTextCursor _ false.
		textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r ]]! !

!methodRemoval: PasteUpMorph #invalidateBlobalRect:from:!
PasteUpMorph removeSelector: #invalidateBlobalRect:from:!
!methodRemoval: PasteUpMorph #invalidateRect:from:!
PasteUpMorph removeSelector: #invalidateRect:from:!
!methodRemoval: Morph #clippingRect!
Morph removeSelector: #clippingRect!
!methodRemoval: Morph #invalidateBlobalRect:from:!
Morph removeSelector: #invalidateBlobalRect:from:!
!methodRemoval: Morph #invalidateRect:!
Morph removeSelector: #invalidateRect:!
!methodRemoval: Morph #invalidateRect:from:!
Morph removeSelector: #invalidateRect:from:!
!methodRemoval: Morph #submorphBounds!
Morph removeSelector: #submorphBounds!
