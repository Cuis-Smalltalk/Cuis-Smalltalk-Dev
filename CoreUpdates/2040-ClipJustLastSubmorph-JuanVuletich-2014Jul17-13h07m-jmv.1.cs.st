'From Cuis 4.2 of 25 July 2013 [latest update: #2039] on 17 July 2014 at 1:17:14.401204 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 7/14/2014 14:27'!
clipsLastSubmorph
	"Drawing specific. If this property is set, clip the receiver's  
	last submorph to the receiver's shape"
	^ false! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 7/14/2014 14:54'!
clippedSubmorph
	| i |
	^(self clipsLastSubmorph and: [
		i _ submorphs size.
		i ~= 0]) ifTrue: [
			submorphs at: i ]! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 7/14/2014 14:58'!
unclippedSubmorphsReverseDo: aBlock
	self clippedSubmorph
		ifNil: [ submorphs reverseDo: aBlock ]
		ifNotNil: [ :clipped |
			"horrible... si se que es el 1ro que itero..."
			submorphs reverseDo: [ :each | each == clipped ifFalse: [
					aBlock value: each ]]]! !


!PluggableScrollPane methodsFor: 'drawing' stamp: 'jmv 7/14/2014 14:27'!
clipsLastSubmorph
	"Drawing specific. If this property is set, clip the receiver's  
	last submorph to the receiver's shape"
	^ true! !


!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/14/2014 15:14'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	"To replace #fullDrawOn:"
	| oldClipRect |
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	aMorph layoutSubmorphsIfNeeded.	"preferiria ni preguntar... para M3 al menos"
	(self isVisible: aMorph morphFullBoundsInWorld) ifFalse: [
		self outOfMorph.
		^ self].

	aMorph isKnownFailing ifTrue: [
		self drawAsError: aMorph.
		self outOfMorph.
		^ self].

	(aMorph isOwnedByHand and: [ Preferences cheapWindowReframe ]) ifTrue: [
		self drawAsOutline: aMorph.
		self outOfMorph.
		^ self].
	
	" --------------------------------------------------" 
	"Aca es donde hay que dibujar primero el morph, y despues los submorphs, sabiendo los bounds hallados al dibujar el morph!!!!!!!!!! (en M3)
	este es el mismo metodo que hace into: y outOfMorph    :D  "
	" --------------------------------------------------"
	"preferiria ni preguntar... para M3 al menos"
	(self isVisible: aMorph morphBoundsInWorld) ifTrue: [
		aMorph drawOn: self ].

"hablo tanto de aMorph como de currentMorph... y son lo mismo..."
	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')"
	aMorph hasSubmorphs ifTrue: [
		aMorph clippedSubmorph ifNotNil: [ :clipped |
			currentOwnerIfClips _ aMorph.
			oldClipRect _ clipRect.
			self setClipRect: (clipRect intersect: self clippingRectForCurrentMorph).
			self fullDraw: clipped.
			self setClipRect: oldClipRect.
			currentOwnerIfClips _ nil ].
		aMorph unclippedSubmorphsReverseDo: [ :m |
			self fullDraw: m ].
		].
	self outOfMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/14/2014 14:33'!
into: aMorph

	self intoLocation: aMorph location.
	currentMorph _ aMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/14/2014 14:33'!
outOfMorph

	self outOfLocation.
	currentMorph _ currentMorph owner! !


!Morph methodsFor: 'change reporting' stamp: 'jmv 7/14/2014 15:11'!
invalidateRect: aRectangle

	| rectInOwner rectInOwns |

	"warning. Senders are using global coordinates. Redesign!!"
	"local now!!!!!!!!!!"
	self flag: #jmvVer2.	"ok?"

	self visible ifFalse: [ ^self].

	"uh, aca necesito saber de que morph vengo, para saber si clippear o no...
	igual, para m3, repensar, no?"
 	rectInOwns _ self clipsSubmorphs
		ifTrue: [ aRectangle intersect: (0@0 extent: self morphExtent) ]
		ifFalse: [ aRectangle ].
	owner ifNotNil: [
		rectInOwner _ location displayBoundsOfTransformOf: rectInOwns.
		owner invalidateRect: rectInOwner ]! !

!Morph methodsFor: 'drawing' stamp: 'jmv 7/14/2014 14:36'!
clipsSubmorphs
"Eliminar. Reemplazar por clipsLastSubmorph y la logica apripiada, diferenciiando entre los morphs que clippeo y los que no. Quizas hacer #clippedMorphOrNil y #unclippedMorphsDo: o algo asi..."
	"Drawing specific. If this property is set, clip the receiver's  
	submorphs to the receiver's clipping bounds."
	^ false! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 7/17/2014 13:14'!
containsPoint: aLocalPoint event: anEvent
	"Return true if aPoint is considered to be inside the receiver for the given event.
	The default implementation treats locked children as integral part of their owners."
	self visible ifFalse: [ ^false ].
	(self morphContainsPoint: aLocalPoint) ifTrue: [ ^true ].
	self unclippedSubmorphsReverseDo: [ :m |
		(m isLocked and: [ m fullContainsPoint: (m internalize: aLocalPoint) ])
			ifTrue: [ ^true ]].
	^false! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/17/2014 13:15'!
morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."
	"Warning:
		Not #visible morphs will not be considered in recursion, but will answer if asked directly"

	| r |
	self flag: #jmvVer2.
	"IF I remove this, then layout of buttons in FileList breaks when selecting / deselecting code files. Besides, ProgressMorph example breaks too"
	self layoutSubmorphsIfNeeded.

	self flag: #jmvVer2.
	r _ self morphBoundsInWorld.
	self unclippedSubmorphsReverseDo: [ :m |
		m visible ifTrue: [
			r _ r merge: m morphFullBoundsInWorld ]].
	^r! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/17/2014 13:15'!
fullContainsPoint: aPoint
"
	This alternative implementation is included in this comment because it could be useful someday.
	If we start to rely heavily on the use of #ownShadowForm in #morphContainsPoint, this could be cheaper.
	
	| shadow |
	self clipSubmorphs
		ifTrue: [ ^self morphContainsPoint: aPoint ]
		ifFalse: [
			(self fullBounds containsPoint: aPoint) ifFalse: [^ false].
			(self morphContainsPoint: aPoint) ifTrue: [^ true].
			shadow _ self shadowForm.
			^(shadow pixelValueAt: aPoint - shadow offset) > 0 ]
"
	
	self flag: #jmvVer2.
	"Is the comment relevant now?"

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	(self morphContainsPoint: aPoint) ifTrue: [ ^ true ].  "quick acceptance"
	self unclippedSubmorphsReverseDo: [ :m |
		(m fullContainsPoint: (m internalize: aPoint)) ifTrue: [ ^ true ]].
	^ false! !

!Morph methodsFor: 'updating' stamp: 'jmv 7/14/2014 15:13'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Include submorphs if we don't clip!!
	Think about it. We don't to know about a specific display rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"

	self invalidateRect: (0@0 extent: self morphExtent).
	"caro al pedo en casi todoso los casos..."
	self unclippedSubmorphsReverseDo: [ :m | m redrawNeeded ]! !

