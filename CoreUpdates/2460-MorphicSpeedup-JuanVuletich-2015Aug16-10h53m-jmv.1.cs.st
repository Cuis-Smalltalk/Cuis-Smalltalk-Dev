'From Cuis 4.2 of 25 July 2013 [latest update: #2459] on 16 August 2015 at 11:34:49.204928 am'!

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 8/15/2015 21:12'!
submorphsDrawingOutsideReverseDo: aBlock
	"Might be redefined in subclasses that know that its submorphs are never outside itself"
	self unclippedSubmorphsReverseDo: aBlock! !


!SystemWindow methodsFor: 'submorphs-accessing' stamp: 'jmv 8/15/2015 21:14'!
submorphsDrawingOutsideReverseDo: aBlock
	"All our submorphs are inside us"! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 8/16/2015 11:34'!
containsPoint: aLocalPoint event: anEvent
	"Return true if aPoint is considered to be inside the receiver for the given event.
	The default implementation treats locked children as integral part of their owners."
	
	"Should this method be called #fullContainsPoint:event: ?
	Should it be merged with #fullContainsPoint: ?
	"
	self visible ifFalse: [ ^false ].
	(self morphContainsPoint: aLocalPoint) ifTrue: [ ^true ].
	self submorphsDrawingOutsideReverseDo: [ :m |
		(m isLocked and: [ m fullContainsPoint: (m internalize: aLocalPoint) ])
			ifTrue: [ ^true ]].
	^false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 8/15/2015 21:15'!
fullContainsPoint: aPoint

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	(self morphContainsPoint: aPoint) ifTrue: [ ^ true ].  "quick acceptance"
	self submorphsDrawingOutsideReverseDo: [ :m |
		(m fullContainsPoint: (m internalize: aPoint)) ifTrue: [ ^ true ]].
	^ false! !

!Morph methodsFor: 'updating' stamp: 'jmv 8/16/2015 10:54'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Include submorphs if we don't clip!!
	Think about it. We don't to know about a specific display rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"

	self morphBoundsInWorld ifNotNil: [ :r |
		self invalidateDisplayRect: r from: nil.
		"Expensive in many cases..."
		self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]]! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/15/2015 21:16'!
displayFullBoundsInWorldOf: aMorph
	"Answer a rectangle that completely bounds aMorph and submorphs when drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world).
	Might be larger than strictly required. See comment at #displayBoundsInWorldOf:"

	| r |
	"Think about doing a BoundsFinderCanvas even for Morphic 2"

	self flag: #jmvVer2.

	r _ self displayBoundsInWorldOf: aMorph.
	aMorph submorphsDrawingOutsideReverseDo: [ :m |
		m visible ifTrue: [
			r _ r merge: (self displayFullBoundsInWorldOf: m) ]].

	^r! !

