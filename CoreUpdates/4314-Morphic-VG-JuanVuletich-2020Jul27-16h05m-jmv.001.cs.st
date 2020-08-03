'From Cuis 5.0 [latest update: #4309] on 27 July 2020 at 4:50:50 pm'!
!classDefinition: #HaloMorph category: #'Morphic-Halos'!
RectangleLikeMorph subclass: #HaloMorph
	instanceVariableNames: 'target positionOffset angleOffset growingOrRotating haloBox scaleOffset '
	classVariableNames: 'HandleSize Icons '
	poolDictionaries: ''
	category: 'Morphic-Halos'!

!Preferences class methodsFor: 'halos' stamp: 'jmv 7/27/2020 16:34:43'!
iconicHaloSpecifications
	"Answer an array that characterizes the locations, colors, icons, and selectors of the halo handles that may be used in the iconic halo scheme"

	"
	Preferences resetHaloSpecifications
	"

^ #(
	"selector						horiz			vert			color info						icon key 						balloon help
	 ---------						------			-----------			--------------------------						--------------------------						---------------"
	(addCollapseHandle:						left			topCenter			(tan)						haloCollapseIcon 						'Collapse')
	(addDebugHandle:						right			topCenter			(orange)						haloDebugIcon 						'Debug')
	(addDismissHandle:						left			top			(red)						haloDismissIcon 						'Remove')
	(addRotateHandle:						left			bottom			(blue)						haloRotateIcon 						'Rotate')
	(addMenuHandle:						leftCenter			top			(blue lighter)						haloMenuIcon 						'Menu')
	(addGrabHandle:						center			top			(black)						haloGrabIcon 						'Pick up')
	(addDragHandle:						rightCenter			top			(brown)						haloDragIcon 						'Move')
	(addDupHandle:						right			top			(green)						haloDuplicateIcon 						'Duplicate')	
	(addHelpHandle:						center			bottom			(lightBlue)						haloHelpIcon 						'Help')
	(addGrowHandle:						right			bottom			(yellow)						haloScaleIcon 						'Change size')
	(addFontSizeHandle:						leftCenter			bottom			(lightGreen)						haloFontSizeIcon 						'Change font')
	(addFontEmphHandle:						rightCenter			bottom			(lightBrown darker)						haloFontEmphasisIcon 						'Emphasis & alignment')
	"FIXME - Currently non-functional...
	(addRecolorHandle:						right			bottomCenter			(magenta darker)						haloColorIcon 						'Change color')
	"
)! !


!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 7/27/2020 16:24:40'!
rotatedBy: radians
	"rotate the receiver by radians angle.
	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a21 a22 |
	s _ radians sin.
	c _ radians cos.
	a11 _ self a11.
	a12 _ self a12.
	a21 _ self a21.
	a22 _ self a22.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	^ self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 7/27/2020 16:28:09'!
scaledByNumber: aNumber rotatedBy: radians
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling.

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a21 a22 |
	s _ radians sin.
	c _ radians cos.
	a11 _ self a11 * aNumber.
	a12 _ self a12 * aNumber.
	a21 _ self a21 * aNumber.
	a22 _ self a22 * aNumber.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	^self! !


!MorphicTranslation methodsFor: 'accessing' stamp: 'jmv 7/27/2020 16:49:10'!
scale
	"Answer the *scalar* scale applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^1.0! !


!Morph methodsFor: 'as yet unclassified' stamp: 'jmv 7/27/2020 16:37:56'!
rotationDegrees: degrees
	location _ location rotatedBy: degrees degreesToRadians - location radians.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 7/27/2020 16:29:42'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	(#(addRecolorHandle:) statePointsTo: aSelector)
		ifTrue: ["FIXME - hack to disable for non-functional halo items"
			^ false].

	Preferences selectiveHalos ifFalse: [
		^true ].

	(#(#addDismissHandle: ) includes: aSelector)
		ifTrue: [ ^ self resistsRemoval not ].
	(#(#addDragHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToBrownDragEasily ].
	(#(#addGrowHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToResizeEasily ].
	(#(#addRotateHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToRotateEasily ].
	(#(#addRecolorHandle: ) includes: aSelector)
		ifTrue: [ ^ self wantsRecolorHandle ].
	^ true! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 7/27/2020 16:50:25'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable.  Keep the relevant command object up to date."

	| radians scale |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
	radians _ (evt eventPosition - target referencePosition) theta.
	radians _ radians - angleOffset theta.
	"degrees _ degrees detentBy: 10.0 atMultiplesOf: 90.0 snap: false."
	radians = 0.0
		ifTrue: [rotHandle color: `Color lightBlue`]
		ifFalse: [rotHandle color: `Color blue`].
	rotHandle submorphsDo:
		[:m | m color: rotHandle color makeForegroundColor].
	scale _ (evt eventPosition - target referencePosition) rho * scaleOffset.
	self removeAllHandlesBut: rotHandle.
	target rotation: radians scale: scale.
	rotHandle morphPositionInWorld: evt eventPosition - (rotHandle morphExtent // 2)! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/27/2020 16:48:47'!
startRot: evt with: rotHandle
	"Initialize rotation of my target if it is rotatable.  Launch a command object to represent the action"

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	growingOrRotating _ true.

	self removeAllHandlesBut: rotHandle.  "remove all other handles"
	angleOffset _ evt eventPosition - target referencePosition.
	angleOffset _ Point
			r: angleOffset r
			degrees: angleOffset degrees - target rotationDegrees.
	scaleOffset _ target location scale / (evt eventPosition - target referencePosition) rho.

! !

!classDefinition: #HaloMorph category: #'Morphic-Halos'!
RectangleLikeMorph subclass: #HaloMorph
	instanceVariableNames: 'target positionOffset angleOffset growingOrRotating haloBox scaleOffset'
	classVariableNames: 'HandleSize Icons'
	poolDictionaries: ''
	category: 'Morphic-Halos'!
