'From Cuis 5.0 [latest update: #4468] on 20 November 2020 at 10:22:22 am'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 11/20/2020 09:53:28'!
hasOwnLocation
	^false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 11/20/2020 09:54:41'!
hasVariableExtent
	^false! !


!MovableMorph methodsFor: 'geometry testing' stamp: 'jmv 11/20/2020 09:53:46'!
hasOwnLocation
	^true! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 11/20/2020 10:20:34'!
referencePosition
	"Return the current reference position of the receiver"
	"a rather ugly way to say #center . At least, we avoid false polymorphism"
	"remove some day"
	self flag: #jmvVer2.
	^self morphExtentInWorld // 2 + self morphPositionInWorld! !


!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 11/20/2020 09:54:52'!
hasVariableExtent
	^true! !


!Preferences class methodsFor: 'halos' stamp: 'jmv 11/20/2020 10:02:58'!
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
	(addResizeHandle:						right			bottom			(yellow)						haloResizeIcon 						'Change size')
	(addScaleHandle:						right			bottom			(blue)						haloScaleIcon 						'Change scale')
	(addFontSizeHandle:						leftCenter			bottom			(lightGreen)						haloFontSizeIcon 						'Change font')
	(addFontEmphHandle:						rightCenter			bottom			(lightBrown darker)						haloFontEmphasisIcon 						'Emphasis & alignment')
	"FIXME - Currently non-functional...
	(addRecolorHandle:						right			bottomCenter			(magenta darker)						haloColorIcon 						'Change color')
	"
)! !


!Morph methodsFor: 'geometry' stamp: 'jmv 11/20/2020 09:57:32'!
morphExtent
	"In our own coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^`150 @ 140`! !


!MovableMorph methodsFor: 'geometry' stamp: 'jmv 11/20/2020 10:20:45'!
referencePosition
	"Return the current reference position of the receiver"
	"a rather ugly way to say #center . At least, we avoid false polymorphism"
	"remove some day"
	self flag: #jmvVer2.
	^self morphPositionInWorld! !


!HaloMorph methodsFor: 'handles' stamp: 'jmv 11/20/2020 09:56:12'!
addResizeHandle: haloSpec

	target hasVariableExtent ifTrue: [
		(self addHandle: haloSpec)
			mouseDownSelector: #startResize:with:;
			mouseMoveSelector: #doResize:with: ]! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 11/20/2020 10:02:21'!
addRotateHandle: haloSpec

target hasVariableExtent ifFalse: [
	target hasOwnLocation ifTrue: [
		(self addHandle: haloSpec)
			mouseDownSelector: #startRot:with:;
			mouseMoveSelector: #doRot:with: ]
]! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 11/20/2020 10:02:32'!
addScaleHandle: haloSpec

target hasVariableExtent ifFalse: [
	target hasOwnLocation ifTrue: [
		(self addHandle: haloSpec)
			mouseDownSelector: #startScale:with:;
			mouseMoveSelector: #doScale:with: ]
]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/19/2020 17:10:02'!
doScale: evt with: scaleHandle
	"Update the scale of my target if it is scalable."

	| scale |
	evt hand obtainHalo: self.
	scale _ (evt eventPosition - target referencePosition) rho * scaleOffset.
	scale = 1.0
		ifTrue: [scaleHandle color: `Color lightBlue`]
		ifFalse: [scaleHandle color: `Color blue`].
	scaleHandle submorphsDo:
		[:m | m color: scaleHandle color makeForegroundColor].
	self removeAllHandlesBut: scaleHandle.
	target scale: scale.
	scaleHandle morphPositionInWorld: evt eventPosition - (scaleHandle morphExtent // 2)! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/20/2020 10:12:35'!
startRot: evt with: rotHandle
	"Initialize rotation of my target if it is rotatable."

	evt hand obtainHalo: self.						"Make sure the event's hand correlates with the receiver"

	self removeAllHandlesBut: rotHandle.  "remove all other handles"
	angleOffset _ evt eventPosition - target referencePosition.
	angleOffset _ Point
			r: 1.0
			degrees: angleOffset degrees - target rotationDegrees.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/20/2020 10:09:48'!
startScale: evt with: scaleHandle
	"Initialize scale of my target if it is scalable."

	evt hand obtainHalo: self.						"Make sure the event's hand correlates with the receiver"

	self removeAllHandlesBut: scaleHandle.  "remove all other handles"
	scaleOffset _ target scale / (evt eventPosition - target referencePosition) rho.! !


!MovableMorph reorganize!
('accessing' location)
('geometry' allocHeightForFactor: allocWidthForFactor: externalize: externalizeDisplayBounds: externalizeDistance: internalize: internalizeDistance: minimumLayoutExtent morphAlign:with: morphPosition morphPosition: morphPositionInWorld: orbitBy: referencePosition referencePosition: rotateBy: rotation: rotation:scale: rotationDegrees: scale scale: scaleBy:)
('initialization' initialize openInWorld:)
('dropping/grabbing' aboutToBeGrabbedBy:)
('layout' minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth)
('geometry testing' hasOwnLocation)
('private' privateOwner: privatePosition:)
('layout-properties' layoutSpec layoutSpec:)
('testing' isProportionalHeight isProportionalWidth)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences resetHaloSpecifications!

