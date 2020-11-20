'From Cuis 5.0 [latest update: #4466] on 19 November 2020 at 5:26:30 pm'!

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 11/18/2020 16:51:10'!
withScale: scale
	"Set scaling according to parameters.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already an AffineTransformation."

	self scaledBy: scale / self scale! !


!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 11/18/2020 17:00:28'!
withScale: scale
	"Set scaling according to parameters.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already an AffineTransformation."

	^self scaledBy: scale! !


!MovableMorph methodsFor: 'geometry' stamp: 'jmv 11/18/2020 16:54:07'!
scale: scale
	"Change the scale of this morph. Arguments are an angle and a scale."
	location _ location withScale: scale.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !


!HaloMorph methodsFor: 'handles' stamp: 'jmv 11/19/2020 17:06:08'!
addScaleHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #startScale:with:;
		mouseMoveSelector: #doScale:with:! !

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

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/19/2020 17:10:27'!
startScale: evt with: scaleHandle
	"Initialize scale of my target if it is scalable."

	evt hand obtainHalo: self.						"Make sure the event's hand correlates with the receiver"
	growingOrRotating _ true.

	self removeAllHandlesBut: scaleHandle.  "remove all other handles"
	scaleOffset _ target scale / (evt eventPosition - target referencePosition) rho.

! !


!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 11/19/2020 17:10:59'!
haloScaleIcon

	^ self icons
		at: #haloScaleIcon
		ifAbsentPut: [ Theme current haloScaleIcon ]! !


!Theme methodsFor: 'icons' stamp: 'jmv 11/19/2020 17:21:02'!
haloScaleIcon

	^ Form
		small1BitExtent: 16@16
		pixelBits: #(
			2r0000000000000000
			2r0000000000000000
			2r0000111110000000
			2r0001000001000000
			2r0010001000100000
			2r0010001000100000
			2r0010111110100000
			2r0010001000100000
			2r0010001000100000
			2r0001000001100000
			2r0000111110010000
			2r0000000000001000
			2r0000000000000100
			2r0000000000000010
			2r0000000000000000
			2r0000000000000000)
		offset: 0@0! !


!Preferences class methodsFor: 'halos' stamp: 'jmv 11/19/2020 17:13:26'!
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
	(addScaleHandle:						right			center			(blue)						haloScaleIcon 						'Change scale')
	(addFontSizeHandle:						leftCenter			bottom			(lightGreen)						haloFontSizeIcon 						'Change font')
	(addFontEmphHandle:						rightCenter			bottom			(lightBrown darker)						haloFontEmphasisIcon 						'Emphasis & alignment')
	"FIXME - Currently non-functional...
	(addRecolorHandle:						right			bottomCenter			(magenta darker)						haloColorIcon 						'Change color')
	"
)! !


!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 11/18/2020 16:47:32'!
scaledBy: aPointOrNumber
	"Multiply by a scale.
	Argument can be a point, applying different scaling in x and in y directions.
	Keep the transformed position of 0@0, i.e. don't change offset.

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt sx sy |
	pt _ aPointOrNumber asPoint.
	sx _ pt x.
	sy _ pt y.
	self a11: self a11 * sx.
	self a12: self a12 * sx.
	self a21: self a21 * sy.
	self a22: self a22 * sy.
	^ self! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

Preferences resetHaloSpecifications!

