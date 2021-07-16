'From Cuis 5.0 [latest update: #4669] on 15 July 2021 at 4:20:55 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 7/15/2021 16:20:29'!
okayToScaleEasily
	"Answer whether it is appropriate for a scale handle to be shown for the receiver."

	^ self hasOwnLocation and: [MorphicCanvas activeSubclass ~~ BitBltCanvas ]! !


!HaloMorph methodsFor: 'handles' stamp: 'jmv 7/15/2021 15:35:39'!
addExploreHandle: handleSpec

	Preferences debugHaloHandle ifTrue: [
		(self addHandle: handleSpec)
			mouseDownSelector: #doExplore:with: ]
! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/15/2021 15:41:53'!
doExplore: evt with: aHandle

	evt hand obtainHalo: self.	
	^ target explore! !


!Preferences class methodsFor: 'halos' stamp: 'jmv 7/15/2021 15:35:00'!
iconicHaloSpecifications
	"Answer an array that characterizes the locations, colors, icons, and selectors of the halo handles that may be used in the iconic halo scheme"

	"
	Preferences resetHaloSpecifications
	"

^ #(
	"selector						horiz			vert			color info						icon key 						balloon help
	 ---------						------			-------			---------						------------						----------"
	(addDismissHandle:						left			top			(red)						haloDismissIcon 						'Remove')
	(addMenuHandle:						leftCenter			top			(blue lighter)						haloMenuIcon 						'Menu')
	(addGrabHandle:						center			top			(black)						haloGrabIcon 						'Pick up')
	(addDragHandle:						rightCenter			top			(brown)						haloDragIcon 						'Move')
	(addDupHandle:						right			top			(green)						haloDuplicateIcon 						'Duplicate')	

	(addExploreHandle:						left			topCenter			(orange)						haloDebugIcon 						'Explore')
	(addDebugHandle:						right			topCenter			(orange)						haloDebugIcon 						'Debug')
	(addCollapseHandle:						left			center			(tan)						haloCollapseIcon 						'Collapse')
	(addScaleHandle:						right			center			(blue)						haloScaleIcon 						'Change scale')

	(addRotateHandle:						left			bottom			(blue)						haloRotateIcon 						'Rotate')
	(addHelpHandle:						center			bottom			(lightBlue)						haloHelpIcon 						'Help')
	(addResizeHandle:						right			bottom			(yellow)						haloResizeIcon 						'Change size')

	"FIXME - Currently non-functional...
	(addRecolorHandle:						right			bottomCenter			(magenta darker)						haloColorIcon 						'Change color')
	"
)! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 7/15/2021 15:50:50'!
okayToResizeEasily
	"Answer whether it is appropriate to have the receiver be easily resized by the user from the halo"

	^ self hasVariableExtent! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 7/15/2021 16:20:37'!
okayToRotateEasily
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver."

	^ self hasOwnLocation and: [MorphicCanvas activeSubclass ~~ BitBltCanvas ]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 7/15/2021 15:56:06'!
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
	(#(#addResizeHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToResizeEasily | self okayToScaleEasily ].
	(#(#addRotateHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToRotateEasily ].
	(#(#addScaleHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToScaleEasily ].
	(#(#addRecolorHandle: ) includes: aSelector)
		ifTrue: [ ^ self wantsRecolorHandle ].
	^ true! !


!HaloMorph methodsFor: 'handles' stamp: 'jmv 7/15/2021 15:57:34'!
addResizeHandle: haloSpec

	target okayToResizeEasily ifTrue: [
		^(self addHandle: haloSpec)
			mouseDownSelector: #startResize:with:;
			mouseMoveSelector: #doResize:with: ].

	target okayToScaleEasily ifTrue: [
		^(self addHandle: haloSpec)
			mouseDownSelector: #startScale:with:;
			mouseMoveSelector: #doScale:with: ].! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 7/15/2021 15:52:54'!
addRotateHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #startRot:with:;
		mouseMoveSelector: #doRot:with:! !

!HaloMorph methodsFor: 'handles' stamp: 'jmv 7/15/2021 15:54:51'!
addScaleHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #startScale:with:;
		mouseMoveSelector: #doScale:with:! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/15/2021 15:41:47'!
doDebug: evt with: menuHandle
	"Ask hand to invoke the a debugging menu for my inner target.  If shift key is down, immediately put up an inspector on the inner target"

	| menu |
	evt hand obtainHalo: self.
	evt shiftPressed ifTrue: [
		^ target inspect].

	menu _ target buildDebugMenu: evt hand.
	menu addTitle: (target printStringLimitedTo: 40).
	menu popUpInWorld: self world! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/15/2021 15:42:00'!
doMenu: evt with: menuHandle
	"Ask hand to invoke the halo menu for my inner target."

	| menu |
	evt hand obtainHalo: self.	
	menu _ target buildHandleMenu: evt hand.
	target addTitleForHaloMenu: menu.
	menu popUpInWorld: self world.
! !


!BitBltCanvas reorganize!
('drawing' line:to:width:color:)
('drawing-icons' drawButtonIconFromCurrentMorph)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:topLeftColor:bottomRightColor:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:from:to:atBaseline:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('drawing - Global Coordinates' frameAndFillGlobalRect:fillColor:borderWidth:color: frameGlobalRect:borderWidth:color: frameReverseGlobalRect:borderWidth:)
('private' buildAuxWith:multipliedWith: fillRectangle:tilingWith:sourceRect:rule: fontToUse: frameRectangle:borderWidth:topLeftColor:bottomRightColor: setClipRect: setForm:preferSubPixelAntiAliasing: setPaintColor: updatingMorphBoundsDo:)
('morphic' boundingRectOfCurrentMorphAfterDraw canDraw: drawCurrentAndSubmorphs morph:isAtPoint:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Preferences resetHaloSpecifications!

