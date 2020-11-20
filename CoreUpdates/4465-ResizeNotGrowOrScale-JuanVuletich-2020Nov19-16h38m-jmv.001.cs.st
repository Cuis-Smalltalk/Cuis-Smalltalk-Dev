'From Cuis 5.0 [latest update: #4464] on 19 November 2020 at 4:53:48 pm'!

!HaloMorph methodsFor: 'handles' stamp: 'jmv 11/19/2020 16:39:17'!
addResizeHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #startResize:with:;
		mouseMoveSelector: #doResize:with:! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/19/2020 16:44:22'!
doResize: evt with: resizeHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
"Como podria andar el resize de un morph embebido en otro? andara ahora?"
newExtent _ evt eventPosition - positionOffset - target morphPositionInWorld.
	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target morphExtentInWorld: newExtent.
	resizeHandle morphPositionInWorld: evt eventPosition - (resizeHandle morphExtent // 2)! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 11/19/2020 16:47:00'!
startResize: evt with: resizeHandle
	"Initialize resizing of my target."

	| botRt |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self removeAllHandlesBut: resizeHandle.  "remove all other handles"
	botRt _ target morphPositionInWorld + target morphExtentInWorld.
	positionOffset _ (self world viewBox containsPoint: botRt)
		ifTrue: [evt eventPosition - botRt]
		ifFalse: [`0@0`]! !


!HaloMorph class methodsFor: 'accessing - icons' stamp: 'jmv 11/19/2020 16:40:22'!
haloResizeIcon

	^ self icons
		at: #haloResizeIcon
		ifAbsentPut: [ Theme current haloResizeIcon ]! !


!Theme methodsFor: 'icons' stamp: 'jmv 11/19/2020 16:49:17'!
haloResizeIcon

	^ Form
		small1BitExtent: 16@16
		pixelBits: #(
			2r0000000000000000
			2r0000000000000000
			2r0000000000000000
			2r0000000000000000
			2r0000111111010000
			2r0000100001010000
			2r0000100001010000
			2r0000100001010000
			2r0000100001010000
			2r0000111111010000
			2r0000000000010000
			2r0000111111110000
			2r0000000000000000
			2r0000000000000000
			2r0000000000000000
			2r0000000000000000)
		offset: 0@0! !


!Preferences class methodsFor: 'halos' stamp: 'jmv 11/19/2020 16:40:31'!
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
	(addFontSizeHandle:						leftCenter			bottom			(lightGreen)						haloFontSizeIcon 						'Change font')
	(addFontEmphHandle:						rightCenter			bottom			(lightBrown darker)						haloFontEmphasisIcon 						'Emphasis & alignment')
	"FIXME - Currently non-functional...
	(addRecolorHandle:						right			bottomCenter			(magenta darker)						haloColorIcon 						'Change color')
	"
)! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 11/19/2020 16:39:54'!
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
		ifTrue: [ ^ self okayToResizeEasily ].
	(#(#addRotateHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToRotateEasily ].
	(#(#addRecolorHandle: ) includes: aSelector)
		ifTrue: [ ^ self wantsRecolorHandle ].
	^ true! !

!methodRemoval: Theme #haloScaleIcon stamp: 'jmv 11/19/2020 16:49:57'!
Theme removeSelector: #haloScaleIcon!
!methodRemoval: HaloMorph class #haloScaleIcon stamp: 'jmv 11/19/2020 16:49:53'!
HaloMorph class removeSelector: #haloScaleIcon!
!methodRemoval: HaloMorph #doGrow:with: stamp: 'jmv 11/19/2020 16:46:35'!
HaloMorph removeSelector: #doGrow:with:!
!methodRemoval: HaloMorph #addGrowHandle: stamp: 'jmv 11/19/2020 16:46:25'!
HaloMorph removeSelector: #addGrowHandle:!
!methodRemoval: HaloMorph #startGrow:with: stamp: 'jmv 11/19/2020 16:47:06'!
HaloMorph removeSelector: #startGrow:with:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

Preferences resetHaloSpecifications!

