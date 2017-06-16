'From Cuis 5.0 of 7 November 2016 [latest update: #3099] on 9 June 2017 at 1:04:48 am'!

!Preferences class methodsFor: 'halos' stamp: 'pb 6/9/2017 00:46:36'!
iconicHaloSpecifications
	"Answer an array that characterizes the locations, colors, icons, and selectors of the halo handles that may be used in the iconic halo scheme"

	"
	Preferences resetHaloSpecifications
	"

^ #(
	"selector						horiz				vert					color info						icon key 						balloon help
	 ---------						------				-----------			-------------------------------		---------------"
	(addCollapseHandle:		left				topCenter		(tan)							haloCollapseIcon 			'Collapse')
	(addDebugHandle:			right				topCenter		(orange)						haloDebugIcon 				'Debug')
	(addDismissHandle:			left				top				(red)							haloDismissIcon 				'Remove')
	"FIXME - Currently non-functional...
	(addRotateHandle:			left				bottom			(blue)							haloRotateIcon 				'Rotate')
	"
	(addMenuHandle:			leftCenter		top				(blue lighter)					haloMenuIcon 				'Menu')
	(addGrabHandle:				center			top				(black)							haloGrabIcon 				'Pick up')
	(addDragHandle:				rightCenter		top				(brown)						haloDragIcon 				'Move')
	(addDupHandle:				right				top				(green)						haloDuplicateIcon 			'Duplicate')	
	(addHelpHandle:				center			bottom			(lightBlue)					haloHelpIcon 				'Help')
	(addGrowHandle:			right				bottom			(yellow)						haloScaleIcon 				'Change size')
	(addFontSizeHandle:		leftCenter		bottom			(lightGreen)					haloFontSizeIcon 			'Change font')
	(addFontEmphHandle:		rightCenter		bottom			(lightBrown darker)			haloFontEmphasisIcon 		'Emphasis & alignment')
	"FIXME - Currently non-functional...
	(addRecolorHandle:			right				bottomCenter	(magenta darker)			haloColorIcon 				'Change color')
	"
)! !


!Morph methodsFor: 'halos and balloon help' stamp: 'pb 6/9/2017 00:52:09'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	(#(addRotateHandle: addRecolorHandle:) statePointsTo: aSelector)
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


!PasteUpMorph methodsFor: 'halos and balloon help' stamp: 'pb 6/9/2017 00:51:44'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"
	(#(addHelpHandle: addRotateHandle: addRecolorHandle:) statePointsTo: aSelector)
		ifTrue: ["FIXME - hack to disable for non-functional halo items"
			^ false].
	
	self isWorldMorph ifFalse: [
		^super wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph ].

	^#(addDebugHandle: addMenuHandle: addHelpHandle:)
		statePointsTo: aSelector! !

