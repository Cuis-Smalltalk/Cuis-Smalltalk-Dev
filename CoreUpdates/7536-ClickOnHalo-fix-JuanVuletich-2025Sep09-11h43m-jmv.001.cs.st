'From Cuis7.5 [latest update: #7534] on 9 September 2025 at 11:51:55 am'!

!HaloMorph methodsFor: 'event handling testing' stamp: 'jmv 9/9/2025 11:46:12'!
handlesMouseDown: aMouseButtonEvent

	^true! !

!HaloMorph methodsFor: 'events' stamp: 'jmv 9/9/2025 11:49:40'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Just remove us"
	
	self delete! !

!HaloMorph methodsFor: 'events' stamp: 'jmv 9/9/2025 11:49:43'!
mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition
	"Just remove us"
	
	self delete! !


!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 9/9/2025 11:51:10'!
rejectsEvent: aMorphicEvent
	"Return true to reject the given event. Submorphs might still handle it."

	"Only mouse button down events are handled by the halo itself"
	(aMorphicEvent isMouse and: [ aMorphicEvent isMouseDown ])
	ifTrue: [
		^ false ].
	^true! !


!TheWorldMenu class methodsFor: 'instance creation' stamp: 'jmv 9/9/2025 11:03:56'!
getMenuIn: aWorld at: anActiveHand
	^ (self new world: aWorld hand: anActiveHand)
		buildWorldMenu! !


!HaloMorph reorganize!
('accessing' defaultLayerNumber target target:)
('drawing' drawCoordinateSystemOn: drawOn:)
('dropping/grabbing' startDrag:with:)
('event handling' popUpFor:handPosition:hand: staysUpWhenMouseIsDownIn: transferHalo:localPosition:)
('event handling testing' handlesMouseDown:)
('events' mouseButton1Down:localPosition: mouseButton2Down:localPosition: mouseButton3Down:localPosition:)
('events-processing' rejectsEvent:)
('forward to target' deleteBalloon mouseDownOnHelpHandle:)
('geometry testing' knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude)
('handles' addCollapseHandle: addDebugHandle: addDismissHandle: addDragHandle: addDupHandle: addExploreHandle: addGrabHandle: addHelpHandle: addMenuHandle: addRecolorHandle: addResizeHandle: addRotateHandle: addScaleHandle: positionIn:horizontalPlacement:verticalPlacement:)
('initialization' defaultColor delete)
('stepping' step stepTime wantsSteps)
('testing' is: isIncludedInTaskbar wantsHalo)
('private' addHandle: addHandles addHandles: addNameString: doDebug:with: doDrag:with: doDup:with: doExplore:with: doGrab:with: doMenu:with: doRecolor:with: doResize:with: doRot:with: doScale:with: endInteraction handlesBox maybeCollapse:with: maybeDismiss:with: mouseDownInCollapseHandle:with: removeAllHandlesBut: setDismissColor:with: startResize:with: startRot:with: startScale:with:)
!

