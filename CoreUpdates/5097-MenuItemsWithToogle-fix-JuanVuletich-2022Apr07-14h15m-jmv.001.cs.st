'From Cuis 6.0 [latest update: #5096] on 7 April 2022 at 2:16:39 pm'!

!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 4/7/2022 14:15:05'!
contentsWithMarkers: aString inverse: inverse 
	"Set the menu item entry. Parse aString for embedded markers."

	| markerIndex marker |
	contentString _ nil.	"get rid of old"
	self removeAllMorphs.	"get rid of old markers if updating"
	icon _ nil.
	(aString notEmpty and: [aString first = $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	contentString _ aString.	"remember actual string"
	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents:  (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph new image: marker.
	self addMorphFront: marker position: `0@2`.
	marker lock! !

