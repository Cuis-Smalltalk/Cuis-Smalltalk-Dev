'From Cuis6.3 [latest update: #6161] on 2 January 2024 at 2:47:39 pm'!

!MenuItemMorph methodsFor: 'font' stamp: 'jmv 1/2/2024 14:37:54'!
font: aFont emphasis: emphasisCode

	super font: aFont emphasis: emphasisCode.
	contentString ifNotNil: [
		self contentsWithMarkers: contentString inverse: false ].! !


!LabelMorph methodsFor: 'font' stamp: 'jmv 1/2/2024 14:40:03'!
emphasis: aNumber
	"Set the receiver's emphasis as indicated. aNumber is a bitmask with the following format:

	bit	attribute
	1	bold
	2	italic
	4	underlined
	8	struckThrough
	16	withUnderscoreGlyphs
	"

	"examples: 0 -> plain.  
	1 -> bold.  2 -> italic.  3 -> bold italic.  4 -> underlined  
	5 -> bold underlined.  6 -> italic underlined.   7 -> bold italic underlined   
	etc...

	Prefer AbstractFont method category: 'derivatives - emphasis' 
	to raw numbers:
	    self emphasis: AbstractFont boldItalic.
	etc."

	self font: font emphasis: aNumber.! !

!LabelMorph methodsFor: 'font' stamp: 'jmv 1/2/2024 14:39:45'!
font: aFont
	"Set the font my text will use. The emphasis remains unchanged."

	self font: aFont emphasis: emphasis.! !

!LabelMorph methodsFor: 'font' stamp: 'jmv 1/2/2024 14:41:15'!
fontFamily: aFontFamily
	"Set the font my text will use. The emphasis remains unchanged."

	self
		font: (aFontFamily atPointSize: (font
			ifNotNil: [ font pointSize ]
			ifNil: [ FontFamily defaultPointSize ]))
		emphasis: emphasis.! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 1/2/2024 14:46:12'!
contentsWithMarkers: aString inverse: invertMarkers
	"Set the menu item entry. Parse aString for embedded markers."

	| markerIndex marker |
	contentString := nil.	"get rid of old"
	self removeAllMorphs.	"get rid of old markers if updating"
	(aString notEmpty and: [aString first = $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	contentString := aString.	"remember actual string"
	marker := ((marker = '<on>' or: [marker = '<yes>']) xor: invertMarkers)
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents: (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph new image: marker.
	self addMorphFront: marker position: 1@(self morphHeight +1- marker morphHeight // 2).
	marker lock.! !

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 1/2/2024 14:43:33'!
offImage
	"Return the form to be used for indicating an '<off>' marker"
	| e ps1 ps2 |
	ps1 := self fontToUse pointSize.
	ps2 := FontFamily defaultPointSize.
	e := (ps1 < ps2 ifTrue: [ ps1+ ps2 // 2 ] ifFalse: [ ps1 * 2 + (ps2 * 6) // 8 ]) asPoint.
	(OffImage isNil or: [ OffImage extent ~= e ]) ifTrue: [
		OffImage := Form extent: e depth: 32.
		OffImage getCanvas
			frameAndFillRectangle: OffImage boundingBox fillColor: `(Color gray: 0.9)`
				borderWidth: 2 borderColor: `Color black` ].
	^OffImage! !

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 1/2/2024 14:43:41'!
onImage
	"Return the form to be used for indicating an '<on>' marker"
	| e ps1 ps2 |
	ps1 := self fontToUse pointSize.
	ps2 := FontFamily defaultPointSize.
	e := (ps1 < ps2 ifTrue: [ ps1+ ps2 // 2 ] ifFalse: [ ps1 * 2 + (ps2 * 6) // 8 ]) asPoint.
	(OnImage isNil or: [ OnImage extent ~= e ]) ifTrue: [
		OnImage := Form extent: e depth: 32.
		OnImage getCanvas
			frameAndFillRectangle: OnImage boundingBox fillColor: `Color gray: 0.9`
				borderWidth: 2 borderColor: `Color black`;
			fillRectangle: (OnImage boundingBox insetBy: 3) color: `Color black` ].
	^OnImage! !


!LabelMorph reorganize!
('accessing' contents contents: contentsFromPrintString: font font:emphasis: fontToUse label)
('drawing' drawOn:)
('font' emphasis: font: fontFamily:)
('halos and balloon help')
('initialization' defaultBorderWidth defaultColor initWithContents:font:emphasis: initialize)
('menu' addCustomMenuItems:hand: changeEmphasis changeFont changeFontSize)
('printing' printOn:)
('geometry' fitContents fontPreferenceChanged measureContents minimumExtent)
!


!MenuItemMorph reorganize!
('accessing' contents: contentsWithMarkers:inverse: hasIcon hasMarker hasSubMenu isEnabled isEnabled: setBlankIcon setIcon: subMenu subMenu: target:selector:arguments:)
('drawing' drawOn:)
('event handling testing' handlesMouseDown: handlesMouseOver:)
('events' activateOwnerMenu: activateSubmenu: invokeWithEvent: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition: mouseEnter:)
('events-processing' processMouseEnter:localPosition: processMouseLeave:localPosition:)
('font' font:emphasis:)
('grabbing' aboutToBeGrabbedBy: duplicateMorph:)
('initialization' deleteIfPopUp: initialize)
('layout' magnifiedIcon measureContents minItemWidth)
('selecting' deselect isSelected: select)
('private' actionBlockForEvent: offImage onImage)
('testing' is:)
('debug and other' buildDebugMenu: debugAction)
!

