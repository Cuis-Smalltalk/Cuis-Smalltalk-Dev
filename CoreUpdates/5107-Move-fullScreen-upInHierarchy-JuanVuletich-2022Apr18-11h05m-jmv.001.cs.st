'From Cuis 6.0 [latest update: #5106] on 18 April 2022 at 11:07:10 am'!

!BoxedMorph methodsFor: 'window like behavior' stamp: 'jmv 4/18/2022 11:06:19'!
fullScreen
	"Zoom Window to Full World size with possible DeskMargins"

	| left right possibleBounds |
	(self hasProperty: #originalBounds)
		ifFalse: [ "Expand"
			self setProperty: #originalBounds toValue: self displayBounds.
			left := right := 0.
			possibleBounds := (RealEstateAgent maximumUsableAreaInWorld: self world) 
				insetBy: (left @ 0 corner: right @ 0).
			possibleBounds := possibleBounds insetBy: Theme current fullScreenDeskMargin
		] 
		ifTrue: [ "Contract"
			possibleBounds := self valueOfProperty: #originalBounds.
			self removeProperty: #originalBounds.
		].
	self morphPosition: possibleBounds topLeft extent: possibleBounds extent! !

!BoxedMorph methodsFor: 'window like behavior' stamp: 'jmv 4/18/2022 11:05:48'!
resize: boundingBox
	(self hasProperty: #originalBounds) ifFalse: [
		self setProperty: #originalBounds toValue: self displayBounds].
	self morphPosition: boundingBox origin extent: boundingBox extent! !

!methodRemoval: SystemWindow #resize: stamp: 'jmv 4/18/2022 11:05:57'!
SystemWindow removeSelector: #resize:!
!methodRemoval: SystemWindow #fullScreen stamp: 'jmv 4/18/2022 11:06:03'!
SystemWindow removeSelector: #fullScreen!

!BoxedMorph reorganize!
('accessing' borderColor borderColor: borderWidth borderWidth: color color:)
('initialization' defaultBorderColor defaultBorderWidth defaultColor initialize noBorder)
('drawing' drawOn:)
('geometry' ensureMinimimExtent extentBorder fitInto: fullBoundsInOwner morphBottomLeft morphBottomRight morphExtent morphExtent: morphExtentInOwner: morphExtentInWorld morphExtentInWorld: morphHeight morphHeight: morphHeightInOwner: morphLocalBounds morphLocalBoundsForError morphPosition:extent: morphTopLeft morphTopRight morphWidth morphWidth: morphWidthInOwner: privateExtent: rotationCenter topLeftInOwner)
('geometry services' coversLocalPoint: coversPixel: includesPixel:)
('geometry testing' hasVariableExtent knowsOwnLocalBounds requiresVectorCanvas submorphsMightProtrude wantsContour)
('layout' minItemWidth)
('layout-properties' layoutSpec)
('testing' is:)
('halos and balloon help' haloShowsCoordinateSystem okayToResizeEasily okayToRotateEasily okayToScaleEasily)
('window like behavior' fullScreen resize:)
!

