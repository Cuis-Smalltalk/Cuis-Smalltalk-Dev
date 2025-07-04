'From Cuis7.5 [latest update: #7322] on 4 July 2025 at 3:25:41 pm'!
!classDefinition: #LayoutEdgesSpec category: #'Morphic-Layouts'!
LayoutSpec subclass: #LayoutEdgesSpec
	instanceVariableNames: 'topLeftEdgesWeight topLeftOffset bottomRightEdgesWeight bottomRightOffset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutEdgesSpec commentStamp: '<historical>' prior: 0!
To be used with FormLayout.
Inspired by 
https://archive.eclipse.org/eclipse/downloads/documentation/2.0/html/plugins/org.eclipse.platform.doc.isv/reference/api/org/eclipse/swt/layout/FormData.html and
https://archive.eclipse.org/eclipse/downloads/documentation/2.0/html/plugins/org.eclipse.platform.doc.isv/reference/api/org/eclipse/swt/layout/FormAttachment.html!


!Rectangle methodsFor: 'mathematical functions' stamp: 'jmv 6/24/2025 18:03:19'!
interpolateAt: amountDone
	"Interpolate between the instance and end after the specified amount has been done (0 - 1)."

	^ self extent * amountDone + origin! !


!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/4/2025 14:49:27'!
initializeForm

	layout := FormLayout new.
	doAdoptWidgetsColor := false.
	self initialize.! !


!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 7/4/2025 14:50:02'!
newForm
	^self basicNew initializeForm! !

!LayoutMorph class methodsFor: 'FormLayout examples' stamp: 'jmv 7/4/2025 15:08:42'!
example1Form
"
	self example1Form
"
	| pane |
	pane := (LayoutMorph newForm separation: 5) name: #example1.
	pane 
		color: Color red;
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #A)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@20 brEdgesWeight: 0@0 offset: 100@40);
		addMorph: (ColoredBoxMorph new color: (Color blue); name: #B)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@0 offset: 30@20 brEdgesWeight: 1@0 offset: -30@40);
		addMorph: (ColoredBoxMorph new color: (Color lightGreen); name: #C)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@60 brEdgesWeight: 0.5@0 offset: -10@140);
		addMorph: (ColoredBoxMorph new color: (Color green); name: #D)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@0 offset: 10@60 brEdgesWeight: 1@0 offset: -30@140);
		addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@160 brEdgesWeight: 1@1 offset: -30@ -30).
	pane position: 150@130 extent: 400@300.
	pane openInWorld! !


!FormLayout methodsFor: 'layout' stamp: 'jmv 7/4/2025 14:45:00'!
layoutMorphs: morphs in: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	morphs do: [ :eachMorph | | ls r |
		eachMorph visible ifTrue: [
			ls := eachMorph layoutSpec.
			r := (boundsForLayout interpolateAt: ls topLeftEdgesWeight) + ls topLeftOffset
				corner: 
					(boundsForLayout interpolateAt: ls bottomRightEdgesWeight) + ls bottomRightOffset.
			eachMorph fitInto: r ]]! !

!FormLayout methodsFor: 'services' stamp: 'jmv 7/4/2025 14:58:13'!
minimumExtentFor: morphs cachedValues: cachedValues
	"Ojo!!"
	^100@100! !


!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 7/4/2025 14:35:06'!
bottomRightEdgesWeight
	^bottomRightEdgesWeight! !

!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 7/4/2025 14:34:24'!
bottomRightEdgesWeight: aPoint
	"Argument x and y must be in [0.0 .. 1.0]"
	bottomRightEdgesWeight := aPoint! !

!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 7/4/2025 14:35:16'!
bottomRightOffset
	^bottomRightOffset! !

!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 7/4/2025 14:34:41'!
bottomRightOffset: aPoint
	bottomRightOffset := aPoint! !

!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 7/4/2025 14:34:54'!
topLeftEdgesWeight
	^topLeftEdgesWeight! !

!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 7/4/2025 14:34:11'!
topLeftEdgesWeight: aPoint
	"Argument x and y must be in [0.0 .. 1.0]"
	topLeftEdgesWeight := aPoint! !

!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 7/4/2025 14:35:00'!
topLeftOffset
	^topLeftOffset! !

!LayoutEdgesSpec methodsFor: 'accessing' stamp: 'jmv 7/4/2025 14:34:34'!
topLeftOffset: aPoint
	topLeftOffset := aPoint! !

!LayoutEdgesSpec methodsFor: 'layout' stamp: 'jmv 7/4/2025 14:36:17'!
minimumSpecExtent
	"We specify position, but not extent."

	^0@0! !


!LayoutEdgesSpec class methodsFor: 'instance creation' stamp: 'jmv 7/4/2025 14:53:37'!
tlEdgesWeight: tlWeight offset: tlOffset brEdgesWeight: brWeight offset: brOffset

	^LayoutEdgesSpec new
		topLeftEdgesWeight: tlWeight;
		topLeftOffset: tlOffset;
		bottomRightEdgesWeight: brWeight;
		bottomRightOffset: brOffset! !


!Rectangle reorganize!
('accessing' area bottom bottom: bottomCenter bottomLeft bottomRight boundingBox center corner corners extent height innerCorners left left: leftCenter origin right right: rightCenter top top: topCenter topLeft topRight width)
('comparing' = hash)
('rectangle functions' amountToTranslateWithin: areasOutside: areasOutside:do: encompass: expandBy: extendBy: insetBy: insetOriginBy:cornerBy: intersect: merge: outsetBy: quickMerge: rectanglesAt: translatedToBeWithin: updateMerging: withHeight: withLeft: withRight:)
('mathematical functions' interpolateAt:)
('testing' containsPoint: containsRect: hasPositiveExtent intersects: is: isTall isWide)
('truncation and round off' encompassingIntegerRectangle rounded roundedHAFZ truncateTo: truncated)
('transforming' aligned:with: flippedBy:centerAt: newRect2From: newRectFrom: scaledBy: squishedWithin: translatedAndSquishedToBeWithin: translatedBy:)
('printing' printOn: storeOn:)
('private' setOrigin:corner:)
!


!LayoutMorph class reorganize!
('instance creation' initializedInstance new newColumn newForm newRow)
('LinearLayout examples' example1 example10 example101VG example11 example13 example1b example2 example20 example3 example6 example7 exampleEdgesColumn exampleEdgesRow iconsExample launcherExample)
('FormLayout examples' example1Form)
('new-morph participation' categoryInNewMorphMenu)
!

