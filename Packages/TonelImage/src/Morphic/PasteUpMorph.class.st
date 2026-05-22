"
My instances are free areas where you can play with Morphs. Most important, the World is an instance of my subclass WorldMorph.
"
Class {
	#name : 'PasteUpMorph',
	#superclass : 'ColoredBoxMorph',
	#instVars : [
		'backgroundImage',
		'backgroundImageData'
	],
	#category : 'Morphic-Kernel',
	#package : 'Morphic',
	#tag : 'Kernel'
}

{ #category : 'new-morph participation' }
PasteUpMorph class >> categoryInNewMorphMenu [
	^ 'Worlds'
]

{ #category : 'system startup' }
PasteUpMorph class >> initClassCachedState [

	self allSubInstancesDo: [ :each |
		each buildMagnifiedBackgroundImage.
		each redrawNeeded ].
	Cursor defaultCursor activateCursor.
]

{ #category : 'dropping/grabbing' }
PasteUpMorph >> allowsMorphDrop [
	"Answer whether we accept dropping morphs."

	^ true
]

{ #category : 'dropping/grabbing' }
PasteUpMorph >> allowsSubmorphDrag [
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo.
	Both 'aMorph allowsGrabWithHand' and 'aMorph owner allowsSubmorphDrag' must be true for aMorph to be grabbed by the hand. It is also required that 'aMorph handlesMouseDown:' be false."

	^ true
]

{ #category : 'misc' }
PasteUpMorph >> backgroundImage [
	^backgroundImage
]

{ #category : 'misc' }
PasteUpMorph >> backgroundImageData: aByteArray [
	"
	| filename |
	filename := 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	filename := 'bg/free-desktop-wallpaper.jpg'.
	filename := 'bg/jellyfish-thumb.jpg'.
	filename := 'bg/splash_by_beefpepsi.jpg'.
	filename := 'bg/gray ocean and pier.jpg'.
	filename := 'bg/newyork.jpg'.
	filename := 'bg/download-free-desktop-wallpaper-nature-conquestofparadise-marirs-pic.jpg'.
	filename := 'bg/desktop-wallpaper-tropical-1280x1024.jpg'.

	self runningWorld backgroundImageData: (filename asFileEntry readStream) binary contentsOfEntireFile.
	"
	backgroundImageData := aByteArray.
	self buildMagnifiedBackgroundImage
]

{ #category : 'misc' }
PasteUpMorph >> buildMagnifiedBackgroundImage [
	"
	Here's an example::

	filePath := '/Users/volkmannm/Pictures/images/altitude1600.jpg'.
	PreferenceSet sysPreferences at: #backgroundEffect put: #tile.
	stream := filePath asFileEntry readStream.
	self runningWorld backgroundImageData: stream binary contentsOfEntireFile.

	You can also set the #backgroundEffect preference to #cover, #stretch or #none.
	"
	| effect image scale |
		
	backgroundImage := nil.
	backgroundImageData
		ifNil: [ self redrawNeeded ]
		ifNotNil: [
			[
 				Smalltalk primitiveGarbageCollect.
				image := Form fromBinaryStream: backgroundImageData readStream.
				effect := PreferenceSet sysPreferences at: #backgroundEffect.
				backgroundImage := effect caseOf: {
					[#cover] -> [
						scale := (extent x / image width) max: (extent y / image height).
						image magnifyBy: scale.
					].
					[#stretch] -> [image magnifyTo: extent]
				} otherwise: [ image "for #tile and #none" ].

				"Save some memory. Enable if desired."
 				"backgroundImage := backgroundImage orderedDither32To16 asColorFormOfDepth: 8."
				
				image := nil.
				Smalltalk primitiveGarbageCollect.
				backgroundImage bits pin.
				self redrawNeeded.
			] on: Error do: [backgroundImage := nil]. "Can happen if JPEG plugin not built"
		]
]

{ #category : 'accessing' }
PasteUpMorph >> color: aColor [
	super color: aColor.
	self backgroundImageData: nil
]

{ #category : 'initialization' }
PasteUpMorph >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ `Color
		r: 0.861
		g: 1.0
		b: 0.722`
]

{ #category : 'initialization' }
PasteUpMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 1
]

{ #category : 'initialization' }
PasteUpMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ `Color
		r: 0.8
		g: 1.0
		b: 0.6`
]

{ #category : 'drawing' }
PasteUpMorph >> drawOn: aCanvas [
	"draw background image"
	
	backgroundImage
		ifNil: [ super drawOn: aCanvas ]
		ifNotNil: [
			| effect |
			effect := PreferenceSet sysPreferences at: #backgroundEffect.

			effect = #none ifTrue: [ super drawOn: aCanvas ].
			
			effect = #tile
				ifFalse: [ aCanvas image: backgroundImage at: `0 @ 0` ]
				ifTrue: [
					| height width x y |
					height := backgroundImage height.
					width := backgroundImage width.
					x := 0.
					y := 0.
					[ x < extent x ] whileTrue: [
						[ y < extent y ] whileTrue: [
							aCanvas image: backgroundImage at: x @ y.
							y := y + height
						].
						x := x + width.
						y := 0
					]
				]
		]
]

{ #category : 'geometry' }
PasteUpMorph >> extentChanged: oldExtent [
	"Our extent changed."

	super extentChanged: oldExtent.
	self buildMagnifiedBackgroundImage.
]

{ #category : 'event handling testing' }
PasteUpMorph >> handlesMouseDown: aMouseButtonEvent [
	^true
]

{ #category : 'testing' }
PasteUpMorph >> honorsLayerOrder [

	^true
]

{ #category : 'events' }
PasteUpMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse down event."

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent
		clkSel: #click:localPosition:.
]

{ #category : 'caching' }
PasteUpMorph >> releaseCachedState [
	super releaseCachedState.
	backgroundImage := nil.
]

{ #category : 'halos and balloon help' }
PasteUpMorph >> wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph [
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"
	(#(addHelpHandle: addRotateHandle: addRecolorHandle:) statePointsTo: aSelector)
		ifTrue: ["FIXME - hack to disable for non-functional halo items"
			^ false].
	
	^super wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
]

{ #category : 'stepping and presenter' }
PasteUpMorph >> wantsSteps [
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message.
	jmv: Not really sure. Sub-world stepping needs some review."

	^true
]

{ #category : 'event handling' }
PasteUpMorph >> wantsWindowEvent: anEvent [
	^false
]
