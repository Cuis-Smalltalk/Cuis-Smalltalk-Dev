'From Cuis7.1 [latest update: #6770] on 14 October 2024 at 11:11:27 am'!

!PasteUpMorph methodsFor: 'drawing' stamp: 'RMV 10/4/2024 14:57:28'!
drawOn: aCanvas
	"draw background image"
	
	backgroundImage
		ifNil: [ super drawOn: aCanvas ]
		ifNotNil: [
			| effect |
			effect := PreferenceSet sysPreferences at: #backgroundEffect.
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
		]! !

!PasteUpMorph methodsFor: 'misc' stamp: 'RMV 10/4/2024 14:57:10'!
buildMagnifiedBackgroundImage
	"
	Here's an example of using the latest attached change set::

	filePath := '/Users/volkmannm/Pictures/images/altitude1600.jpg'.
	PreferenceSet sysPreferences at: #backgroundEffect put: #tile.
	stream := filePath asFileEntry readStream.
	self runningWorld backgroundImageData: stream binary contentsOfEntireFile.

	You can also set the #backgroundEffect preference to #cover or #stretch.
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
					[#tile] -> [image]
				} otherwise: [ image magnifyTo: extent "for #stretch" ].

				"Save some memory. Enable if desired."
 				"backgroundImage := backgroundImage orderedDither32To16 asColorFormOfDepth: 8."
				
				image := nil.
				Smalltalk primitiveGarbageCollect.
				backgroundImage bits pin.
				self redrawNeeded.
			] on: Error do: [backgroundImage := nil]. "Can happen if JPEG plugin not built"
		]! !


!WorldMorph methodsFor: 'drawing' stamp: 'RMV 10/3/2024 13:44:36'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			super drawOn: aCanvas ]
		ifNil: [
			"draw background fill"
			(aCanvas drawsOnDisplay and: [ color mightBeTranslucent ])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Cuis Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: `0@0` in: nil
						fillColor: color rule: Form over.
					Display forceToScreen]
				ifFalse: [ super drawOn: aCanvas ]]! !


!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 10/14/2024 11:08:49'!
installMiscPreferences
" Preference value defined with closure. We could insert in a dynamic array...
  Preference with event handler "
	self sysPreferences

		name: #haloHandleSize 
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 5 // 3 max: 16];

		name: #halosMorphBoundsFrameColor
		description: 'When set, use this color for a rectangle on halos morph bounds. Useful for debugging morph sizes.' 
		category: #gui 
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;

		name: #halosMorphBoundsFillColor
		description: 'When set, use this color for filling a rectangle on halos morph bounds. Useful for debugging morph sizes.'
		category: #gui
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;

		name: #roundedButtonRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 8 // 14];

		name: #roundedWindowRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize] ;

		name: #scrollbarThickness 
		category: #gui 		
		value: [(Preferences at: #windowTitleFont) pointSize + 2];

		name: #backgroundEffect
		description: 'determines how backgroundImage is applied'
		category: #gui
		type: Symbol
		value: #stretch;

		name: #classFinder 
		category: #programming 
		value: [ Smalltalk findAndBrowseClass ];

		name: #defaultAuthorName 
		category: #programming 
		value: [Utilities authorName];

		name: #worldMenu
		category: #system
		value: TheWorldMenu;

		name: #browserWindowClass
		category: #programming
		value: BrowserWindow;

		name: #clearPackagePathsOnImageMove
		category: #programming
		value: true.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences
		name: #backgroundEffect
		description: 'determines how backgroundImage is applied'
		category: #gui
		type: Symbol
		value: #stretch.
!

