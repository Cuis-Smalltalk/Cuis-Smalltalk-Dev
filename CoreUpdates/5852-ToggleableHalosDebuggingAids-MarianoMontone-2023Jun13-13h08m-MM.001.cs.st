'From Cuis 6.0 [latest update: #5851] on 13 June 2023 at 1:10:26 pm'!

!PreferenceSet class methodsFor: 'sys preference' stamp: 'MM 5/30/2023 10:35:14'!
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
		
		name: #classFinder 
		category: #programming 
		value: [ BrowserWindow findClass ];
		
		name: #defaultAuthorName 
		category: #programming 
		value: [Utilities authorName];
		
		name: #worldMenu
		category: #system
		value: TheWorldMenu ! !


!HaloMorph methodsFor: 'drawing' stamp: 'MM 5/30/2023 10:37:04'!
drawOn: aCanvas 

	| b |
	target ifNil: [^super drawOn: aCanvas].
	
	"For debugging halos' morph bounds visually."
	
	"Fill the bounds of halo's morph. To activate:
	Preferences at: #halosMorphBoundsFillColor put: (Color blue alpha: 0.2)
	"
	(Preferences at: #halosMorphBoundsFillColor) ifNotNil: [:fillColor |
		aCanvas
			fillRectangle: self morphLocalBounds
			color: fillColor].
	
	"Draw a frame over halos morph bounds. To activate:
	Preferences at: #halosMorphBoundsFrameColor put: Color blue.
	"
	(Preferences at: #halosMorphBoundsFrameColor) ifNotNil: [:frameColor |
		aCanvas frameRectangle: self morphLocalBounds 
			borderWidth: 1 
			color: frameColor].
		
	target haloShowsCoordinateSystem ifTrue: [
		target displayFullBounds ifNotNil: [ :fb |
			aCanvas privateFrameGlobalRect: fb borderWidth: 3 color: `Color black alpha: 0.1`.
			b := target displayBounds.
			b = fb ifFalse: [
				aCanvas privateFrameGlobalRect: b borderWidth: 3 color: `Color black alpha: 0.1` ].
			self drawCoordinateSystemOn: aCanvas ]].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
PreferenceSet sysPreferences
	name: #halosMorphBoundsFrameColor
		description: 'When set, use this color for a rectangle on halos morph bounds. Useful for debugging morph sizes.' 
		category: #gui 
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;
	name: #halosMorphBoundsFillColor
		description: 'When set, use this color for filling a rectangle on halos morph bounds. Useful for debugging morph sizes.'
		category: #gui
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil.!

