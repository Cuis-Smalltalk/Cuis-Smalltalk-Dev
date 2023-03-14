'From Cuis 6.0 [latest update: #5689] on 14 March 2023 at 3:25:53 pm'!

!MenuItemMorph class methodsFor: 'cached state access' stamp: 'HAW 3/14/2023 15:24:59'!
initializeSubMenuMarker

	| rightArrowForm |
	
	rightArrowForm := BitBltCanvas buildArrowOfDirection: #right size: FontFamily defaultPointSize depth: 8.
	SubMenuMarker := ColorForm mappingWhiteToTransparentFrom: rightArrowForm.
	! !


!BitBltCanvas class methodsFor: 'cached button forms' stamp: 'HAW 3/14/2023 15:10:18'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger depth: depth
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	 (self buildArrowOfDirection: #up size: 120) display
	"

	| resizeFactor resizedForm f e c bottomMargin topMargin |
	resizeFactor := 4.
	e := finalSizeInteger@finalSizeInteger*resizeFactor.
	f := Form extent: e depth: depth.
	c := `Color gray: 0.5`.
	topMargin := finalSizeInteger * 3//4.
	bottomMargin := finalSizeInteger * 4//4.
	0 to: e y-1-bottomMargin do: [ :y |
		0 to: e x -1 do: [ :x |
			(e x / 2 - 1 - x) abs * 2 + topMargin < y ifTrue: [
				f colorAt: x@y put: c
			]
		]
	].
	resizedForm := f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.

	aSymbolDirection == #right ifTrue: [
		resizedForm := resizedForm rotatedByDegrees: 90 ].
	aSymbolDirection == #down ifTrue: [
		resizedForm := resizedForm rotatedByDegrees: 180 ].
	aSymbolDirection == #left ifTrue: [
		resizedForm := resizedForm rotatedByDegrees:  270 ].
		
	aSymbolDirection == #up ifFalse: [
		resizedForm := resizedForm
			copy: (resizedForm boundingBox insetBy: (resizedForm width - finalSizeInteger/ 2.0) rounded) ].
		
	^resizedForm! !


!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'HAW 3/14/2023 15:23:13'!
defaultFontSize: guiElementSizePref
" #guiElementSize changed "
	| font titleFont pointSize |
	pointSize := self fontSizes at: guiElementSizePref value.
	font := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize.
	Preferences name: #standardListFont category: #font value: font.
	Preferences name: #standardMenuFont category: #font value: font.
	Preferences name: #standardCodeFont category: #font value: font.
	Preferences name: #standardButtonFont category: #font value: font.
	FontFamily defaultFamilyName: font familyName defaultPointSize: pointSize.	
	titleFont := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	Preferences name: #windowTitleFont category: #font value: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	Preferences at: #biggerCursors put: (pointSize > 14).
	Cursor defaultCursor activateCursor.
	MenuItemMorph initializeSubMenuMarker.
	^ font! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'HAW 3/14/2023 15:19:37'!
drawOn: aCanvas 
	| stringColor leftEdge magnifiedIcon |

	stringColor := color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: self morphLocalBounds color: Theme current menuHighlight].
	leftEdge := 0.

	self hasMarker ifTrue: [
		leftEdge := leftEdge + submorphs first morphWidth + 8 ].

	icon ifNotNil: [
		magnifiedIcon := self magnifiedIcon.
		aCanvas image: magnifiedIcon at: leftEdge+1 @ (magnifiedIcon height *1//10).
		leftEdge := magnifiedIcon width *12//10 + leftEdge].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: self class subMenuMarker
			at: extent x - self class subMenuMarker width @ (extent y - self class subMenuMarker height // 2) ]! !


!MenuItemMorph class methodsFor: 'cached state access' stamp: 'HAW 3/14/2023 15:25:15'!
subMenuMarker

	SubMenuMarker ifNotNil: [ ^SubMenuMarker ].
	
	self initializeSubMenuMarker.
	
	^SubMenuMarker! !


!BitBltCanvas class methodsFor: 'cached button forms' stamp: 'HAW 3/14/2023 15:10:37'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	 (self buildArrowOfDirection: #up size: 120) display
	"

	^self buildArrowOfDirection: aSymbolDirection size: finalSizeInteger depth: 32! !

