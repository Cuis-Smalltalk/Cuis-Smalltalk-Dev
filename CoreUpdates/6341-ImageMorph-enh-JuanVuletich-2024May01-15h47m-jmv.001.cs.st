'From Cuis6.3 [latest update: #6340] on 1 May 2024 at 3:54:44 pm'!

!ImageMorph class methodsFor: 'instance creation' stamp: 'jmv 5/1/2024 15:51:56'!
new
	^super new image: self defaultForm! !

!ImageMorph class methodsFor: 'instance creation' stamp: 'jmv 5/1/2024 15:52:45'!
newWith: aForm
	^super new image: aForm! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 5/1/2024 15:52:59'!
retrieveMorph
	"Answer a Morph or nil."
	| answer |
	
	answer := self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer is: #Morph) ifTrue: [
		^ answer ].

	"If clipboard contains a Form, prepare a Morph including it."
	(answer is: #Form) ifTrue: [
		^ ImageMorph newWith: answer ].

	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^ (TextModelMorph withText: answer asText)
			embeddedInMorphicWindowLabeled: 'Text editor' ].
	
	"No useful contents"
	^nil.! !


!ImageReadWriter class methodsFor: 'instance creation' stamp: 'jmv 5/1/2024 15:53:04'!
imageMorphFromFileEntry: imageFileEntry
	"Import an image from a file"
	
	(ImageMorph newWith: (ImageReadWriter formFromFileEntry: imageFileEntry) ) openInWorld.
	! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 5/1/2024 15:53:24'!
iconsExample
	"
	LayoutMorph iconsExample
	"
	| lm ilm counter |
	lm := LayoutMorph newColumn.
	ilm := LayoutMorph newRow.
	counter := 1.
	Theme methodsDo: [ :method | | sel | 
		(sel := method selector) numArgs = 0 ifTrue: [
			(sel endsWith: 'Icon') ifTrue: [ | mayBeForm |
				mayBeForm := Theme current perform: sel.
				(mayBeForm isKindOf: Form) ifTrue: [ | imageMorph |
					counter \\ 10 = 0 ifTrue: [
						lm addMorph: ilm.
						ilm := LayoutMorph newRow ].
					counter := counter + 1.
					ilm addMorph: (imageMorph := ImageMorph newWith: (mayBeForm magnifyBy: 2)).
					imageMorph 
						setBalloonText: 'Copy ''', sel storeString bold, ''' to the Clipboard';
						setProperty: #'handlesMouseDown:' toValue: true;
						setProperty: #mouseButton1Down:localPosition:  toValue: [ :event :pos |
							Clipboard storeObject: sel storeString ] ] ] ] ].
	lm 
		setProperty: #taskbarButtonIcon toValue: Theme current imageIcon;
		setBalloonText: 'Drag me here!!'.
	lm openInWorld
! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 5/1/2024 15:53:52'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient. Return the handle."

	| handle aPoint colorToUse form icon e |
	aPoint := self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse := Color colorFrom: handleSpec color.
	handle := HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	e := (Preferences at: #haloHandleSize) asPoint.
	handle morphPosition: aPoint-(e//2) extent: e.
	handleSpec iconSymbol ifNotNil: [ :iconName |
			form := self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				form extent = e ifFalse: [ 
					": Non default size, scale that bugger!!"
					form := form  ": Be as smooth as possible, these images are small."
						magnify: form boundingBox
						to: e
						smoothing: 2 ].
				icon := (ImageMorph newWith: form)
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: `0@0` ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^ handle! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 5/1/2024 15:54:13'!
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
	marker := ImageMorph newWith: marker.
	self addMorphFront: marker position: 1@(self morphHeight +1- marker morphHeight // 2).
	marker lock.! !

!methodRemoval: ImageMorph #initialize stamp: 'jmv 5/1/2024 15:52:03'!
ImageMorph removeSelector: #initialize!

!ImageMorph class reorganize!
('cached state access' defaultForm releaseClassCachedState)
('new-morph participation' categoryInNewMorphMenu)
('instance creation' new newWith:)
!

