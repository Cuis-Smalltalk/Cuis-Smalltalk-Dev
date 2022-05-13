'From Cuis 6.0 [latest update: #5146] on 12 May 2022 at 11:44:21 am'!

!PreferenceNG class methodsFor: 'class initialization' stamp: 'hlsf 5/12/2022 11:35:43'!
installMiscPreferences
" Preference value defined with closure. We could insert in a dynamic array...
  Preference with event handler "
	PreferenceNG 
		name: #haloHandleSize category: #gui 
		value: [(PreferenceNG at: #standardListFont) pointSize * 5 // 3 max: 16].
	PreferenceNG
		name: #roundedButtonRadius 	category: #gui 
		value: [(PreferenceNG at: #standardListFont) pointSize * 8 // 14].
	PreferenceNG
		name: #roundedWindowRadius 	category: #gui value: [(PreferenceNG at: #standardListFont) pointSize].
	PreferenceNG
		name: #scrollbarThickness category: #gui 		value: [(PreferenceNG at: #windowTitleFont) pointSize + 2].
	PreferenceNG name: #classFinder category: #programming value: [ BrowserWindow findClass ].
	PreferenceNG name: #defaultAuthorName category: #programming value: [Utilities authorName].! !


!WorldMorph methodsFor: 'world menu' stamp: 'hlsf 5/12/2022 11:43:42'!
invokeWorldMenu
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed"

	| menu |
	menu _ (TheWorldMenu new 
		world: self
		hand: self activeHand) buildWorldMenu.
	menu addTitle: PreferenceNG desktopMenuTitle.
	menu popUpInWorld: self! !


!HaloMorph methodsFor: 'private' stamp: 'hlsf 5/12/2022 11:36:53'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient. Return the handle."

	| handle aPoint colorToUse form icon e |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	e _ (PreferenceNG at: #haloHandleSize) asPoint.
	handle morphPosition: aPoint-(e//2) extent: e.
	handleSpec iconSymbol ifNotNil: [ :iconName |
			form _ self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				form extent = e ifFalse: [ 
					": Non default size, scale that bugger!!"
					form _ form  ": Be as smooth as possible, these images are small."
						magnify: form boundingBox
						to: e
						smoothing: 2 ].
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: `0@0` ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^ handle! !

!HaloMorph methodsFor: 'private' stamp: 'hlsf 5/12/2022 11:36:59'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph verticalNamePosition namePosition nameBackground |
	nameBackground _ BoxedMorph new noBorder
		color: ((target is: #SystemWindow)
			ifTrue: [target windowColor]
			ifFalse: [`Color lightBlue alpha: 0.9`]).
	nameMorph _ LabelMorph contents: aString.
	nameMorph color: `Color black`.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	verticalNamePosition _ haloBox bottom + (PreferenceNG at: #haloHandleSize).
	namePosition _ haloBox width - nameMorph morphWidth // 2 + haloBox left @ verticalNamePosition.
	self addMorph: nameBackground.
	nameBackground morphPosition: namePosition - 2.
	self addMorph: nameMorph.
	nameMorph morphPosition: namePosition.
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'hlsf 5/12/2022 11:37:08'!
handlesBox
	"handlesBox is in local coordinates.
	We assume we are direct submorph of the world, without any scaling or rotation."

	| minSide hs c e box |
	hs _ PreferenceNG at: #haloHandleSize.
	minSide _ 4 * hs.
	e _ extent + (hs*2) max: minSide@minSide.
	c _ extent // 2 + self morphPosition.
	box _ Rectangle center: c extent: e.
	self world ifNotNil: [ :w | box _ box intersect: (w viewBox insetBy: (hs@hs corner: hs@(hs*3))) ].
	"Make it local"
	^box translatedBy: self morphPosition negated.
! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'hlsf 5/12/2022 11:41:12'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue: [
		self isFindClassShortcut
			ifTrue: [ ^ (PreferenceNG at: #classFinder) value ].
		self isCloseWindowShortcut
			ifTrue: [ ^ self closeCurrentWindowOf: aMorph ].
		^ aMorph processKeystroke: self ].
	type == #keyDown ifTrue: [
		^ aMorph processKeyDown: self ].
	type == #keyUp ifTrue: [ 
		^ aMorph processKeyUp: self ].
	^ super sendEventTo: aMorph.! !


!Theme methodsFor: 'other options' stamp: 'hlsf 5/12/2022 11:36:17'!
roundedButtonRadius
	"Only effective if #roundButtons answers true.
	Provide a reasonable default for subclasses."
	^ PreferenceNG at: #roundedButtonRadius! !

!Theme methodsFor: 'other options' stamp: 'hlsf 5/12/2022 11:39:14'!
roundedWindowRadius
	"Only effective if #roundWindowCorners answers true.
	Provide a reasonable default for subclasses."
	^PreferenceNG at: #roundedWindowRadius! !

!Theme methodsFor: 'other options' stamp: 'hlsf 5/12/2022 11:40:09'!
scrollbarThickness
	^ PreferenceNG at: #scrollbarThickness! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'hlsf 5/12/2022 11:42:06'!
preambleTemplate
	"Answer a string that will form the default contents for a change set's preamble.
	Just a first stab at what the content should be."

	^ String streamContents: [:strm |
		strm nextPutAll: '"Change Set:'.  "NOTE: fileIn recognizes preambles by this string."
		strm tab;tab; nextPutAll: self name.
		strm newLine; nextPutAll: 'Date:'; tab; tab; tab; nextPutAll: Date today printString.
		strm newLine; nextPutAll: 'Author:'; tab; tab; tab; nextPutAll: (PreferenceNG at: #defaultAuthorName).
		strm newLine; newLine; nextPutAll: '<your descriptive text goes here>"']
"
ChangeSet changeSetForBaseSystem preambleTemplate
"! !

