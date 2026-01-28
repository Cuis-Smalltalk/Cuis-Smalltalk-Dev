'From Cuis7.7 [latest update: #7816] on 28 January 2026 at 9:21:18 am'!

!Cursor commentStamp: 'jmv 28/Jan/2026 09:09:37' prior: 0!
I am a Form that is a possible appearance for a mouse cursor.  My size is always 16x16, ever since the original implementation on the Alto.

Predefined cursors should accessed via #cursorAt: which caches Cursor instances.  For example "Cursor cursorAt: #normalCursor".  You can also dynamically add your own cursors or modify existing ones via #cursorAt:put: as desired.!

!classDefinition: #CursorWithAlpha category: #'Graphics-Display Objects'!
Cursor subclass: #CursorWithAlpha
	instanceVariableNames: 'fallback '
	classVariableNames: 'Constants '
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!CursorWithMask commentStamp: 'jmv 28/Jan/2026 09:11:20' prior: 0!
A Cursor which additionally has a 16x16 transparency bitmap called a "mask".  See #derivedFrom: for details on how the mask is treated.!


!CursorWithAlpha methodsFor: 'testing' stamp: 'jmv 28/Jan/2026 09:02:59'!
hasMask
	"Alpha behaves quite like a mask. Please don't convert into a CursorWithMask."
	^true! !


!Cursor class methodsFor: 'accessing' stamp: 'jmv 28/Jan/2026 08:31:16'!
cursorAt: cursorKey ifAbsent: aBlock
	^ (CursorDict
		at: cursorKey
		ifAbsent: [
			(self respondsTo: cursorKey) ifTrue: [ | newCursor |
				newCursor := self perform: cursorKey.
				newCursor hasMask ifFalse: [ newCursor := newCursor withMask ].
				(Preferences at: #biggerCursors) ifTrue: [
					newCursor := newCursor asBigCursor ].
				self
					cursorAt: cursorKey
					put: newCursor ]])
	ifNil: aBlock! !

!Cursor class methodsFor: 'accessing' stamp: 'jmv 28/Jan/2026 09:19:48'!
defaultCursor
	"Answer the instance of me that is the shape of an arrow slanted left.
	Just a convenience to access #normalCursor."

	^ self cursorAt: #normalCursor! !

!Cursor class methodsFor: 'class initialization' stamp: 'jmv 28/Jan/2026 08:21:04'!
initialize
	CursorDict := Dictionary new.
	self currentCursor: self defaultCursor.! !

!Cursor class methodsFor: 'current cursor' stamp: 'jmv 28/Jan/2026 08:31:38'!
currentCursor: aCursor
	"Make the instance of cursor, aCursor, be the current cursor. Display it. "

	CurrentCursor := aCursor.
	CurrentCursor installCursor.! !

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 28/Jan/2026 09:09:17'!
normalCursor
	"Answer the default top left pointing arrow cursor"

	^ ((CursorWithMask
		small1BitExtent: `16@16`
		pixelBits: #(
			2r0000000000000000
			2r0100000000000000
			2r0110000000000000
			2r0111000000000000
			2r0111100000000000
			2r0111110000000000
			2r0111111000000000
			2r0111111100000000
			2r0111111110000000
			2r0111110000000000
			2r0110110000000000
			2r0100011000000000
			2r0000011000000000
			2r0000001100000000
			2r0000001100000000
			2r0000000000000000)
		offset: `-1@ -1`)
	setMaskForm: 
		(Form
			small1BitExtent: `16@16`
			pixelBits: #(
				2r1100000000000000
				2r1110000000000000
				2r1111000000000000
				2r1111100000000000
				2r1111110000000000
				2r1111111000000000
				2r1111111100000000
				2r1111111110000000
				2r1111111111000000
				2r1111111111100000
				2r1111111000000000
				2r1110111100000000
				2r1100111100000000
				2r1000011110000000
				2r0000011110000000
				2r0000001110000000)
			offset: `0@0`))! !

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 28/Jan/2026 08:55:10'!
resizeBottomCursor
	^ self resizeTopCursor! !

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 28/Jan/2026 08:55:17'!
resizeBottomLeftCursor
	^ self resizeTopRightCursor! !

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 28/Jan/2026 08:55:27'!
resizeBottomRightCursor
	^ self resizeTopLeftCursor! !

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 28/Jan/2026 08:55:31'!
resizeRightCursor
	^ self resizeLeftCursor! !


!CursorWithAlpha methodsFor: 'accessing' stamp: 'jmv 28/Jan/2026 09:07:17'!
fallback
	^fallback ifNil: [self class cursorAt: #normalCursor]! !


!CursorWithMask class methodsFor: 'as yet unclassified' stamp: 'jmv 28/Jan/2026 09:10:46'!
derivedFrom: aForm
	"aForm is presumably a cursor"
	| cursor mask ext |
	ext := aForm extent.
	cursor := self extent: ext.
	cursor copy: (1@1 extent: ext) from: `0@0` in: aForm rule: Form over.
	mask := Form extent: ext.
	(1@1) eightNeighbors do:
		[:p | mask copy: (p extent: ext) from: `0@0` in: aForm rule: Form under].
	cursor setMaskForm: mask.
	cursor offset: ((aForm offset - (1@1)) max: ext negated).
	^ cursor! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 28/Jan/2026 09:12:05'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus := nil.
	mouseFocus := nil.
	extent := Cursor defaultCursor extent.
	grabMorphData := IdentityDictionary new.
	self initForEvents.! !


!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 28/Jan/2026 08:24:37'!
defaultFontSize: aPreference
	"
	Preferences at: #defaultFontSize put: '12'
	Preferences at: #defaultFontSize put: 14
	Preferences at: #defaultFontSize put: 21
	"
	| pointSize font titleFont |
	pointSize := aPreference value.
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
	Preferences at: #biggerCursors put: (pointSize > 13). Cursor initialize.
	Cursor defaultCursor activateCursor.
	MenuItemMorph initializeSubMenuMarker.
	^ font! !

!methodRemoval: CursorWithAlpha class #biggerNormal stamp: 'jmv 28/Jan/2026 09:19:53'!
CursorWithAlpha class removeSelector: #biggerNormal!
!methodRemoval: CursorWithAlpha class #constants stamp: 'jmv 28/Jan/2026 09:19:57'!
CursorWithAlpha class removeSelector: #constants!
!methodRemoval: Cursor class #normalCursorWithMask stamp: 'jmv 28/Jan/2026 09:09:42'!
Cursor class removeSelector: #normalCursorWithMask!

!Cursor reorganize!
('displaying' activateCursor showWhile:)
('converting' asBigCursor asCursorForm enlargedBy: withMask)
('printing' printOn:)
('testing' hasMask is:)
('primitives' installCursor installCursorWithMask:)
!

Cursor initialize!
!classDefinition: #CursorWithAlpha category: #'Graphics-Display Objects'!
Cursor subclass: #CursorWithAlpha
	instanceVariableNames: 'fallback'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!CursorWithAlpha reorganize!
('accessing' fallback fallback:)
('converting' asCursorForm)
('testing' hasMask)
('primitives' installCursor)
!


!CursorWithMask reorganize!
('mask' maskForm setMaskForm: storeOn:base: withMask)
('converting' asCursorForm)
('testing' hasMask)
('primitives' installCursor)
!

