'From Cuis 4.2 of 25 July 2013 [latest update: #1823] on 13 September 2013 at 10:03:06.554065 am'!

!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 9/13/2013 09:41'!
initializedInstance
	"Answer an instance of the receiver which in some sense is initialized.  In the case of Morphs, this will yield an instance that can be attached to the Hand after having received the same kind of basic initialization that would be obtained from an instance chosen from the 'new morph' menu.
	Return nil if the receiver is reluctant for some reason to return such a thing"

	^ self contents: 'A Hover Help'! !


!InnerPluggableMorph class methodsFor: 'new-morph participation' stamp: 'jmv 9/13/2013 10:00'!
includeInNewMorphMenu
	"Not to be instantiated from the menu"
	^ false! !


!IndentingListItemMorph class methodsFor: 'new-morph participation' stamp: 'jmv 9/13/2013 10:00'!
includeInNewMorphMenu
	"Not to be instantiated from the menu"
	^ false! !


!MagnifierMorph methodsFor: 'initialization' stamp: 'jmv 9/13/2013 10:02'!
initialize
	super initialize.
	trackPointer _ true.
	magnification _ 2.
	self morphExtent: 128@128! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 9/13/2013 09:37'!
addStayUpIcons
	| closeBox pinBox w |
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	closeBox _ PluggableButtonMorph model: self action: #delete.
	closeBox icon: Theme current closeIcon.
	pinBox _ PluggableButtonMorph model: self action: #stayUp.
	pinBox icon: Theme current pushPinIcon.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 60.
	self addMorphFront: 
		(LayoutMorph newRow
			morphHeight: (titleMorph morphHeight max: 19);
			morphWidth: w;	"Make room for buttons"
			color: Color transparent;
			addMorph: closeBox fixedWidth: 20;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: pinBox fixedWidth: 20).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 9/13/2013 09:36'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w |
	
	titleMorph _ RectangleLikeMorph new.
	titleMorph color: Theme current menuTitleBar.
	pp _ 8@2.
	aString asString linesDo: [ :line |
		s _ StringMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s.
		s morphPosition: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphHeight: pp y; morphWidth: w + 16.
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !


!PasteUpMorph methodsFor: 'accessing' stamp: 'jmv 9/13/2013 09:15'!
activeHand
	"Answer the currently active hand, if any..."
	^worldState
		ifNotNil: [ :ws | ws activeHand ]
		ifNil: [ self world ifNotNil: [ :w | w activeHand ]]! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 9/13/2013 09:18'!
fullRepaintNeeded
	self redrawNeeded.
	SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ifTrue: [ w makeMeVisible ].
			false ]! !


!OneLineEditorMorph methodsFor: 'initialization' stamp: 'jmv 9/13/2013 09:13'!
initialize
	super initialize.
	contents _ ''.
	font _ nil.
	emphasis _ 0.
	showCaret _ false. "Should never be nil"! !


!UpdatingStringMorph methodsFor: 'initialization' stamp: 'jmv 9/13/2013 09:23'!
initialize
	super initialize.
	target _ self.
	getSelector _ #contents.
	stepTime _ 50! !

