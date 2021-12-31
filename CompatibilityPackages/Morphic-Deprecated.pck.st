'From Cuis 5.0 [latest update: #4758] on 16 August 2021 at 11:47:15 am'!
'Description '!
!provides: 'Morphic-Deprecated' 1 4!
SystemOrganization addCategory: 'Morphic-Deprecated'!


!classDefinition: #RectangleLikeMorph category: 'Morphic-Deprecated'!
PlacedMorph subclass: #RectangleLikeMorph
	instanceVariableNames: 'extent color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Deprecated'!
!classDefinition: 'RectangleLikeMorph class' category: 'Morphic-Deprecated'!
RectangleLikeMorph class
	instanceVariableNames: ''!

!classDefinition: #StringMorph category: 'Morphic-Deprecated'!
RectangleLikeMorph subclass: #StringMorph
	instanceVariableNames: 'font emphasis contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Deprecated'!
!classDefinition: 'StringMorph class' category: 'Morphic-Deprecated'!
StringMorph class
	instanceVariableNames: ''!

!classDefinition: #UpdatingStringMorph category: 'Morphic-Deprecated'!
StringMorph subclass: #UpdatingStringMorph
	instanceVariableNames: 'target getSelector stepTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Deprecated'!
!classDefinition: 'UpdatingStringMorph class' category: 'Morphic-Deprecated'!
UpdatingStringMorph class
	instanceVariableNames: ''!


!RectangleLikeMorph commentStamp: '<historical>' prior: 0!
Hierarchy for morphs that are rectangle like, and that occupy an area that can be specified as a Rectangle. This means that, besides being of rectangular shape, sides are aligned with local coordinate axes. Including rectangles with rounded corners and such. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape. Subclasses can add things like 'roundedCornerRadious' or such.!

!StringMorph commentStamp: '<historical>' prior: 0!
StringMorph is a "lightweight" Morph to display a String. It supports only a single font, color, and emphasis combination. For multiple text styles, use TextModelMorph.

Structure:
instance var    	Type              Description 
font 			StrikeFont 		(normally nil; then the accessor #font gives back  a Font or nil #defaultFont) 
emphasis 		SmallInteger	bitmask determining character attributes (underline, bold, 	italics, struckThrough) 
contents 		String 			The text that will be displayed. 
!

!UpdatingStringMorph commentStamp: 'jmv 1/5/2013 23:49' prior: 0!
UpdatingStringMorph new
     target: [self runningWorld activeHand morphPosition asString];
     getSelector: #value;
     stepTime: 10;
     openInWorld!

!StringMorph methodsFor: 'printing' stamp: 'jm 11/3/97 16:52'!
printOn: aStream

	super printOn: aStream.
	aStream print: contents.
! !

!UpdatingStringMorph class methodsFor: 'new-morph participation' stamp: 'jmv 9/5/2019 17:51:19'!
initializedInstance
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: Preferences windowTitleFont 
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value.
		
	^ newInst! !

!RectangleLikeMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 21:54'!
color

	^ color! !

!RectangleLikeMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 21:59'!
color: aColor
	"Set the receiver's color. "
	color = aColor ifFalse: [
		color _ aColor.
		self redrawNeeded ]! !

!RectangleLikeMorph methodsFor: 'initialization' stamp: 'jmv 3/10/2018 21:56:39'!
defaultColor
	^ `Color orange`! !

!RectangleLikeMorph methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:55:58'!
initialize
	super initialize.
	extent _ `50@40`.
	color _ self defaultColor! !

!RectangleLikeMorph methodsFor: 'drawing' stamp: 'jmv 4/14/2015 08:53'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		fillRectangle: self morphLocalBounds
		color: self color! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 7/24/2020 10:22:57'!
morphBounds: aRectangle
	self morphPosition: aRectangle topLeft.
	self morphExtent: aRectangle extent! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/22/2020 16:35:34'!
morphExtent
	"In our own coordinates!!"

	^ extent! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 15:52:14'!
morphExtent: newExtent
	"assume it is always in our coordinates!!"

	extent = newExtent ifFalse: [
		(self privateExtent: newExtent) ifTrue: [
			self someSubmorphPositionOrExtentChanged.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 7/8/2014 11:41'!
morphExtentInWorld: newExtent
	"world coordinates"
	self flag: #jmvVer2.
	self morphExtent: (self internalizeDistanceFromWorld: newExtent)! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:56'!
morphHeight

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	^ extent y! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/22/2020 16:36:56'!
morphHeight: aNumber

	self morphExtent: extent x@aNumber! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/16/2021 11:45:32'!
morphLocalBounds

	^`0@0` extent: extent.! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 15:53:07'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| someChange |
	someChange _ false.
	(location isTranslation: newPos) ifFalse: [
		location _ location withTranslation: newPos.
		someChange _ true ].

	extent = newExtent ifFalse: [
		(self privateExtent: newExtent) ifTrue: [
			someChange _ true ]].

	someChange ifTrue: [
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2020 11:19:35'!
morphTopLeft
	"Our hierarchy occupies a rectangle specified by #morphTopLef and #morphExtent"
	^`0@0`! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/22/2012 14:57'!
morphWidth

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	^ extent x! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/22/2020 16:37:29'!
morphWidth: aNumber

	self morphExtent: aNumber@extent y! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2014 09:59'!
privateExtent: aPoint
	"Answer whether extent was actually changed.
	If some subclass may reject the update, answer false in those cases."

	| newExtent |
	newExtent _ aPoint max: self minimumExtent.
	^extent = newExtent
		ifFalse: [ extent _ newExtent ]; not! !

!RectangleLikeMorph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:35:50'!
isOrthoRectangularMorph
	"True if we are of rectangular shape, aligned with local coordinate axes.
	Doesn't take into account possible rotation of our or some owner's local coordinates.
	Note: SystemWindow answers true, but implements #addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^true! !

!RectangleLikeMorph methodsFor: 'geometry testing' stamp: 'jmv 7/28/2020 12:16:53'!
morphContainsPoint: aLocalPoint
	"Answer true even if aLocalPoint is in a submorph, as long as it is in our shape"

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ self morphLocalBounds containsPoint: aLocalPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	"Can't do better. Please redefine in subclasses as appropriate!! (or finish Morphic 3!!)"
	"Mhhh. Podria usar el #imageForm: y ver si es transparente... deberia andar"
	^ self morphLocalBounds containsPoint: aLocalPoint! !

!RectangleLikeMorph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:35:08'!
requiresVectorCanvas
	"True if we use VectorCanvas protocol, or we are rotated or zoomed.
	If false, we occupy a Rectangle in local coordinates. Allows for many optimizations to be done.
	Note: If answer is false, but some owner requiresVectorCanvas, then we also do. Canvas takes care of this.
	Note: SystemWindow answers false, but implements #addPossiblyUncoveredAreasIn:to: to take care of rounded corners."

	^location isPureTranslation not! !

!RectangleLikeMorph methodsFor: 'geometry testing' stamp: 'jmv 7/18/2020 21:38:44'!
submorphsMightProtrude
	"Answered false by morphs that can guarantee that submorphs, and all morphs in their submorphs tree, will lie inside their bounds,
	either 'naturally' (by construction) or by being clipped by their owner (#clipsLastSubmorph). Allows for many optimizations to be done."

	^false! !

!RectangleLikeMorph methodsFor: 'layout-properties' stamp: 'jmv 2/16/2016 13:05'!
layoutSpec: aLayoutSpec
	"Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout"
	self layoutSpec == aLayoutSpec ifTrue: [ ^self ].
	aLayoutSpec morph: self.
	layoutSpec := aLayoutSpec.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!RectangleLikeMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:47:05'!
categoryInNewMorphMenu
	^ 'Kernel'! !

!StringMorph methodsFor: 'printing' stamp: 'efc 2/22/2003 21:35'!
font: aFont 
	"Set the font my text will use. The emphasis remains unchanged."

	font _ aFont.
	^ self font: font emphasis: emphasis! !

!StringMorph methodsFor: 'accessing'!
contents

	^ contents! !

!StringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:24'!
contents: newContents
	contents _ (newContents is: #Text)
		ifTrue: [
			emphasis _ newContents emphasisAt: 1.
			 newContents string ]
		ifFalse: [
			contents = newContents ifTrue: [ ^self ].	"no substantive change"
			newContents].
	self fitContents.
	self redrawNeeded! !

!StringMorph methodsFor: 'accessing' stamp: 'ar 1/31/2001 19:33'!
font
	"who came up with #fontToUse rather than font?!!"
	^self fontToUse! !

!StringMorph methodsFor: 'accessing' stamp: 'jmv 6/6/2014 10:58'!
font: aFont emphasis: emphasisCode
	font _ aFont.
	emphasis _ emphasisCode.
	self fitContents.
	self redrawNeeded
"
in inspector say,
	 self font: StrikeFont default emphasis: 1
"! !

!StringMorph methodsFor: 'accessing' stamp: 'jmv 6/11/2020 10:03:16'!
fontToUse
	| fontToUse |
	fontToUse := font ifNil: [FontFamily defaultFamilyAndPointSize].
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [ fontToUse]
		ifFalse: [ fontToUse emphasized: emphasis]! !

!StringMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:52:32'!
drawOn: aCanvas
	aCanvas
		drawString: (contents ifNil: [ '' ])
		at: self morphTopLeft 
		font: self fontToUse
		color: color! !

!StringMorph methodsFor: 'font' stamp: 'jmv 7/31/2015 00:59'!
emphasis: aNumber
	"Set the receiver's emphasis as indicated. aNumber is a bitmask with the following format:

	bit	attribute
	1	bold
	2	italic
	4	underlined
	8	struckThrough
	16	withUnderscoreGlyphs
	"

	"examples: 0 -> plain.  
	1 -> bold.  2 -> italic.  3 -> bold italic.  4 -> underlined  
	5 -> bold underlined.  6 -> italic underlined.   7 -> bold italic underlined   
	etc..."

	emphasis _ aNumber.
	^ self font: font emphasis: emphasis! !

!StringMorph methodsFor: 'halos and balloon help' stamp: 'sw 6/15/1998 15:34'!
addOptionalHandlesTo: aHalo box: box
	self flag: #deferred.

	"Eventually...
	self addFontHandlesTo: aHalo box: box"! !

!StringMorph methodsFor: 'initialization' stamp: 'cbr 12/3/2010 23:28'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Theme current menuText! !

!StringMorph methodsFor: 'initialization' stamp: 'jmv 11/10/2008 13:21'!
initWithContents: aString font: aFont emphasis: emphasisCode 
	self initialize.
	
	font _ aFont.
	emphasis _ emphasisCode.
	self contents: aString! !

!StringMorph methodsFor: 'initialization' stamp: 'jmv 12/9/2016 09:09:45'!
initialize
	super initialize.
	font _ nil.
	emphasis _ 0.
	self contents: 'String Morph'
! !

!StringMorph methodsFor: 'menu' stamp: 'jmv 2/22/2011 22:51'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'change font' action: #changeFont.
	aCustomMenu add: 'change emphasis' action: #changeEmphasis.
! !

!StringMorph methodsFor: 'menu' stamp: 'jmv 12/12/2014 15:53'!
changeEmphasis

	| reply aList |
	aList _ #(normal bold italic underlined struckThrough).
	reply _ (SelectionMenu labelList: aList selections: aList) startUpMenu.
	reply ifNotNil:[
		self emphasis: (TextEmphasis perform: reply) emphasisCode.
	].
! !

!StringMorph methodsFor: 'menu' stamp: 'jmv 4/15/2014 09:25'!
changeFont
	| newFont |
	newFont _ AbstractFont fromUser: self fontToUse.
	newFont ifNotNil:[self font: newFont].! !

!StringMorph methodsFor: 'geometry' stamp: 'jmv 3/12/2018 15:56:36'!
fitContents
	"Measures contents later at #minimumExtent"
	self morphExtent: `0@0`! !

!StringMorph methodsFor: 'geometry' stamp: 'jmv 4/24/2019 20:45:28'!
measureContents
	| f |
	f _ self fontToUse.
	^((f widthOfString: contents) max: 3)  @ f lineSpacing! !

!StringMorph methodsFor: 'geometry' stamp: 'KenD 12/22/2013 20:34'!
minimumExtent

	^ self measureContents ! !

!StringMorph class methodsFor: 'instance creation' stamp: 'sw 8/22/97 22:19'!
contents: aString
	" 'StringMorph contents: str' is faster than 'StringMorph new contents: str' "
	^ self contents: aString font: nil! !

!StringMorph class methodsFor: 'instance creation' stamp: 'di 4/1/1999 17:15'!
contents: aString font: aFont
	^ self basicNew initWithContents: aString font: aFont emphasis: 0! !

!StringMorph class methodsFor: 'instance creation' stamp: 'di 4/1/1999 17:15'!
contents: aString font: aFont emphasis: emphasisCode
	^ self basicNew initWithContents: aString font: aFont emphasis: emphasisCode! !

!StringMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:55:41'!
categoryInNewMorphMenu
	^ 'Basic'! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
getSelector: aSymbol
	getSelector _ aSymbol! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'KenD 4/26/2020 07:11:30'!
showPrintStringFor: anObject

	self contents: anObject printString! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:18'!
stepTime

	^stepTime! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
stepTime: aNumber
	stepTime _ aNumber! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
target: anObject
	target _ anObject! !

!UpdatingStringMorph methodsFor: 'initialization' stamp: 'jmv 9/13/2013 09:23'!
initialize
	super initialize.
	target _ self.
	getSelector _ #contents.
	stepTime _ 50! !

!UpdatingStringMorph methodsFor: 'stepping' stamp: 'pb 7/15/2017 22:07:40'!
stepAt: millisecondSinceLast

	self contents: (target perform: getSelector) asString! !

!UpdatingStringMorph methodsFor: 'stepping' stamp: 'jmv 1/4/2013 13:18'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run"

	^true! !

!UpdatingStringMorph methodsFor: 'geometry' stamp: 'jmv 1/15/2017 14:25:27'!
fitContents
	"Don't shrink each time contents change.
	Might shrink during layout"
	self morphExtent: (extent max: self measureContents)! !

!UpdatingStringMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/9/2017 00:04:50'!
categoryInNewMorphMenu
	^ 'Widgets'! !
