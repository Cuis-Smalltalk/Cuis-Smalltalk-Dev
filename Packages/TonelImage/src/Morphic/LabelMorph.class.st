"
LabelMorph is a "lightweight" Morph to display a String. It supports only a single font, color, and emphasis combination. For multiple text styles, use TextModelMorph or TextParagraphMorph.

Structure:
instance var    	Type              Description 
font 			Font 		(normally nil; then the accessor #font gives back  a Font or nil #defaultFont) 
emphasis 		SmallInteger	bitmask determining character attributes (underline, bold, 	italics, struckThrough) 
contents 		String 			The text that will be displayed. 

"
Class {
	#name : 'LabelMorph',
	#superclass : 'BorderedBoxMorph',
	#instVars : [
		'font',
		'emphasis',
		'contents'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
LabelMorph class >> categoryInNewMorphMenu [
	^ 'Basic'
]

{ #category : 'instance creation' }
LabelMorph class >> contents: aString [

	^ self contents: aString font: nil
]

{ #category : 'instance creation' }
LabelMorph class >> contents: aString font: aFont [
	^ self basicNew initWithContents: aString font: aFont emphasis: 0
]

{ #category : 'instance creation' }
LabelMorph class >> contents: aString font: aFont emphasis: emphasisCode [
	^ self basicNew initWithContents: aString font: aFont emphasis: emphasisCode
]

{ #category : 'instance creation' }
LabelMorph class >> contents: aString fontSize: pointSize [
	"
	(LabelMorph contents: 'I am a LabelMorph' fontSize: 25) openInWorld.
	"
	^ self basicNew
		initWithContents: aString
		font: (FontFamily defaultFamilyPointSize: pointSize)
		emphasis: 0.
]

{ #category : 'instance creation' }
LabelMorph class >> contents: aString fontSize: pointSize emphasis: emphasisCode [
	"
	(LabelMorph contents: 'I am a LabelMorph' fontSize: 25 emphasis: 3) openInWorld.
	"
	^ self basicNew
		initWithContents: aString
		font: (FontFamily defaultFamilyPointSize: pointSize)
		emphasis: emphasisCode.
]

{ #category : 'menu' }
LabelMorph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'change font' action: #changeFont.
	aCustomMenu add: 'change font size' action: #changeFontSize.
	aCustomMenu add: 'change emphasis' action: #changeEmphasis.

]

{ #category : 'menu' }
LabelMorph >> changeEmphasis [

	| reply aList |
	aList := #(normal bold italic underlined struckThrough).
	reply := (SelectionMenu labelList: aList selections: aList) startUpMenu.
	reply ifNotNil:[
		self emphasis: (TextEmphasis perform: reply) emphasisCode.
	].

]

{ #category : 'menu' }
LabelMorph >> changeFont [
	| currentFont |
	currentFont := self fontToUse.
	(FontFamily promptUser: currentFont familyName) ifNotNil: [ :fontFamily |
		self font: (fontFamily atPointSize: currentFont pointSize) ].
]

{ #category : 'menu' }
LabelMorph >> changeFontSize [
	| fontFamily |
	fontFamily := self fontToUse family.
	(FontFamily
		promptUserForSize: self fontToUse pointSize
		fontFamily: fontFamily) ifNotNil: [ :ptSize |
			self font: (fontFamily atPointSize: ptSize) ].
]

{ #category : 'accessing' }
LabelMorph >> contents [

	^ contents
]

{ #category : 'accessing' }
LabelMorph >> contents: newContents [
	contents := (newContents is: #Text)
		ifTrue: [
			emphasis := newContents emphasisAt: 1.
			color := newContents colorAt: 1.
			font := newContents fontAt: 1 default: font.
			newContents string ]
		ifFalse: [
			contents = newContents ifTrue: [ ^self ].	"no substantive change"
			newContents].
	self fitContents.
	self redrawNeeded
]

{ #category : 'accessing' }
LabelMorph >> contents: newContents constrainedTo: aWidth [
"
	Label may need to be displayed given a constrained width
"
	| f adjustedContents |
	f := self font.
	adjustedContents := (f widthOfString: newContents ) > aWidth 
		ifTrue: [" label to be truncated, adjsust the avalaible space for label considering the additional ellipsis "
			newContents truncateWithElipsisTo: (aWidth - (f widthOfString: '...') / f averageCharWidth) rounded]
		ifFalse: [newContents].
	self contents: adjustedContents 

]

{ #category : 'accessing' }
LabelMorph >> contentsFromPrintString: anObject [

	self contents: anObject printString
]

{ #category : 'initialization' }
LabelMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 0
]

{ #category : 'initialization' }
LabelMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ Theme current menuText
]

{ #category : 'drawing' }
LabelMorph >> drawOn: aCanvas [
	aCanvas
		drawString: (contents ifNil: [ '' ])
		at: self interiorOrigin 
		font: self fontToUse
		color: color.
	(borderWidth > 0) ifTrue: [
		aCanvas
			frameRectangle: (`0@0` extent: extent)
			color: borderColor
			borderWidth: borderWidth
			borderStyleSymbol: #simple ]
]

{ #category : 'font' }
LabelMorph >> emphasis: aNumber [
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
	etc...

	Prefer AbstractFont method category: 'derivatives - emphasis' 
	to raw numbers:
	    self emphasis: AbstractFont boldItalic.
	etc."

	self font: font emphasis: aNumber.
]

{ #category : 'geometry' }
LabelMorph >> fitContents [
	"Measures contents later at #minimumExtent"
	self morphExtent: `0@0`
]

{ #category : 'accessing' }
LabelMorph >> font [
	"who came up with #fontToUse rather than font?!"
	^self fontToUse
]

{ #category : 'font' }
LabelMorph >> font: aFont [
	"Set the font my text will use. The emphasis remains unchanged."

	self font: aFont emphasis: emphasis.
]

{ #category : 'accessing' }
LabelMorph >> font: aFont emphasis: emphasisCode [
	font := aFont.
	emphasis := emphasisCode.
	self fitContents.
	self redrawNeeded
"
in inspector say,
	 self font: AbstractFont fromUser emphasis: 1
"
]

{ #category : 'font' }
LabelMorph >> fontFamily: aFontFamily [
	"Set the font my text will use. The emphasis remains unchanged."

	self
		font: (aFontFamily atPointSize: (font
			ifNotNil: [ font pointSize ]
			ifNil: [ FontFamily defaultPointSize ]))
		emphasis: emphasis.
]

{ #category : 'geometry' }
LabelMorph >> fontPreferenceChanged [

	super fontPreferenceChanged.
	self font: (Preferences at: #standardListFont).
	self fitContents.
]

{ #category : 'accessing' }
LabelMorph >> fontToUse [
	| fontToUse |
	fontToUse := font ifNil: [FontFamily defaultFamilyAndPointSize].
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [ fontToUse]
		ifFalse: [ fontToUse emphasized: emphasis]
]

{ #category : 'initialization' }
LabelMorph >> initWithContents: aString font: aFont emphasis: emphasisCode  [
	self initialize.
	
	font := aFont.
	emphasis := emphasisCode.
	self contents: aString
]

{ #category : 'initialization' }
LabelMorph >> initialize [
	super initialize.
	font := nil.
	emphasis := 0.
	self contents: 'Label Morph'

]

{ #category : 'accessing' }
LabelMorph >> label [
	"Answer the name to show in a list of windows-and-morphs to represent the receiver"

	^contents
]

{ #category : 'geometry' }
LabelMorph >> measureContents [
	| f |
	f := self fontToUse.
	^((f widthOfString: contents) max: 3)  @ f lineSpacing
]

{ #category : 'geometry' }
LabelMorph >> minimumExtent [

	^ self measureContents + (2 * self interiorOrigin) 
]

{ #category : 'printing' }
LabelMorph >> printOn: aStream [

	super printOn: aStream.
	aStream print: contents.

]
