'From Haver 5.0 [latest update: #4587] on 6 May 2021 at 3:57:03 pm'!


!MenuMorph methodsFor: 'construction' stamp: 'KLG 5/6/2021 15:45:51'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w newMorph |
	
	newMorph _ WidgetMorph new noBorder.
	newMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asString linesDo: [ :line |
		s _ LabelMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		newMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ newMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	newMorph morphExtent: (w + 16) @ (pp y).
	titleMorph
		 ifNil: [
			titleMorph _ newMorph.
			self addMorphFront: titleMorph ]
		ifNotNil: [ self addMorphBack: newMorph ].
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !
