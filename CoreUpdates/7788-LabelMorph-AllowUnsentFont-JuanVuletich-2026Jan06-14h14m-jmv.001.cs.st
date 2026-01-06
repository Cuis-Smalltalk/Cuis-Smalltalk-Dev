'From Cuis7.7 [latest update: #7784] on 6 January 2026 at 2:21:29 pm'!

!LabelMorph methodsFor: 'accessing' stamp: 'jmv 6/Jan/2026 14:17:53'!
defaultFont
	^FontFamily defaultFamilyAndPointSize! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 6/Jan/2026 14:18:41'!
defaultFont
	^Preferences at: #standardMenuFont! !


!LabelMorph methodsFor: 'accessing' stamp: 'jmv 6/Jan/2026 14:18:00'!
fontToUse
	| fontToUse |
	fontToUse := font ifNil: [self defaultFont].
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [ fontToUse]
		ifFalse: [ fontToUse emphasized: emphasis]! !

!LabelMorph methodsFor: 'geometry' stamp: 'jmv 6/Jan/2026 14:21:14'!
fontPreferenceChanged

	super fontPreferenceChanged.
	self fitContents.! !


!MenuItemMorph methodsFor: 'initialization' stamp: 'jmv 6/Jan/2026 14:18:48'!
initialize
	"initialize the state of the receiver"
	super initialize.
	isEnabled := true.
	subMenu := nil.
	isSelected := false.
	target := nil.
	selector := nil.
	arguments := nil.
	font := nil.
	self contents: ''.! !

!methodRemoval: IndentingListItemMorph #fontPreferenceChanged stamp: 'jmv 6/Jan/2026 14:21:07'!
IndentingListItemMorph removeSelector: #fontPreferenceChanged!
