'From Cuis7.1 [latest update: #6372] on 17 May 2024 at 12:21:48 pm'!

!AbstractFont class methodsFor: 'ui' stamp: 'jmv 5/17/2024 12:21:35'!
fromUser
	"
	Query the user for a Font (FontFamily + pointSize).
	 AbstractFont fromUser
	"
	^self fromUser: FontFamily defaultFamilyAndPointSize ! !

!AbstractFont class methodsFor: 'ui' stamp: 'jmv 5/17/2024 12:21:42'!
fromUser: priorFont
	"
	Query the user for a Font (FontFamily + pointSize).
	AbstractFont fromUser
	"
	"Present a menu of available fonts, and if one is chosen, return it.
	Otherwise return nil.
	Show only baseFonts i.e. FamilyName, pointSize (but do not include emphasis, such as italic or bold)"

	| fontList fontMenu active ptMenu label spec |
	fontList := FontFamily familyNames.
	fontMenu := MenuMorph new defaultTarget: self.
	fontList do: [:fontName | 
			active := priorFont familyName sameAs: fontName.
			ptMenu := MenuMorph new defaultTarget: self.
			(FontFamily pointSizesFor:fontName ) do: [ :pt | 
					(active and: [pt = priorFont pointSize]) 
						ifTrue: [label := '<on>']
						ifFalse: [label := '<off>'].
					label := label , pt printString , ' pt'.
					ptMenu 
						add: label
						target: fontMenu
						action: #modalSelection:
						argument: { 
								fontName.
								pt}].
			active ifTrue: [label := '<on>'] ifFalse: [label := '<off>'].
			label := label , fontName.
			fontMenu add: label subMenu: ptMenu].
	spec := fontMenu invokeModal.
	spec ifNil: [^nil].
	^FontFamily familyName: spec first pointSize: spec last! !


!AbstractFont class reorganize!
('constants' boldCode italicCode normalCode struckThroughCode subscriptCode superscriptCode underlinedCode)
('ui' fromUser fromUser:)
!

