'From Cuis 4.2 of 25 July 2013 [latest update: #2027] on 7 July 2014 at 1:33:13.229915 pm'!

!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 7/7/2014 13:30'!
bold
	^self emphasized: 1! !

!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 7/7/2014 13:31'!
boldItalic
	^self emphasized: 3! !

!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 7/7/2014 13:32'!
emphasized: code
	^self subclassResponsibility! !

!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 7/7/2014 13:31'!
italic
	^self emphasized: 2! !

!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 7/7/2014 13:31'!
struckThrough
	^self emphasized: 8! !

!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 7/7/2014 13:30'!
underlined
	^self emphasized: 4! !


!AbstractFont reorganize!
('accessing' baseKern characterToGlyphMap derivativeFonts height pointSize xTable)
('measuring' approxWidthOfText: widthOf: widthOfString: widthOfString:from:to: widthOfStringOrText:)
('caching' releaseCachedState)
('as yet unclassified' on:displayString:from:to:at:color:)
('derivatives - emphasis' bold boldItalic emphasized: italic struckThrough underlined)
!

