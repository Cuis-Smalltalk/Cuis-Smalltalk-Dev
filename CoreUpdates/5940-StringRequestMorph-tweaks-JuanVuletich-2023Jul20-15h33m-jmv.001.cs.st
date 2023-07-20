'From Cuis 6.0 [latest update: #5939] on 20 July 2023 at 3:34:51 pm'!

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 7/20/2023 15:07:55'!
addTitle: aString
	| titleMorph s pp w |
	titleMorph := BoxedMorph new noBorder.
	titleMorph color: Theme current menuTitleBar.
	pp := `8@2`.
	aString asPlainString linesDo: [ :line |
		s := LabelMorph new
			contents: line;
			font: (Preferences at: #standardMenuFont) bold.
		titleMorph addMorphBack: s position: pp.
		pp := pp + (0@(s morphHeight+2)) ].
	w := titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 24) @ (pp y).
	self addMorphKeepMorphHeight: titleMorph.
	^titleMorph morphExtent! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 7/20/2023 15:32:38'!
setQuery: queryString initialAnswer: initialAnswer
	| titleExtent height width |
	response := initialAnswer.
	titleExtent := self addTitle: queryString.
	self addTextPane.
	width := 40 * FontFamily defaultLineSpacing max: titleExtent x.
	height := (response lineCount + 1) * FontFamily defaultLineSpacing + titleExtent y.
	self morphExtent: width@height! !

