'From Cuis7.3 [latest update: #7102] on 28 April 2025 at 5:48:05 pm'!

!StringRequestMorph methodsFor: 'initialization' stamp: 'ml 4/28/2025 17:46:54'!
setQuery: queryString initialAnswer: initialAnswer
	| titleMorphExtent titleHeight titleWidth requiredLines lineWidth contentsWidth maxLineWidth extraWidth |
	response := initialAnswer.
	titleMorphExtent := self addTitle: queryString.
	titleWidth := 20 * FontFamily defaultPointSize max: titleMorphExtent x.
	titleHeight := titleMorphExtent y + FontFamily defaultLineSpacing. "Some extra room for borders, etc."
	self addTextPane.

	requiredLines := 0.
	"Single line preferred. For instance, for file names and paths."
	(response noneSatisfy: [ :each | each isSeparator ]) ifTrue: [
		maxLineWidth := Display width * 0.8.
		contentsWidth := FontFamily defaultFamilyAndPointSize widthOfString: response.
		contentsWidth < maxLineWidth ifTrue: [
			"Will fit in the screen. Ok."
			requiredLines := 1.
			extraWidth := (10 * FontFamily defaultPointSize). "Some extra room for edition plus accept/cancel buttons." ]].

	"May need to be multiple lines"
	requiredLines = 0 ifTrue: [
		maxLineWidth := Display width // 3.
		contentsWidth := 0.
		response linesDo: [ :eachLine |
			lineWidth := FontFamily defaultFamilyAndPointSize widthOfString: eachLine.
			contentsWidth := contentsWidth max: (lineWidth min: maxLineWidth).
			requiredLines := requiredLines + (lineWidth / maxLineWidth) ceiling ].
		extraWidth := (10 * FontFamily defaultPointSize). "Some extra room for edition plus accept/cancel buttons."
		requiredLines := requiredLines min: 20. "Use scrollbars if needed." 		].

	self morphExtent: ((titleWidth max: contentsWidth) + extraWidth)@(titleHeight + (FontFamily defaultLineSpacing * requiredLines)).! !

