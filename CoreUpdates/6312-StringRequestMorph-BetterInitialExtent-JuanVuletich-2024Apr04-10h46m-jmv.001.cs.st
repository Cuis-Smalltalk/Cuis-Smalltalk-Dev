'From Cuis6.3 [latest update: #6311] on 4 April 2024 at 10:46:55 am'!

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 4/4/2024 10:43:44'!
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
		extraWidth := (6 * FontFamily defaultPointSize). "Some extra room for edition plus accept/cancel buttons." ].

	self morphExtent: ((titleWidth max: contentsWidth) + extraWidth)@(titleHeight + (FontFamily defaultLineSpacing * requiredLines)).! !

