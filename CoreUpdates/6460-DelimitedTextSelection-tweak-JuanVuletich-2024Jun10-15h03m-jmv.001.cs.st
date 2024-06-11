'From Cuis7.1 [latest update: #6459] on 10 June 2024 at 3:03:53 pm'!

!TextEditor methodsFor: 'new selection' stamp: 'jmv 6/10/2024 11:44:27'!
selectWordOrDelimitedTextAt: localEventPosition
	"Select delimited text or word--the result of double-clicking."

	| hitCharacterBlock range p |
	p := localEventPosition x - 1 @ localEventPosition y.
	hitCharacterBlock := textComposition characterBlockIncludingPoint: p.
	range := self
		delimitedRangeAt: hitCharacterBlock stringIndex
		hitOnRightHalf: p x  > hitCharacterBlock center x
		leftDelimiters: '([{<'
		rightDelimiters: ')]}>'
		simpleDelimiters: '|''"`'.
	self selectFrom: range first to: range last.! !

