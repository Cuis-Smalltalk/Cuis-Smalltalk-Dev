'From Cuis 4.2 of 25 July 2013 [latest update: #2308] on 6 May 2015 at 9:54:24.952831 pm'!

!Text methodsFor: 'attributes' stamp: 'jmv 5/6/2015 21:48'!
hasAnyParagraphAttribute
	"Return false if there are no emphasis (i.e., a String would not make a difference)"

	^runs values anySatisfy: [ :emphArray | emphArray anySatisfy: [ :attr | attr isParagraphAttribute ]]! !


!Text methodsFor: 'private' stamp: 'jmv 5/6/2015 21:49'!
fixParagraphAttributesFrom: start to: end
	"Helper method to ensure the invariant that TextAttributes that answer true to
	 #isParagraphAttribute are only applied to whole paragraphs.
	See senders"
	
	| paragraphEnd paragraphInterval paragraphStart paragraphAttributes|
	self hasAnyParagraphAttribute ifFalse: [ ^self ].
	paragraphEnd _ end max: start.	"end could be start-1 when new text is empty, for example, when backspacing"
	[
		paragraphInterval _ self encompassParagraph: (paragraphEnd to: paragraphEnd).
		paragraphStart _ paragraphInterval first.
		paragraphEnd _ paragraphInterval last.
	
		"We must honor the paragraph attributes as defined in the newline (Lf) Character that ends the paragraph"
		paragraphAttributes _ (self attributesAt: paragraphEnd) select: [ :attr | attr isParagraphAttribute ].

		"if paragraphEnd is inside the interval just modified, and it doesn't bring any paragraph attributes (i.e., it doesn't end in CR),
		then try to keep the paragraph attributes previously in use in this paragraph..
		This is needed, for example, when pasting an image  or a plain text at the end of the document"
		(paragraphEnd = end and: [ paragraphStart < start and: [ end > 0 and: [ (string at: end) isLineSeparator not ]]]) ifTrue: [
			paragraphAttributes _ (self attributesAt: paragraphStart) select: [ :attr | attr isParagraphAttribute ]].

		self privateSetParagraphAttributes: paragraphAttributes from: paragraphStart to: paragraphEnd.
		paragraphEnd _ paragraphStart - 1.
		paragraphStart > start ] whileTrue.
	runs coalesce! !

