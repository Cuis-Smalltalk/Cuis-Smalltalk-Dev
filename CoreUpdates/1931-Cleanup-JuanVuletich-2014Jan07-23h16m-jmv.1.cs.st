'From Cuis 4.2 of 25 July 2013 [latest update: #1930] on 7 January 2014 at 11:21:30.27299 pm'!

!AbstractFont methodsFor: 'measuring' stamp: 'jmv 1/7/2014 22:55'!
approxWidthOfText: aText
	"Return the width of aText -- quickly, and a little bit dirty.
	Used by lists morphs containing Text objects to get a quick,
	fairly accurate measure of the width of a list item."
	| w |
	(aText isNil or: [ aText size = 0 ]) ifTrue: [ ^ 0 ].
	w _ self
		widthOfString: aText string
		from: 1
		to: aText size.
	"If the text has no emphasis, just return the string size.  If it is empasized, 
	just approximate the width by adding about 20% to the width"
	^ ((aText runLengthFor: 1) = aText size and: [ (aText emphasisAt: 1) = 0 ])
		ifTrue: [ w ]
		ifFalse: [ w * 6 // 5 ]! !


!ClassCategoryReader methodsFor: 'fileIn/Out' stamp: 'jmv 1/7/2014 23:01'!
scanFrom: aStream 
	"File in methods from the stream, aStream."
	| methodSource |
	[
		methodSource _ aStream nextChunk.
		methodSource size > 0] whileTrue: [
		class compile: methodSource classified: category
			withStamp: changeStamp
			notifying: nil ]! !

!methodRemoval: AbstractFont class #fontMenuForFamily:target:selector:highlight:!
AbstractFont class removeSelector: #fontMenuForFamily:target:selector:highlight:!
