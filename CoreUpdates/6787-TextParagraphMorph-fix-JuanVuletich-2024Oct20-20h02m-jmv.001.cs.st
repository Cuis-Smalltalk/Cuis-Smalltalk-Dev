'From Cuis7.1 [latest update: #6786] on 20 October 2024 at 8:06:32 pm'!

!TextParagraphMorph methodsFor: 'accessing' stamp: 'jmv 10/20/2024 20:05:30'!
contents: aStringOrText

	| current |
	current := textComposition textComposed.
	current = aStringOrText ifTrue: [
		((current is: #Text) and: [aStringOrText is: #Text])
		ifTrue: [
			current runs = aStringOrText runs ifTrue: [
				^self ]]
		ifFalse: [.
			^self ]].
	textComposition	 	setModel: (TextModel withText: aStringOrText).
	self reflowAndMaybeResize.! !

