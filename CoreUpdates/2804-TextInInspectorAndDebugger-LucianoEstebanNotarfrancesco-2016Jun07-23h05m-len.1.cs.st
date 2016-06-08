'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 7 June 2016 at 11:06:50.221913 pm'!

!Object methodsFor: 'printing' stamp: 'len 6/7/2016 21:33'!
printTextLimitedTo: limit
	"Answer a Text whose characters are a description of the receiver."
	| limitedString |
	limitedString _ Text streamContents: [:s | self printOn: s] limitedTo: limit.
	limitedString size < limit ifTrue: [^ limitedString].
	^ limitedString , '[..]'! !


!Inspector methodsFor: 'selecting' stamp: 'len 6/7/2016 21:35'!
selectionPrintString
	| text |
	text _ [self selection printTextLimitedTo: 12000]
						on: Error
						do: [text := self printStringErrorText.
							text
								addAttribute: TextColor red
								from: 1
								to: text size.
							text].
	^ text! !

