'From Cuis 4.2 of 25 July 2013 [latest update: #2806] on 8 June 2016 at 12:57:13.201707 pm'!

!RunNotArray methodsFor: 'Text support' stamp: 'jmv 6/8/2016 11:40'!
basicReplaceAttributesFrom: start to: stop with: replacement
	start to: stop do: [ :i |
		self at: i put: (replacement at: i-start+1) ]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 6/8/2016 11:45'!
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."
	self
		evaluateSelectionAndDo: [ :result |
			self afterSelectionInsertAndSelect: ' ', result printText, ' ']
		ifFail: [ morph flash ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
 SmalltalkCompleter initialize!

