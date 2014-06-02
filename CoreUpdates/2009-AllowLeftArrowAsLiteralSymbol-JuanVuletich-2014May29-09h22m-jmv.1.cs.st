'From Cuis 4.2 of 25 July 2013 [latest update: #2005] on 29 May 2014 at 9:23:19.827265 am'!

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/29/2014 09:13'!
xLitQuote
	"Symbols and vectors: #(1 (4 5) 2 3) #ifTrue:ifFalse: #'abc'."
	| start |
	start := mark.
	self step. "litQuote"
	self scanToken.
	tokenType == #leftParenthesis
		ifTrue: [
			self scanToken; scanLitVec.
			mark := start + 1.
			tokenType == #doIt
				ifTrue: [self offEnd: 'Unmatched parenthesis']]
		ifFalse: [
			tokenType == #leftBracket
				ifTrue: [
					self scanToken; scanLitByteVec.
					mark := start + 1.
					tokenType == #doIt
						ifTrue: [self offEnd: 'Unmatched bracket']]
				ifFalse: [
					(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon or: [tokenType == #leftArrow]]])
						ifTrue: [self scanLitWord]
						ifFalse: [tokenType == #string
							ifTrue: [token := token asSymbol]
							ifFalse: [
								(tokenType == #binary or: [ tokenType == #verticalBar ]) 
									ifFalse: [self notify: 'Invalid literal character' at: start + 1]]]]].
	mark := start.
	tokenType := #literal

	"#(Pen)
	#Pen
	#'Pen'
	##Pen
	###Pen
	"! !

