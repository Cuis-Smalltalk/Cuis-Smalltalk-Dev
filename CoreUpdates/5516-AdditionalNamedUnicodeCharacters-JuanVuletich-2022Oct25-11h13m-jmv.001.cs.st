'From Cuis 6.0 [latest update: #5506] on 25 October 2022 at 12:17:36 pm'!

!UnicodeCodePoint class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/4/2022 17:57:44'!
arrowDown
	"
	UnicodeCodePoint arrowDown asString
	"
	^ self codePoint: 8595! !

!UnicodeCodePoint class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/4/2022 17:58:01'!
arrowLeft
	"
	UnicodeCodePoint arrowLeft asString
	"
	^ self codePoint: 8592! !

!UnicodeCodePoint class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/4/2022 17:57:39'!
arrowRight
	"
	UnicodeCodePoint arrowRight asString
	"
	^ self codePoint: 8594! !

!UnicodeCodePoint class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/4/2022 17:57:16'!
arrowUp
	"
	UnicodeCodePoint arrowUp asString
	"
	^ self codePoint: 8593! !

!UnicodeCodePoint class methodsFor: 'constants' stamp: 'jmv 10/25/2022 12:16:22'!
namedCharactersTable
	"Table of named characters that we can enter in an editor using \name.
	Please keep the names short and try to follow the naming convention used in LaTeX."

	^`{ {#left. UnicodeCodePoint arrowLeft }. { #right. UnicodeCodePoint arrowRight }. { #up. UnicodeCodePoint arrowUp}. {#down. UnicodeCodePoint arrowDown }},
	#((oplus $⊕) (otimes $⊗) (times $×) (div $÷) #(circ $∘) (cdot $·) (prod $⊓) (coprod $⊔) (perp $⊥)),
	
	#((alpha $α) (beta $β) (gamma $γ) (delta $δ) (epsilon $ε) (zeta $ζ) (eta $η) (theta $θ) (iota $ι) (kappa $κ) (lambda $λ) (mu $μ) (nu $ν) (xi $ξ) (omicron $ο) (pi $π) (rho $ρ) (sigmaf $ς) (sigma $σ) (tau $τ) (upsilon $υ) (phi $φ) (chi $χ) (psi $ψ) (omega $ω) (thetasym $ϑ) (upsih $ϒ) (piv $ϖ)),
	#((Alpha $Α) (Beta $Β) (Gamma $Γ) (Delta $Δ) (Epsilon $Ε) (Zeta $Ζ) (Eta $Η) (Theta $Θ) (Iota $Ι) (Kappa $Κ) (Lambda $Λ) (Mu $Μ) (Nu $Ν) (Xi $Ξ) (Omicron $Ο) (Pi $Π) (Rho $Ρ) (Sigma $Σ) (Tau $Τ) (Upsilon $Υ) (Phi $Φ) (Chi $Χ) (Psi $Ψ) (Omega $Ω)),
	#((A $𝔸) (B $𝔹) (C $ℂ) (D $𝔻) (E $𝔼) (F $𝔽) (G $𝔾) (H $ℍ) (I $𝕀) (J $𝕁) (K $𝕂) (L $𝕃) (M $𝕄) (N $ℕ) (O $𝕆) (P $ℙ) (Q $ℚ) (R $ℝ) (S $𝕊) (T $𝕋) (U $𝕌) (V $𝕍) (W $𝕎) (X $𝕏) (Y $𝕐) (Z $ℤ)),
	
	#((infty $∞) (aleph $ℵ) (sqrt $√) (partial $∂) (deg $°)),
	
	#((dag $†) (ddag $‡) (emdash $—) (endash $–) (questiondown $¿) (exclamdown $¡)
	(euro $€) (pounds $£) 
	(celsius $℃) (farenheit $℉)
	(registered $®) (copyright $©) (trademark $™) 
	(snowman $☃)
	(section $§))`.! !


!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/4/2022 17:58:31'!
arrowDown
	"
	Character arrowDown
	"
	^ UnicodeCodePoint arrowDown! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/4/2022 17:58:40'!
arrowLeft
	"
	Character arrowLeft
	"
	^ UnicodeCodePoint arrowLeft! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/4/2022 17:58:45'!
arrowRight
	"
	Character arrowRight
	"
	^ UnicodeCodePoint arrowRight! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 8/4/2022 17:58:51'!
arrowUp
	"
	Character arrowUp
	"
	^ UnicodeCodePoint arrowUp! !


!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 10/25/2022 12:05:16'!
printOn: aStream 
	aStream nextPut: $$.
	aStream nextPut: self.
	aStream nextPut: $(.
	aStream nextPutAll: self hex.
	aStream nextPut: $).! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 10/25/2022 12:04:05'!
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."
	| rpt |
	self
		evaluateSelectionAndDo: [ :result |
			rpt _ model fullPrintIt
				ifTrue: [result printText]
				ifFalse: [result printTextLimitedTo: 10000].
			rpt _ rpt copyReplaceAll: String lfString with: String lfString, ' '.
			self afterSelectionInsertAndSelect: 
				((' ', rpt, ' ') initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false.! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/25/2022 11:37:39'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| stopIndex startIndex string key |
	aKeyboardEvent keyCharacter isLetter ifTrue: [^ super normalCharacter: aKeyboardEvent].
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	string _ self privateCurrentString.
	stopIndex _ self pointIndex - 1.
	(stopIndex between: 2 and: string size)
		ifFalse: [^ super normalCharacter: aKeyboardEvent].
	startIndex _ stopIndex.
	"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
	[startIndex > 0 and: [stopIndex - startIndex < 20 and: [(string at: startIndex) isLetter]]] whileTrue: [startIndex _ startIndex - 1].
	(startIndex > 0 and: [(string at: startIndex) = $\])
		ifFalse: [^ super normalCharacter: aKeyboardEvent].
	key _ string copyFrom: startIndex+1 to: stopIndex.
	(UnicodeCodePoint namedCharactersTable detect: [:one| key = one first] ifNone: [])
		ifNotNil: [:aPair| self selectFrom: startIndex to: stopIndex; replaceSelectionWith: (self stringClassToUse with: aPair second)].
	^ super normalCharacter: aKeyboardEvent! !

!methodRemoval: Character class #namedCharactersTable stamp: 'jmv 10/25/2022 11:36:12'!
Character class removeSelector: #namedCharactersTable!

!UnicodeCodePoint class reorganize!
('instance creation' codePoint:)
('class initialization' initialize)
('accessing untypeable characters' arrowDown arrowLeft arrowRight arrowUp)
('constants' namedCharactersTable)
!

