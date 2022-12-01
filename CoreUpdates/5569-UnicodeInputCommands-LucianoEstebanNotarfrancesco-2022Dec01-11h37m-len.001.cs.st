'From Cuis 6.0 [latest update: #5566] on 1 December 2022 at 12:39:32 pm'!
!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value '
	classVariableNames: 'DiacriticalMarksTable LowercaseLettersTable SymbolsLikeTable UppercaseLettersTable NamedCharactersMap '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'len 12/1/2022 12:23:57'!
initializeNamedCharactersMap
	"Dictionary of named characters that we can enter in an editor using \name.
	Please keep the names short and try to follow the naming convention used in LaTeX."

	NamedCharactersMap _ Dictionary new.
	"Superscripts:"
	4 to: 9 do: [:each| NamedCharactersMap at: '^', each printString put: (self codePoint: 16r2070 + each)].
	"Subscripts:"
	0 to: 9 do: [:each| NamedCharactersMap at: '_', each printString put: (self codePoint: 16r2080 + each)].
	#(
		"Some superscripts in the Latin-1 range:"
		('^0' $⁰) ('^1' $¹) ('^2' $²) ('^3' $³)
		"Arrows:"
		(left $←) (right $→) (up $↑) (down $↓) (leftright $↔) (updown $↕)
		(Left $⇐) (Up $⇑) (Right $⇒) (Down $⇓) (Leftright $⇔) (Updown $⇕)
		(mapsto $↦) (implies $⟹) (iff $⟺)
		(gets $←) (to $→) (hom $⇒) #(fromall $⇇) (toall $⇉)
		"Greek alphabet:"
		(alpha $α) (beta $β) (gamma $γ) (delta $δ) (epsilon $ε) (zeta $ζ) (eta $η) (theta $θ) (iota $ι) (kappa $κ) (lambda $λ) (mu $μ) (nu $ν) (xi $ξ) (omicron $ο) (pi $π) (rho $ρ) (sigmaf $ς) (sigma $σ) (tau $τ) (upsilon $υ) (phi $φ) (chi $χ) (psi $ψ) (omega $ω) (thetasym $ϑ) (upsih $ϒ) (piv $ϖ)
		(Alpha $Α) (Beta $Β) (Gamma $Γ) (Delta $Δ) (Epsilon $Ε) (Zeta $Ζ) (Eta $Η) (Theta $Θ) (Iota $Ι) (Kappa $Κ) (Lambda $Λ) (Mu $Μ) (Nu $Ν) (Xi $Ξ) (Omicron $Ο) (Pi $Π) (Rho $Ρ) (Sigma $Σ) (Tau $Τ) (Upsilon $Υ) (Phi $Φ) (Chi $Χ) (Psi $Ψ) (Omega $Ω)
		"Blackboard bold:"
		(A $𝔸) (B $𝔹) (C $ℂ) (D $𝔻) (E $𝔼) (F $𝔽) (G $𝔾) (H $ℍ) (I $𝕀) (J $𝕁) (K $𝕂) (L $𝕃) (M $𝕄) (N $ℕ) (O $𝕆) (P $ℙ) (Q $ℚ) (R $ℝ) (S $𝕊) (T $𝕋) (U $𝕌) (V $𝕍) (W $𝕎) (X $𝕏) (Y $𝕐) (Z $ℤ)
		"Math symbols:"
		(oplus $⊕) (otimes $⊗) (times $×) (div $÷) (circ $∘) (bullet $∙) (cdot $·) (prod $⊓) (coprod $⊔) (perp $⊥)		
		(cap $∩) (cup $∪) (sqcap $⊓) (sqcup $⊔) (wedge $∧) (vee $∨) (perp $⊥) (parallel $∥)	
		(in $∈) (notin $∉) (exists $∃) (nexists $∄) (forall $∀) (emptyset $∅) (infty $∞) (aleph $ℵ) (ell $ℓ)
		(sqrt $√) (partial $∂) (deg $°)
(leq $≤) (geq $≥) (neq $≠) (prec $≺) (succ $≻) (simeq $≃) (cong $≅) (equiv $≡) (approx $≈) (subset $⊂) (supset $⊃)
		"Other:"
		(lowast $⁎) (dag $†) (ddag $‡) (emdash $—) (endash $–) (questiondown $¿) (exclamdown $¡)
		(euro $€) (pounds $£) (celsius $℃) (farenheit $℉)
		(registered $®) (copyright $©) (trademark $™) (snowman $☃) (section $§))
		do: [:each| NamedCharactersMap at: each first put: each second]! !

!UnicodeCodePoint class methodsFor: 'constants' stamp: 'len 12/1/2022 12:23:44'!
namedCharactersMap
	"Dictionary of named characters that we can enter in an editor using \name."

	^ NamedCharactersMap! !


!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'len 12/1/2022 12:32:46'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| string key |
	aKeyboardEvent keyCharacter isLetter ifTrue: [^ super normalCharacter: aKeyboardEvent].
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	string _ self privateCurrentString.
	"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
	self pointIndex - 1 to: (self pointIndex - 20 max: 1) by: -1 do: [:i|
		(string at: i) = $\ ifTrue: 
			[key _ string copyFrom: i + 1 to: self pointIndex - 1.
			UnicodeCodePoint namedCharactersMap at: key ifPresent: [:value| self selectFrom: i to: self pointIndex - 1; replaceSelectionWith: (Utf8String with: value)].
			^ super normalCharacter: aKeyboardEvent]].
	^ super normalCharacter: aKeyboardEvent! !

!methodRemoval: UnicodeCodePoint class #namedCharactersTable stamp: 'len 12/1/2022 12:35:15'!
UnicodeCodePoint class removeSelector: #namedCharactersTable!
!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value'
	classVariableNames: 'DiacriticalMarksTable LowercaseLettersTable NamedCharactersMap SymbolsLikeTable UppercaseLettersTable'
	poolDictionaries: ''
	category: 'Kernel-Text'!

UnicodeCodePoint initializeNamedCharactersMap!
