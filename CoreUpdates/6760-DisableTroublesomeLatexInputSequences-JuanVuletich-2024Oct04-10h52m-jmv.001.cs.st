'From Cuis7.1 [latest update: #6758] on 4 October 2024 at 11:13:47 am'!

!Character class methodsFor: 'class initialization' stamp: 'jmv 10/4/2024 11:13:02'!
initialize
	"https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	http://www.unicode.org/L2/L1999/UnicodeData.html
	Character initialize
	"

	| fields codePoint codePointName generalCategory canonicalCombiningClass characterDecompositionMapping decomposition composition |
	UppercaseLettersTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	LowercaseLettersTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	SymbolsLikeTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	DiacriticalMarksTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	CanonicalCombiningClasses := Dictionary new.
	DecompositionMapping := Dictionary new.
	CompositionMapping := Dictionary new.
	(DirectoryEntry smalltalkImageDirectory // 'UnicodeData.txt') readStreamDo: [ :strm |
		[strm atEnd] whileFalse: [
			fields := strm nextLine substringsSeparatedBy: $;.
			(#(4 5 6) includes: fields first size) ifFalse: [ fields first print. self error: 'Bad UnicodeData.txt file' ].
			codePoint := ('16r', fields first) asNumber.
			codePointName := fields at: 2.
			generalCategory := fields at: 3.
			codePoint <= 16r1FFFF ifTrue: [
				({ 'Lu'. 'Lt' } includes: generalCategory) ifTrue: [ UppercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Ll'. 'Lo' } includes: generalCategory) ifTrue: [ LowercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Pc'. 'Pd'. 'Po'. 'Sm'. 'Sc'. 'Sk'. 'So' } includes: generalCategory) ifTrue: [ SymbolsLikeTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Mn'. 'Mc'. 'Me' } includes: generalCategory) ifTrue: [ DiacriticalMarksTable bitBooleanAt: codePoint+1 put: true ]].
			canonicalCombiningClass := (fields at: 4) asNumber.
			canonicalCombiningClass = 0 ifFalse: [
				CanonicalCombiningClasses at: codePoint put: canonicalCombiningClass ].
			characterDecompositionMapping := fields at: 6.
			(characterDecompositionMapping size > 0 and: [ characterDecompositionMapping first ~= $< ]) ifTrue: [
				decomposition := characterDecompositionMapping substrings collect: [ :s | ('16r', s) asNumber].
				DecompositionMapping at: codePoint put: decomposition.
				decomposition size > 1 ifTrue: [
					composition := CompositionMapping at: decomposition first ifAbsentPut: [Dictionary new].
					composition at: decomposition second put: codePoint.
					decomposition size > 2 ifTrue: [self error: decomposition printString print].
					]].
			"
			uppercaseMapping := fields at: 13. lowercaseMapping := fields at: 14. titlecaseMapping := fields at: 15.
			Random next > 10.999 | (generalCategory = 'So') ifTrue: [
				{codePoint hex. codePointName. generalCategory} print ].
			decimalDigitValue := fields at: 7. digitValue := fields at: 8. numericValue := fields at: 9.
			decimalDigitValue isEmpty & digitValue isEmpty & numericValue isEmpty ifFalse: [
				{'numeric'. codePoint. codePointName. generalCategory. decimalDigitValue. digitValue. numericValue} print].
			"
			]].
	self initializeNamedCharactersMap.! !

!Character class methodsFor: 'class initialization' stamp: 'jmv 10/4/2024 11:12:55'!
initializeNamedCharactersMap
	"Dictionary of named characters that we can enter in an editor using \name.
	Please keep the names short and try to follow the naming convention used in LaTeX."

	NamedCharactersMap := Dictionary new.
	"Superscripts:"
	4 to: 9 do: [:each| NamedCharactersMap at: '^', each printString put: (self codePoint: 16r2070 + each)].
	"Subscripts:"
	0 to: 9 do: [:each| NamedCharactersMap at: '_', each printString put: (self codePoint: 16r2080 + each)].
	'hklmnpst' withIndexDo: [:each :i| NamedCharactersMap at: '_', each asString put: (self codePoint: 16r2094 + i)].
	'iruv' withIndexDo: [:each :i| NamedCharactersMap at: '_', each asString put: (self codePoint: 16r1D61 + i)].
	"Combining characters:"
	#(
		(acute 16r0301) "('''' 16r0301)"
		(grave 16r0300) "('`' 16r0300)"
		(tilde 16r0303) "('~' 16r0303)"
		(diaeresis 16r0308) "('THEDOUBLEQUOTECHAR' 16r0308)" "When uncommenting, replace THEDOUBLEQUOTECHAR with an actual double quote..."
		(bar 16r0304) (breve 16r0306) (check 16r030C)
		(hat 16r0302) (not 16r0338) (overline 16r0305)
		(vec 16r20D7)
	) do: [:each| NamedCharactersMap at: each first put: (self codePoint: each second)].
	#(
		"Some more superscripts and subscripts:"
		('^0' $⁰) ('^1' $¹) ('^2' $²) ('^3' $³) ('^i' $ⁱ) ('^n' $ⁿ) ('_j' $ⱼ) ('^+' $⁺) ('^-' $⁻) ('_+' $₊) ('_-' $₋)
		"Arrows:"
		(left $←) (right $→) (up $↑) (down $↓) (leftright $↔) (updown $↕)
		(Left $⇐) (Up $⇑) (Right $⇒) (Down $⇓) (Leftright $⇔) (Updown $⇕)
		(gets $←) (to $→) (mapsto $↦) (mapsfrom $↤) (implies $⟹) (impliedby $⟸) (iff $⟺)
		(from $←) (iso $↔) (From $⇐) (To $⇒) (dfrom $⇇) (dto $⇉) "non-standard"
		"Greek alphabet:"
		(alpha $α) (beta $β) (gamma $γ) (delta $δ) (epsilon $ε) (zeta $ζ) (eta $η) (theta $θ) (iota $ι) (kappa $κ) (lambda $λ) (mu $μ) (nu $ν) (xi $ξ) (omicron $ο) (pi $π) (rho $ρ) (sigmaf $ς) (sigma $σ) (tau $τ) (upsilon $υ) (phi $φ) (chi $χ) (psi $ψ) (omega $ω) (thetasym $ϑ) (upsih $ϒ) (piv $ϖ)
		(Alpha $Α) (Beta $Β) (Gamma $Γ) (Delta $Δ) (Epsilon $Ε) (Zeta $Ζ) (Eta $Η) (Theta $Θ) (Iota $Ι) (Kappa $Κ) (Lambda $Λ) (Mu $Μ) (Nu $Ν) (Xi $Ξ) (Omicron $Ο) (Pi $Π) (Rho $Ρ) (Sigma $Σ) (Tau $Τ) (Upsilon $Υ) (Phi $Φ) (Chi $Χ) (Psi $Ψ) (Omega $Ω)
		"Blackboard bold:"
		(A $𝔸) (B $𝔹) (C $ℂ) (D $𝔻) (E $𝔼) (F $𝔽) (G $𝔾) (H $ℍ) (I $𝕀) (J $𝕁) (K $𝕂) (L $𝕃) (M $𝕄) (N $ℕ) (O $𝕆) (P $ℙ) (Q $ℚ) (R $ℝ) (S $𝕊) (T $𝕋) (U $𝕌) (V $𝕍) (W $𝕎) (X $𝕏) (Y $𝕐) (Z $ℤ)
		"Math symbols:"
		(oplus $⊕) (otimes $⊗) (ominus $⊖) (odot $⊙) (oslash $⊘)
		(times $×) (div $÷) (circ $∘) (bullet $∙) (cdot $·) (pm $±) (wr $≀)
		(perp $⊥)		
		(cap $∩) (cup $∪) (sqcap $⊓) (sqcup $⊔) (wedge $∧) (vee $∨) (perp $⊥) (parallel $∥)	
		(in $∈) (notin $∉) (exists $∃) (nexists $∄) (forall $∀) (emptyset $∅) (infty $∞) (aleph $ℵ) (ell $ℓ)
		(Im $ℑ) (Re $ℜ) (wp $℘)
		(sqrt $√) (partial $∂) (deg $°)
(leq $≤) (geq $≥) (neq $≠) (prec $≺) (succ $≻) (simeq $≃) (cong $≅) (equiv $≡) (approx $≈) (subset $⊂) (supset $⊃)
		(ldots $…) (cdots $⋯) (ddots $⋱) (iddots $⋰) (vdots $⋮) "(prime $′) (dprime $″) (tprime $‴)"
		"Other:"
		(langle $⟨) (rangle $⟩) (lceil $⌈) (rceil $⌉) (lfloor $⌊) (rfloor $⌋) (ltimes $⋉) (rtimes $⋊) 
		(lowast $⁎) (dag $†) (ddag $‡) (emdash $—) (endash $–) (questiondown $¿) (exclamdown $¡)
		(euro $€) (pounds $£) (celsius $℃) (farenheit $℉)
		(registered $®) (copyright $©) (trademark $™) (snowman $☃) (section $§))
	do: [:each| NamedCharactersMap at: each first put: each second].
	
	"Some synonyms:"
	#((prod sqcap) (coprod sqcup) (stack sqcap) (augment sqcup) (and wedge) (or vee) (xor oplus))
	do: [:each| NamedCharactersMap at: each first put: (NamedCharactersMap at: each second)]! !

Character initialize!
