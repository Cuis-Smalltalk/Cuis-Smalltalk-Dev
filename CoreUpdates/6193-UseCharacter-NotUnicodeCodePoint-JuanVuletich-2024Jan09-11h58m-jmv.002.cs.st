'From Cuis6.3 [latest update: #6184] on 9 January 2024 at 12:01:01 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 1/9/2024 11:39:35'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	self isNullCharacter ifTrue: [ ^false ].
	self = $′ ifTrue: [ ^true ]. 		"Also see #xLetter"
	^#(xLetter xDigit xUnderscore xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!Character methodsFor: 'converting' stamp: 'jmv 1/9/2024 11:26:03'!
asString
	^self codePoint > 255
		ifTrue: [ UnicodeString with: self ]
		ifFalse: [ String with: self ].! !

!Character methodsFor: 'converting' stamp: 'jmv 1/9/2024 11:27:54'!
withDiacriticalMark: codePointOrCharacter
	"Answer the character resulting from adding a diacritical mark (accent) to a letter.
	If the result is unsupported in ISO 8859-15, answer the receiver.
	Supported diacritical marks are:
		U+0300 	COMBINING GRAVE ACCENT
		U+0301 	COMBINING ACUTE ACCENT
		U+0302 	COMBINING CIRCUMFLEX ACCENT
		U+0303 	COMBINING TILDE
		U+0308 	COMBINING DIAERESIS
		U+030A 	COMBINING RING ABOVE
		U+030C 	COMBINING CARON
		$a withDiacriticalMark: 16r301
		$N withDiacriticalMark: $~
		$Z withDiacriticalMark: $v
		
		invalid:
		$9 withDiacriticalMark:$v
		$A withDiacriticalMark: $v
		$Á withDiacriticalMark: $v
		$A withDiacriticalMark: 1244
		"
	| answer i |
	i := ((codePointOrCharacter isNumber
			ifTrue: [#(16r300 16r301 16r302 16r303 16r308 16r30A 16r30C)]
			ifFalse: [#($` $' $^ $~ $" $° $v)]) indexOf: codePointOrCharacter
				) + 1.
	answer := (Character accentedLetters detect: [ :group | group first = self ] ifNone: [ ^self ]) at: i.
	^answer = $- ifFalse: [answer] ifTrue: [self]! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 1/9/2024 11:33:11'!
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
				({ 'Pc'. 'Po'. 'Sm'. 'Sc'. 'Sk'. 'So' } includes: generalCategory) ifTrue: [ SymbolsLikeTable bitBooleanAt: codePoint+1 put: true ].
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

!Character class methodsFor: 'class initialization' stamp: 'jmv 1/9/2024 11:35:51'!
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
		(acute 16r0301) ('''' 16r0301) (bar 16r0304) (breve 16r0306) (check 16r030C)
		(grave 16r0300) ('`' 16r0300) (hat 16r0302) (not 16r0338) (overline 16r0305)
		(tilde 16r0303) ('~' 16r0303) ('"' 16r0308) (vec 16r20D7)
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


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 1/9/2024 11:39:40'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	self isNullCharacter ifTrue: [ ^false ].
	self = $′ ifTrue: [ ^true ]. 		"Also see #xLetter"
	^#(xLetter xDigit xUnderscore xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 1/9/2024 11:28:07'!
withDiacriticalMark: codePointOrCharacter
self flag: #unicodeNeedsImprovement.
"hack"
	value < 128 ifTrue: [
		^UnicodeCodePoint codePoint: (
			(Character codePoint: value) withDiacriticalMark: codePointOrCharacter)
				codePoint ].
	^self! !


!UnicodeCodePoint class methodsFor: 'instance creation' stamp: 'jmv 1/9/2024 11:51:27'!
codePoint: aNumber
	^Character codePoint: aNumber! !

!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'jmv 1/9/2024 11:33:04'!
initialize
	"https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	http://www.unicode.org/L2/L1999/UnicodeData.html
	UnicodeCodePoint initialize
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
				({ 'Pc'. 'Po'. 'Sm'. 'Sc'. 'Sk'. 'So' } includes: generalCategory) ifTrue: [ SymbolsLikeTable bitBooleanAt: codePoint+1 put: true ].
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

!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'jmv 1/9/2024 11:35:57'!
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
		(acute 16r0301) ('''' 16r0301) (bar 16r0304) (breve 16r0306) (check 16r030C)
		(grave 16r0300) ('`' 16r0300) (hat 16r0302) (not 16r0338) (overline 16r0305)
		(tilde 16r0303) ('~' 16r0303) ('"' 16r0308) (vec 16r20D7)
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


!EventSensor methodsFor: 'test' stamp: 'jmv 1/9/2024 11:28:42'!
printEventBuffer: evtBuf
	"Print the event buffer, currently only used by the method `test`."

	| type buttons macRomanCode modifiers pressType stamp unicodeCodePoint |
	type := evtBuf first.
	stamp := evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	type = EventSensor eventTypeMouse
		ifTrue: [ | position |
			position := evtBuf third @ evtBuf fourth.
			buttons := evtBuf fifth.
			modifiers := evtBuf sixth.
			Transcript
				newLine;
				show: 'Mouse';
				show: ' position:', position printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeMouseScroll
		ifTrue: [ | delta |
			delta := evtBuf third @ evtBuf fourth.
			buttons := evtBuf fifth.
			modifiers := evtBuf sixth.
			Transcript
				newLine;
				show: 'Scroll';
				show: ' delta:', delta printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeKeyboard 
		ifTrue: [
			macRomanCode := evtBuf third.
			unicodeCodePoint := evtBuf sixth.
			pressType := evtBuf fourth.
			modifiers := evtBuf fifth.
			pressType = EventSensor eventKeyDown ifTrue: [
				type := #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				type := #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				type := #keystroke].
			{type. 'macRoman:'. macRomanCode. macRomanCode hex.
				'unicodeCodePoint:'. unicodeCodePoint. unicodeCodePoint hex. Character codePoint: unicodeCodePoint} print.
			Transcript nextPutAll: ' modifiers: ', modifiers printString.
			(modifiers anyMask: 8) ifTrue: [ Transcript nextPutAll: ' [commandWinAlt]' ].
			(modifiers anyMask: 4) ifTrue: [ Transcript nextPutAll: ' [macOption]' ].
			(modifiers anyMask: 2) ifTrue: [ Transcript nextPutAll: ' [control]' ].
			(modifiers anyMask: 1) ifTrue: [ Transcript nextPutAll: ' [shift]' ].
			''print.
			].! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 1/9/2024 11:39:46'!
asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in char rest did |
	in := self readStream.
	^self species streamContents: [ :out |
		[ in atEnd ] whileFalse: [
			in peek = $<
				ifTrue: [in unCommand] 	"Absorb <...><...>"
				ifFalse: [(char := in next) = $&
					ifTrue: [
						rest := in upTo: $;.
						did := out position.
						rest = 'lt' ifTrue: [out nextPut: $<].
						rest = 'gt' ifTrue: [out nextPut: $>].
						rest = 'amp' ifTrue: [out nextPut: $&].
						rest = 'deg' ifTrue: [out nextPut: $¡].
						rest = 'quot' ifTrue: [out nextPut: $"].
						rest first = $# ifTrue: [ out nextPut: (Character codePoint: rest findPositiveInteger) ].
						did = out position ifTrue: [
							out nextPut: $&; nextPutAll: rest.
							"self error: 'unknown encoded HTML char'."
							"Please add it to this method"]]
					ifFalse: [out nextPut: char]].
			]].! !


!UnicodeString methodsFor: 'accessing' stamp: 'jmv 1/9/2024 11:26:50'!
at: codePointIndex
	"Answer the UnicodeCodePoint at code point index codePointIndex.
	(not a byte index!!)"

	| byteIndex |
	self boundsCheck: codePointIndex.
	self isAscii ifTrue: [ ^ Character codePoint: (bytes at: codePointIndex) ].
	byteIndex := self byteIndexAt: codePointIndex.
	Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
		^Character codePoint: codePoint ].
	^nil "Invalid UTF-8"! !

!UnicodeString methodsFor: 'enumerating' stamp: 'jmv 1/9/2024 11:24:34'!
do: aBlock
	"Evaluate aBlock for all elements."

	| byteIndex stopByteIndex n |
	byteIndex := 1.
	stopByteIndex := bytes size.
	[ byteIndex <= stopByteIndex ] whileTrue: [
		n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
			aBlock value: (Character codePoint: codePoint) ].
		byteIndex := byteIndex + n ].! !

!UnicodeString methodsFor: 'enumerating' stamp: 'jmv 1/9/2024 11:24:30'!
from: startIndex to: stopIndex do: aBlock
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	| byteIndex n cpIndex |
	byteIndex := self byteIndexAt: startIndex.
	cpIndex := startIndex.
	[ cpIndex <= stopIndex ] whileTrue: [
		n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
			aBlock value: (Character codePoint: codePoint).
			cpIndex := cpIndex + 1 ].
		byteIndex := byteIndex + n ].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 1/9/2024 11:26:32'!
basicCanonicalComposition
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalComposition.
	(UnicodeString fromCodePoints: #(119128 119141)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119135 119151)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119128 119141 119151)) basicCanonicalComposition asCodePoints
	"
	| byteIndex stopByteIndex n combining cls |
	byteIndex := 1.
	stopByteIndex := bytes size.
	combining := OrderedCollection new.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls := Character combiningClass: codePoint.
				cls = 0 ifTrue: [
					self basicCompose: combining on: strm.
					combining := OrderedCollection new ].
				combining add: codePoint ].
			byteIndex := byteIndex + n ].
		self basicCompose: combining on: strm ].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 1/9/2024 11:26:14'!
basicCanonicalDecomposition
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalDecomposition.
	(Character codePoint: 119135) asString basicCanonicalDecomposition asCodePoints
	(Character codePoint: 119137) asString basicCanonicalDecomposition asCodePoints
	"
	| byteIndex stopByteIndex n |
	byteIndex := 1.
	stopByteIndex := bytes size.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				Character canonicalDecomposition: codePoint on: strm ].
			byteIndex := byteIndex + n ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 1/9/2024 11:24:46'!
basicCanonicallyOrdered
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	(UnicodeString fromCodePoints: #(16r61  16r0300 16r0315 16r031B)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0300 16r031B 16r0315)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r031B 16r0300)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r0300 16r031B)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0300 16r0315)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0315 16r0300)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	"
	| byteIndex stopByteIndex n combining cls element |
	byteIndex := 1.
	stopByteIndex := bytes size.
	combining := OrderedCollection new.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls := Character combiningClass: codePoint.
				cls = 0
					ifTrue: [
						combining notEmpty ifTrue: [
							combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
							combining do: [ :e | strm nextPutCodePoint: e third ]].
						strm nextPutCodePoint: codePoint.
						combining := OrderedCollection new ]
					ifFalse: [
						element := { cls. combining size+1. codePoint }.
						combining add: element ]].
			byteIndex := byteIndex + n ].
		combining notEmpty ifTrue: [
			combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
			combining do: [ :e | strm nextPutCodePoint: e third ] ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 1/9/2024 11:24:41'!
basicCompose: combiningCodePoints on: aStream
	| continue composition i base |
	combiningCodePoints notEmpty ifTrue: [
		continue := combiningCodePoints size > 1.
		[continue] whileTrue: [
			continue := false.
			base := combiningCodePoints first.
			i := 2.
			composition := nil.
			[composition isNil and: [i <= combiningCodePoints size ]] whileTrue: [
				composition := Character canonicalCompositionOf: base and: (combiningCodePoints at: i).
				composition ifNotNil: [
					combiningCodePoints at: 1 put: composition.
					combiningCodePoints removeAt: i.
					continue := combiningCodePoints size > 1 ].
				i := i + 1 ]].
		combiningCodePoints do: [ :e | aStream nextPutCodePoint: e ]].! !


!UniFileStream methodsFor: 'private' stamp: 'jmv 1/9/2024 11:23:45'!
nextCodePoint
	| answer |
	answer := nil.
	self nextUtf8BytesAndCodePointInto: nil into: [ :codePoint |
		answer := Character codePoint: codePoint ].

	"If we'd skipped BOM, try again. See comment at
	utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into:
	Feel free to blame Microsoft for this nonsense."
	answer ifNil: [
		(self position = 3 and: [self atEnd not]) ifTrue: [
			answer := self nextCodePoint ]].

	^answer! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 1/9/2024 11:37:11'!
xLetter
	"Form a word or keyword."

	| type |
	buffer reset.
	[(type := self typeTableAt: hereChar) == #xLetter
		or: [type == #xDigit
		or: [type == #xUnderscore
		or: [type == #xDiacritical
		or: [ hereChar = $′ ]]]]  						"Also see #isValidInIdentifiers"
	] whileTrue: [
			"open code step for speed"
			buffer nextPut: hereChar.
			hereChar := aheadChar.
			aheadChar := source atEnd
				ifTrue: [DoItCharacter "doit"]
				ifFalse: [source next]].
	tokenType := (type == #colon or: [type == #xColon and: [aheadChar ~= $=]])
		ifTrue: [
			buffer nextPut: self step.
			"Allow any number of embedded colons in literal symbols"
			[(self typeTableAt: hereChar) == #xColon]
				whileTrue:
					[buffer nextPut: self step].
			#keyword]
		ifFalse: [
			#word].
	token := buffer contents.! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 1/9/2024 11:35:09'!
typeTable: aTypeTable at: aCharacterOrCodePoint

	"Prebuilt tables for the ASCII range"
	aCharacterOrCodePoint asciiValue ifNotNil: [ :ascii |
		^aTypeTable at: ascii ].

	"For CodePoints not in the ASCII range"
	aCharacterOrCodePoint isDiacriticalMark ifTrue: [ ^#xDiacritical ].
	aCharacterOrCodePoint isLetter ifTrue: [ ^#xLetter ].
	aCharacterOrCodePoint isSymbolLike ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint = $­ ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint isSeparator ifTrue: [ ^#xDelimiter ].
	"Many should actually be #xIllegal"
	^#xLetter! !

!Scanner class methodsFor: 'class initialization' stamp: 'jmv 1/9/2024 11:34:21'!
initialize
	"
	Scanner initialize.
	"
	self initTypeTable.
	self initializeDoitCharacter! !

!Scanner class methodsFor: 'class initialization' stamp: 'jmv 1/9/2024 11:23:19'!
initializeDoitCharacter

	DoItCharacter := Character codePoint: self doItCharacterValue ! !


!Encoder methodsFor: 'encoding' stamp: 'jmv 1/9/2024 11:23:12'!
asciiStringLiteral: literalObject
	"Convert UnicodeStrings to Strings if ASCII.
	Same for UnicodeCodePoint and Character.
	Recursive for Arrays"
	
	literalObject class == Array ifTrue: [
		^literalObject collect: [ :each | self asciiStringLiteral: each ]].

	"Prefer String over UnicodeString for literals, if ASCII only."
	(literalObject class == UnicodeString and: [ literalObject isAscii ]) ifTrue: [
		^ literalObject asByteString ].
	
	"Prefer Character over UnicodeCodePoint for literals, if ASCII only."
	"bueh......"
	literalObject class == UnicodeCodePoint ifTrue: [
		^ Character codePoint: literalObject codePoint ].

	^literalObject! !


!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 1/9/2024 11:22:20'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| character i replacement |
	"Convert strings like '\+161 ' (š) to unicode"
	aKeyboardEvent isSpace ifTrue: [ | string |
		string := self privateCurrentString.
		self pointIndex - 1 to: (self pointIndex - 7 max: 1) by: -1 do: [ :index |
		string at: index :: = $\ ifTrue: [ | key |
			key := string copyFrom: index + 1 to: self pointIndex - 1.
			key size > 2 and: [ key at: 1 :: = $+ ] ::
				ifTrue: [ | stream number value |
					stream := ReadStream on: key asUppercase from: 2 to: key size.
					[ number := Integer readFrom: stream  base: 16 ] on: Error do: [ ^ false ].
					value := Character codePoint: number.
					self
						selectFrom: index to: self pointIndex - 1;
						replaceSelectionWith: (UnicodeString with: value).
						^ false ] ] ] ].
	character := aKeyboardEvent keyCharacter.
	"Linux VMs don't automatically replace combining marks + space with ASCII,
	as Windows and Mac VMs do. It seems it is an app responsability to do that in Linux."
	character isDiacriticalMark ifTrue: [
		i := self pointIndex-1.
		(i > 0 and: [ (self privateCurrentString at: i) = $  ]) ifTrue: [
			replacement := character caseOf: {
				[ `Character codePoint: 16r300` ] -> [ '`' ].
				[ `Character codePoint: 16r301` ] -> [ '''' ].
				[ `Character codePoint: 16r302` ] -> [ '^' ].
				[ `Character codePoint: 16r303` ] -> [ '~' ].
				[ `Character codePoint: 16r308` ] -> [ '"' ].
			} otherwise: [].
			replacement notNil ifTrue: [
				self selectFrom: i to: i; replaceSelectionWith: replacement.
				^ false ]
			]
		].
	self addString: (UnicodeString with: character).
	^false! !


!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 1/9/2024 11:22:04'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| string key |
	aKeyboardEvent keyCharacter isLetter ifTrue: [^ super normalCharacter: aKeyboardEvent].
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	string := self privateCurrentString.
	"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
	self pointIndex - 1 to: (self pointIndex - 20 max: 1) by: -1 do: [:i|
		(string at: i) = $\ ifTrue: 
			[key := string copyFrom: i + 1 to: self pointIndex - 1.
			Character namedCharactersMap at: key ifPresent: [:value| self selectFrom: i to: self pointIndex - 1; replaceSelectionWith: (UnicodeString with: value)].
			^ super normalCharacter: aKeyboardEvent]].
	^ super normalCharacter: aKeyboardEvent! !


!KeyboardEvent methodsFor: 'keyboard' stamp: 'jmv 1/9/2024 11:21:46'!
keyCharacter
	"Answer the character corresponding this keystroke. This is defined only for keystroke events."
	| codePoint |
	codePoint := keyValue.
	self isArrowLeft ifTrue: [ codePoint := 16r2190 ].
	self isArrowUp ifTrue: [ codePoint := 16r2191 ].
	self isArrowRight ifTrue: [ codePoint := 16r2192 ].
	self isArrowDown ifTrue: [ codePoint := 16r2193 ].
	^ Character codePoint: codePoint.! !


!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'jmv 1/9/2024 11:21:39'!
computeMessageEntriesFor: allSource in: contextClass and: specificModel  
	
	| id rangeType |

	canShowSelectorDocumentation := true.
	id := allSource copyFrom: currentRange start to: currentRange end.
	rangeType := currentRange rangeType.

	rangeType == #globalVar
		ifTrue: [ ^self computeMessageEntriesForClass: (Smalltalk at: id asSymbol) class ].
	rangeType == #self
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass ].
	rangeType == #super
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass superclass ].
	rangeType == #true 
		ifTrue: [ ^self computeMessageEntriesForClass: True ].
	rangeType == #false
		ifTrue: [ ^self computeMessageEntriesForClass: False ].
	rangeType == #nil
		ifTrue: [ ^self computeMessageEntriesForClass: UndefinedObject ].
	rangeType == #character
		ifTrue: [ ^self computeMessageEntriesForClass: Character ].
	rangeType == #number
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #string
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #symbol
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #stringSymbol
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #instVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofInstVarNamed: id ].
	rangeType == #methodArg
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id ].
	rangeType == #tempVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id ].
	rangeType == #blockArg
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockArgNamed: id ].
	rangeType == #blockTempVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockTempVarNamed: id ].
	rangeType == #workspaceVar
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (specificModel classOfWorkspaceVarNamed: id) ].
	rangeType == #thisContext
		ifTrue: [ ^self computeMessageEntriesForClass: (specificModel classOfThisContext) ]. 
	rangeType == #classVar
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) ].
	rangeType == #poolConstant
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) ].
	(rangeType beginsWith: #blockEnd)
		ifTrue: [ ^self computeMessageEntriesForClass: BlockClosure ].
	rangeType == #arrayEnd
		ifTrue: [ ^self computeMessageEntriesForClass: Array ].
	(rangeType beginsWith: #rightBrace)
		ifTrue: [ ^self computeMessageEntriesForClass: Array ].
	rangeType == #unary
		ifTrue: [ ^self computeEntriesOfUnaryMessageReturnNamed: id ].
	(rangeType beginsWith: #rightParenthesis)
		ifTrue: [ ^self computeMessageEntriesOfEnclosedExpressionReturn].
	rangeType == #cascadeSeparator
		ifTrue: [ ^self computeMessageEntriesOfCascadeReceiver]. 

	self computeMessageEntriesForUnknowClass 
	
	! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 1/9/2024 11:31:52'!
initialize
	"
	It takes about 6 seconds, mostly because of the time to fetch method stamps from source files...
	[ SmalltalkCompleter initialize ]timeToRun
	Selectors inspect
	"
	| maxSortValue allImplemented |
	
	EntriesLimit := 100.
	
	SystemChangeNotifier uniqueInstance
		removeActionsWithReceiver: self.	 "avoid double registration"

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self.
	
	self protected: [
		allImplemented := Smalltalk allImplementedMessages.
		Selectors := Trie new.
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				Selectors at: sel ifAbsentPut: [ 0 ].
				method messages do: [ :sentMsg |
					Selectors at: sentMsg put: (Selectors at: sentMsg ifAbsent: [ 0 ]) + 1 ]]].
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				self addSelector: sel method: method allImplemented: allImplemented]].
		""
	
		"The following might not be found in #messages. Give them maximum priority."
		maxSortValue := SmallInteger maxVal.
		"From MessageNode>>#initialize"
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:) do: [ :sel |
				Selectors at: sel put: maxSortValue ].
	
		maxSortValue := SmallInteger maxVal-1.
		"From SystemDictionary >>#recreateSpecialObjectsArray"
		(1 to: Smalltalk specialSelectorSize) do:  [ :i | | sym |
			sym := Smalltalk specialSelectorAt: i.
			(Selectors includesKey: sym)
				ifTrue: [ Selectors at: sym put: maxSortValue ]]]! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 1/9/2024 11:21:10'!
mapAndStoreGlyphsUsing: characterMap
	"
	https://developer.apple.com/fonts/TrueType-Reference-Manual/RM07/appendixB.html
	"
	| glyphsByUTF8 unknownGlyph nullGlyph spaceGlyph mapSize preferredCodePoint arrowGlyphIndexes leftArrowGlyph upArrowGlyph rightArrowGlyph downArrowGlyph carriageReturnGlyph leftArrowGlyphCodePoint upArrowGlyphCodePoint rightArrowGlyphCodePoint downArrowGlyphCodePoint carriageReturnGlyphCodePoint |
	unknownGlyph := glyphs first.
	nullGlyph := glyphs second.
	mapSize := characterMap size.
	glyphsByUTF8 := Array new: 256.
	leftArrowGlyphCodePoint := self class smalltalkAssignmentArrowGlyphCodePoint.
	upArrowGlyphCodePoint := self class smalltalkReturnArrowGlyphCodePoint.
	rightArrowGlyphCodePoint := 8594.
	downArrowGlyphCodePoint := 8595.
	carriageReturnGlyphCodePoint := 8617.
	
	"Store Unicode arrow glyphs for future use if appropriate,"
	(FallbackArrowGlyphs isNil or: [fontDescription name = 'DejaVuSans']) ifTrue: [
		mapSize > 8617 ifTrue: [								"Usually true. Just in case."
			arrowGlyphIndexes := {leftArrowGlyphCodePoint. upArrowGlyphCodePoint. rightArrowGlyphCodePoint. downArrowGlyphCodePoint. carriageReturnGlyphCodePoint}
					collect: [ :codePoint | (characterMap at: codePoint+1)+1 ].
				(arrowGlyphIndexes allSatisfy: [ :i | i > 1 ]) ifTrue: [
					FallbackArrowGlyphs := arrowGlyphIndexes collect: [ :i | glyphs at: i ]]]].
	"Take arrow glyphs if present, or use fallbacks if not."
	leftArrowGlyph := upArrowGlyph := rightArrowGlyph := downArrowGlyph := carriageReturnGlyph := unknownGlyph.
	mapSize > downArrowGlyphCodePoint ifTrue: [
		leftArrowGlyph := glyphs at: (characterMap at: leftArrowGlyphCodePoint+1)+1.
		upArrowGlyph := glyphs at: (characterMap at: upArrowGlyphCodePoint+1)+1.
		rightArrowGlyph := glyphs at: (characterMap at: rightArrowGlyphCodePoint+1)+1.
		downArrowGlyph := glyphs at: (characterMap at: downArrowGlyphCodePoint+1)+1 ].
	leftArrowGlyph == unknownGlyph ifTrue: [ leftArrowGlyph := FallbackArrowGlyphs at: 1 ].
	upArrowGlyph == unknownGlyph ifTrue: [ upArrowGlyph := FallbackArrowGlyphs at: 2 ].
	rightArrowGlyph == unknownGlyph ifTrue: [ rightArrowGlyph := FallbackArrowGlyphs at: 3 ].
	downArrowGlyph == unknownGlyph ifTrue: [ downArrowGlyph := FallbackArrowGlyphs at: 4 ].
	mapSize > carriageReturnGlyphCodePoint ifTrue: [
		carriageReturnGlyph := glyphs at: (characterMap at: carriageReturnGlyphCodePoint+1)+1 ].
	carriageReturnGlyph == unknownGlyph ifTrue: [ carriageReturnGlyph := FallbackArrowGlyphs at: 5 ].

	0 to: mapSize - 1 do: [ :codePoint | | g array prevByte|

		preferredCodePoint := codePoint.
		(codePoint = $- codePoint and: [mapSize > 16r2212]) ifTrue: [ preferredCodePoint := 16r2212 ]. "WIDE MINUS"
		(codePoint = $* codePoint and: [mapSize > 16r2217]) ifTrue: [ preferredCodePoint := 16r2217 ]. "CENTERED ASTERISK"
		g := glyphs at: (characterMap at: preferredCodePoint+1)+1 ifAbsent: [ unknownGlyph ].
		g == unknownGlyph ifTrue: [
			g := glyphs at: (characterMap at: codePoint+1)+1 ifAbsent: [ unknownGlyph ]].

		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ].
					].
				prevByte := byte.
				]
			withUtf8BytesOfUnicodeCodePoint: codePoint.
			array at: prevByte+1 put: g.
		codePoint = 32 ifTrue: [
			spaceGlyph := g ].

		].

	"Synthetic Unicode zero width space glyph. Add others as needed."
	{ Character zeroWidthSpace } do: [ :codePoint | | g array prevByte|
		g := TTGlyph new.					"Suitable as zero width space"
		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ]].
				prevByte := byte ]
			withUtf8BytesOfUnicodeCodePoint: codePoint codePoint.
			array at: prevByte+1 put: g ].

	Character separators do: [ :s |
		s codePoint < 256 ifTrue: [
			glyphsByUTF8 at: s codePoint +1 put: spaceGlyph ]].
	1 to: 4 do: [ :i |			glyphsByUTF8 at: 28+i put: i ].
	glyphsByUTF8 at: 13 +1 put: carriageReturnGlyph.
	fontDescription
		glyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: {leftArrowGlyph.​ upArrowGlyph.​ rightArrowGlyph.​ downArrowGlyph. carriageReturnGlyph}! !

Character initialize!
UnicodeCodePoint initialize!
Scanner initialize!
SmalltalkCompleter initialize!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
DebuggerMethodMap voidMapCache.
ChangeSet zapAllChangeSets.
Smalltalk garbageCollect.
!

