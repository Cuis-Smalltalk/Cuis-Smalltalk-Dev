'From Cuis 6.0 [latest update: #5999] on 9 September 2023 at 11:27:34 am'!

!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'len 5/19/2023 17:27:22'!
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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

UnicodeCodePoint initializeNamedCharactersMap!

