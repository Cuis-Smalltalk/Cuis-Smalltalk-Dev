"
I represent elements in a writing system. In Cuis, instances of Characters are Unicode Characters, each one associated with a numerical Unicode Code Point.

Characters are the elements of the CharacterSequence hierarchy, including String and UnicodeString.
"
Class {
	#name : 'Character',
	#superclass : 'Object',
	#classVars : [
		'CanonicalCombiningClasses',
		'CompositionMapping',
		'DecompositionMapping',
		'DiacriticalMarksTable',
		'LowercaseLettersTable',
		'NamedCharactersMap',
		'SymbolsLikeTable',
		'UppercaseLettersTable'
	],
	#category : 'Kernel-Strings',
	#package : 'Kernel',
	#tag : 'Strings'
}

{ #category : 'class initialization' }
Character class >> accentedLetters [
	"Answer all letters with all possible accents (combining diacritical marks).
	Order of this array is:
		unaccented
		U+0300 	COMBINING GRAVE ACCENT
		U+0301 	COMBINING ACUTE ACCENT
		U+0302 	COMBINING CIRCUMFLEX ACCENT
		U+0303 	COMBINING TILDE
		U+0308 	COMBINING DIAERESIS
		U+030A 	COMBINING RING ABOVE
		U+030C 	COMBINING CARON
	Do not include letters that can't have any accent in ISO 8859-1
	Denote unsupported combinations (for example A with caron, or N with accute accent) by a $-"
self flag: #unicodeNeedsImprovement.
	^#(
		'AÀÁÂÃÄÅ-'
		'aàáâãäå-'
		'EÈÉÊ-Ë--'
		'eèéê-ë--'
		'IÌÍÎ-Ï--'
		'iìíî-ï--'
		'OÒÓÔÕÖ--'
		'oòóôõö--'
		'UÙÚÛ-Ü--'
		'uùúû-ü--'
		'N---Ñ---'
		'n---ñ---'
		'S------Š'
		's------š'
		'Y-Ý--Ÿ--'
		'y-ý--ÿ--'
		'Z------Ž'
		'z------ž'
	).
]

{ #category : 'accessing untypeable characters' }
Character class >> backQuote [
	"Not specifically untypeable, but cumbersome to use in source code."

	^ `Character codePoint: 96`
]

{ #category : 'accessing untypeable characters' }
Character class >> backspace [
	"Answer the Character representing a backspace.
	This character is generally not used in text editors and files, and doesn't have a visible glyph.
	This is the code for the [<-] backspace key. See #keyboardTest See #primGetNextEvent:"

	^`Character codePoint: 8`
]

{ #category : 'static state' }
Character class >> basicLookupTables [
	| lowercaseMappingTable uppercaseMappingTable unaccentedTable |
	lowercaseMappingTable := Array new: 256.
	uppercaseMappingTable := Array new: 256.
	unaccentedTable := ByteArray new: 256.
	0
		to: 255
		do: [ :idx | | char |
			"Default to an identity mapping with a false truth mapping"
			char := self codePoint: idx.
			lowercaseMappingTable
				at: idx + 1 put: char.
			uppercaseMappingTable
				at: idx + 1 put: char.
			unaccentedTable at: idx + 1 put: idx].
	"Now override as needed"
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :group | | uppercase lowercase |
		(group first == $ÿ or: [group first == $ß])
			ifFalse: [ | lowercaseChar uppercaseChar |
				uppercase := group first codePoint.
				lowercase := group second codePoint.
				lowercaseChar := self codePoint: lowercase.
				uppercaseChar := self codePoint: uppercase.
				lowercaseMappingTable
					at: lowercase + 1 put: lowercaseChar;
					at: uppercase + 1 put: lowercaseChar.
				uppercaseMappingTable
					at: lowercase + 1 put: uppercaseChar;
					at: uppercase + 1 put: uppercaseChar.
				group size > 2
					ifTrue: [|unaccentedUppercase unaccentedLowercase|
						unaccentedUppercase := group third codePoint.
						unaccentedLowercase := group fourth codePoint.
						unaccentedTable
							at: uppercase+1 put: unaccentedUppercase;
							at: lowercase+1 put: unaccentedLowercase]]
			ifTrue: [ | lowercaseChar |
				lowercase := group first codePoint.
				lowercaseChar := self codePoint: lowercase.
				lowercaseMappingTable
					at: lowercase + 1 put: lowercaseChar.
				uppercaseMappingTable
					at: lowercase + 1 put: lowercaseChar.
				group size > 1
					ifTrue: [|unaccentedLowercase|
						unaccentedLowercase := group second codePoint.
						unaccentedTable
							at: lowercase+1 put: unaccentedLowercase]]].
	^{ lowercaseMappingTable. uppercaseMappingTable. unaccentedTable}.
]

{ #category : 'instance creation' }
Character class >> binarySelectorCharacters [
	^ '!%&+-*/\,<=>?@~'
]

{ #category : 'services' }
Character class >> canonicalCompositionOf: numericCodePoint and: combiningCodePoint [

	| dict |
	dict := CompositionMapping at: numericCodePoint
		ifAbsent: [^nil ].
	^dict at: combiningCodePoint ifAbsent: nil
]

{ #category : 'services' }
Character class >> canonicalDecomposition: numericCodePoint on: aStream [

	| decomposition |
	decomposition := DecompositionMapping at: numericCodePoint
		ifAbsent: [^aStream nextPutCodePoint: numericCodePoint].
	decomposition do: [ :each |
		self canonicalDecomposition: each on: aStream ]
]

{ #category : 'instance creation' }
Character class >> codePoint: anInteger [
	"Answer the Character whose value is anInteger.
	
	self assert: (Character codePoint: 16r41) == $A.
	self assert: (Character codePoint: 16r20AC) == $€.
	"

	<primitive: 170>
	^self primitiveFailed
]

{ #category : 'services' }
Character class >> combiningClass: numericCodePoint [
	(self isDiacriticalMark: numericCodePoint) ifFalse: [ ^0 ].
	^CanonicalCombiningClasses at: numericCodePoint ifAbsent: [ 	0 ]
]

{ #category : 'private' }
Character class >> constantNameFor: aCharacter [
	self constantNames do:
		[ :constantName |
		(self perform: constantName) == aCharacter ifTrue:
			[^constantName]].
	^nil
]

{ #category : 'private' }
Character class >> constantNames [
	^ #(backQuote backspace cr delete doubleQuote end escape home insert lf newPage noBreakSpace null pageUp singleQuote smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow space tab zeroWidthSpace)
]

{ #category : 'accessing untypeable characters' }
Character class >> cr [
	"Answer the Character representing a carriage return.
	This character is generally not used in text editors and files, because Cuis uses the LF line ending convention.
	It has a visible glyph, to make it easier to spot text that doesn't conform to the Cuis line ending convention.
	This is the code for the [ENTER] or [RETURN] key. See #keyboardTest See #primGetNextEvent:"

	^`Character codePoint: 13`
]

{ #category : 'accessing untypeable characters' }
Character class >> delete [

	^ `Character codePoint: 127`
]

{ #category : 'instance creation' }
Character class >> digitValue: x  [
	"Answer the Character whose digit value is x. For example,
	 answer $9 for x=9, $0 for x=0, $A for x=10, $Z for x=35."

	| n |
	n := x asInteger.
	^self codePoint: (n < 10 ifTrue: [n + 48] ifFalse: [n + 55])
]

{ #category : 'accessing untypeable characters' }
Character class >> doubleQuote [
	"Not specifically untypeable, but cumbersome to use in source code."

	^ `Character codePoint: 34`
]

{ #category : 'accessing untypeable characters' }
Character class >> end [
	"Answer the Character for the [END] key. See #keyboardTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 4`
]

{ #category : 'accessing untypeable characters' }
Character class >> escape [
	"Answer the ASCII ESC character
	This character is generally not used in text editors and files, and doesn't have a visible glyph.
	This is the code for the [ESC] key. See #keyboardTest See #primGetNextEvent:"

	^ `Character codePoint: 27`
]

{ #category : 'converting' }
Character class >> evaluate: aBlock withUtf16WordsOf: aCodePoint [
	"See senders for typical usage"

	| highSurrogate lowSurrogate |
	(aCodePoint between: 16rD800 and: 16rDFFF) ifTrue: [
		self error: 'invalid CodePoint' ].

	aCodePoint <= 16rFFFF ifTrue: [
		^aBlock value: aCodePoint ].

	highSurrogate := (aCodePoint - 16r10000) >> 10 + 16rD800.
	lowSurrogate := ((aCodePoint - 16r10000) bitAnd: 1023) + 16rDC00.
	aBlock value: highSurrogate.
	aBlock value: lowSurrogate.
]

{ #category : 'converting' }
Character class >> evaluate: aBlock withUtf8BytesOf: aCodePoint [
	"See senders for typical usage"

	| mask nBytes shift |
	aCodePoint < 128 ifTrue: [
		^aBlock value: aCodePoint ].
	nBytes := aCodePoint highBit + 3 // 5.
	mask := #(128 192 224 240 248 252 254 255) at: nBytes.
	shift := nBytes - 1 * -6.
	aBlock value: (aCodePoint bitShift: shift) + mask.
	2 to: nBytes do: [ :i | 
		shift := shift + 6.
		aBlock value: ((aCodePoint bitShift: shift) bitAnd: 63) + 128 ]
]

{ #category : 'converting' }
Character class >> evaluateOnce: aBlock withUtf8BytesOf: aCodePoint [
	"Similar to #evaluate:withUtf8BytesOf:, but aBlock is evaluated just once, and must have 4 parameters."

	| mask nBytes shift byte1 byte2 byte3 byte4 |
	aCodePoint < 128 ifTrue: [
		^aBlock value: aCodePoint value: nil value: nil value: nil ].
	nBytes := aCodePoint highBit + 3 // 5.
	mask := #(128 192 224 240 248 252 254 255) at: nBytes.
	shift := nBytes - 1 * -6.
	byte1 := (aCodePoint bitShift: shift) + mask.
	nBytes >= 2 ifTrue: [
		shift := shift + 6.
		byte2 :=  ((aCodePoint bitShift: shift) bitAnd: 63) + 128.
		nBytes >= 3 ifTrue: [
			shift := shift + 6.
			byte3 :=  ((aCodePoint bitShift: shift) bitAnd: 63) + 128.
			nBytes = 4 ifTrue: [
				shift := shift + 6.
				byte4 :=  ((aCodePoint bitShift: shift) bitAnd: 63) + 128.
				].
			].
		].
		^aBlock value: byte1 value: byte2 value: byte3 value: byte4.
]

{ #category : 'accessing untypeable characters' }
Character class >> home [
	"Answer the Character for the [HOME] key. See #keyboardTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 1`
]

{ #category : 'class initialization' }
Character class >> initialize [
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
			(characterDecompositionMapping size > 0 and: [ characterDecompositionMapping first ~~ $< ]) ifTrue: [
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
	self initializeNamedCharactersMap.
]

{ #category : 'class initialization' }
Character class >> initializeNamedCharactersMap [
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
	do: [:each| NamedCharactersMap at: each first put: (NamedCharactersMap at: each second)]
]

{ #category : 'accessing untypeable characters' }
Character class >> insert [
	"Answer the Character for the [INSERT] key. See #keyboardTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 5`
]

{ #category : 'services' }
Character class >> isCombiningStarter: numericCodePoint [
	^ (self combiningClass: numericCodePoint) = 0
]

{ #category : 'services' }
Character class >> isDiacriticalMark: numericCodePoint [
	"
	(Character isDiacriticalMark: 16r0041) 
	(Character codePoint: 16r0041) isDiacriticalMark
	(Character isDiacriticalMark: 16r20D7)
	(Character codePoint: 16r20D7) isDiacriticalMark
	"
	| i |
	i := numericCodePoint + 1.
	DiacriticalMarksTable bitSize < i ifTrue: [ ^false ].
	^ DiacriticalMarksTable bitBooleanAt: i.
]

{ #category : 'converting' }
Character class >> isValidUtf8: aByteArray [
	| s position usedBytes |
	s := aByteArray size.
	position := 1.
	[ position <= s ] whileTrue: [
		usedBytes := Character
			utf8BytesAndCodePointFor: (aByteArray at: position)
			byte2: (position+1 <= s ifTrue: [aByteArray at: position+1])
			byte3: (position+2 <= s ifTrue: [aByteArray at: position+2])
			byte4: (position+3 <= s ifTrue: [aByteArray at: position+3])
			into:nil
			into: nil
			assumeLatinIfInvalid: false.
		usedBytes ifNil: [ ^false ].
		position := position + usedBytes ].
	^true
]

{ #category : 'accessing untypeable characters' }
Character class >> lf [
	"Answer the Character representing a linefeed.
	It is the standard line terminator character for systems following the nearly universal LF line ending convention.
	It doesn't have a visible glyph."

	^`Character codePoint: 10`
]

{ #category : 'static state' }
Character class >> lowercaseMappingTable [
	^`Character basicLookupTables at: 1`.
]

{ #category : 'converting' }
Character class >> macRomanToLatin1: charCode [
	"Convert the receiver from MacRoman to  ISO 8895-1 Latin-1 encoding"
	
	charCode < 128 ifTrue: [^ charCode].
	^ #[
		196 197 199 201 209 214 220 225 224 226 228 227 229 231 233 232 
		234 235 237 236 238 239 241 243 242 244 246 245 250 249 251 252 
		134 176 162 163 167 149 182 223 174 169 153 180 168 128 198 216 
		129 177 138 141 165 181 142 143 144 154 157 170 186 158 230 248 
		191 161 172 166 131 173 178 171 187 133 160 192 195 213 140 156 
		150 151 147 148 145 146 247 179 255 159 185 164 139 155 188 189 
		135 183 130 132 137 194 202 193 203 200 205 206 207 204 211 212 
		190 210 218 219 217 208 136 152 175 215 221 222 184 240 253 254
	] at: charCode - 127
]

{ #category : 'constants' }
Character class >> namedCharactersMap [
	"Dictionary of named characters that we can enter in an editor using \name."

	^ NamedCharactersMap
]

{ #category : 'instance creation' }
Character class >> new [
	"Creating new characters is not allowed."

	self error: 'cannot create new characters'
]

{ #category : 'accessing untypeable characters' }
Character class >> newLineCharacter [
	"Answer the Character representing a line terminator or newLine, that is, a linefeed.
	This should be the only method in the system that knows about this detail.
	Everybody else should eventually ask us."

	^self lf
]

{ #category : 'accessing untypeable characters' }
Character class >> newPage [
	"Answer the Character representing a form feed.
	This character is also returned for the [Page Down] key. See #pageDown
	This character doesn't have a visible glyph."

	^ `Character codePoint: 12`
]

{ #category : 'accessing untypeable characters' }
Character class >> noBreakSpace [

	^ `Character codePoint: 16rA0`
]

{ #category : 'accessing untypeable characters' }
Character class >> null [
	"Answer the null Character.
	Sometimes it is used as a String terminator. It is not used in files often, but some files may be 'null terminated', as some API calls."

	^ `Character codePoint: 0`
]

{ #category : 'accessing untypeable characters' }
Character class >> pageDown [
	"Answer the Character for the [Page Down] key. See #keyboardTest See #primGetNextEvent:
	This character may be used in text files. The ASCII control character name for it is 'Form Feed'. See #newPage
	This character doesn't have a visible glyph."

	^ `Character codePoint: 12`
]

{ #category : 'accessing untypeable characters' }
Character class >> pageUp [
	"Answer the Character for the [Page Up] key. See #keyboardTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 11`
]

{ #category : 'instance creation' }
Character class >> separators [
	"Answer the separator characters"
	"Also see #isSeparator"

	^ #(32 "space"
		9 "tab"
		10 "line feed"
		13 "cr"
		12 "form feed"
		160 "non-breaking space"
		8203 "Character zeroWidthSpace")
			collect: [ :v | Character codePoint: v ]
]

{ #category : 'accessing untypeable characters' }
Character class >> singleQuote [
	"Not specifically untypeable, but cumbersome to use in source code."

	^ `Character codePoint: 39`
]

{ #category : 'accessing untypeable characters' }
Character class >> smalltalkDownArrow [
	"
	The use of code 31 for a down arrow is not part of the ASCII or Unicode standards.
	This character is generally not used in text.
	Cuis holds a down arrow glyph at this code. This is for possible future use.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkDownArrow
	"
	^ `Character codePoint: 31`
]

{ #category : 'accessing untypeable characters' }
Character class >> smalltalkLeftArrow [
	"
	The use of code 28 for a left arrow is not part of the ASCII or Unicode standards.
	This character is generally not used in text files.

	Cuis holds a left arrow glyph at this code, that is only used to show $_ as a left arrow,
	when used for assignment of a Smalltalk variable.
	Note that it will be replaced by $_ when saving code files or exporting to OS clipboard.
	Browse senders to see how this works.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkLeftArrow
	"
	^ `Character codePoint: 28`
]

{ #category : 'accessing untypeable characters' }
Character class >> smalltalkRightArrow [
	"
	The use of code 29 for a right arrow is not part of the ASCII or Unicode standards.
	This character is generally not used in text.
	Cuis holds a right arrow glyph at this code. This is for possible future use.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkRightArrow
	"
	^ `Character codePoint: 29`
]

{ #category : 'accessing untypeable characters' }
Character class >> smalltalkUpArrow [
	"
	The use of code 30 for an up arrow is not part of the ASCII or Unicode standards.
	This character is generally not used in text files.

	Cuis holds an up arrow glyph at this code, that is only used to show $^ as an up arrow,
	when used for return from a Smalltalk method.
	Note that it will be replaced by $^ when saving code files or exporting to OS clipboard.
	Browse senders to see how this works.

	The actual glyph used is decided by the TrueType font. See #mapAndStoreGlyphsUsing:
		and references to FallbackArrowGlyphs.

	Character smalltalkUpArrow
	"
	^ `Character codePoint: 30`
]

{ #category : 'accessing untypeable characters' }
Character class >> space [
	"Answer the Character representing a space."

	^ `Character codePoint: 32`
]

{ #category : 'accessing untypeable characters' }
Character class >> tab [
	"Answer the Character representing a tab."

	^ `Character codePoint: 9`
]

{ #category : 'static state' }
Character class >> unaccentedTable [
	^`Character basicLookupTables at: 3`.
]

{ #category : 'class initialization' }
Character class >> uppercaseLowercaseAndUnaccentedLetters [
	"Answer all uppercase and lowercase letters in standard collation order.
	For accented characters, include the non-accented ones.
	Note that $ß is 'LATIN SMALL LETTER SHARP S' and ISO 8859-1 doesn't have an uppercase version.
	Note: for $ÿ, 'Latin Small Letter Y with diaeresis' the uppercase version is not Latin 1"
self flag: #unicodeNeedsImprovement.
	^#(
		'Aa' 'ÀàAa' 'ÁáAa' 'ÂâAa' 'ÃãAa' 'ÄäAa' 'ÅåAa'
		'Bb'
		'Cc' 'ÇçCc'
		'Dd'
		'Ee' 'ÈèEe' 'ÉéEe' 'ÊêEe' 'ËëEe'
		'Ff' 'Gg' 'Hh'
		'Ii' 'ÌìIi' 'ÍíIi' 'ÎîIi' 'ÏïIi'
		'Jj' 'Kk' 'Ll' 'Mm'
		'Nn' 'ÑñNn'
		'Oo' 'ÒòOo' 'ÓóOo' 'ÔôOo' 'ÕõOo' 'ÖöOo'
		'Pp' 'Qq' 'Rr' 'Ss' 'Tt'
		'Uu' 'ÙùUu' 'ÚúUu' 'ÛûUu' 'ÜüUu'
		'Vv' 'Ww' 'Xx'
		'Yy' 'ÝýYy' 'ÿy'
		'Zz'
		'Ææ' 'Ðð' 'Øø' 'Þþ' 'ß'
	).
]

{ #category : 'static state' }
Character class >> uppercaseMappingTable [
	^`Character basicLookupTables at: 2`.
]

{ #category : 'converting' }
Character class >> utf8BytesAndCodePointAt: byteIndex in: aByteArray into: bytesBlock into: codePointBlock [
	"Find next UTF-8 encoded code point in utf8ByteArray starting at byte byteIndex.
	Evaluate bytesBlock (if not nil). Arguments are utf8 bytes for the codePoint found.
	Evaluate codePointBlock (if not nil) with codePoint found as argument.
	Answer the number of bytes that comprised this codePoint.
	If there is an invalid UTF-8 sequence, consider the first byte an ISO-8859-1 Character (i.e. Latin-1), and answer 1.
	Skip codePoint U+FEFF (BOM, unneededly added by Win clipboard). Still, answer the number of bytes of the BOM sequence, to allow senders to update an index in an iteration.
	See senders."

	| i s byte1 byte2 byte3 byte4 |
	
	s := aByteArray size.
	i := byteIndex.
	byte1 := aByteArray at: i.
	i := byteIndex+1.
	byte2 := i <= s ifTrue: [aByteArray at: i].
	i := byteIndex+2.
	byte3 := i <= s ifTrue: [aByteArray at: i].
	i := byteIndex+3.
	byte4 := i <= s ifTrue: [aByteArray at: i].

	^self
		utf8BytesAndCodePointFor: byte1
		byte2: byte2
		byte3: byte3
		byte4: byte4
		into: bytesBlock
		into: codePointBlock
		assumeLatinIfInvalid: true
]

{ #category : 'converting' }
Character class >> utf8BytesAndCodePointFor: byte1 byte2: byte2 byte3: byte3 byte4: byte4 into: bytesBlock into: codePointBlock assumeLatinIfInvalid: aBoolean [
	"Find next UTF-8 encoded code point in argument bytes.
	Evaluate bytesBlock (if not nil). Arguments are utf8 bytes for the codePoint found.
	Evaluate codePointBlock (if not nil) with codePoint found as argument.
	Answer the number of bytes that comprised this codePoint.
	If there is an invalid UTF-8 sequence:
		if aBoolean == true:
			'This will try to next bytes as UTF-8 if possible. Good if (for instance) a single invalid byte appears in the middle of valid UTF-8.'.
			consider the first byte an ISO-8859-1 Character, and answer 1.
		else
			'This is useful when validating if a byte sequence is valid UTF-8 or not'.
			answer nil
	Skip codePoint U+FEFF (BOM, unneededly added by Win clipboard). Still, answer the number of bytes of the BOM sequence, to allow senders to update an index in an iteration.
	See senders."

	| codePoint |
	byte1 < 128 ifTrue: [	"single byte"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: nil value: nil value: nil ].
		codePointBlock ifNotNil: [
			codePointBlock value: byte1 ].
		^ 1 ].
	
	"At least 2 bytes"
	(byte2 notNil and: [ (byte2 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1, or answer nil."
		^ aBoolean
			ifTrue: [ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock ]].
	(byte1 bitAnd: 16rE0) = 16rC0  ifTrue: [ "two bytes"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: nil value: nil ].
		codePointBlock ifNotNil: [
			codePoint := ((byte1 bitAnd: 31) bitShift: 6) + (byte2 bitAnd: 63).
			codePointBlock value: codePoint ].
		^2 ].
	
	"At least 3 bytes"
	(byte3 notNil and: [ (byte3 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1, or answer nil."
		^ aBoolean
			ifTrue: [ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock ]].
	(byte1 bitAnd: 16rF0) = 16rE0  ifTrue: [ "three bytes"
		codePoint :=  ((byte1 bitAnd: 15) bitShift: 12) + ((byte2 bitAnd: 63) bitShift: 6) + (byte3 bitAnd: 63).
		codePoint = 16rFEFF ifTrue: [ "BOM. Ignore it. Skip ti."
			^3 ].
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: byte3 value: nil ].
		codePointBlock ifNotNil: [
			codePointBlock value: codePoint ].
		^3 ].

	"4 bytes"
	(byte4 notNil and: [ (byte4 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1, or answer nil."
		^ aBoolean
			ifTrue: [ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock ]].
	(byte1 bitAnd: 16rF8) = 16rF0  ifTrue: [  "four bytes"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: byte3 value: byte4 ].
		codePointBlock ifNotNil: [
			codePoint := ((byte1 bitAnd: 16r7) bitShift: 18) + ((byte2 bitAnd: 63) bitShift: 12)
					+ ((byte3 bitAnd: 63) bitShift: 6) + (byte4 bitAnd: 63).
			codePointBlock value: codePoint ].
		^ 4].

	"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1, or answer nil."
	^ aBoolean
		ifTrue: [ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock ]
]

{ #category : 'converting' }
Character class >> utf8BytesAndCodePointForIso8859s1: byte into: bytesBlock into: codePointBlock [
	"Latin-1"
	| codePoint |
	codePoint := byte.
	bytesBlock ifNotNil: [
		Character
			evaluateOnce: bytesBlock
			withUtf8BytesOf: codePoint ].
	codePointBlock ifNotNil: [
		codePointBlock value: codePoint ].
	^1
]

{ #category : 'converting' }
Character class >> utf8BytesOf: aCodePoint [

	^ ByteArray streamContents: [ :strm |
		Character
			evaluate: [ :byte |
				strm nextPut: byte ]
			withUtf8BytesOf: aCodePoint ]
]

{ #category : 'instance creation' }
Character class >> value: anInteger [
	"Answer the Character whose value is anInteger."
	<primitive: 170>
	^self primitiveFailed
]

{ #category : 'class initialization' }
Character class >> vowels [
	"Answer a String with all the vowels"
	
	^'AaÀàÁáÂâÃãÄäÅåEeÈèÉéÊêËëIiÌìÍíÎîÏïOoÒòÓóÔôÕõÖöUuÙùÚúÛûÜü'
]

{ #category : 'accessing untypeable characters' }
Character class >> zeroWidthSpace [

	^ `Character codePoint: 16r200B`
]

{ #category : 'comparing' }
Character >> < aCharacter [
	"Compare with the standard case sensitive collation order."

	self == aCharacter ifTrue: [ ^false ].

	(self codePoint < 256 and: [ aCharacter codePoint < 256 ]) ifTrue: [
		^String does: self collateBefore: aCharacter ].

	^self collatesBefore: aCharacter.
]

{ #category : 'comparing' }
Character >> <= aCharacter [
	"Answer whether the receiver is less than or equal to the argument."
	"Reduce all cases to #< and #= to simplify code."

	self == aCharacter ifTrue: [ ^true ].
	^self < aCharacter.
]

{ #category : 'comparing' }
Character >> > aCharacter [
	"Answer whether the receiver is greater than the argument."
	"Reduce all cases to #< and #= to simplify code."

	self == aCharacter ifTrue: [ ^false ].
	^(self < aCharacter) not.
]

{ #category : 'comparing' }
Character >> >= aCharacter [
	"Answer whether the receiver is greater than or equal to the argument."
	"Reduce all cases to #< and #= to simplify code."

	^(self < aCharacter) not.
]

{ #category : 'converting' }
Character >> asLowercase [
self flag: #unicodeNeedsImprovement.
	self codePoint > 255 ifTrue: [ ^self ].
	^ Character lowercaseMappingTable at: self codePoint + 1.
]

{ #category : 'converting' }
Character >> asString [
	^self codePoint > 255
		ifTrue: [ UnicodeString with: self ]
		ifFalse: [ String with: self ].
]

{ #category : 'converting' }
Character >> asSymbol [
	^self asString asSymbol
]

{ #category : 'converting' }
Character >> asText [
	^ self asString asText
]

{ #category : 'converting' }
Character >> asUnaccented [
	"
	$A asUnaccented
	$Á asUnaccented
	(0 to: 255) collect: [ :i | (Character codePoint: i) asUnaccented ]
	(0 to: 255) collect: [ :i | (Character codePoint: i) asUnaccented  asLowercase]
	"
self flag: #unicodeNeedsImprovement.
	self codePoint > 255 ifTrue: [ ^self ].
	^ Character
		codePoint: (self class unaccentedTable at: self codePoint + 1)
]

{ #category : 'converting' }
Character >> asUppercase [
	"If the receiver is lowercase, answer its matching uppercase Character."
self flag: #unicodeNeedsImprovement.
	self codePoint > 255 ifTrue: [ ^self ].
	^ Character uppercaseMappingTable at: self codePoint + 1.
]

{ #category : 'accessing' }
Character >> asciiValue [
	"Answer the value of the receiver that represents its ASCII encoding.
	This is the same as the Unicode Code Point for the first 128 characters.
	Answer nil if out of range, i.e. we are not part of the ASCII Character set.

	Note: Senders, please assume this method could be redefined to answer
	the #codePoint even if not ASCII. Please protect yourselves from this."

	^self isAscii ifTrue: [ self codePoint ].
]

{ #category : 'testing' }
Character >> canBeGlobalVarInitial [
	^self isUppercase
]

{ #category : 'accessing' }
Character >> codePoint [
	"Code points are the numbers assigned by the Unicode Consortium to every character in every writing system."
	"
	self assert: $A codePoint hex = '16r41'.
	self assert: $€ codePoint hex = '16r20AC'.
	"
	<primitive: 171>
	^self primitiveFailed
]

{ #category : 'comparing' }
Character >> collatesBefore: aCharacter [
	"Compare with the standard case sensitive collation order.
	Sort by #collatingCategory, and if same category,
	sort by unaccented version of the character."

	| myCat theirCat |
	self == aCharacter ifTrue: [ ^false ].

	myCat := self collatingCategory.
	theirCat := aCharacter collatingCategory.
	myCat = theirCat ifFalse: [
		"Lower collating category goes first"
		^myCat < theirCat ].

	self isLetter ifTrue: [ "Then both are"
		| meWithoutDiacritics theyWithoutDiacritics |
		meWithoutDiacritics := self  asUnaccented.
		theyWithoutDiacritics := aCharacter asUnaccented.
		"If not the same base character, compare base character"
		meWithoutDiacritics = theyWithoutDiacritics ifFalse: [
			^meWithoutDiacritics codePoint < theyWithoutDiacritics codePoint ].
		"Same base character... just compare code points"
		^self codePoint < aCharacter codePoint ].
	
	"Non-letters in same collating category? -> Lower codePoint goes first."
	^self codePoint < aCharacter codePoint.
]

{ #category : 'comparing' }
Character >> collatingCategory [
	"Needs to be consistent with #basicCaseSensitiveOrder"
	"
	0: Character null
	1: Separators
	2: #isSymbolLine
	3: Decimal digits
	4: Uppercase Letters
	5: Lowercase Letters
	6: other
	"
	self codePoint = 0 ifTrue: [ ^0 ].
	self isSeparator ifTrue: [ ^1 ].
	self isSymbolLike ifTrue: [ ^2 ].
	self isDigit ifTrue: [ ^3 ].
	self isUppercase ifTrue: [ ^4 ].
	self isLowercase ifTrue: [ ^5 ].
	^6
]

{ #category : 'accessing' }
Character >> digitValue [
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 
	otherwise. This is used to parse literal numbers of radix 2-36.
	$0 numericValue = 48
	$9 numericValue = 57
	$A numericValue = 65
	$Z numericValue = 90 
	$7 digitValue = 7 
	"

	| nv |
	nv := self codePoint.
	(nv between: 48 and: 57)
		ifTrue: [ ^ nv - 48 ].
	(nv between: 65 and: 90)
		ifTrue: [ ^ nv - 55 ].
	^ -1
]

{ #category : 'comparing' }
Character >> hash [
	"Hash is reimplemented because #hash for long Strings relies on it, and especially on being the same as the codePoint's for the ASCII range. Also see the comment at Integer>>#hash.
	| longString |
	longString := (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars! ' ].
	self assert: longString asByteString hash = longString asUnicodeString hash.
	self assert: longString asByteString = longString asUnicodeString.
	"
	^self codePoint hash
]

{ #category : 'printing' }
Character >> hex [
	self codePoint < 256 ifTrue: [
		^ String
			with: ('0123456789ABCDEF' at: self codePoint//16+1)
			with: ('0123456789ABCDEF' at: self codePoint\\16+1) ].
	^self codePoint printStringBase: 16 length: 6 padded: true
]

{ #category : 'comparing' }
Character >> identityHash [
	"Answer the receiver's character code."
	<primitive: 171>
	^self primitiveFailed
]

{ #category : 'testing' }
Character >> isAccented [
	^self isLetter and: [ self asUnaccented ~~ self ]
]

{ #category : 'testing' }
Character >> isAlphaNumeric [
	"Answer whether the receiver is a letter or a digit."
	^self isLetter or: [self isDigit]
]

{ #category : 'testing' }
Character >> isAscii [
	"Am I a 7-bit ASCII Character?"

	^ self codePoint < 128
]

{ #category : 'testing' }
Character >> isCharacter [
	^true
]

{ #category : 'testing' }
Character >> isDiacriticalMark [
	"
	(Character codePoint: 16r0041) isDiacriticalMark
	(Character codePoint: 16r20D7) isDiacriticalMark
	"
	| i |
	i := self codePoint + 1.
	DiacriticalMarksTable bitSize < i ifTrue: [ ^false ].
	^ DiacriticalMarksTable bitBooleanAt: i.
]

{ #category : 'testing' }
Character >> isDigit [
	"Answer whether the receiver is a digit."

	^ self codePoint between: 48 and: 57
]

{ #category : 'fileman-testing' }
Character >> isDriveLetter [
	^'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' includes: self
]

{ #category : 'fileman-testing' }
Character >> isDriveSeparator [
	^self == $:
	
]

{ #category : 'testing' }
Character >> isLetter [
	"Answer whether the receiver is a letter."
	"
	(Character codePoint: 16r22C7) isLetter
	(Character codePoint: 16r042F) isLetter
	(Character codePoint: 16r0431) isLetter
	"
	^self isLowercase or: [ self isUppercase ]
]

{ #category : 'testing' }
Character >> isLineSeparator [
	"Answer whether the receiver is a line separator character:
	line feed, cr, or form feed."

	^ #(10 13 12 16r0085 16r2028 16r2029) statePointsTo: self codePoint
]

{ #category : 'printing' }
Character >> isLiteral [
	^true
]

{ #category : 'testing' }
Character >> isLowercase [
	"Answer whether the receiver is a letter."
	"
	(Character codePoint: 16r22C7) isLowercase
	(Character codePoint: 16r042F) isLowercase
	(Character codePoint: 16r0431) isLowercase
	"
	| i |
	"If no UnicodeData.txt tables, consider only ASCII"
	LowercaseLettersTable isNil ifTrue: [
		^ self codePoint between: 97 and: 122 ].
		
	i := self codePoint + 1.
	LowercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^LowercaseLettersTable bitBooleanAt: i.
]

{ #category : 'testing' }
Character >> isNullCharacter [
	"Answer whether the receiver is the NULL character."

	^ self codePoint = 0
]

{ #category : 'fileman-testing' }
Character >> isPathSeparator [

	^CharacterSequence pathSeparators includes: self
]

{ #category : 'testing' }
Character >> isSeparator [
	"Answer whether the receiver is one of the separator characters"
	"Also see #separators"

	^ #(32 "space"
		9 "tab"
		10 "line feed"
		13 "cr"
		12 "form feed"
		160 "non-breaking space"
		8203 "Character zeroWidthSpace")
			statePointsTo: self codePoint
]

{ #category : 'testing' }
Character >> isSymbolLike [
	"
	(Character codePoint: 16r0041) isSymbolLike
	(Character codePoint: 16r002B) isSymbolLike
	(Character codePoint: 16r03F6) isSymbolLike
	(Character codePoint: 16r2140) isSymbolLike
	"
	| i |
	i := self codePoint + 1.
	SymbolsLikeTable bitSize < i ifTrue: [ ^false ].
	^ SymbolsLikeTable bitBooleanAt: i.
]

{ #category : 'testing' }
Character >> isUnaccented [
	^self isLetter and: [ self asUnaccented == self ]
]

{ #category : 'testing' }
Character >> isUppercase [
	"Answer whether the receiver is an uppercase letter."
	"
	(Character codePoint: 16r22C7) isUppercase
	(Character codePoint: 16r042F) isUppercase
	(Character codePoint: 16r0431) isUppercase
	"
	| i |
	"If no UnicodeData.txt tables, consider only ASCII"
	UppercaseLettersTable isNil ifTrue: [
		^ self codePoint between: 65 and: 90 ].

	i := self codePoint + 1.
	UppercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^UppercaseLettersTable bitBooleanAt: i.
]

{ #category : 'testing' }
Character >> isValidInBinarySelectors [
	"Can be part of a binary selector?
		$< isValidInBinarySelectors
		$| isValidInBinarySelectors
		$^ isValidInBinarySelectors
		$: isValidInBinarySelectors
	(Character codePoint: 16r22C7) isValidInBinarySelectors
	(Character codePoint: 16r042F) isValidInBinarySelectors
	(Character codePoint: 16r0431) isValidInBinarySelectors
	(Character codePoint: 16r20D7) isValidInBinarySelectors
	"
	self isNullCharacter ifTrue: [ ^false ].
	^#(verticalBar upArrow xColon xBinary xDiacritical) statePointsTo: (Scanner typeTableAt: self).
]

{ #category : 'testing' }
Character >> isValidInFilenames [
	"Can be part of a filename?
		$< isValidInFilenames
		$a isValidInFilenames
		$| isValidInFilenames
		$^ isValidInFilenames
		$: isValidInFilenames
	"
self flag: #unicodeNeedsImprovement.
	self isNullCharacter ifTrue: [ ^false ].
	self isSeparator ifTrue: [ ^false ].
	^(#( $: $< $> $| $/ $\ $? $* $") statePointsTo: self) not.
]

{ #category : 'testing' }
Character >> isValidInIdentifiers [
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	self isNullCharacter ifTrue: [ ^false ].
	self == $′ ifTrue: [ ^true ]. 		"Also see #xLetter"
	^#(xLetter xDigit xUnderscore xDiacritical) statePointsTo: (Scanner typeTableAt: self).
]

{ #category : 'testing' }
Character >> isValidStartOfIdentifiers [
	"Can I be the first character of an identifier? (unary or keyword selector, or variable name)"

	self isNullCharacter ifTrue: [ ^false ].
	^#(xLetter xUnderscore) statePointsTo: (Scanner typeTableAt: self).
]

{ #category : 'testing' }
Character >> isVowel [
	"Answer whether the receiver is one of the vowels, AEIOU, in upper or 
	lower case, and with various diacritical marks."

self flag: #unicodeNeedsImprovement.
	^Character vowels includes: self
]

{ #category : 'printing' }
Character >> printAsLiteralOn: aStream [
	aStream nextPut: $$; nextPut: self
]

{ #category : 'printing' }
Character >> printOn: aStream [
	| integerValue |
	((integerValue := self codePoint) > 32 and: [integerValue ~= 127]) ifTrue:
		[^self printAsLiteralOn: aStream].
	(self class constantNameFor: self)
		ifNotNil: [ :name | aStream nextPutAll: 'Character '; nextPutAll: name ]
		ifNil: [ aStream nextPutAll: 'Character value: '; print: integerValue ]
]

{ #category : 'copying' }
Character >> shallowCopy [
	"Answer the receiver, because Characters are unique."
	^self
]

{ #category : 'testing' }
Character >> shouldBePrintedAsLiteral [

	| integerValue |
	(integerValue := self codePoint) < 33 ifTrue: [ ^false ].
	^integerValue ~= 127
]

{ #category : 'testing' }
Character >> shouldBePrintedAsLiteralVisiting: aSet [

	^self shouldBePrintedAsLiteral
]

{ #category : 'printing' }
Character >> storeOn: aStream [
	"Common character literals are preceded by '$', however special need to be encoded differently: for some this might be done by using one of the shortcut constructor methods for the rest we have to create them by ascii-value."

	self shouldBePrintedAsLiteral ifTrue:
		[^self printAsLiteralOn: aStream].
	(self class constantNameFor: self)
		ifNotNil:
			[ :name | aStream nextPutAll: self class name; space; nextPutAll: name]
		ifNil:
			[aStream nextPutAll: '(Character value: '; print: self codePoint; nextPut: $)]
]

{ #category : 'converting' }
Character >> to: other [
	"Answer with a collection in ascii order -- $a to: $z"
	^ (self codePoint to: other codePoint) collect: [ :cp |
		Character codePoint: cp ]
]

{ #category : 'testing' }
Character >> tokenish [
	"Answer whether the receiver is a valid token-character--letter, digit, or 
	colon."

	^self isValidInIdentifiers or: [self == $:]
]

{ #category : 'converting' }
Character >> withDiacriticalMark: codePointOrCharacter [
	"Answer the character resulting from adding a diacritical mark (accent) to a letter.
	If the result is unsupported in ISO 8859-1, answer the receiver.
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
	^answer == $- ifFalse: [answer] ifTrue: [self]
]
