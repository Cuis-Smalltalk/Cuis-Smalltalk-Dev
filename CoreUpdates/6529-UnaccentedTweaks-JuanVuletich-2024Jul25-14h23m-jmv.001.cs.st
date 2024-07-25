'From Cuis7.1 [latest update: #6528] on 25 July 2024 at 2:39:16 pm'!

!String class methodsFor: 'static state' stamp: 'jmv 7/25/2024 14:36:28'!
basicCaseInsensitiveOrder
	"Case insensitive compare sorts null, space, digits, letters, all the rest..."
	| newOrder order |
	newOrder := Array new: 256.
	order := -1.
	newOrder at: 0+1 put:  (order := order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order := order+1)].
	#(92 94 124 126 183 215 247) do: [ :c | "\^|~·÷×"
		newOrder at: c + 1 put: (order := order+1)].
	16r90 to: 16r9F do: [:c| "special math operators"
		newOrder at: c + 1 put: (order := order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		order := order+1.
		newOrder at: upperAndLowercase first codePoint + 1 put: order.
		(upperAndLowercase first = $ÿ or: [upperAndLowercase first = $ß]) ifFalse: [
			newOrder at: upperAndLowercase second codePoint + 1 put: order ]].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order := order+1)]].
	^newOrder asByteArray.! !

!String class methodsFor: 'static state' stamp: 'jmv 7/25/2024 14:36:42'!
basicCaseSensitiveOrder
	"Case sensitive compare sorts null, space, digits, letters, all the rest..."
	| newOrder order lowercase |
	newOrder := Array new: 256.
	order := -1.
	newOrder at: 0+1 put:  (order := order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order := order+1)].
	#(92 94 124 126 183 215 247) do: [ :c | "\^|~·÷×"
		newOrder at: c + 1 put: (order := order+1)].
	16r90 to: 16r9F do: [:c| "special math operators"
		newOrder at: c + 1 put: (order := order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		(upperAndLowercase first = $ÿ or: [upperAndLowercase first = $ß]) ifFalse: [
			newOrder at: upperAndLowercase first codePoint + 1 put: (order := order+1) ]].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		lowercase := (upperAndLowercase first = $ÿ or: [upperAndLowercase first = $ß])
			ifTrue: [ upperAndLowercase first ]
			ifFalse: [ upperAndLowercase second ].
		newOrder at: lowercase codePoint + 1 put: (order := order+1) ].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order := order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	^newOrder asByteArray.! !

!String class methodsFor: 'static state' stamp: 'jmv 7/25/2024 14:37:08'!
caseInsensitiveOrder
	^`String basicCaseInsensitiveOrder`.! !

!String class methodsFor: 'static state' stamp: 'jmv 7/25/2024 14:37:12'!
caseSensitiveOrder
	^`String basicCaseSensitiveOrder`.! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 7/25/2024 14:36:05'!
uppercaseLowercaseAndUnaccentedLetters
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
	).! !

!Character class methodsFor: 'static state' stamp: 'jmv 7/25/2024 14:36:48'!
basicLookupTables
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
		(group first = $ÿ or: [group first = $ß])
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
	^{ lowercaseMappingTable. uppercaseMappingTable. unaccentedTable}.! !

!Character class methodsFor: 'static state' stamp: 'jmv 7/25/2024 14:37:20'!
lowercaseMappingTable
	^`Character basicLookupTables at: 1`.! !

!Character class methodsFor: 'static state' stamp: 'jmv 7/25/2024 14:37:25'!
unaccentedTable
	^`Character basicLookupTables at: 3`.! !

!Character class methodsFor: 'static state' stamp: 'jmv 7/25/2024 14:37:29'!
uppercaseMappingTable
	^`Character basicLookupTables at: 2`.! !

