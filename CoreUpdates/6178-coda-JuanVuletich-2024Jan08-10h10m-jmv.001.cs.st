'From Cuis6.3 [latest update: #6170] on 8 January 2024 at 10:14:17 am'!

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 10:10:47'!
basicLookupTables
	| lowercaseMappingTable uppercaseMappingTable unaccentedTable |
	lowercaseMappingTable := Array new: 256.
	uppercaseMappingTable := Array new: 256.
	unaccentedTable := ByteArray new: 256.
	0
		to: 255
		do: [ :idx | | char |
			"Default to an identity mapping with a false truth mapping"
			char := self numericValue: idx.
			lowercaseMappingTable
				at: idx + 1 put: char.
			uppercaseMappingTable
				at: idx + 1 put: char.
			unaccentedTable at: idx + 1 put: idx].
	"Now override as needed"
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :group | | uppercase lowercase |
		group size > 1
			ifTrue: [ | lowercaseChar uppercaseChar |
				uppercase := group first numericValue.
				lowercase := group second numericValue.
				lowercaseChar := self numericValue: lowercase.
				uppercaseChar := self numericValue: uppercase.
				lowercaseMappingTable
					at: lowercase + 1 put: lowercaseChar;
					at: uppercase + 1 put: lowercaseChar.
				uppercaseMappingTable
					at: lowercase + 1 put: uppercaseChar;
					at: uppercase + 1 put: uppercaseChar.
				group size > 2
					ifTrue: [|unaccentedUppercase unaccentedLowercase|
						unaccentedUppercase := group third numericValue.
						unaccentedLowercase := group fourth numericValue.
						unaccentedTable
							at: uppercase+1 put: unaccentedUppercase;
							at: lowercase+1 put: unaccentedLowercase]]
			ifFalse: [ | lowercaseChar |
				lowercase := group first numericValue.
				lowercaseChar := self numericValue: lowercase.
				lowercaseMappingTable
					at: lowercase + 1 put: lowercaseChar.
				uppercaseMappingTable
					at: lowercase + 1 put: lowercaseChar ]].
	^{ lowercaseMappingTable. uppercaseMappingTable. unaccentedTable}.! !

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 10:10:52'!
lowercaseMappingTable
	^`Character basicLookupTables at: 1`.! !

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 10:10:54'!
unaccentedTable
	^`Character basicLookupTables at: 3`.! !

!Character class methodsFor: 'static state' stamp: 'jmv 1/8/2024 10:10:57'!
uppercaseMappingTable
	^`Character basicLookupTables at: 2`.! !


!String class methodsFor: 'static state' stamp: 'jmv 1/8/2024 10:10:18'!
tokenishTable
	"a table for testing tokenish (for fast numArgs)"
	^`String withAll: (
		((0 to: 255) collect: [ :c | (Character numericValue: c) ])
			collect: 	[ :c | c tokenish ifTrue: [c] ifFalse: [$~]]	)`.! !

