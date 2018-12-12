'From Cuis 5.0 of 7 November 2016 [latest update: #3512] on 4 December 2018 at 12:26:03 pm'!

!Float methodsFor: 'printing' stamp: 'jmv 12/3/2018 18:27:17'!
hex
	^ String streamContents: [ :strm | | word nibble |
		1 to: 2 do: [ :i |
			word := self at: i.
			1 to: 8 do: [ :s |
				nibble := (word bitShift: -8+s*4) bitAnd: 16rF.
				strm nextPut: ('0123456789ABCDEF' at: nibble+1)]]]
"
(-2.0 to: 2.0) collect: [:f | f hex]
"! !

