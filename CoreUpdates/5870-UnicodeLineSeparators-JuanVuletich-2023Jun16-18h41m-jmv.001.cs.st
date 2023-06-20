'From Cuis 6.0 [latest update: #5867] on 16 June 2023 at 6:46:55 pm'!

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/16/2023 18:46:40'!
isLineSeparator
	"Answer whether the receiver is a line separator character:
	line feed, cr, or form feed."

	^ #(10 13 12 16r0085 16r2028 16r2029) statePointsTo: value! !

