'From Haver6.3 [latest update: #6206] on 8 February 2024 at 3:10:35 pm'!

!OrderedDictionary methodsFor: 'enumerating' stamp: 'KLG 2/8/2024 14:50:59'!
reverseDo: aBlock
	"Evaluate aBlock for each of the receiver's values in reverse order."

	orderedKeys reverseDo: [ :key |
		aBlock value: (self at: key ifAbsent: nil) ]! !

