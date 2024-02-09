'From Cuis6.3 [latest update: #6223] on 9 February 2024 at 1:16:59 pm'!

!OrderedCollection methodsFor: 'copying' stamp: 'len 2/9/2024 13:15:46'!
copyFrom: startIndex to: endIndex
	"Answer a copy of the receiver that contains elements from position startIndex to endIndex."
	| delta newArray |
	delta := firstIndex - 1.
	newArray := array copyFrom: startIndex + delta to: endIndex + delta.
	^ OrderedCollection basicNew setContents: newArray.! !

