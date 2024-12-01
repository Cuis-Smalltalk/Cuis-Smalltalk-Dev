'From Cuis7.1 [latest update: #6770] on 1 December 2024 at 1:41:12 am'!

!SymbolSet methodsFor: 'private' stamp: 'sqr 12/1/2024 01:40:24'!
rehashSymbolClass: aClass

	aClass allInstances do:
		[:symbol | self basicInternNew: symbol withHash: symbol hash]! !


!SymbolSet methodsFor: 'lookup' stamp: 'sqr 12/1/2024 01:40:51'!
rehash

	| newBuckets |
	newBuckets := self newBuckets: self newBucketCount sized: self initialBucketSize.
	self buckets: newBuckets.
	self rehashSymbolClass: Symbol.
	self rehashSymbolClass: UnicodeSymbol! !

