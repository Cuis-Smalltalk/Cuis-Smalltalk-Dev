'From Cuis7.5 [latest update: #7609] on 24 September 2025 at 2:45:34 pm'!

!LargePositiveInteger methodsFor: 'object serialization' stamp: 'hjh 9/24/2025 19:18:17'!
comeFullyUpOnReload: smartRefStream
	"Squeak may serialize SmallIntegers as LargeIntegers. Convert them back to SmallInteger if needed."
         ^self normalize! !

