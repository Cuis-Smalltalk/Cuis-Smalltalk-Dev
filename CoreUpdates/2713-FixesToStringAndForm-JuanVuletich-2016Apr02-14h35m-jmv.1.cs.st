'From Cuis 4.2 of 25 July 2013 [latest update: #2712] on 2 April 2016 at 2:55:15.867643 pm'!

!String methodsFor: 'accessing' stamp: 'jmv 4/2/2016 14:35'!
indexOfAnyOf: aCharacterSet
	"returns the index of the first character in the given set, starting from start"

	^String findFirstInString: self inSet: aCharacterSet byteArrayMap startingAt: 1! !


!Form methodsFor: 'comparing' stamp: 'jmv 4/2/2016 14:45'!
= other 
	"Compare for equal contents. Expensive!!
	See comment at #hash"

	^ (other class == self class) 
		and: [ other width = width
			and: [ other height = height
				and: [ other depth = depth
					and: [ other offset = self offset
						and: [ other bits = bits ]]]]]! !

