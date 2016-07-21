'From Cuis 4.2 of 25 July 2013 [latest update: #2851] on 20 July 2016 at 5:23:37 pm'!

!Form methodsFor: 'comparing' stamp: 'jmv 7/20/2016 15:25:39'!
= other 
	"Compare for equal contents. Expensive!!
	See comment at #hash"

	^ (other class == self class) 
		and: [ other width = width
			and: [ other height = height
				and: [ other depth = self depth
					and: [ other offset = self offset
						and: [ other bits = bits ]]]]]! !

