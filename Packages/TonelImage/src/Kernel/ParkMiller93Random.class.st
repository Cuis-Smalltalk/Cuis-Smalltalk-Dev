"
The 1993 version of the Park-Miller RNG.
"
Class {
	#name : 'ParkMiller93Random',
	#superclass : 'ParkMiller88Random',
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'private' }
ParkMiller93Random >> a [
	"See https://en.wikipedia.org/wiki/Lehmer_random_number_generator"

	^48271.0
]
