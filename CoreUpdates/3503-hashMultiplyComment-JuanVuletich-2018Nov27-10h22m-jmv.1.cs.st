'From Cuis 5.0 of 7 November 2016 [latest update: #3501] on 27 November 2018 at 10:24:18 am'!
!classDefinition: #ColorExtrasPackage category: #'Color-Extras'!
CodePackage subclass: #ColorExtrasPackage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Color-Extras'!

!SmallInteger methodsFor: 'bit manipulation' stamp: 'SqR 8/3/2000 13:29'!
hashMultiply
	"Multiply by 1664525, take lower 28 bits, do not use LargeIntegers (not even in 32 bit images)"
	| low |

	low _ self bitAnd: 16383.
	^(16r260D * low + ((16r260D * (self bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384))
			bitAnd: 16r0FFFFFFF! !

!classMoveToSomePackage: #ColorExtrasPackage!
Smalltalk removeClassNamedIfInBaseSystem: #ColorExtrasPackage!
