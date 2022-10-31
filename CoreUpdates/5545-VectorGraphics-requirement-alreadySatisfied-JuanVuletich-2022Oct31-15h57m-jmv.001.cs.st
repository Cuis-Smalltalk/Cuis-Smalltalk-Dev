'From Cuis 6.0 [latest update: #5544] on 31 October 2022 at 3:58:11 pm'!

!FeatureRequirement methodsFor: 'testing' stamp: 'jmv 10/31/2022 15:57:48'!
isAlreadySatisfied
	"Answer true if requirement is satisfied by some installed package, or by base system."

	(Feature baseSystemFeature satisfies: self)
		ifTrue: [ ^true ].

	"VectorGraphics is now part of the base Cuis image, and therefore already satisfied."
	name = 'VectorGraphics' ifTrue: [ ^true ].

	^CodePackage installedPackages anySatisfy: [ :package |
		package provides satisfies: self ]! !

