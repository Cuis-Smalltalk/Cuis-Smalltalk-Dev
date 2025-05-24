'From Cuis7.3 [latest update: #7138] on 16 April 2025 at 9:58:00 am'!

!VectorEngineWithPlugin methodsFor: 'accessing' stamp: 'jmv 4/16/2025 09:55:27'!
clippingSpec: aClippingSpecOrNil
	clippingSpec := aClippingSpecOrNil.
	clippingSpec
		ifNil: [ self primClearClippingSpec ]
		ifNotNil: [
			clippingSpec pin.
			self primSetClippingSpec: clippingSpec ].! !

