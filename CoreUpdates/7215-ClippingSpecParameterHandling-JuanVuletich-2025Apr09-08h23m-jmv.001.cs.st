'From Cuis7.3 [latest update: #7121] on 9 April 2025 at 8:31:31 am'!

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 4/9/2025 08:29:28'!
primClearClippingSpec
	<primitive: 'primClearClippingSpec' module: 'VectorEnginePlugin'>
	self primitiveFailed! !


!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 4/9/2025 08:30:45'!
clippingSpec: aClippingSpecOrNil
	clippingSpec := aClippingSpecOrNil.! !


!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 4/9/2025 08:29:02'!
primSetClippingSpec: anIntegerArray
	<primitive: 'primSetClippingSpec' module: 'VectorEnginePlugin'>
	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'accessing' stamp: 'jmv 4/9/2025 08:31:07'!
clippingSpec: aClippingSpecOrNil
	clippingSpec := aClippingSpecOrNil.
	clippingSpec
		ifNil: [ self primClearClippingSpec ]
		ifNotNil: [ self primSetClippingSpec: clippingSpec ].! !

!methodRemoval: VectorEngineWithPlugin #primClearClippingSpec: stamp: 'jmv 4/9/2025 08:29:33'!
VectorEngineWithPlugin removeSelector: #primClearClippingSpec:!
