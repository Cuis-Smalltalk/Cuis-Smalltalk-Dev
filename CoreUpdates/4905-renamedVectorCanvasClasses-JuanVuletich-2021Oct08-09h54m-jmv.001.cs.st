'From Cuis 5.0 [latest update: #4904] on 8 October 2021 at 9:54:33 am'!

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 10/8/2021 09:46:17'!
activeSubclass: aMorphicCanvasSubclass
	"
	self runningWorld canvas class
	MorphicCanvas activeSubclass: BitBltCanvas
	MorphicCanvas activeSubclass: HybridCanvas
	MorphicCanvas activeSubclass: VectorCanvas
	"
	(#(BitBltCanvas HybridCanvas VectorCanvas)
		includes: aMorphicCanvasSubclass name) ifFalse: [
			^self error: 'Invalid Canvas class' ].
	ActiveSubclass _ aMorphicCanvasSubclass.
	UISupervisor ui ifNotNil: [ :world |
		world whenUIinSafeState: [
			world setMainCanvas ]].! !

